# Collect the status of the packages listed in packages.json, and write it
# to data/status.json for the pages to display.
#
# Quarto runs this script before rendering the site (see pre-render in
# _quarto.yml). It can also be run from the repository root:
#
#   Rscript scripts/build_status.R
#
# Needs the jsonlite package, and the GitHub CLI (gh), either logged in or
# with the GH_TOKEN environment variable set (as in the GitHub Action).
#
# If data/status.json is less than max_age_hours old, it is not rebuilt,
# which keeps local previews quick. Set STATUS_REFRESH=true to force a rebuild.

library(jsonlite)

config_file <- "packages.json"
out_file <- file.path("data", "status.json")
max_age_hours <- 6
universes <- c("ices-tools-prod", "ices-tools-dev")
cran <- "https://cran.r-project.org"

# the tools::CRAN_*() functions read from this repository, which must be CRAN
# itself: package manager mirrors (e.g. p3m.dev) do not have the web/ files
options(repos = c(CRAN = cran))

# order of check results, from best to worst
check_levels <- c("OK", "NOTE", "WARNING", "ERROR", "FAILURE")


if (file.exists(out_file) && !identical(Sys.getenv("STATUS_REFRESH"), "true")) {
  age <- difftime(Sys.time(), file.mtime(out_file), units = "hours")
  if (age < max_age_hours) {
    message("using ", out_file, ", built ", round(age, 1), " hours ago")
    quit(save = "no")
  }
}


# helpers ---------------------------------------------------------------------

msg <- function(...) message(format(Sys.time(), "%H:%M:%S "), ...)

# call the GitHub API, returning NULL on failure
gh_api <- function(path, raw = FALSE) {
  args <- c("api", shQuote(path))
  if (raw) {
    args <- c(args, "-H", shQuote("Accept: application/vnd.github.raw"))
  }
  res <- suppressWarnings(system2("gh", args, stdout = TRUE, stderr = FALSE))
  status <- attr(res, "status")
  if (!is.null(status) && status != 0) {
    return(NULL)
  }
  res <- paste(res, collapse = "\n")
  if (raw) res else fromJSON(res, simplifyVector = FALSE)
}

# read a url, returning NULL on failure
try_read <- function(expr, what) {
  tryCatch(expr, error = function(e) {
    msg("could not read ", what, ": ", conditionMessage(e))
    NULL
  })
}

worst_check <- function(x) {
  x <- intersect(check_levels, toupper(x))
  if (length(x)) x[length(x)] else NA_character_
}

# Authors@R is evaluated with only person() and c() available, so that
# no other code in a DESCRIPTION file can run
safe_person <- function(x) {
  env <- new.env(parent = emptyenv())
  env$person <- utils::person
  env$c <- base::c
  tryCatch(eval(str2lang(x), env), error = function(e) NULL)
}

field <- function(d, name) {
  if (name %in% colnames(d) && !is.na(d[1, name])) unname(d[1, name]) else NA_character_
}

maintainer <- function(d) {
  m <- field(d, "Maintainer")
  if (!is.na(m)) {
    return(m)
  }
  authors <- field(d, "Authors@R")
  p <- if (!is.na(authors)) safe_person(authors) else NULL
  if (!inherits(p, "person")) {
    return(NA_character_)
  }
  cre <- p[vapply(p, function(x) "cre" %in% x$role, logical(1))]
  if (!length(cre)) {
    return(NA_character_)
  }
  format(cre[[1]], include = c("given", "family", "email"))
}

# package names from Depends, Imports and LinkingTo
dependencies <- function(d) {
  deps <- c(field(d, "Depends"), field(d, "Imports"), field(d, "LinkingTo"))
  deps <- unlist(strsplit(deps[!is.na(deps)], ","))
  deps <- trimws(gsub("\\(.*\\)", "", deps))
  setdiff(deps[nzchar(deps)], "R")
}

compare_versions <- function(a, b) {
  if (!length(a) || !length(b) || is.na(a) || is.na(b)) {
    return(NA_integer_)
  }
  tryCatch(utils::compareVersion(a, b), error = function(e) NA_integer_)
}


# shared data -----------------------------------------------------------------

config <- fromJSON(config_file, simplifyDataFrame = FALSE)
pkgs <- config$packages
msg("collecting status for ", length(pkgs), " packages")

msg("reading CRAN package database")
cran_db <- try_read(tools::CRAN_package_db(), "CRAN package database")

msg("reading CRAN check results")
cran_checks <- try_read(tools::CRAN_check_results(), "CRAN check results")
cran_issues <- try_read(tools::CRAN_check_issues(), "CRAN check issues")

msg("reading CRAN archive")
cran_archive <-
  try_read(
    readRDS(gzcon(url(paste0(cran, "/src/contrib/Meta/archive.rds")))),
    "CRAN archive"
  )
cran_overrides <-
  try_read(
    read.dcf(url(paste0(cran, "/src/contrib/PACKAGES.in"))),
    "CRAN PACKAGES.in"
  )

msg("reading r-universe")
runiverse <- list()
for (u in universes) {
  res <-
    try_read(
      fromJSON(
        paste0("https://", u, ".r-universe.dev/api/packages?limit=1000"),
        simplifyVector = FALSE
      ),
      paste(u, "r-universe")
    )
  for (p in res) {
    if (!is.null(p$Package) && is.null(runiverse[[p$Package]])) {
      p$universe <- u
      runiverse[[p$Package]] <- p
    }
  }
}


# per package -----------------------------------------------------------------

status <- lapply(pkgs, function(cfg) {
  pkg <- cfg$package
  repo <- cfg$repo
  msg(pkg)

  # GitHub
  gh_repo <- gh_api(paste0("repos/", repo))
  if (isTRUE(gh_repo$private)) {
    msg("skipping ", pkg, ": ", repo, " is a private repository")
    return(NULL)
  }
  desc <- gh_api(paste0("repos/", repo, "/contents/DESCRIPTION"), raw = TRUE)
  d <- if (!is.null(desc)) try_read(read.dcf(textConnection(desc), keep.white = "Authors@R"), paste(pkg, "DESCRIPTION")) else NULL

  github <- list(
    repo = repo,
    url = paste0("https://github.com/", repo),
    found = !is.null(gh_repo),
    archived = isTRUE(gh_repo$archived),
    default_branch = gh_repo$default_branch,
    description = gh_repo$description,
    homepage = if (length(gh_repo$homepage) && nzchar(gh_repo$homepage)) gh_repo$homepage else NULL,
    version = if (!is.null(d)) field(d, "Version") else NA_character_
  )

  if (!is.null(gh_repo)) {
    prs <- gh_api(paste0("repos/", repo, "/pulls?state=open&per_page=100"))
    github$open_prs <- length(prs)
    github$open_issues <- max(0, gh_repo$open_issues_count - length(prs))
    head <- gh_api(paste0("repos/", repo, "/commits/", gh_repo$default_branch))
    github$last_commit <- head$commit$committer$date
    github$last_commit_message <- if (!is.null(head)) strsplit(head$commit$message, "\n")[[1]][1] else NULL
  }

  # CRAN
  on_cran <- !is.null(cran_db) && pkg %in% cran_db$Package
  in_archive <- !is.null(cran_archive) && pkg %in% names(cran_archive)
  cran_status <- if (on_cran) "cran" else if (in_archive) "archived" else "none"

  cran_info <- list(status = cran_status)
  if (cran_status != "none") {
    cran_info$url <- paste0("https://CRAN.R-project.org/package=", pkg)
  }
  if (on_cran) {
    row <- cran_db[match(pkg, cran_db$Package), ]
    cran_info$version <- row$Version
    cran_info$published <- row$Published
    if ("Deadline" %in% names(row) && !is.na(row$Deadline)) {
      cran_info$deadline <- row$Deadline
    }

    if (!is.null(cran_checks)) {
      chk <- cran_checks[cran_checks$Package == pkg, ]
      cran_info$checks <- list(
        worst = worst_check(chk$Status),
        counts = as.list(table(factor(toupper(chk$Status), levels = check_levels))),
        url = paste0(cran, "/web/checks/check_results_", pkg, ".html")
      )
    }
    if (!is.null(cran_issues)) {
      cran_info$additional_issues <- I(unique(cran_issues$kind[cran_issues$Package == pkg]))
    }
  }
  if (!is.null(cran_overrides) && pkg %in% cran_overrides[, "Package"]) {
    comment <- cran_overrides[match(pkg, cran_overrides[, "Package"]), ]
    comment <- comment[grepl("^X-CRAN-Comment", names(comment)) & !is.na(comment)]
    if (length(comment)) {
      cran_info$comment <- gsub("\\s+", " ", trimws(comment[[1]]))
    }
  }
  if (in_archive) {
    archived <- cran_archive[[pkg]]
    cran_info$archived_versions <- nrow(archived)
    cran_info$first_release <- format(min(archived$mtime), "%Y-%m-%d")
  }

  # r-universe
  ru <- runiverse[[pkg]]
  universe <- NULL
  if (!is.null(ru)) {
    jobs <- ru[["_jobs"]]
    job_checks <- vapply(jobs, function(j) if (is.null(j$check)) NA_character_ else j$check, character(1))
    universe <- list(
      universe = ru$universe,
      url = paste0("https://", ru$universe, ".r-universe.dev/", pkg),
      version = ru$Version,
      build_status = ru[["_status"]],
      build_url = ru[["_buildurl"]],
      published = ru[["_published"]],
      checks = list(
        worst = worst_check(job_checks),
        failing = I(unique(vapply(jobs[toupper(job_checks) %in% c("WARNING", "ERROR", "FAILURE")], function(j) j$config, character(1))))
      )
    )
  }

  list(
    package = pkg,
    title = if (!is.null(d)) gsub("\\s+", " ", field(d, "Title")) else NA_character_,
    maintainer = if (!is.null(d)) maintainer(d) else NA_character_,
    notes = cfg$notes,
    dependencies = I(if (!is.null(d)) dependencies(d) else character(0)),
    github = github,
    cran = cran_info,
    runiverse = universe
  )
})
status <- Filter(Negate(is.null), status)
names(status) <- vapply(status, `[[`, "", "package")


# derived fields ----------------------------------------------------------------

listed <- names(status)
for (pkg in listed) {
  s <- status[[pkg]]

  # dependencies and reverse dependencies within the listed packages
  s$ices_dependencies <- I(intersect(s$dependencies, listed))
  s$ices_reverse_dependencies <-
    I(listed[vapply(status, function(x) pkg %in% x$dependencies, logical(1))])

  # is the GitHub version ahead of the CRAN version?
  s$unreleased <- isTRUE(compare_versions(s$github$version, s$cran$version) > 0)

  # things that need attention, most serious first
  issues <- list()
  add <- function(level, text) issues[[length(issues) + 1]] <<- list(level = level, text = text)

  if (!s$github$found) add("error", "GitHub repository not found")
  if (s$github$found && is.na(s$github$version)) add("error", "No DESCRIPTION file on the default branch")
  if (!is.null(s$cran$deadline)) add("error", paste("CRAN deadline:", s$cran$deadline, "- the package will be archived unless fixed"))
  if (identical(s$cran$checks$worst, "ERROR") || identical(s$cran$checks$worst, "FAILURE")) add("error", "CRAN checks have errors")
  if (s$cran$status == "archived") add("warning", paste("Archived on CRAN", if (!is.null(s$cran$comment)) paste0("(", s$cran$comment, ")") else ""))
  if (identical(s$cran$checks$worst, "WARNING")) add("warning", "CRAN checks have warnings")
  if (length(s$cran$additional_issues)) add("warning", paste("CRAN additional issues:", paste(s$cran$additional_issues, collapse = ", ")))
  if (!is.null(s$runiverse) && !identical(s$runiverse$build_status, "success")) add("warning", "r-universe build failed")
  failing <- s$runiverse$checks$failing
  if (length(failing) > 3) add("warning", paste("r-universe checks failing on", length(failing), "platforms"))
  else if (length(failing)) add("warning", paste("r-universe checks failing on:", paste(failing, collapse = ", ")))
  if (is.null(s$runiverse)) add("info", "Not on the ICES r-universe")
  if (s$github$archived) add("info", "GitHub repository is archived")
  if (s$unreleased) add("info", paste0("Unreleased changes: ", s$github$version, " on GitHub, ", s$cran$version, " on CRAN"))
  if (isTRUE(compare_versions(s$github$version, s$cran$version) < 0)) {
    add("warning", paste0("GitHub version (", s$github$version, ") is older than CRAN (", s$cran$version, ")"))
  }

  s$issues <- issues
  levels <- vapply(issues, `[[`, "", "level")
  s$health <- if ("error" %in% levels) "error" else if ("warning" %in% levels) "warning" else "ok"

  status[[pkg]] <- s
}


# write -----------------------------------------------------------------------

out <- list(
  generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  universes = universes,
  packages = unname(status)
)

dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
writeLines(
  toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"),
  out_file
)
msg("wrote ", out_file)
