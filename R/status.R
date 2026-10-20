# Helpers used by the pages (index.qmd and packages.qmd) to read and
# display the package status written by scripts/build_status.R.

library(htmltools)

# show missing values as blank in tables
options(knitr.kable.NA = "")

read_status <- function(file = "data/status.json") {
  jsonlite::fromJSON(file, simplifyVector = FALSE)
}

# NULL to NA, for building data frames
`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x

# one row per package, with the columns shown in the tables
status_table <- function(status) {
  rows <- lapply(status$packages, function(p) {
    data.frame(
      package = p$package,
      title = p$title %||% NA_character_,
      maintainer = p$maintainer %||% NA_character_,
      health = p$health,
      github_url = p$github$url,
      github_version = p$github$version %||% NA_character_,
      cran_status = p$cran$status,
      cran_version = p$cran$version %||% NA_character_,
      cran_url = p$cran$url %||% NA_character_,
      cran_checks = p$cran$checks$worst %||% NA_character_,
      cran_checks_url = p$cran$checks$url %||% NA_character_,
      universe_build = p$runiverse$build_status %||% NA_character_,
      universe_checks = p$runiverse$checks$worst %||% NA_character_,
      universe_url = p$runiverse$url %||% NA_character_,
      unreleased = isTRUE(p$unreleased),
      direct_dependencies = length(p$direct_dependencies),
      all_dependencies = length(p$all_dependencies),
      open_issues = p$github$open_issues %||% NA_integer_,
      open_prs = p$github$open_prs %||% NA_integer_,
      last_commit = as.Date(substr(p$github$last_commit %||% NA_character_, 1, 10)),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# the problems listed for each package, one row per problem
status_issues <- function(status, levels = c("error", "warning", "info")) {
  rows <- lapply(status$packages, function(p) {
    if (!length(p$issues)) return(NULL)
    data.frame(
      package = p$package,
      github_url = p$github$url,
      level = vapply(p$issues, `[[`, "", "level"),
      text = vapply(p$issues, `[[`, "", "text"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[out$level %in% levels, ]
  out[order(match(out$level, c("error", "warning", "info")), tolower(out$package)), ]
}

updated <- function(status) {
  format(as.POSIXct(status$generated, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
         "%d %B %Y, %H:%M UTC")
}


# badges ----------------------------------------------------------------------

# Bootstrap colours used for each status
status_colour <- c(
  ok = "success", OK = "success", success = "success", cran = "success",
  NOTE = "warning", warning = "warning", WARNING = "warning", archived = "warning",
  error = "danger", ERROR = "danger", FAILURE = "danger", failure = "danger",
  info = "info", none = "secondary"
)

badge <- function(text, colour = "secondary", href = NULL, title = NULL) {
  tag <- span(class = paste0("badge rounded-pill text-bg-", colour), title = title, text)
  if (!is.null(href) && !is.na(href)) a(href = href, tag) else tag
}

health_label <- c(ok = "OK", warning = "Needs attention", error = "Action needed")

health_badge <- function(health) {
  badge(health_label[[health]], status_colour[[health]])
}

cran_badge <- function(status, version, url, comment = NULL) {
  switch(status,
    cran = badge(version, "success", url, "On CRAN"),
    archived = badge("archived", "warning", url, comment),
    badge("not on CRAN", "secondary")
  )
}

check_badge <- function(result, url = NULL) {
  if (is.na(result)) return(span(class = "text-body-secondary", "–"))
  badge(result, status_colour[[result]], url)
}

universe_badge <- function(build, checks, url) {
  if (is.na(build)) return(badge("not listed", "secondary"))
  if (build != "success") return(badge("build failed", "danger", url))
  check_badge(if (is.na(checks)) "OK" else checks, url)
}


# package and API pages -------------------------------------------------------------

find_package <- function(status, pkg) {
  status$packages[[match(pkg, vapply(status$packages, `[[`, "", "package"))]]
}

listed_packages <- function(status) vapply(status$packages, `[[`, "", "package")

# link to a package page, if the package is listed; 'from' is the folder of
# the page the link is on
package_link <- function(pkg, status, from = ".") {
  listed <- pkg %in% listed_packages(status)
  prefix <- if (from == "package") "" else if (from == ".") "package/" else "../package/"
  ifelse(listed, sprintf("[%s](%s%s.html)", pkg, prefix, pkg), pkg)
}

read_apis <- function(file = "data/apis.json") {
  jsonlite::fromJSON(file, simplifyVector = FALSE)
}

# APIs a package calls directly, and through the packages it depends on
package_apis <- function(apis, p) {
  direct <- Filter(function(a) p$package %in% unlist(a$packages), apis$apis)
  via <- Filter(function(a) any(unlist(a$packages) %in% unlist(p$all_dependencies)), apis$apis)
  via <- Filter(function(a) !(p$package %in% unlist(a$packages)), via)
  list(direct = direct, via = via)
}

# packages that call an API directly, or through a dependency
api_packages <- function(api, status) {
  direct <- unlist(api$packages)
  via <- Filter(function(p) any(direct %in% unlist(p$all_dependencies)), status$packages)
  list(direct = direct, via = setdiff(vapply(via, `[[`, "", "package"), direct))
}

# one row per endpoint
endpoint_table <- function(api) {
  do.call(rbind, lapply(api$endpoints, function(e) {
    data.frame(
      name = e$name,
      url = e$url,
      functions = paste(unlist(e$functions), collapse = ", "),
      status = e$status,
      http_status = e$http_status %||% NA_integer_,
      time_s = e$time_s %||% NA_real_,
      size_kb = round((e$size_bytes %||% NA_real_) / 1024, 1),
      records = e$records %||% NA_integer_,
      fields = length(e$fields),
      changes = length(e$changes),
      stringsAsFactors = FALSE
    )
  }))
}

api_status_colour <- c(ok = "success", changed = "warning", "no baseline" = "secondary", error = "danger")
api_status_label <- c(ok = "No changes", changed = "Changed", "no baseline" = "No baseline", error = "Error")

# write markdown from a chunk with results: asis
md <- function(...) cat(..., "\n\n", sep = "")
md_list <- function(x, empty = "None.") {
  if (length(x)) cat(paste0("- ", x, collapse = "\n"), "\n\n") else md(empty)
}
md_table <- function(x, ...) {
  print(knitr::kable(x, format = "pipe", ...))
  cat("\n")
}
