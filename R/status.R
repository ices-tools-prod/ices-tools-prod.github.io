# Helpers used by the pages (index.qmd and packages.qmd) to read and
# display the package status written by scripts/build_status.R.

library(htmltools)

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
