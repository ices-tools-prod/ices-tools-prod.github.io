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

api_status_colour <- c(ok = "success", changed = "warning", "no baseline" = "secondary", "not checked" = "secondary", error = "danger")
api_status_label <- c(ok = "No changes", changed = "Changed", "no baseline" = "No baseline", "not checked" = "Not checked", error = "Error")

# the endpoints of an API by the HTTP status of their response
endpoint_outcomes <- c(
  success = "Success (2xx)", client = "Client errors (4xx)", server = "Server errors (5xx)",
  none = "No response", skipped = "Not checked"
)
# status colours, kept apart from the Bootstrap theme so that they are the
# same in light and dark mode
outcome_colour <- c(success = "#0ca30c", client = "#fab219", server = "#d03b3b", none = "#ec835a", skipped = "#8c8c88")

count_outcomes <- function(eps) {
  checked <- eps$status != "not checked"
  code <- eps$http_status
  c(
    success = sum(checked & code %in% 200:299),
    client = sum(checked & code %in% 400:499),
    server = sum(checked & code %in% 500:599),
    none = sum(checked & is.na(code)),
    skipped = sum(!checked)
  )
}

# a bar for each API, of its endpoints by outcome, on a common scale. Made
# of HTML, so that it fits the page width and follows the light and dark
# themes; each segment has a tooltip
api_status_plot <- function(apis, href = function(id) paste0("api/", id, ".html")) {
  counts <- lapply(apis$apis, function(a) count_outcomes(endpoint_table(a)))
  totals <- Reduce(`+`, counts)
  longest <- max(vapply(counts, sum, 0))

  legend <- div(class = "api-plot-legend",
    lapply(names(endpoint_outcomes), function(o) {
      span(class = "api-plot-key",
        span(class = "api-plot-swatch", style = paste0("background:", outcome_colour[[o]])),
        sprintf("%s %d", endpoint_outcomes[[o]], totals[[o]]))
    })
  )
  rows <- lapply(seq_along(apis$apis), function(i) {
    a <- apis$apis[[i]]
    n <- counts[[i]]
    errors <- n[["client"]] + n[["server"]] + n[["none"]]
    segments <- lapply(names(n)[n > 0], function(o) {
      span(class = "api-plot-segment",
        style = sprintf("flex-grow:%d;background:%s", n[[o]], outcome_colour[[o]]),
        title = sprintf("%s: %d of %d endpoints, %s", a$name, n[[o]], sum(n), tolower(endpoint_outcomes[[o]])))
    })
    list(
      a(class = "api-plot-name", href = href(a$id), a$name),
      div(class = "api-plot-track",
        div(class = "api-plot-bar", style = sprintf("width:%.1f%%", 100 * sum(n) / longest), segments)),
      span(class = "api-plot-value",
        sprintf("%d of %d OK", n[["success"]], sum(n)),
        if (errors) strong(sprintf(" · %d %s", errors, if (errors == 1) "error" else "errors")),
        if (n[["skipped"]]) sprintf(" · %d not checked", n[["skipped"]]))
    )
  })
  tagList(
    tags$style(HTML("
      .api-plot-legend { display: flex; flex-wrap: wrap; gap: .25rem 1rem; margin-bottom: .75rem; font-size: .875rem; }
      .api-plot-key { display: inline-flex; align-items: center; gap: .375rem; color: var(--bs-secondary-color); }
      .api-plot-swatch { width: .75rem; height: .75rem; border-radius: 2px; }
      .api-plot { display: grid; grid-template-columns: minmax(8rem, max-content) 1fr max-content; gap: .5rem 1rem; align-items: center; }
      .api-plot-track { min-width: 4rem; }
      .api-plot-bar { display: flex; gap: 2px; height: 1rem; border-radius: 0 4px 4px 0; overflow: hidden; }
      .api-plot-segment { min-width: 2px; }
      .api-plot-value { font-size: .875rem; font-variant-numeric: tabular-nums; }
      @media (max-width: 576px) {
        .api-plot { grid-template-columns: 1fr max-content; }
        .api-plot-name { grid-column: 1 / -1; margin-bottom: -.25rem; }
      }
    ")),
    legend,
    div(class = "api-plot", do.call(tagList, unlist(rows, recursive = FALSE)))
  )
}

# write markdown from a chunk with results: asis
md <- function(...) cat(..., "\n\n", sep = "")
md_list <- function(x, empty = "None.") {
  if (length(x)) cat(paste0("- ", x, collapse = "\n"), "\n\n") else md(empty)
}
md_table <- function(x, ...) {
  print(knitr::kable(x, format = "pipe", ...))
  cat("\n")
}
