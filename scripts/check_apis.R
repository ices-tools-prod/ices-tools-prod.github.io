# Check the web services set up in apis/<id>/config.json, and write the
# results to data/apis.json for the pages to display.
#
# For each API:
#
# - each endpoint is called, and the structure of its response (the field
#   names, their types, and the format of text values) is compared with the
#   baseline in apis/<id>/baseline.json, to find changes such as fields being
#   renamed (including changes of case), added or removed, or changing type.
#   JSON, XML, and CSV (also in zip files) responses are described;
# - if the API has documentation that can be read (OpenAPI / swagger, or a
#   WSDL), the documented operations and their parameters are compared with
#   the baseline too;
# - the R code of the packages that call the API is read from GitHub, to find
#   the R functions that call each operation.
#
# Quarto runs this script before rendering the site (see pre-render in
# _quarto.yml). It can also be run from the repository root:
#
#   Rscript scripts/check_apis.R
#
# To accept the current responses as the new baseline, run:
#
#   Rscript scripts/check_apis.R --update-baseline vocab
#
# giving the ids of the APIs to update, or no ids to update them all,
# and commit the baseline.json files in apis/.
#
# If data/apis.json is less than max_age_hours old, it is not rebuilt. Set
# STATUS_REFRESH=true to force a rebuild.
#
# Needs the jsonlite, curl and xml2 packages.

library(jsonlite)
source(file.path("scripts", "api_helpers.R"))

config_dir <- "apis"
out_file <- file.path("data", "apis.json")
packages_file <- "packages.json"
max_age_hours <- 6

args <- commandArgs(TRUE)
update_baseline <- "--update-baseline" %in% args
# APIs to update the baseline for; all if none are given
update_ids <- setdiff(args, "--update-baseline")

if (!update_baseline && file.exists(out_file) && !identical(Sys.getenv("STATUS_REFRESH"), "true")) {
  age <- difftime(Sys.time(), file.mtime(out_file), units = "hours")
  if (age < max_age_hours) {
    message("using ", out_file, ", built ", round(age, 1), " hours ago")
    quit(save = "no")
  }
}

msg <- function(...) message(format(Sys.time(), "%H:%M:%S "), ...)
now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")


# checking an endpoint -------------------------------------------------------------

check_endpoint <- function(api, ep) {
  url <- ep$url %||% paste0(api$base_url, "/", ep$path)
  msg(api$id, ": ", ep$name)

  out <- list(name = ep$name, url = url, checked = now())

  handle <- curl::new_handle(timeout = 300, useragent = "ices-tools-prod.github.io status check")
  resp <- tryCatch(curl::curl_fetch_memory(url, handle = handle), error = function(e) e)
  if (inherits(resp, "error")) {
    out$error <- conditionMessage(resp)
    return(out)
  }

  out$http_status <- resp$status_code
  out$content_type <- if (length(resp$type) && !is.na(resp$type)) sub(";.*$", "", resp$type) else ""
  out$time_s <- round(resp$times[["total"]], 2)
  out$size_bytes <- length(resp$content)

  described <- tryCatch(describe_response(resp$content, out$content_type), error = function(e) e)
  if (inherits(described, "error")) {
    out$error <- paste("the response could not be read:", conditionMessage(described))
  } else {
    out$records <- described$records
    out$fields <- described$fields
  }

  out
}


# run ------------------------------------------------------------------------------

config_files <- Sys.glob(file.path(config_dir, "*", "config.json"))
configs <- lapply(config_files, fromJSON, simplifyDataFrame = FALSE)

# GitHub repositories of the packages
repos <- fromJSON(packages_file, simplifyDataFrame = FALSE)$packages
repos <- setNames(vapply(repos, `[[`, "", "repo"), vapply(repos, `[[`, "", "package"))
package_code <- list()

results <- lapply(seq_along(configs), function(i) {
  api <- configs[[i]]
  base_path <- sub("/$", "", sub("^https?://[^/]+", "", api$base_url))

  endpoints <- lapply(api$endpoints, check_endpoint, api = api)

  # the documented operations
  spec <- NULL
  if (!is.null(api$spec)) {
    msg(api$id, ": documentation")
    spec <- tryCatch(
      if (grepl("wsdl", api$spec, ignore.case = TRUE)) summarise_wsdl(api$spec, api$base_url) else summarise_openapi(api$spec),
      error = function(e) list(error = conditionMessage(e))
    )
    spec$url <- api$spec
  }
  ops <- spec$operations %||% list()

  # endpoints checked that are not documented are added as operations, so
  # that their R functions are found too
  for (e in endpoints) {
    segs <- relative_segments(e$url, base_path)
    matched <- vapply(ops, function(o) match_operation(segs, relative_segments(o$full_path, base_path)), 0)
    if (!any(matched > 0)) {
      path <- sub("^https?://[^/]+", "", sub("[?#].*$", "", e$url))
      ops[[paste("GET", path)]] <- list(method = "GET", path = path, full_path = path,
                                        parameters = list(), undocumented = TRUE)
    }
  }

  # the R functions that call each operation
  pkgs <- intersect(unlist(api$packages), names(repos))
  for (p in setdiff(pkgs, names(package_code))) {
    msg(api$id, ": reading the R code of ", p)
    package_code[[p]] <<- tryCatch(read_package_code(repos[[p]]), error = function(e) {
      msg("could not read ", p, ": ", conditionMessage(e))
      NULL
    })
  }
  functions <- map_functions(Filter(Negate(is.null), package_code[pkgs]), ops, base_path)

  fn_names <- vapply(functions, `[[`, "", "name")
  for (op in names(ops)) {
    # endpoint functions: call the operation themselves, and only it (or a variant)
    is_endpoint <- vapply(functions, function(f) f$kind == "endpoint" && op %in% f$operations, logical(1))
    # other functions that call the operation, directly or through other functions
    uses_op <- vapply(functions, function(f) op %in% c(f$operations, f$other_operations), logical(1))
    ops[[op]]$endpoint_functions <- I(sort(fn_names[is_endpoint]))
    is_combined <- vapply(functions, function(f) f$kind == "combined", logical(1))
    ops[[op]]$combined_functions <- I(sort(fn_names[uses_op & is_combined]))
    ops[[op]]$writes <- writes_data(ops[[op]])
    ops[[op]]$suggested <- if (!any(is_endpoint) && !ops[[op]]$writes) suggest_name(ops[[op]], base_path, existing = unlist(lapply(package_code[pkgs], function(x) names(x$objects))))
    # the endpoints checked that call this operation
    op_segs <- relative_segments(ops[[op]]$full_path, base_path)
    ops[[op]]$checked <- I(as.character(unique(unlist(lapply(endpoints, function(e) {
      if (ops[[op]]$method == "GET" && match_operation(relative_segments(e$url, base_path), op_segs) > 0) e$name
    })))))
  }
  # each endpoint, matched to the operation it matches most closely
  endpoints <- lapply(endpoints, function(e) {
    segs <- relative_segments(e$url, base_path)
    score <- vapply(ops, function(o) if (o$method == "GET") match_operation(segs, relative_segments(o$full_path, base_path)) else 0, 0)
    if (any(score > 0)) {
      op <- ops[[which.max(score)]]
      e$operation <- paste(op$method, op$path)
      e$functions <- I(as.character(c(op$endpoint_functions, op$combined_functions)))
    }
    e
  })

  # baseline
  baseline_file <- file.path(dirname(config_files[i]), "baseline.json")
  if (update_baseline && (!length(update_ids) || api$id %in% update_ids)) {
    baseline <- list(
      recorded = now(),
      endpoints = lapply(endpoints, function(e) {
        e <- e[c("name", "url", "http_status", "content_type", "fields")]
        if (!is.null(e$fields)) e$fields <- lapply(e$fields, I)
        e
      }),
      spec = if (!is.null(spec$operations)) lapply(spec$operations, function(o) o[c("method", "path", "parameters")])
    )
    writeLines(toJSON(baseline, auto_unbox = TRUE, pretty = TRUE, null = "null"), baseline_file)
    msg("wrote ", baseline_file)
  }
  baseline <- if (file.exists(baseline_file)) fromJSON(baseline_file, simplifyVector = FALSE) else NULL
  baseline_eps <- if (!is.null(baseline)) setNames(baseline$endpoints, vapply(baseline$endpoints, `[[`, "", "name")) else list()

  endpoints <- lapply(endpoints, function(e) {
    b <- baseline_eps[[e$name]]
    if (!is.null(e$error)) {
      e$status <- "error"
    } else if (is.null(b)) {
      e$status <- "no baseline"
    } else {
      e$changes <- compare_response(e, b)
      e$status <- if (length(e$changes)) "changed" else "ok"
    }
    if (!is.null(e$fields)) e$fields <- lapply(e$fields, I)
    e
  })

  if (!is.null(spec$operations) && !is.null(baseline$spec)) {
    spec$changes <- I(compare_spec(spec$operations, baseline$spec))
  }
  spec$operations <- NULL

  ops <- unname(lapply(ops, function(o) {
    o$parameters <- I(if (length(o$parameters)) paste0(names(o$parameters), " (", unlist(o$parameters), ")") else character(0))
    o
  }))

  status <- vapply(endpoints, `[[`, "", "status")
  spec_problem <- !is.null(spec$error) || length(spec$changes) > 0
  list(
    id = api$id,
    name = api$name,
    description = api$description,
    base_url = api$base_url,
    website = api$website,
    docs = api$docs,
    packages = I(unlist(api$packages)),
    notes = api$notes,
    baseline_recorded = baseline$recorded,
    health = if (any(status == "error")) "error" else if (any(status %in% c("changed", "no baseline")) || spec_problem) "warning" else "ok",
    endpoints = endpoints,
    spec = spec,
    operations = ops,
    functions = functions
  )
})

out <- list(generated = now(), apis = results)

dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
writeLines(toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"), out_file)
msg("wrote ", out_file)
