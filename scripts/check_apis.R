# Check the web services set up in apis/<id>/config.json, and write the
# results to data/apis.json for the pages to display.
#
# Each endpoint is called, and the structure of its response (the field
# names, their types, and the format of text values) is compared with the
# baseline in apis/<id>/baseline.json, to find changes such as fields being
# renamed (including changes of case), added or removed, or changing type.
#
# If the API has an OpenAPI (swagger) description, the documented operations
# and their parameters are also compared with the baseline.
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
# Needs the jsonlite and curl packages.

library(jsonlite)

config_dir <- "apis"
out_file <- file.path("data", "apis.json")
max_age_hours <- 6
# number of array elements looked at when describing a response
max_elements <- 500

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


# describing a response ---------------------------------------------------------

# the type of a single JSON value; text values are also classified by format
value_type <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) return(if (is.null(names(x))) "array" else "object")
  if (is.logical(x)) return("boolean")
  if (is.numeric(x)) return("number")
  if (is.character(x)) {
    x <- trimws(x)
    if (grepl("^\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}", x)) return("string (date-time)")
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) return("string (date)")
    if (grepl("^-?\\d+(\\.\\d+)?([eE][-+]?\\d+)?$", x)) return("string (number)")
    if (!nzchar(x)) return("string (empty)")
    return("string")
  }
  class(x)[1]
}

# all field paths in a parsed JSON value, with the set of types seen at each
describe <- function(x) {
  types <- new.env()
  add <- function(path, type) {
    types[[path]] <- union(types[[path]], type)
  }
  walk <- function(x, path) {
    type <- value_type(x)
    add(path, type)
    if (type == "object") {
      for (key in names(x)) walk(x[[key]], paste0(path, ".", key))
    } else if (type == "array") {
      for (el in utils::head(x, max_elements)) walk(el, paste0(path, "[]"))
    }
  }
  walk(x, "$")
  paths <- sort(ls(types, all.names = TRUE))
  out <- lapply(paths, function(p) sort(types[[p]]))
  names(out) <- ifelse(paths == "$", "(root)", sub("^\\$\\.?", "", paths))
  out
}

# the types to compare: nulls and empty strings are ignored, as a field can
# be empty in one response and filled in the next
comparable <- function(types) {
  setdiff(unlist(types), c("null", "string (empty)"))
}


# comparing with a baseline -------------------------------------------------------

compare <- function(current, baseline) {
  changes <- list()
  add <- function(kind, field, from, to, text) {
    changes[[length(changes) + 1]] <<- list(kind = kind, field = field, from = from, to = to, text = text)
  }

  if (!identical(current$http_status, baseline$http_status)) {
    add("status", NULL, baseline$http_status, current$http_status,
        paste0("HTTP status changed from ", baseline$http_status, " to ", current$http_status))
  }
  if (!identical(current$content_type, baseline$content_type)) {
    add("content type", NULL, baseline$content_type, current$content_type,
        paste0("Content type changed from '", baseline$content_type, "' to '", current$content_type, "'"))
  }
  if (is.null(current$fields) || is.null(baseline$fields)) {
    return(changes)
  }

  old <- names(baseline$fields)
  new <- names(current$fields)
  removed <- setdiff(old, new)
  added <- setdiff(new, old)

  # fields whose name only changed case
  for (r in removed) {
    match <- added[tolower(added) == tolower(r)]
    if (length(match)) {
      add("renamed", r, r, match[1], paste0("Field '", r, "' renamed to '", match[1], "' (change of case)"))
      removed <- setdiff(removed, r)
      added <- setdiff(added, match[1])
    }
  }
  for (r in removed) add("removed", r, r, NULL, paste0("Field '", r, "' removed"))
  for (a in added) add("added", a, NULL, a, paste0("Field '", a, "' added"))

  for (f in intersect(old, new)) {
    from <- comparable(baseline$fields[[f]])
    to <- comparable(current$fields[[f]])
    if (length(from) && length(to) && !setequal(from, to)) {
      add("type", f, paste(from, collapse = " | "), paste(to, collapse = " | "),
          paste0("Field '", f, "' changed type from ", paste(from, collapse = " | "),
                 " to ", paste(to, collapse = " | ")))
    }
  }

  changes
}


# checking an endpoint -------------------------------------------------------------

check_endpoint <- function(api, ep) {
  url <- paste0(api$base_url, "/", ep$path)
  msg(api$id, ": ", ep$name)

  out <- list(
    name = ep$name,
    url = url,
    functions = I(unlist(ep$functions)),
    checked = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )

  handle <- curl::new_handle(timeout = 120, useragent = "ices-tools-prod.github.io status check")
  resp <- tryCatch(curl::curl_fetch_memory(url, handle = handle), error = function(e) e)
  if (inherits(resp, "error")) {
    out$error <- conditionMessage(resp)
    return(out)
  }

  out$http_status <- resp$status_code
  out$content_type <- if (length(resp$type) && !is.na(resp$type)) sub(";.*$", "", resp$type) else ""
  out$time_s <- round(resp$times[["total"]], 2)
  out$size_bytes <- length(resp$content)

  if (grepl("json", out$content_type) && length(resp$content)) {
    parsed <- tryCatch(
      parse_json(rawToChar(resp$content)),
      error = function(e) e
    )
    if (inherits(parsed, "error")) {
      out$error <- paste("response is not valid JSON:", conditionMessage(parsed))
    } else {
      out$records <- if (is.list(parsed) && is.null(names(parsed))) length(parsed) else 1L
      out$fields <- describe(parsed)
    }
  }

  out
}

`%||%` <- function(x, y) if (is.null(x)) y else x


# the documented operations -------------------------------------------------------

# the operations in an OpenAPI description, with their parameters
summarise_spec <- function(spec) {
  ops <- list()
  for (path in names(spec$paths)) {
    for (method in intersect(names(spec$paths[[path]]), c("get", "post", "put", "patch", "delete"))) {
      o <- spec$paths[[path]][[method]]
      params <- lapply(o$parameters, function(x) {
        paste0(x[["in"]] %||% "", ", ", x$schema$type %||% "", if (isTRUE(x$required)) ", required" else "")
      })
      names(params) <- vapply(o$parameters, function(x) x$name %||% "", "")
      ops[[paste(toupper(method), path)]] <- list(
        method = toupper(method),
        path = path,
        summary = o$summary %||% "",
        parameters = params
      )
    }
  }
  ops
}

compare_spec <- function(current, baseline) {
  changes <- character(0)
  removed <- setdiff(names(baseline), names(current))
  added <- setdiff(names(current), names(baseline))
  for (r in removed) {
    match <- added[tolower(added) == tolower(r)]
    if (length(match)) {
      changes <- c(changes, paste0("Operation `", r, "` renamed to `", match[1], "` (change of case)"))
      added <- setdiff(added, match[1])
    } else {
      changes <- c(changes, paste0("Operation `", r, "` removed"))
    }
  }
  changes <- c(changes, paste0("Operation `", added, "` added")[length(added) > 0])
  for (op in intersect(names(current), names(baseline))) {
    old <- baseline[[op]]$parameters
    new <- current[[op]]$parameters
    p_removed <- setdiff(names(old), names(new))
    p_added <- setdiff(names(new), names(old))
    for (r in p_removed) {
      match <- p_added[tolower(p_added) == tolower(r)]
      if (length(match)) {
        changes <- c(changes, paste0("`", op, "`: parameter `", r, "` renamed to `", match[1], "` (change of case)"))
        p_added <- setdiff(p_added, match[1])
      } else {
        changes <- c(changes, paste0("`", op, "`: parameter `", r, "` removed"))
      }
    }
    changes <- c(changes, paste0("`", op, "`: parameter `", p_added, "` added")[length(p_added) > 0])
    for (p in intersect(names(old), names(new))) {
      if (!identical(old[[p]], new[[p]])) {
        changes <- c(changes, paste0("`", op, "`: parameter `", p, "` changed from (", old[[p]], ") to (", new[[p]], ")"))
      }
    }
  }
  changes
}

check_spec <- function(api, endpoints) {
  if (is.null(api$spec)) {
    return(NULL)
  }
  msg(api$id, ": documentation")
  spec <- tryCatch(fromJSON(api$spec, simplifyVector = FALSE), error = function(e) e)
  if (inherits(spec, "error")) {
    return(list(url = api$spec, error = conditionMessage(spec)))
  }
  ops <- summarise_spec(spec)

  # which checked endpoints call each operation
  endpoint_paths <- tolower(vapply(endpoints, function(e) sub("\\?.*$", "", e$url), ""))
  for (op in names(ops)) {
    pattern <- paste0(gsub("\\{[^}]+\\}", "[^/]+", tolower(ops[[op]]$path)), "$")
    checked <- grepl(pattern, endpoint_paths) & ops[[op]]$method == "GET"
    ops[[op]]$checked <- I(unique(vapply(endpoints[checked], `[[`, "", "name")))
  }

  list(
    url = api$spec,
    title = spec$info$title,
    version = spec$info$version,
    operations = ops
  )
}


# run ------------------------------------------------------------------------------

config_files <- Sys.glob(file.path(config_dir, "*", "config.json"))
configs <- lapply(config_files, fromJSON, simplifyDataFrame = FALSE)

results <- lapply(seq_along(configs), function(i) {
  api <- configs[[i]]
  endpoints <- lapply(api$endpoints, check_endpoint, api = api)
  spec <- check_spec(api, endpoints)

  baseline_file <- file.path(dirname(config_files[i]), "baseline.json")

  if (update_baseline && (!length(update_ids) || api$id %in% update_ids)) {
    baseline <- list(
      recorded = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
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
      e$changes <- compare(e, b)
      e$status <- if (length(e$changes)) "changed" else "ok"
    }
    if (!is.null(e$fields)) {
      # store types as arrays, also when there is only one
      e$fields <- lapply(e$fields, I)
    }
    e
  })

  if (!is.null(spec$operations)) {
    if (!is.null(baseline$spec)) {
      spec$changes <- I(compare_spec(spec$operations, baseline$spec))
    }
    spec$operations <- unname(lapply(spec$operations, function(o) {
      o$parameters <- I(if (length(o$parameters)) paste0(names(o$parameters), " (", unlist(o$parameters), ")") else character(0))
      o
    }))
  }

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
    spec = spec
  )
})

out <- list(
  generated = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  apis = results
)

dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)
writeLines(toJSON(out, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"), out_file)
msg("wrote ", out_file)
