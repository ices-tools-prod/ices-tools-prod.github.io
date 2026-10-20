# Functions used by scripts/check_apis.R to describe web service responses,
# read API documentation, compare both with a baseline, and find the R
# functions in a package that call each operation.

`%||%` <- function(x, y) if (is.null(x)) y else x

# number of elements (array items, child elements or CSV rows) looked at
# when describing a response
max_elements <- 500


# describing a response -----------------------------------------------------------

# the type of a single value; text values are also classified by format
value_type <- function(x) {
  if (is.null(x)) return("null")
  if (is.list(x)) return(if (is.null(names(x))) "array" else "object")
  if (is.logical(x)) return("boolean")
  if (is.numeric(x)) return("number")
  if (is.character(x)) {
    if (is.na(x)) return("null")
    x <- trimws(x)
    if (grepl("^\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}", x)) return("string (date-time)")
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) return("string (date)")
    if (grepl("^-?\\d+(\\.\\d+)?([eE][-+]?\\d+)?$", x)) return("string (number)")
    if (!nzchar(x)) return("string (empty)")
    return("string")
  }
  class(x)[1]
}

# collects the set of types seen at each path
type_collector <- function() {
  types <- new.env()
  list(
    add = function(path, type) types[[path]] <- union(types[[path]], type),
    result = function() {
      paths <- sort(ls(types, all.names = TRUE))
      out <- lapply(paths, function(p) sort(types[[p]]))
      names(out) <- ifelse(paths == "$", "(root)", sub("^\\$[./]?", "", paths))
      out
    }
  )
}

# JSON: field paths such as [].YearStatus[].year
describe_json <- function(x) {
  tc <- type_collector()
  walk <- function(x, path) {
    type <- value_type(x)
    tc$add(path, type)
    if (type == "object") {
      for (key in names(x)) walk(x[[key]], paste0(path, ".", key))
    } else if (type == "array") {
      for (el in utils::head(x, max_elements)) walk(el, paste0(path, "[]"))
    }
  }
  walk(x, "$")
  tc$result()
}

# XML: element paths such as ArrayOfCls_Datras_SurveyList/Cls_Datras_SurveyList/Survey,
# without namespaces; attributes as path/@name
describe_xml <- function(doc) {
  tc <- type_collector()
  walk <- function(node, path) {
    path <- paste0(path, "/", sub(".*:", "", xml2::xml_name(node)))
    for (a in names(xml2::xml_attrs(node))) {
      if (!grepl("^xmlns|^xsi:", a)) tc$add(paste0(path, "/@", a), value_type(xml2::xml_attr(node, a)))
    }
    kids <- xml2::xml_children(node)
    if (length(kids)) {
      tc$add(path, "element")
      for (k in utils::head(kids, max_elements)) walk(k, path)
    } else {
      nil <- identical(xml2::xml_attr(node, "nil"), "true")
      tc$add(path, if (nil) "null" else value_type(xml2::xml_text(node)))
    }
  }
  # names are read without namespace prefixes (xml_ns_strip() is too slow
  # for large responses)
  walk(xml2::xml_root(doc), "$")
  tc$result()
}

# CSV: the columns, as file/column
describe_csv <- function(file, label = basename(file)) {
  tc <- type_collector()
  x <- utils::read.csv(file, nrows = max_elements, colClasses = "character",
                       check.names = FALSE, na.strings = character(0))
  for (col in names(x)) {
    for (v in unique(x[[col]])) tc$add(paste0("$/", label, "/", col), value_type(v))
  }
  list(fields = tc$result(), records = max(0, length(readLines(file, warn = FALSE)) - 1))
}

# describe a response, by its content type
describe_response <- function(content, content_type) {
  if (!length(content)) {
    return(list())
  }
  if (grepl("json", content_type)) {
    x <- jsonlite::parse_json(rawToChar(content))
    return(list(
      records = if (is.list(x) && is.null(names(x))) length(x) else 1L,
      fields = describe_json(x)
    ))
  }
  if (grepl("xml", content_type)) {
    doc <- suppressWarnings(xml2::read_xml(content))
    root <- xml2::xml_root(doc)
    return(list(records = length(xml2::xml_children(root)), fields = describe_xml(doc)))
  }
  if (grepl("zip|csv", content_type)) {
    dir <- tempfile("response")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE))
    if (grepl("zip", content_type)) {
      zip <- file.path(dir, "response.zip")
      writeBin(content, zip)
      utils::unzip(zip, exdir = dir)
      files <- setdiff(list.files(dir, recursive = TRUE), "response.zip")
    } else {
      writeBin(content, file.path(dir, "response.csv"))
      files <- "response.csv"
    }
    csvs <- files[grepl("\\.(csv|txt)$", files, ignore.case = TRUE)]
    out <- lapply(csvs, function(f) describe_csv(file.path(dir, f), f))
    fields <- c(list("(root)" = sub(".*/", "", content_type)), do.call(c, lapply(out, `[[`, "fields")))
    if (grepl("zip", content_type)) {
      fields <- c(fields, setNames(lapply(files, function(f) "file"), files))
    }
    return(list(records = sum(vapply(out, `[[`, 0, "records")), fields = fields[order(names(fields))]))
  }
  list()
}


# comparing responses with a baseline -------------------------------------------------

# the types to compare: nulls and empty strings are ignored, as a field can
# be empty in one response and filled in the next
comparable <- function(types) {
  setdiff(unlist(types), c("null", "string (empty)"))
}

compare_response <- function(current, baseline) {
  changes <- list()
  add <- function(kind, field, text) {
    changes[[length(changes) + 1]] <<- list(kind = kind, field = field, text = text)
  }

  if (!identical(current$http_status, baseline$http_status)) {
    add("status", NULL, paste0("HTTP status changed from ", baseline$http_status, " to ", current$http_status))
  }
  if (!identical(current$content_type, baseline$content_type)) {
    add("content type", NULL, paste0("Content type changed from '", baseline$content_type, "' to '", current$content_type, "'"))
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
      add("renamed", r, paste0("Field '", r, "' renamed to '", match[1], "' (change of case)"))
      removed <- setdiff(removed, r)
      added <- setdiff(added, match[1])
    }
  }
  for (r in removed) add("removed", r, paste0("Field '", r, "' removed"))
  for (a in added) add("added", a, paste0("Field '", a, "' added"))

  for (f in intersect(old, new)) {
    from <- comparable(baseline$fields[[f]])
    to <- comparable(current$fields[[f]])
    if (length(from) && length(to) && !setequal(from, to)) {
      add("type", f, paste0("Field '", f, "' changed type from ", paste(from, collapse = " | "),
                            " to ", paste(to, collapse = " | ")))
    }
  }

  changes
}


# the documented operations -------------------------------------------------------

# OpenAPI (swagger): operations with their parameters
summarise_openapi <- function(url) {
  spec <- jsonlite::fromJSON(url, simplifyVector = FALSE)
  server <- sub("/$", "", spec$servers[[1]]$url %||% "")
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
        full_path = paste0(server, path),
        parameters = params
      )
    }
  }
  list(title = spec$info$title, version = spec$info$version, operations = ops)
}

# WSDL (SOAP / ASMX web services): the HTTP GET operations with their parameters
summarise_wsdl <- function(url, base_url) {
  doc <- xml2::read_xml(url)
  # match elements by name, whatever their namespace prefix (wsdl:, s:, ...)
  el <- function(name) paste0("*[local-name()='", name, "']")
  messages <- xml2::xml_find_all(doc, paste0("//", el("message")))
  parts <- lapply(messages, function(m) {
    p <- xml2::xml_find_all(m, el("part"))
    if (!length(p)) return(list())
    setNames(as.list(paste0("query, ", sub(".*:", "", xml2::xml_attr(p, "type")))), xml2::xml_attr(p, "name"))
  })
  names(parts) <- xml2::xml_attr(messages, "name")
  base_path <- sub("^https?://[^/]+", "", base_url)
  ops <- list()
  for (op in xml2::xml_find_all(doc, paste0("//", el("portType"), "[contains(@name, 'HttpGet')]/", el("operation")))) {
    name <- xml2::xml_attr(op, "name")
    input <- sub(".*:", "", xml2::xml_attr(xml2::xml_find_first(op, el("input")), "message"))
    ops[[paste("GET", paste0("/", name))]] <- list(
      method = "GET",
      path = paste0("/", name),
      full_path = paste0(base_path, "/", name),
      parameters = parts[[input]] %||% list()
    )
  }
  # operations only available by SOAP or POST, such as uploads
  for (op in xml2::xml_find_all(doc, paste0("//", el("portType"), "[contains(@name, 'Soap') and not(contains(@name, 'Soap12'))]/", el("operation")))) {
    name <- xml2::xml_attr(op, "name")
    if (is.null(ops[[paste("GET", paste0("/", name))]])) {
      ops[[paste("POST", paste0("/", name))]] <- list(method = "POST", path = paste0("/", name),
                                                      full_path = paste0(base_path, "/", name), parameters = list())
    }
  }
  title <- gsub("_x0020_", " ", xml2::xml_attr(xml2::xml_find_first(doc, paste0("//", el("service"))), "name"))
  list(title = title, version = "WSDL", operations = ops)
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


# paths relative to the API ------------------------------------------------------

# a url or path, relative to the API base path, split into segments; query
# strings are dropped
relative_segments <- function(x, base_path) {
  x <- sub("^https?://[^/]+", "", x)
  x <- sub("[?#].*$", "", x)
  absolute <- startsWith(x, "/")
  x <- sub("^/", "", x)
  segs <- strsplit(x, "/", fixed = TRUE)[[1]] %||% character(0)
  # a trailing / means a further segment follows, e.g. paste0("Code/", type)
  if (length(segs) && endsWith(x, "/")) segs <- c(segs, "")
  # an absolute path loses the leading segments it shares with the base path
  if (absolute) {
    base <- strsplit(sub("^/", "", base_path), "/", fixed = TRUE)[[1]]
    n <- 0
    while (n < min(length(base), length(segs)) && tolower(segs[n + 1]) == tolower(base[n + 1])) n <- n + 1
    segs <- segs[seq_along(segs) > n]
  }
  segs
}

# does a string from R code refer to an operation? Placeholders in the R
# string (sprintf %s, glue {x}) match anything, and so do path parameters of
# the operation. Returns the number of fixed segments matched, or 0.
match_operation <- function(r_segments, op_segments) {
  if (!length(r_segments) || length(r_segments) > length(op_segments)) return(0)
  matched <- 0
  for (i in seq_along(op_segments)) {
    op_seg <- op_segments[i]
    if (grepl("^\\{.*\\}$", op_seg)) next
    r_seg <- if (i <= length(r_segments)) r_segments[i] else ""
    escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", r_seg)
    pattern <- gsub("\\\\\\{[^}]*\\\\\\}|%[-0-9.]*[sidfx]", ".*", escaped)
    # names are matched ignoring case, as the services do; names with
    # placeholders (e.g. get%sdata) are matched exactly, as they match more
    if (!nzchar(r_seg) || !grepl(paste0("^", pattern, "$"), op_seg, ignore.case = pattern == escaped)) return(0)
    matched <- matched + 1
  }
  matched
}


# R functions that call each operation ----------------------------------------------

# download and parse the R code of a package from GitHub. The code is only
# parsed, never run. Returns, for each top-level object, the strings in its
# code and the names it uses, and the exported names.
read_package_code <- function(repo) {
  dir <- tempfile("pkg")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  archive <- file.path(dir, "source.tar.gz")
  curl::curl_download(paste0("https://codeload.github.com/", repo, "/tar.gz/HEAD"), archive, quiet = TRUE)
  files <- utils::untar(archive, list = TRUE)
  wanted <- files[grepl("^[^/]+/(R/[^/]+\\.[Rr]|NAMESPACE)$", files)]
  utils::untar(archive, files = wanted, exdir = dir)
  root <- file.path(dir, sub("/.*", "", wanted[1]))

  objects <- list()
  for (f in list.files(file.path(root, "R"), pattern = "\\.[Rr]$", full.names = TRUE)) {
    exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) expression())
    for (e in exprs) {
      if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=", "<<-") && is.name(e[[2]])) {
        objects[[as.character(e[[2]])]] <- list(
          strings = code_strings(e[[3]]),
          names = all.names(e[[3]])
        )
      }
    }
  }

  ns <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  exports <- unlist(regmatches(ns, gregexpr("(?<=^export\\()[^)]+", ns, perl = TRUE)))
  exports <- gsub("[\"` ]", "", unlist(strsplit(exports, ",")))
  patterns <- unlist(regmatches(ns, gregexpr("(?<=^exportPattern\\(\")[^\"]+", ns, perl = TRUE)))
  for (p in patterns) exports <- c(exports, grep(p, names(objects), value = TRUE))

  list(objects = objects, exports = unique(exports))
}

# all character constants in an R expression
code_strings <- function(e) {
  if (is.character(e)) return(e)
  if (is.call(e) || is.pairlist(e) || is.expression(e) || is.list(e)) {
    return(unlist(lapply(as.list(e), function(x) if (missing(x)) NULL else code_strings(x))))
  }
  character(0)
}

# for each function in the packages: the operations it calls directly, and
# through the other functions it uses; and whether it is a function for one
# endpoint, or combines several
map_functions <- function(packages, ops, base_path) {
  op_segments <- lapply(ops, function(o) relative_segments(o$full_path, base_path))
  out <- list()
  for (pkg in names(packages)) {
    code <- packages[[pkg]]
    objs <- code$objects

    # operations referred to directly by each object: each string is matched
    # to the operation it matches most closely
    direct <- lapply(objs, function(o) {
      found <- character(0)
      for (s in unique(o$strings)) {
        if (!grepl("[A-Za-z]", s) || grepl("\\s", trimws(s))) next
        score <- vapply(op_segments, match_operation, 0, r_segments = relative_segments(s, base_path))
        if (any(score > 0)) {
          # several operations can match a string with placeholders, e.g. get%sdata
          found <- c(found, names(ops)[score == max(score)])
        }
      }
      unique(found)
    })

    # objects used by each object, by name or as a string (e.g. match.fun)
    uses <- lapply(objs, function(o) intersect(c(o$names, o$strings), names(objs)))

    # operations reached from an object, recursively; with stop, not going
    # into the exported functions in stop
    reach <- function(name, stop = character(0), seen = character(0)) {
      if (name %in% seen) return(character(0))
      seen <- c(seen, name)
      next_objs <- setdiff(uses[[name]], c(name, stop))
      unique(c(direct[[name]], unlist(lapply(next_objs, reach, stop = stop, seen = seen))))
    }

    exported <- intersect(code$exports, names(objs))
    api_exports <- Filter(function(n) length(reach(n)) > 0, exported)
    own <- lapply(setNames(api_exports, api_exports), function(f) reach(f, stop = setdiff(api_exports, f)))

    for (f in api_exports) {
      all <- reach(f)
      uses_exported <- intersect(uses[[f]], setdiff(api_exports, f))
      # builds on other functions that call an operation themselves
      builds_on <- any(lengths(own[uses_exported]) > 0)
      # one operation, or one with a variant (e.g. getHLdata and getHLdataSp)
      single <- length(own[[f]]) == 1 ||
        (length(own[[f]]) == 2 && (startsWith(own[[f]][1], own[[f]][2]) || startsWith(own[[f]][2], own[[f]][1])))
      # the operations this is the R function for: those it is named after,
      # or the single operation it calls, if it does not build on other functions
      endpoint_ops <- Filter(function(op) name_matches(f, ops[[op]], base_path), own[[f]])
      # and their variants, e.g. getHLdataSp for getHLdata
      endpoint_ops <- own[[f]][vapply(own[[f]], function(op) any(startsWith(op, endpoint_ops)), logical(1))]
      if (!length(endpoint_ops) && single && !builds_on) endpoint_ops <- own[[f]]
      out[[length(out) + 1]] <- list(
        name = f,
        package = pkg,
        kind = if (length(endpoint_ops)) "endpoint" else "combined",
        operations = I(sort(endpoint_ops)),
        other_operations = I(sort(setdiff(all, endpoint_ops))),
        uses = I(sort(uses_exported))
      )
    }
  }
  out
}

# the name of an operation: its fixed path segments relative to the API,
# without "api" and file extensions, e.g. CodeType/Relation/{a}/{b} gives
# CodeTypeRelation
operation_name <- function(op, base_path) {
  segs <- relative_segments(op$full_path, base_path)
  segs <- segs[!grepl("^\\{", segs) & tolower(segs) != "api" & nzchar(segs)]
  paste(sub("\\.[a-z]+$", "", segs), collapse = "")
}

# is an R function named after an operation (ignoring case, underscores and
# a leading "get")
name_matches <- function(f, op, base_path) {
  norm <- function(x) sub("^get", "", gsub("[^a-z0-9]", "", tolower(x)))
  identical(norm(f), norm(operation_name(op, base_path)))
}

# suggested name for an R function for an operation, different from the
# names of existing functions
suggest_name <- function(op, base_path, existing = character(0)) {
  name <- operation_name(op, base_path)
  name <- gsub("_([a-z])", "\\U\\1", name, perl = TRUE)
  name <- if (grepl("^get", name)) name else paste0("get", toupper(substring(name, 1, 1)), substring(name, 2))
  if (name %in% existing) name <- paste0(name, "List")
  name
}

# does an operation write or change data, rather than read it
writes_data <- function(op) {
  op$method != "GET" ||
    grepl("(^|/)(upload|save|screen|publish|send|delete|submit)", op$path, ignore.case = TRUE) ||
    grepl("(^|/)set[A-Z_]", op$path)
}
