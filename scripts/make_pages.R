# Create one page per package (package/<name>.qmd) and per API
# (api/<id>.qmd), from packages.json and apis/<id>/config.json. Each page sets the
# package name or API id, and includes the template _package.qmd or _api.qmd.
#
# Quarto runs this script before rendering the site (see pre-render in
# _quarto.yml). The created pages are not committed.

library(jsonlite)

make_pages <- function(dir, names, variable, template, titles = names) {
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  for (i in seq_along(names)) {
    name <- names[i]
    writeLines(
      c(
        "---",
        paste0('title: "', titles[i], '"'),
        "---",
        "",
        "```{r}",
        "#| include: false",
        paste0(variable, ' <- "', name, '"'),
        "```",
        "",
        paste0("{{< include ../", template, " >}}")
      ),
      file.path(dir, paste0(name, ".qmd"))
    )
  }
  message("created ", length(names), " pages in ", dir, "/")
}

packages <- fromJSON("packages.json", simplifyDataFrame = FALSE)$packages
apis <- lapply(Sys.glob("apis/*/config.json"), fromJSON, simplifyDataFrame = FALSE)

make_pages("package", vapply(packages, `[[`, "", "package"), "pkg", "_package.qmd")
make_pages("api", vapply(apis, `[[`, "", "id"), "api_id", "_api.qmd",
           titles = vapply(apis, `[[`, "", "name"))
