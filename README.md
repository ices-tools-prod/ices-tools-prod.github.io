# ICES R packages status

Source for <https://ices-tools-prod.github.io>, a summary of the status of
the R packages maintained by ICES, and of the web services they use. The
site is built with [Quarto](https://quarto.org).

## Adding or removing packages

The packages shown are listed in [`packages.json`](packages.json). Each entry
needs the package name and its GitHub repository:

```json
{ "package": "icesVMS", "repo": "ices-tools-prod/icesVMS" }
```

An optional `"notes"` field is shown on the package page. Private
repositories are left out, as the site is public. After editing the file on
`main`, the site is rebuilt automatically.

## Web services

Each web service (API) that is checked has a folder in [`apis/`](apis), named
by its id (e.g. `apis/sag/`), containing:

- `config.json`: the name, base url, documentation, the packages that call
  it, notes shown in a table on its page, and the endpoints to check. Each
  endpoint has a path with example query values, which should return a
  stable result (for example a published assessment), and the R functions
  that call it.
- `baseline.json`: the recorded structure of each endpoint's response, and
  the documented operations and parameters.

To add an API, copy a folder, edit `config.json`, and record its baseline
(see below).

Every day, each endpoint is called and the structure of its response (the
field names, their types, and the format of text values such as dates and
numbers stored as text) is compared with the baseline. If the API has an
OpenAPI (swagger) description, set as `spec` in `config.json`, the
documented operations and their parameters are compared too.
Changes are reported on the site: fields renamed (including changes of case,
such as `AssessmentKey` to `assessmentKey`), added or removed, fields
changing type, and changes in the HTTP status or content type.

The R code of the packages that call each API is read from GitHub (parsed,
never run) to find the R function for each operation, and the functions
that combine several operations. Operations that read data but have no R
function are listed with a suggested function name.

When a change is expected, or for a new API, record a new baseline for it
and commit it:

```sh
Rscript scripts/check_apis.R --update-baseline sag
git add apis/sag/baseline.json
```

Without an id, the baselines of all APIs are updated.

## Files

| File | Contents |
|---|---|
| `packages.json` | the packages shown |
| `apis/<id>/config.json` | a web service and its endpoints |
| `apis/<id>/baseline.json` | the recorded structure of its responses and documentation |
| `index.qmd` | the overview page |
| `packages.qmd` | the table of all packages |
| `apis.qmd` | the overview of the web services |
| `_package.qmd` | template for the package pages |
| `_api.qmd` | template for the web service pages |
| `R/status.R` | R functions used by the pages |
| `scripts/build_status.R` | collects the status of each package into `data/status.json` |
| `scripts/check_apis.R` | checks the web services, into `data/apis.json` |
| `scripts/api_helpers.R` | functions used by `check_apis.R` |
| `scripts/make_pages.R` | creates a page per package (`package/`) and per web service (`api/`) from the templates |
| `_quarto.yml` | site settings: navigation, theme, footer, and the scripts run before rendering |
| `.github/workflows/build.yml` | renders and publishes the site |

`data/`, `package/` and `api/` are created when the site is built, and are
not committed.

## How it works

A GitHub Action runs every day at 05:00 UTC, on every push to `main`, and on
demand from the Actions tab. It runs `quarto render`, which first runs the
three scripts in `scripts/`, then renders the pages. The status collected
for each package is:

- **GitHub**: version in `DESCRIPTION` on the default branch, maintainer,
  open issues and pull requests, last commit;
- **CRAN**: published version, check results over all flavours, additional
  issues, deadlines, and whether the package has been archived (and why);
- **r-universe**: build status and check results, from the
  `ices-tools-prod` and `ices-tools-dev` universes;
- **dependencies**: the hard dependencies (Depends, Imports, LinkingTo),
  found recursively from CRAN and the ICES r-universes, excluding base R
  packages.

## Building locally

Needs Quarto, the R packages `jsonlite`, `curl`, `xml2`, `knitr`, `rmarkdown`,
`htmltools` and `reactable`, and the GitHub CLI, logged in.

```sh
quarto preview
```

The status data is only collected again if it is more than 6 hours old. To
force it:

```sh
STATUS_REFRESH=true quarto render
```

## Status levels

- **Action needed**: CRAN check errors, a CRAN deadline, the GitHub
  repository or `DESCRIPTION` file is missing, or a web service endpoint
  returns an error.
- **Needs attention**: archived on CRAN, CRAN check warnings or additional
  issues, failing r-universe builds or checks, the GitHub version is older
  than the CRAN version, or a web service response has changed.
- **OK**: none of the above. Unreleased changes and packages not on the
  r-universe are shown for information.
