# ICES R packages status

Source for <https://ices-tools-prod.github.io>, a summary of the status of
the R packages maintained by ICES. The site is built with
[Quarto](https://quarto.org).

## Adding or removing packages

The packages shown are listed in [`packages.json`](packages.json). Each entry
needs the package name and its GitHub repository:

```json
{ "package": "icesVMS", "repo": "ices-tools-prod/icesVMS" }
```

An optional `"notes"` field is shown in the package details. After editing
the file on `main`, the site is rebuilt automatically.

## Files

| File | Contents |
|---|---|
| `packages.json` | the packages shown |
| `index.qmd` | the overview page (a Quarto dashboard) |
| `packages.qmd` | the table of all packages |
| `R/status.R` | R functions used by the pages: reading the status data, badges |
| `scripts/build_status.R` | collects the status of each package into `data/status.json` |
| `_quarto.yml` | site settings: navigation, theme, footer |
| `.github/workflows/build.yml` | renders and publishes the site |

## How it works

A GitHub Action runs every day at 05:00 UTC, on every push to `main`, and on
demand from the Actions tab. It runs `quarto render`, which first runs
`scripts/build_status.R` to collect, for each package:

- **GitHub**: version in `DESCRIPTION` on the default branch, maintainer,
  open issues and pull requests, last commit;
- **CRAN**: published version, check results over all flavours, additional
  issues, deadlines, and whether the package has been archived (and why);
- **r-universe**: build status and check results, from the
  `ices-tools-prod` and `ices-tools-dev` universes;

and then renders the pages from that data.

## Building locally

Needs Quarto, the R packages `jsonlite`, `knitr`, `rmarkdown`, `htmltools`
and `reactable`, and the GitHub CLI, logged in.

```sh
quarto preview
```

The status data is only collected again if `data/status.json` is more than
6 hours old. To force it:

```sh
STATUS_REFRESH=true quarto render
```

## Status levels

- **Action needed**: CRAN check errors, a CRAN deadline, or the GitHub
  repository or `DESCRIPTION` file is missing.
- **Needs attention**: archived on CRAN, CRAN check warnings or additional
  issues, failing r-universe builds or checks, or the GitHub version is older
  than the CRAN version.
- **OK**: none of the above. Unreleased changes and packages not on the
  r-universe are shown for information.
