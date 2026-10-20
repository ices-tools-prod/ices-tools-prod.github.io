# ICES R packages status

Source for <https://ices-tools-prod.github.io>, a summary of the status of
the R packages maintained by ICES.

## Adding or removing packages

The packages shown are listed in [`packages.json`](packages.json). Each entry
needs the package name and its GitHub repository:

```json
{ "package": "icesVMS", "repo": "ices-tools-prod/icesVMS" }
```

An optional `"notes"` field is shown in the package details. After editing
the file on `main`, the site is rebuilt automatically.

## How it works

A GitHub Action ([`.github/workflows/build.yml`](.github/workflows/build.yml))
runs every day at 05:00 UTC, on every push to `main`, and on demand from the
Actions tab. It runs [`scripts/build_status.R`](scripts/build_status.R),
which collects, for each package:

- **GitHub**: version in `DESCRIPTION` on the default branch, maintainer,
  open issues and pull requests, last commit;
- **CRAN**: published version, check results over all flavours, additional
  issues, deadlines, and whether the package has been archived (and why);
- **r-universe**: build status and check results, from the
  `ices-tools-prod` and `ices-tools-dev` universes;

and writes `site/status.json`. The pages in [`site/`](site) read that file.

To build the status locally (needs the `jsonlite` package and the GitHub CLI,
logged in):

```sh
Rscript scripts/build_status.R
python3 -m http.server -d site
```

then open <http://localhost:8000>.

## Status levels

- **Action needed**: CRAN check errors, a CRAN deadline, or the GitHub
  repository or `DESCRIPTION` file is missing.
- **Needs attention**: archived on CRAN, CRAN check warnings or additional
  issues, failing r-universe builds or checks, or the GitHub version is older
  than the CRAN version.
- **OK**: none of the above. Unreleased changes and packages not on the
  r-universe are shown for information.
