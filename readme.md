
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rebib : Parse/Convert embedded LaTeX bibliography to BibTex

<!-- badges: start -->

[![GitHub Workflow Status
(branch)](https://img.shields.io/github/workflow/status/Abhi-1U/rebib/pkgdown/master?label=pkgdown&style=for-the-badge)](https://github.com/Abhi-1U/rebib/actions/workflows/pkg_down.yaml)
[![GitHub Workflow
Status](https://img.shields.io/github/workflow/status/Abhi-1U/rebib/R_cmd_check?label=R-CMD-CHECK&style=for-the-badge)](https://github.com/Abhi-1U/rebib/actions/workflows/cmdcheck.yaml)
[![GitHub R package version (subdirectory of
monorepo)](https://img.shields.io/github/r-package/v/Abhi-1U/rebib?filename=DESCRIPTION&label=rebib&style=for-the-badge)](https://github.com/Abhi-1U/rebib/blob/master/DESCRIPTION)
<!-- badges: end -->

rebib is a spun off package from
[texor](https://github.com/Abhi-1U/texor) project.

The decision to do this was based on the fact that the bibliography
section in texor package was expanding significantly, enough to have its
own package.

-   Reads bib chunks to produce a very close bibtex equivalent
-   Separate title, URL field, year, ISBN (if applicable)
-   Rest of data is stored in journal(internally) and publisher(when
    writing bibtex file)
-   Ignores commented bibliography
-   URL,ISBN,publisher,year are optional fields and will be enabled when
    relevant

## Installation

install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("Abhi-1U/rebib")
# install.packages("pak")
pak::pak("Abhi-1U/rebib")
```

## General Usage for Rjournal articles

here is a quick example to use rebib package with a sample Rjournal
article (included with the package
[inst/article](https://github.com/Abhi-1U/rebib/tree/master/inst/article))

``` r
wd <-  system.file("article", package = "rebib")
rebib::handle_bibliography(wd)
cat(readLines(paste(wd,"example.bib",sep="/")),sep = "\n")
```

## General Usage for any other tex/bbl file

here is a quick example to use rebib package with a sample bbl file
(included with the package
[inst/article](https://github.com/Abhi-1U/rebib/tree/master/inst/article))

``` r
bbl_file <-  "" # /path/to/bbl_file
rebib::biblio_convertor(file_path = bbl_file)
cat(readLines(gsub("bbl","bib",bbl_file)),sep = "\n")
```

## Step By Step Usage

This procedure is meant for debugging errors and finding out what went
wrong.

``` r
wd <-  system.file("article", package = "rebib")
file_name <- rebib::get_texfile_name(wd)
bib_items <- rebib::extract_embeded_bib_items(wd,file_name)
# for debugging single entry
rebib::bibliography_parser(bib_items[1])
# for multiple entries
rebib::bib_handler(bib_items[1:2])
```

## Sample Conversion

Embedded bibliography :

    \bibitem[R Core Team]{R}
    R Core Team
    \newblock R: A Language and Environment for Statistical Computing
    \newblock \emph{R Foundation for Statistical Computing}, Vienna, Austria \penalty0 2016.
    \newblock URL : \url{https://www.R-project.org/}, ISBN 3-900051-07-0

generated BibTeX :

    @book{R,
    author = {{R Core Team}},
    title = {{R: A Language and Environment for Statistical Computing}},
    publisher = {R Foundation for Statistical Computing Vienna Austria},
    year = {2016},
    url = {https://www.R-project.org/},
    isbn = {3-900051-07-0}
    }
