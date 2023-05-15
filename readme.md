
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rebib : Parse/Convert embedded LaTeX bibliography to BibTex

<!-- badges: start -->

[![GitHub Workflow Status
(branch)](https://img.shields.io/github/actions/workflow/status/Abhi-1U/rebib/pkg_down.yaml?branch=master&label=pkgdown&style=for-the-badge)](https://github.com/Abhi-1U/rebib/actions/workflows/pkg_down.yaml)
[![GitHub Workflow
Status](https://img.shields.io/github/actions/workflow/status/Abhi-1U/rebib/cmdcheck.yaml?branch=master&label=R-CMD-CHECK&style=for-the-badge)](https://github.com/Abhi-1U/rebib/actions/workflows/cmdcheck.yaml)
[![GitHub R package version (subdirectory of
monorepo)](https://img.shields.io/github/r-package/v/Abhi-1U/rebib?filename=DESCRIPTION&label=rebib&style=for-the-badge)](https://github.com/Abhi-1U/rebib/blob/master/DESCRIPTION)
<!-- badges: end -->

rebib is a spun-off package from
[texor](https://github.com/Abhi-1U/texor).

The decision to do this is the fact that the bibliography section in
texor package was expanding significantly, enough to deserve a dedicated
space.

- Reads bib chunks to produce a very close BibTeX equivalent
- Title and author are usually mandatory fields
- URL, ISBN, publisher, pages and year are optional fields and will be
  enabled when relevant
- Rest of the data is stored in `"journal"`(internally) and
  `"publisher"`(when writing BibTeX file)
- Ignores commented LaTeX code
- Citation tracker
- Logging of events
- Bibliography Aggregation

This mindmap summarizes the feature set beautifully:

<div class="figure" style="text-align: center">

<img src="man/figures/rebib.svg" alt="A mindmap summary for feature set of rebib package" width="100%" />
<p class="caption">
A mindmap summary for feature set of rebib package
</p>

</div>

This FlowChart Describes the Aggregation Feature:

<div class="figure" style="text-align: center">

<img src="man/figures/bib_agg.svg" alt="A Flow chart of Bibliography Aggregation" width="100%" />
<p class="caption">
A Flow chart of Bibliography Aggregation
</p>

</div>

### Note :

Update : relative paths should also work with version 0.1.7 and above

Please use absolute paths when working with texor/rebib !

~~The reason is that earlier rebib used to work with relative paths by
using setwd(), getwd() to handle working directories. However this was
not an ideal method to handle paths and working on files.~~

~~Hence I had to switch to absolute path system for almost all functions
where automation is possible and works well. As there was no changing of
working directories there is less risk of setting up a wrong working
directory when failing certain function.~~

Use forward slashes (/) in paths and do not add a forward slash at the
end of the path for example

1.  Wrong usage : `C:\projects\texor\main`
2.  wrong usage : `C:/projects/texor/main/`
3.  wrong usage : `C:\\projects\\texor\\main\\`
4.  wrong usage : `C:\\projects\\texor\\main` (this may work with R)
5.  Correct usage : `C:/projects/texor/main`

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
# for files without BibTeX source
rebib::handle_bibliography(wd)
cat(readLines(paste(wd,"example.bib",sep="/")),sep = "\n")
# for files with BibTeX source as well as embedded enntries
rebib::aggregate_bibliography(wd)
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
file_name <- rebib:::get_texfile_name(wd)
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
