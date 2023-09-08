# version 0.3.2

## bug fixes :

1. Fixed #19 where bib parser would fail to extract identifiers.
2. Corrected fix #17 to work better.


# version 0.3.0

## bug fixes :

1. Fixed #15, where certain articles would fail to aggregate bibliography.
2. Fixed #17, where writer would add extra newlines.
3. Updated wrapper tools which work with articles without wrapper as well.


# version 0.2.4

## bug fixes : 

1. Fixed a bug where empty lines in the bunch of strings could not be filtered out.
2. Fixed Issue #14 where the conversion/aggregation would fail in case of one or none bib items.
3. Updated pkgdown site

# version 0.2.2

## features updates :  

1. Updated Contributor Details and DESCRIPTION
2. CRAN Release
3. Updated LICENSE and added `cph` 
4. Logging events is now optional and turned off by default.

## bug fixes : 

1. Corrected usage of `gsub()` for changing extensions with `xfun::with_ext()`. 
2. Cleaned up documentation and removed un-wanted functions/docs.
3. Corrected `rebib::citation_reader()` example.
4. Use of `message()`/`warning()` instead of `print()`
5. Examples/Tests/Vignettes do not write in user/package directory and work in a temp directory.
6. Fixed `on.exit()` call in `R/util.R`, move it from line 64 to line 22.
7. Fixed title and description in `DESCRIPTION` file

# version 0.1.7

## features updates :

1. New Vignettes
2. testthat test cases
3. Added Pages field
    
## bug fixes :
  
1. Github issue #9
2. Updated path for backup file before adding external bib file reference.
3. Multiple citations are split into individual citations.
4. Fixed year handling issue Github issue #11
5. fixed issue with missing spaces in the journal field

# version 0.1.2

## features updates :

1. Logging events
2. Bibliography Aggregation

## bug fixes :

1. BibTeX writer improved
2. more man pages
3. citation reader
4. absolute path support


# version 0.0.9

## features updates :

1. BibTex reference parser/reader
2. year field implemented
3. Passing R CMD check

## bug fixes :

1. BibTeX writer re-implemented
2. more man pages

# version 0.0.5

## feature updates :
1. title field now contains title only
1. publisher field with remaining info
2. URL field (enabled if URL exists)

## bug fixes :

1. fixed stray periods(.) and colons(:) 

# Version 0.0.1

## Spun off from [texor](https://github.com/Abhi-1U/texor) project
The decision to do this was based on the fact that the bibliography section 
in texor package was expanding significantly, enough to have its own
package.

## features :
Reads bib chunks to produce minimum viable bibtex equivalent
