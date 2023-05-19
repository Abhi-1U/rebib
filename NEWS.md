# version 0.2.0

## features updates :  

1. Updated Contributor Details and DESCRIPTION
2. Getting Ready for CRAN
3. Updated LICENSE and added `cph` 
4. Logging events is now optional and turned off by default.

## bug fixes :  
1. Corrected usage of `gsub()` for changing extensions with `xfun::with_ext()`. 
2. Cleaned up documentation and removed un-wanted functions/docs.
3. Corrected `rebib::citation_reader()` example.

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
