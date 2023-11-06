# miWQS 0.5.0

# Package Updates
- Fix error in impute.Lubin()
* Removed period after "et al." in description field
* Changed required versions of downstream packages to reflect their changes
   - Require that dependency package **mvtnorm** is at least v. 1.1-2. 
   - Require that dependency package **matrixNormal** is at least v-0.1.1
(thanks, @prockenschaub, https://github.com/phargarten2/matrixNormal/issues/1)
* Rebuilt vignette to remove CRAN error

# Test environments
- local R installation, x86_64-apple-darwin20 (64-bit), R version 4.3.2 (2023-10-31)
- winbuilder Windows x86_64-w64-mingw32 (64-bit); Server 2022 x64 (build 20348) R version 4.3.2 (2023-10-31 ucrt)
<!--
- Windows Server 2008 R2 SP1, R-devel, 32/64 bit
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)
- R-hub Ubuntu Linux 20.04.1 LTS, R-devel, GCC
- R-hub macOS 10.13.6 High Sierra, R-release, CRAN's setup
-->

## R CMD check results
0 errors | 0 warnings | 2 notes

  
## Reverse Dependencies
There are no reverse dependencies. 




# miWQS 0.4.4
## Package Updates
Edited description field in DESCRIPTION file in compliance with CRAN comments:
> You are using directed quotes in the Description field. Please only use
regular quotes.
> Please also omit the redundant " The `miWQS` package".

## Test environments
- local R installation, R 4.0.4
- win-builder windows x86_64-w64-mingw32 (64-bit)  R 4.0.4
- Windows Server 2008 R2 SP1, R-devel, 32/64 bit
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)
- R-hub Ubuntu Linux 20.04.1 LTS, R-devel, GCC
- R-hub macOS 10.13.6 High Sierra, R-release, CRAN's setup
 
## R CMD check results
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Paul M. Hargarten <hargartenp@alumni.vcu.edu>'

New maintainer:
Old maintainer(s):
  Paul M. Hargarten <hargartenp@vcu.edu>
  Paul M. Hargarten <hargartenp@alumni.vcu.edu>
  
  
## Reverse Dependencies
There are no reverse dependencies. 


# miWQS 0.4.3
## Package Updates
* In R 4.0.5, the `quantile()` function in that the names of quantile()'s result no longer depend on the global getOption("digits"), but quantile() gets a new optional argument digits = 7 instead.
Therefore, the `make.quantile.matrix()` function was updated, as the names are used in the package.

* Changed maintainer email address.

## Test environments
- local R installation, R 4.0.4
- win-builder windows x86_64-w64-mingw32 (64-bit)  R 4.0.4
- R-hub windows-x86_64-devel (r-devel)
- R-hub fedora-clang-devel (r-devel)
- R-hub Windows Server 2008 R2 SP1, R-devel, 32/64 bit
- R-hub Ubuntu Linux 20.04.1 LTS, R-devel, GCC
- R-hub macOS 10.13.6 High Sierra, R-release, CRAN's setup

# Not tested
* R-hub macos-highsierra-release (r-release)
* R-hub ubuntu-gcc-release (r-release)

## R CMD check results
0 errors | 0 warnings | 2 notes
checking CRAN incoming feasibility ... NOTE
Maintainer: 'Paul M. Hargarten <hargartenp@vcu.edu>'
Possibly mis-spelled words in DESCRIPTION:
   Hargarten (21:654, 21:850)

  Maintainer: 'Paul M. Hargarten <hargartenp@alumni.vcu.edu>'
  
  New maintainer:
    Paul M. Hargarten <hargartenp@alumni.vcu.edu>
  Old maintainer(s):
    Paul M. Hargarten <hargartenp@vcu.edu>
    
    
## Reverse Dependencies
There are no reverse dependencies. 


# miWQS 0.4.2 
## Package Updates 
Fixing discrepancy with CRAN standards:
 Bounded global variables: chemical and estimate. 
 
## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* R-hub macos-highsierra-release (r-release)
* R-hub ubuntu-gcc-release (r-release)
* win-builder windows x86_64-w64-mingw32 (64-bit) (devel)
* R-hub windows-x86_64-devel (r-devel), 
* R-hub fedora-clang-devel (r-devel)
* R-hub debian-clang-devel (r-devel)

## R CMD check results
0 errors | 0 warnings | 1 notes
checking CRAN incoming feasibility ... NOTE
Maintainer: 'Paul M. Hargarten <hargartenp@vcu.edu>'
Possibly mis-spelled words in DESCRIPTION:
   Hargarten (21:654, 21:850)

Comment: "Hargarten" is a last name and is spelled correctly.


# miWQS 0.4.1
## Package Updates
Fixing discrepancies with CRAN standards:
*  DESCRIPTION FILE: Rewrote dois to match CRAN standards (<doi:...>, not <\doi:...>)
*  `impute.multivariate.bayesian()`: Removed empty details section
 
 ## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* R-hub macos-highsierra-release (r-release)
* R-hub ubuntu-gcc-release (r-release)
* win-builder windows x86_64-w64-mingw32 (64-bit) (devel)
* R-hub windows-x86_64-devel (r-devel), 
* R-hub fedora-clang-devel (r-devel)
* R-hub debian-clang-devel (r-devel)

## R CMD check results
0 errors | 0 warnings | 2 notes
checking CRAN incoming feasibility ... NOTE
Maintainer: 'Paul M. Hargarten <hargartenp@vcu.edu>'
Possibly mis-spelled words in DESCRIPTION:
   Hargarten (21:654, 21:850)

Comment: "Hargarten" is a last name and is spelled correctly.

checking R code for possible problems ... NOTE
  plot.wqs: no visible binding for global variable ?estimate?
  plot.wqs: no visible binding for global variable ?chemical?
  Undefined global functions or variables:
    chemical estimate
    
Comment:  CRAN Check says that 'chemical' and 'estimate' are undefined global variables, but they are are data variables. They are called by ggplot2 to a newly created dataset "to.plot". This dataset that was created from tidyr::pivot_longer(). This is well documented here upon using tidyr: https://tidyr.tidyverse.org/articles/in-packages.html, but we are unsure how to fix the problem between tidyr and ggplot2. 

## Reverse Dependencies
There are no reverse dependencies. 



#miWQS 0.4.0
## Package updates
New features to use package, documentation clarity, fixed bugs. 


## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* R-hub macos-highsierra-release (r-release)
* R-hub ubuntu-gcc-release (r-release)
* win-builder windows x86_64-w64-mingw32 (64-bit) (devel)
* R-hub windows-x86_64-devel (r-devel), 
* R-hub fedora-clang-devel (r-devel)
* R-hub debian-clang-devel (r-devel)

## R CMD check results
0 errors | 0 warnings | 2 notes
checking CRAN incoming feasibility ... NOTE
Maintainer: 'Paul M. Hargarten <hargartenp@vcu.edu>'
Possibly mis-spelled words in DESCRIPTION:
   Hargarten (21:654, 21:850)

Comment: "Hargarten" is a last name and is not misspelled. 

checking R code for possible problems ... NOTE
  plot.wqs: no visible binding for global variable ?estimate?
  plot.wqs: no visible binding for global variable ?chemical?
  Undefined global functions or variables:
    chemical estimate
    
Comment:  CRAN Check says that 'chemical' and 'estimate' are undefined global variables, but they are are data variables. They are called by ggplot2 to a newly created dataset "to.plot". This dataset that was created from tidyr::pivot_longer(). This is well documented here upon using tidyr: https://tidyr.tidyverse.org/articles/in-packages.html, but we are unsure how to fix the problem between tidyr and ggplot2. 

## Reverse Dependencies
There are no reverse dependencies. 



# miWQS 0.2.0
## Package updates
 As requested, fixed errors using r-devel-linux-x86_64-debian-clang for R 4.0.0 to be released. Apparently your package no longer works correctly when class(matrix(...)) gives a vector of length two and conditions of length greater than one in 'if' or 'while' give an error: please fix as necessary. 
 
## Test environments





# miWQS 0.1.0 
Package updates
New features to use package, documentation clarity, fixed bugs. 

## Test environments
- local OS X install, R version 4.0.4 (2021-02-15)
- x86_64-w64-mingw32 (64-bit) R version 4.0.4 (2021-02-15)
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)
- R-hub macos-elcapitan-release (r-release)

Status: OK
## R CMD check results
0 errors | 0 warnings | 1 notes
? On windows-x86_64-devel (r-devel)
  checking sizes of PDF files under 'inst/doc' ... NOTE
  Unable to find GhostScript executable to run checks on size reduction

Comment: There is no folder "inst/doc"  and thus the "GhostScript" does not exist. I think there's a bug in virtual checking as the filename probably changed. See [link](https://stackoverflow.com/questions/37197603/how-to-fix-unable-to-find-ghostscript-executable-to-run-checks-on-size-reductio/). 

## Reverse dependencies
There are no reverse dependencies. 

# miWQS 0.0.9
 First publically released version submitted to CRAN. 

# miWQS 0.0.8  
* Reworked plot.wqs() function by using ggplot2 instead of base plotting in R. 
* Successfully passed windows check. 

# miWQS 0.0.7 
* Fixed bug of installing packages in examples as requested. 
* Successfully passed windows check. 

# miWQS 0.0.5 
 *Shortened example in estimate.wqs() as requested. 
 *Successfully passed windows check.
  
# miWQS 0.0.4  
  *Fixed bad URLS as requested in the NEWS file. 
  *Successfully passed windows check.
  
# miWQS 0.0.3
*Made comments in `impute.Lubin()` easier to read.
*Removed "\dontrun" for all examples as requested. 
*Merged `quantile.fn()` and `quantile.bdl.fn()` to `make.quantile.matrix()` so that all examples can be run. Had to adjust `estimate.wqs()`to reflect the change.  
*Placed references in form #(Author(Year) <doi:10.prefix/suffix>) #  in the description field of DESCRIPTION file, as requested. 

# miWQS 0.0.2
*Removed non-standard files. 
*Rewrote the comments in `quantile.fn()` example 
*Modified printed messages to screen to be consistent in impute.univariate.bayesian, `estimate.wqs()`,
and pool.mi.

# miWQS 0.0.1
*Minor updates; made sure examples and help files were correct. 
 
---

 
* FAILURE SUMMARY
There are no errors or warnings to report.

