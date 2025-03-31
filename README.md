
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cde <img src="https://docs.ropensci.org/cde/reference/figures/logo.png" align="right" height=140/>

<!-- badges: start -->
[![R-CMD-check](https://github.com/ropensci/cde/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/cde/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/ropensci/cde/branch/master/graph/badge.svg?token=F4R6nEywTx)](https://codecov.io/gh/ropensci/cde)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://badges.ropensci.org/284_status.svg)](https://github.com/ropensci/onboarding/issues/284)
[![DOI](https://zenodo.org/badge/92712854.svg)](https://zenodo.org/badge/latestdoi/92712854)
[![status](http://joss.theoj.org/papers/0d35f75e861fcf47556d70571e226589/status.svg)](http://joss.theoj.org/papers/0d35f75e861fcf47556d70571e226589)
[![CRAN
Version](http://www.r-pkg.org/badges/version/cde)](http://www.r-pkg.org/pkg/cde)
[![](http://cranlogs.r-pkg.org/badges/cde)](http://cran.rstudio.com/web/packages/cde/index.html)
<!-- badges: end -->

## Introduction

Within Europe, the [Water Framework
Directive](http://ec.europa.eu/environment/water/water-framework/index_en.html)
(WFD) sets EU-wide standards for how the quality of surface- and
ground-waters across Europe is assessed and classified. Assessment of
quality using the WFD is based on a range of elements that vary
depending on the type of water being assessed and are combined to give
an overall classification of waterbodies into five classes (High, Good,
Moderate, Poor and Bad) for surface waters and two classes (Good or
Poor) for groundwaters.

In the UK the Environment Agency (EA) is the competent authority
responsible for monitoring and assessment of water quality within
England. The EA have made the reporting data relating to the
requirements of the WFD available via the Catchment Data Explorer (CDE)
website, <https://environment.data.gov.uk/catchment-planning/>.

`cde` is a package for R which facilitates searching and download of the
WFD reporting data for all waterbodies from the EA CDE website. The
ability to access these data from within the R environment allows for
efficient collation and interrogation of data and reproducible analysis
of trends or patterns in water quality and pressures on waterbodies
across England. There are also some inconsistencies in the way in which
the data are structured within the original CDE website; `cde` provides
consistently named and structured output which facilitates further
analysis.

The types of data that can be downloaded are: WFD status classification
data, Reasons for Not Achieving Good (RNAG) status, objectives set for
waterbodies and details of associated protected areas.

The CDE data are made available under the [Open Government Licence
v3.0](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)
and use of the data accessed by and contained within this package
implies acceptance of these licence conditions.

## Installation

You can install the current development version from github with:

``` r
# install.packages("remotes")
remotes::install_github("ropensci/cde")
```

## Basic usage

Details of how to use the package can be found at
<https://docs.ropensci.org/cde>.

## Contributing

For details of how to contribute to this package, see
[here](https://docs.ropensci.org/cde/CONTRIBUTING.html).

[![ropensci_footer](https://ropensci.org/public_images/github_footer.png)](https://ropensci.org)
