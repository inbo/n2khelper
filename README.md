[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)
[![DOI](https://zenodo.org/badge/50571210.svg)](https://zenodo.org/badge/latestdoi/50571210)
[![codecov](https://codecov.io/gh/inbo/n2khelper/branch/master/graph/badge.svg)](https://codecov.io/gh/inbo/n2khelper)

# The n2khelper package

The `n2khelper` package contains auxiliary functions for the analysis and reporting of the Natura 2000 Monitoring.

## Rationale

`n2khelper` is part of the collection of [R packages](https://github.com/search?q=topic%3Anatura2000+org%3Ainbo&type=Repositories) created to analyse the data gathered during the Natura 2000 monitoring.

- [n2kanalysis](https://github.com/inbo/n2kanalysis): R package with generic functions for the analysis
- [watervogelanalysis](https://github.com/inbo/watervogelanalysis): R package to extract the raw data from the wintering bird survey database and prepare the analyses
- [abvanalysis](https://github.com/inbo/abvanalysis): R package to extract the raw data from the common breeding birds survey database and prepare the analyses
- [n2khelper](https://github.com/inbo/n2khelper): auxiliary functions used in the other packages
- [n2kresult](https://github.com/inbo/n2kresult): Liquibase scripts to setup the database in which the results of the analyses are stored
- [n2kupdate](https://github.com/inbo/n2kupdate): R package to read and write to the `n2kresult` database
- [Rn2k](https://github.com/inbo/Rn2k): Docker image with all the required dependencies to run the analyses

`n2khelper` has several goals: 

1. Not to repeat oneself by creating a single package with generic function rather than create different functions that do something similar in the different packages.
1. Put functions together which might be relevant for other users and package authors.
1. Create a proof of concept for some functionality and convince authors of more estabiled packages to incoporate them in their package. Examples are [`digest::sha1()`](https://github.com/eddelbuettel/digest/pull/20) and [`aws.s3::get_bucket()`](https://github.com/cloudyr/aws.s3/pull/104)

## Folder structure

The folder structure is that of a typical R packages with the mandatory `R` folder (definition of the functions) and `man` (helpfiles in Rd format). `data-raw` is an optional folder with the source code the generate `sysdata.rda` in the `R` folder. The optional `test` folder contains the unit tests using the infrastructure from the `testthat` package.

```
n2khelper
|-- data-raw
|-- man
|-- R
|-- tests
   |-- testthat
```
