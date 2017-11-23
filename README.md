
<!-- README.md is generated from README.Rmd. Please edit that file -->
Employment Prospects Application
===========================================================

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) [![LICENSE.](https://img.shields.io/badge/license-OGL--3-brightgreen.svg?style=flat)](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/)

This app is to be used for the dissemination of GB employment statistics using data published [here](https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/adhocs/007605employmentcharacteristicsoflocalauthoritiesgreatbritain2015)

Installation
------------

In order to run this app you will need to install [`gdal`](http://www.gdal.org/) version 2.0 or higher, [`V8`](https://developers.google.com/v8/) version 3.15 or higher and [`udunits`](https://www.unidata.ucar.edu/downloads/udunits/index.jsp).

You can install the package using

``` r
devtools::install_github("datasciencecampus/employmentProspects")
```

If you would like to download images of the maps, you will need to have [`PhatomJS`](http://phantomjs.org/) installed; you can install `PhantomJS` using `webshot::install_phantomjs()`.

Usage
-----

To use the app, once you have it installed run

``` r
library(employmentProspects)
runEmploymentProspectsApp()
```
