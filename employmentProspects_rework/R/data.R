#' Shapefile data for the UK
#'
#' Ultra Generalised Clipped Shapefile data for the Local Authority Districts
#' (LAD) in the UK from December 2015.
#'
#' @docType data
#'
#' @usage data(shape)
#'
#' @format As S4 object of class \code{"sp"}; see \code{\link[sp]{sp}}.
#'
#' @source \href{http://geoportal.statistics.gov.uk/datasets/8edafbe3276d4b56aec60991cbddda50_4}{ONS Geoportal}
"shape"

#' UK Employment Statistics Choices
#'
#' Choices of UK Employment Statistics available in the data. This data set is
#' used within the Shiny App and maps the full column names to their short hand
#' counter parts.
#'
#' @docType data
#'
#' @usage data(dataColumnChoices)
#'
#' @format A \code{data.frame} with 31 observations and 2 columns.
"dataColumnChoices"

#' UK Hex Map Data
#'
#' The LAD data and coordinates for Local Area Districts (LADs) broken down into
#' equally sized hexagons. The data are of class
#' \code{SpatialPolygonsDataFrame}.
#'
#' @docType data
#'
#' @usage data(hexMapJson)
#'
#' @format A \code{list} of length 391 - one for each LAD.
"hexMapJson"

#' Employment Statistics
#'
#' The employment rates for each LAD from 2012 to 2016.
#'
#' @docType data
#'
#' @usage data(empTime)
#'
#' @format A \code{data.frame} with 1895 rows and 4 columns.
"empTime"

#' Dummy employment data
#'
#' Dummy data for use in the app
#'
#' @docType data
#'
#' @usage data(emp)
#'
#' @format A \code{data.frame} with 379 rows and 54 variables.
"emp"
