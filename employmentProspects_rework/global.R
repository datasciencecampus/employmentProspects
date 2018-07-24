# load the package library

#devtools::install_github('datasciencecampus/employmentProspects') #Cannot install packages on shinyapps.io
#library(employmentProspects)

#Load the data from the package manually:

load("data/dataColumnChoices.rda")
load("data/emp.rda")
load("data/empTime.rda")
load("data/hexMapJson.rda")
load("data/shape.rda")

#source required functions:

source("R/barchart.R")
source("R/map.R")
source("R/mapChoicesUI.R")
source("R/scatter.R")
source("R/timeSeriesPlot.R")
source("R/utils.R")

# shiny libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(V8)

# data manipulation libraries
library(dplyr)
library(htmltools)
library(sp)

# plotting libraries
library(ggplot2)
library(plotly)
library(leaflet)
library(mapview)

# test for PhantomJS installation
webshot::install_phantomjs() ###Not sure if this needs to be editted out
phantom <- webshot:::find_phantom()

