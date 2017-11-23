# load the package library

devtools::install_github('datasciencecampus/employmentProspects')
library(employmentProspects)

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
webshot::install_phantomjs()
phantom <- webshot:::find_phantom()
