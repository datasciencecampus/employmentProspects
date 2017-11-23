#' Create the hexagon map data for leaflet
#'
#' Create the hexagon map data for use in leaflet but ensure the columns names
#' are consistent with the other leaflet map data
#'
#' @author Nathan Eastwood
#'
#' @export
createHexMapJson <- function() {
  file <- devtools::package_file("data-raw", "geography.json")
  data <- rgdal::readOGR(file)
  colnames(data@data) <- c("id", "lad15cd", "lad15nm")
  data@data$id <- as.character(data@data$id)
  data@data$lad15cd <- as.character(data@data$lad15cd)
  data@data$lad15nm <- as.character(data@data$lad15nm)
  data
}
