#' Create data for the leaflet map
#'
#' Merge the statistic data with the map data for the leaflet plot
#'
#' @param data The statistics to be combined with the shapefile data.
#'
#' @author Nathan Eastwood
#'
#' @importFrom sp spTransform CRS
#'
#' @export
createLeafletData <- function(data) {
  trans <- sp::spTransform(shape, sp::CRS("+init=epsg:4326"))
  trans@data <- dplyr::left_join(trans@data, data,
                                 by = c("lad15nm" = "la_name")) %>%
    as.data.frame()
  trans
}

#' Generate a leaflet map of the UK
#'
#' Generate a leaflet map of the UK with polygons for each LAD region filled
#' by their statistics.
#'
#' @param data The spatial data.
#' @param fill The data which should be used to fill the map polygons. Defaults
#'   to \code{NULL} in the event a statistics hasn't been provided.
#' @param hex Logical; defaults to \code{FALSE}. Whether to generate a hex map
#'   (\code{TRUE}) or a normal map (\code{FALSE}).
#' @param addLegend Logical; defaults to \code{TRUE}. Should a legend be added
#'   to the map?
#'
#' @author Nathan Eastwood
#'
#' @import leaflet
#' @importFrom dplyr mutate arrange arrange_
#'
#' @export
leafMap <- function(data, fill = NULL, hex = FALSE, addLegend = TRUE) {

  # Extract the map bounds
  bounds <- data@bbox

  # Create the plot
  map <- if (!hex) {
    leaflet(
      options = leafletOptions(
        minZoom = 4,
        maxZoom = 8
      )
    ) %>%
      setView(
        mean(bounds[1, ]),
        mean(bounds[2, ]),
        zoom = 5
      )
  } else {
    leaflet(options = leafletOptions(
      minZoom = 7,
      maxZoom = 10
    )
    ) %>%
      setView(
        mean(bounds[1, ]),
        mean(bounds[2, ]),
        zoom = 7.5
      )
  }

  # Calculate the region's rank
  data@data <- data@data %>%
    mutate(origRows = as.numeric(rownames(.))) %>%
    arrange_(.dots = paste0("desc(", fill, ")")) %>%
    mutate(rank = rownames(.)) %>%
    arrange(origRows)

  # If a specific statistic is selected, generate the polygons layer for it
  if (!is.null(fill)) {
    # Generate the popup details
    details <- sprintf(
      "<strong>LAD: </strong>%s<br><strong>Value: </strong>%s<br><strong>Rank: </strong>%s",
      data@data$lad15nm,
      data@data[, fill],
      data@data[, "rank"]
    ) %>%
      lapply(htmltools::HTML)

    # Generate a colour palette
    pal <- if (length(table(data@data[, fill])) == 4) {
      domain_min <- min(data@data[, fill], na.rm = TRUE)
      domain_max <- max(data@data[, fill], na.rm = TRUE)
      colorFactor("YlOrRd", factor(data@data[, fill]))
    } else if (length(table(data@data[, fill])) == 5) {
      colorFactor(c("#edf8e9",
                    "#bae4b3",
                    "#74c476",
                    "#31a354",
                    "#006d2c"),
                  factor(data@data[, fill]))
    } else {
      domain_min <- min(roundDown(data@data[, fill]), na.rm = TRUE)
      domain_max <- max(roundUp(data@data[, fill]), na.rm = TRUE)
      colorNumeric("YlOrRd", domain = domain_min:domain_max)
    }

    # Add the polygons
    map <-
      map %>%
      addPolygons(
        data = data,
        weight = 1,
        fillColor = pal(data@data[, fill]),
        fillOpacity = 0.8,
        color = "#F5BE29",
        label = details,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        ),
        layerId = data$lad15cd,
        highlightOptions = highlightOptions(
          color = "#000000",
          weight = 3,
          opacity = 1,
          bringToFront = FALSE
        )
      )
    if (addLegend) {
      if (length(table(data@data[, fill])) == 4) {
        map <-
          map %>%
          addLegend(pal = pal, values = data@data[, fill])
      } else if (length(table(data@data[, fill])) == 5) {
        map <-
          map %>%
          addLegend(pal = pal, values = data@data[, fill],
                    labels = unique(data@data[, fill]))
      } else {
        map <-
          map %>%
          addLegend(pal = pal, values = data@data[, fill])#,
        #labFormat = labelFormat(suffix = "%"))
      }
    }
  }

  map
}
