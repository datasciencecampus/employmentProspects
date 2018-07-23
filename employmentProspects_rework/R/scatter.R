#' Calculate quintiles
#'
#' Calculate the quintiles of a given column in a dataframe.
#'
#' @param data The dataframe
#' @param col The column on which to do the calculation
#'
#' @return The function returns the original dataframe with an additional column
#' containing a factor variables of the quintile ranges.
#'
#' @author Nathan Eastwood
#'
#' @importFrom stats quantile as.formula median
#'
#' @export
calculateQuintiles <- function(data, col) {
  quints <- c(
    min(data[, col], na.rm = TRUE),
    quantile(data[, col], probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE),
    max(data[, col], na.rm = TRUE)
  )
  data$quints <- factor(
    ifelse(
      is.na(data[, col]),
      NA,
      ifelse(
        data[, col] >= quints[1] & data[, col] < quints[2],
        paste0(quints[1], " - ", quints[2] - 0.01),
        ifelse(
          data[, col] >= quints[2] & data[, col] < quints[3],
          paste0(quints[2], " - ", quints[3] - 0.01),
          ifelse(
            data[, col] >= quints[3] & data[, col] < quints[4],
            paste0(quints[3], " - ", quints[4] - 0.01),
            ifelse(
              data[, col] >= quints[4] & data[, col] < quints[5],
              paste0(quints[4], " - ", quints[5] - 0.01),
              paste0(quints[5], " - ", quints[6]))
          )
        )
      )
    ),
    levels = c(
      paste0(quints[1], " - ", quints[2] - 0.01),
      paste0(quints[2], " - ", quints[3] - 0.01),
      paste0(quints[3], " - ", quints[4] - 0.01),
      paste0(quints[4], " - ", quints[5] - 0.01),
      paste0(quints[5], " - ", quints[6])
    )
  )
  data
}

#' Comparison of Composite Variables
#'
#' Create a scatter plot of the composite variables
#'
#' @param data The employment data, see details for more information.
#' @param x The variable to be plotted on the x-axis.
#' @param y The variable to be plotted on the y-axis.
#' @param colour The column to colour the points by.
#' @param key The key for the tooltip.
#' @param xLab The x-axis label.
#' @param yLab The y-axis label.
#' @param highlight A vector of Local Authority District names to highlight on
#'   the plot.
#' @param discrete Logical. Whether the data are discrete or continuous.
#'
#' @details
#' The \code{data} needs to contain \code{measureA}, \code{measureB} and the
#' employment rate statistics.
#'
#' @author Nathan Eastwood
#'
#' @importFrom plotly ggplotly layout
#' @importFrom dplyr mutate
#' @import ggplot2
#'
#' @export
scatPlot <- function(data, x, y, colour, key, highlight = NULL,
                     xLab = NULL, yLab = NULL,
                     discrete = TRUE) {

  maxH <- max(data[, x], na.rm = TRUE)
  horizontalLine <- c(0, maxH)
  maxV <- max(data[, y], na.rm = TRUE)
  verticalLine <- c(0, maxV)

  data <- data %>%
    mutate(tip = paste0(.[, key], "<br>x: ", .[, x], ", y: ", .[, y]))

  plot <- ggplot() +
    geom_hline(yintercept = median(data[, y], na.rm = TRUE),
               colour = "grey51") +
    geom_vline(xintercept = median(data[, x], na.rm = TRUE),
               colour = "grey51") +
    labs(x = xLab, y = yLab, colour = NULL) +
    xlim(horizontalLine) +
    ylim(verticalLine) +
    theme_bw()

  if (is.null(highlight)) {
    plot <-
      plot +
      geom_point(
        data = data,
        aes_string(x = x, y = y, colour = colour, key = key, text = "tip")
      )
  } else {
    plot <-
      plot +
      geom_point(
        data = data[!(data$la_name %in% highlight), ],
        aes_string(x = x, y = y, colour = colour, key = key, text = "tip"),
        alpha = 0.3, size = 0.9
      ) +
      geom_point(
        data = data[data$la_name %in% highlight, ],
        aes_string(x = x, y = y, colour = colour, key = key, text = "tip"),
        size = 1.5
      )
  }

  if (discrete) {
    plot <-
      plot +
      scale_colour_manual(values = c("red", "#EF9C61", "#4575B3"))
    if (!is.null(highlight)) {
      plot <-
        plot +
        guides(colour = guide_legend(override.aes = list(alpha = 1)))
    }
  } else {
    plot <-
      plot +
      scale_colour_distiller(palette = "YlOrRd", direction = 1)
  }

  ggplotly(plot, tooltip = "text") %>%
    layout(
      legend = list(x = 100, y = 0.5)
    )
}
