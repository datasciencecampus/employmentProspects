#' Time Series Plot
#'
#' Create a time series plot of employment statistics
#'
#' @param data The data to plot
#' @param x The column to be plotted on the x-axis
#' @param y The column to be plotted on the y-axis
#' @param color The column to group the data by
#'
#' @author Nathan Eastwood
#'
#' @examples
#' timeSeriesPlot(empTime, x = "year", y = "val", color = "la_name")
#'
#' @importFrom plotly plot_ly add_trace layout
#'
#' @export
timeSeriesPlot <- function(data, x, y, color) {
  plotly::plot_ly() %>%
    plotly::add_trace(
      data = data,
      x = as.formula(paste0("~", x)),
      y = as.formula(paste0("~", y)),
      color = as.formula(paste0("~", color)),
      text = ~paste0(
        "Region: ", data[, color],
        "<br>Year: ", data[, x],
        "<br>Employment Rate: ", data[, y], "%"
      ),
      hoverinfo = "text",
      type = "scatter",
      mode = "lines+markers"
    ) %>%
    plotly::layout(
      xaxis = list(title = "Year"),
      yaxis = list(title = "Employment Rate (%)")
    )
}
