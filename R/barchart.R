#' LAD Bar Chart
#'
#' Create an ordered bar chart for a given statistic and highlight selected
#' regions
#'
#' @param data The data.
#' @param var The statistic to plot.
#' @param highlight The LADs to highlight. A vector.
#'
#' @examples
#' \dontrun{
#' ladBarChart(emp, "measure_a", c("Cornwall", "Newport"))
#' }
#'
#' @export
ladBarChart <- function(data, var, highlight = NULL) {
  # Order the data correctly
  data <- data %>%
    arrange_(var) %>%
    mutate(
      text = la_name,
      la_name = factor(
        la_name,
        levels = .$la_name
      )
    )
  # If any regions are selected, highlight them
  if (!is.null(highlight)) {
    data <- data %>%
      mutate(
        opacity = if_else(la_name %in% highlight, 1, 0.3)
      )
  } else {
    data$opacity <- 1
  }

  # Set up the x and y axes
  xform <- list(
    categoryorder = "array",
    categoryarray = rev(levels(data$la_name)),
    title = "Local Authority District",
    showticklabels = FALSE
  )
  yform <- list(
    title = paste(
      strwrap(
        dataColumnChoices[dataColumnChoices$short %in% var, "full"],
        width = 50
      ),
      collapse = "<br>"
    )
  )

  # Generate the plot
  plot_ly(data = data,
          x = ~la_name,
          y = ~eval(parse(text = var)),
          type = "bar",
          color = ~la_name,
          colors = "YlOrRd",
          opacity = ~opacity) %>%
    layout(xaxis = xform,
           yaxis = yform,
           showlegend = FALSE)
}
