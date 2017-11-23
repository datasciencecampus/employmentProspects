#' Set up the map choice buttons.
#'
#' @param id The name for the shiny id.
#' @param select The default variable to be selected.
#'
#' @importFrom shiny NS fluidRow radioButtons
#'
#' @export
mapChoicesUI <- function(id, select) {
  ns <- NS(id)
  fluidRow(
    radioButtons(
      ns("mapType"),
      label = "Choose a map type",
      choices = c("Hexagonal" = "hex", "Geographical" = "geo"),
      selected = "hex",
      inline = TRUE
    ),
    radioButtons(
      ns("fillType"),
      label = "Choose a fill type",
      choices = c("Continuous" = "cont", "Quintile" = "quint"),
      selected = "cont",
      inline = TRUE
    )
  )
}
