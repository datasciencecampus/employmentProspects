#' Create the column names data
#'
#' Create a data set within the package that contains abbreviated column names
#' as well as the full column names.
#'
#' @param overwrite Logical; overwrite the existing data file? Defaults to
#'   \code{TRUE}.
#'
#' @author Nathan Eastwood
createColumnNames <- function(overwrite = TRUE) {
  file <- devtools::package_file("data-raw", "columnNames.csv")
  dataColumnChoices <- read.csv(file, check.names = FALSE,
                                stringsAsFactors = FALSE)
  devtools::use_data(dataColumnChoices, overwrite = overwrite)
}
