#' Start Shiny app to explore variable importance in a randomForest model
#'
#' @param rf A randomForest object.
#' @param data Data used to build \code{rf}. If the original data still exists
#'   in \code{globalenv}, then \code{survey_forest} will locate it based on the
#'   call saved within \code{rf}. However, if it is not available there (if, for
#'   example, it has been \code{\link{remove}}d, or if the \code{randomForest}
#'   object has been loaded into a clean session) then the data must be passed
#'   explicitly.
#'
#' @return A Shiny app.
#' @export
survey_forest <- function(rf, data = NULL) {
  stopifnot(inherits(rf, "randomForest"))

  # Use data if they have been supplied, otherwise attempt to access from the
  # model
  if (is.null(data))
    data <- rf_data(rf)

  # Creates and launches the Shiny server
  rf_name <- deparse(substitute(rf))
  rf_app <- create_rf_app(rf, rf_name, data)
  shiny::runApp(rf_app)
}

#' @import shiny
create_rf_app <- function(rf, rf_name, data) {
  shinyApp(
    ui = create_rf_ui(rf_name),
    server = create_rf_server(rf, data))
}
