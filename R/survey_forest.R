#' Start Shiny app to explore variable importance in a randomForest model
#'
#' @param rf A randomForest object.
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
