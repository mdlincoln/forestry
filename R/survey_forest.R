#' Start Shiny app to explore variable importance in a randomForest model
#'
#' @param rf A randomForest object.
#' @param data Original data used to train \code{rf}.
#'
#' @return A Shiny app.
#' @export
survey_forest <- function(rf, data) {
  stopifnot(inherits(rf, "randomForest"))

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
