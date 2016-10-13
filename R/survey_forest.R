#' Start Shiny app to explore variable importance in a randomForest model
#'
#' @param rf A randomForest object.
#' @return A Shiny app.
#' @export
survey_forest <- function(rf) {
  stopifnot(inherits(rf, "randomForest"))

  # Creates and launches the Shiny server
  rf_name <- deparse(substitute(rf))
  rf_app <- create_rf_app(rf, rf_name)
  shiny::runApp(rf_app)
}
