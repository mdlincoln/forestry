#' @import shiny
create_rf_ui <- function(rf, rf_name) {
  shinyUI(
    fluidPage(
      sidebarLayout(
        h1(rf_name),
        sidebarPanel(
          uiOutput("class_checklist"),
          uiOutput("term_buttons")
        ),
        mainPanel(
          plotOutput("influence_plot")
        )
      )
    )
  )
}
