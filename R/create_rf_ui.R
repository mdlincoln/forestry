#' @import shiny
create_rf_ui <- function(rf_name) {
  shinyUI(
    fluidPage(
      titlePanel(title = rf_name, windowTitle = rf_name),
      sidebarLayout(
        sidebarPanel(
          uiOutput("class_checklist"),
          uiOutput("primary_term_buttons"),
          uiOutput("secondary_term_buttons")
        ),
        mainPanel(
          plotOutput("influence_plot")
        )
      )
    )
  )
}
