#' @import shiny
create_rf_ui <- function(rf_name) {
  shinyUI(
    fluidPage(
      titlePanel(title = rf_name, windowTitle = rf_name),
      sidebarLayout(
        sidebarPanel(
          uiOutput("class_checklist"),
          uiOutput("primary_term_buttons"),
          uiOutput("log_the_x"),
          uiOutput("secondary_term_buttons")
        ),
        mainPanel(
          plotOutput("influence_plot", width = "100%", height = "800px")
        )
      )
    )
  )
}
