#' @import shiny
#' @import ggplot2
#' @import dplyr
create_rf_server <- function(rf, data) {

  shinyServer(function(input, output, session) {

    rf_votes <- rf[["votes"]]

    classes <- reactive({
      rf[["classes"]]
    })

    terms <- reactive({
      attr(rf$terms, "term.labels")
    })

    rf_y <- rf[["y"]]

    rf_predicted <- rf[["predicted"]]

    continuous_terms <- reactive({
      purrr::keep(terms(), function(x) {
        is.numeric(data[[x]])
      })
    })

    discrete_terms <- reactive({
      purrr::keep(terms(), function(x) {
        !is.numeric(data[[x]])
      })
    })

    output$primary_term_buttons <- renderUI({
      selectInput("primary_exp_var", label = "Primary Term", choices = continuous_terms())
    })

    output$secondary_term_buttons <- renderUI({
      selectInput("secondary_exp_var", label = "Secondary Term (optional)", choices = c("(none)", terms()), selected = "(none)")
    })

    output$tertiary_term_buttons <- renderUI({
      selectInput("tertiary_exp_var", label = "Tertiary Term (optional)", choices = c("(none)", terms()), selected = "(none)")
    })

    observeEvent(input$primary_exp_var, {
      updateCheckboxInput(session, "log_x_axis", value = FALSE)
    })

    log_the_x <- reactive({
      if (is.null(input$log_x_axis)) {
        FALSE
      } else {
        input$log_x_axis
      }
    })

    term_data <- reactive({

      var2 <- ifelse(input$secondary_exp_var == "(none)", NULL, input$secondary_exp_var)
      var3 <- ifelse(input$tertiary_exp_var == "(none)", NULL, input$teriary_exp_var)

      harvest_forest(rf, data, var2 = var2, var3 = var3)
    })

    is_primary_continuous <- reactive({
      is.numeric(term_data()[[input$primary_exp_var]])
    })

    output$influence_plot <- renderPlot({
      chart_forest(rf, data,
                   var1 = input$primary_exp_var,
                   var2 = input$secondary_exp_var,
                   var3 = input$tertiary_exp_var,
                   log_var1 = log_the_x())
    })
  })
}
