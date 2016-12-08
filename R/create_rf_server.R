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
      rownames(rf[["importance"]])
    })

    continuous_terms <- reactive({
      purrr::keep(terms(), function(x) {
        is.numeric(data[[x]])
      })
    })

    discrete_terms <- reactive({
      purrr::discard(terms(), function(x) {
        is.numeric(data[[x]])
        })
    })

    output$class_checklist <- renderUI({
      selectInput("class_var", label = "Prediction Class", choices = classes())
    })

    output$primary_term_buttons <- renderUI({
      selectInput("primary_exp_var", label = "Primary Term", choices = continuous_terms())
    })

    output$secondary_term_buttons <- renderUI({
      selectInput("secondary_exp_var", label = "Secondary Term (optional)", choices = c("(none)", terms()), selected = "(none)")
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

    pdp_data <- reactive({
      if (input$calc == 0)
        return()

      isolate({

        var1 <- input$primary_exp_var
        if (input$secondary_exp_var == "(none)") {
          var2 <- NULL
        } else {
          var2 <- input$secondary_exp_var
        }

        simulate_data(rf, data,
                      class = input$class_var,
                      var1 = var1,
                      var2 = var2,
                      shiny_session = session)
      })
    })

    output$influence_plot <- renderPlot({
      if (input$calc == 0)
        return()

      print(head(pdp_data()))

      chart_forest(pdp_data(),
                   log_var1 = log_the_x())
    })
  })
}
