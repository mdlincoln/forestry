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
      selectInput("primary_exp_var", label = "Term", choices = continuous_terms())
    })

    output$secondary_term_buttons <- renderUI({
      selectInput("secondary_exp_var", label = "Term", choices = c("(none)", discrete_terms()), selected = "(none)")
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

    output$class_checklist <- renderUI({
      selectInput("class_var", label = "Actual classes to compare", choices = classes(), selected = classes()[1])
    })

    term_data <- reactive({
      d <- bind_cols(data, as.data.frame(rf_votes)) %>%
        mutate(
          actual = rf_y,
          predicted = rf_predicted,
          accurate_prediction = actual == predicted) %>%
        rename_(.dots = list("class_votes" = input$class_var))

      if (input$secondary_exp_var != "(none)" && is.numeric(d[[input$secondary_exp_var]])) {
        mdots <- list(lazyeval::interp(~cut(var2, breaks = quantile(var2, probs = seq(0, 1, length.out = 5))), var2 = as.name(input$secondary_exp_var)))
        d <- mutate_(d, .dots = setNames(mdots, input$secondary_exp_var))
      }

      d
    })

    is_primary_continuous <- reactive({
      is.numeric(term_data()[[input$primary_exp_var]])
    })

    output$influence_plot <- renderPlot({

      if (input$secondary_exp_var == "(none)") {
        p <- ggplot(term_data(), aes_(x = as.name(input$primary_exp_var), y = ~class_votes))
      } else {
        p <- ggplot(term_data(), aes_(x = as.name(input$primary_exp_var), y = ~class_votes, color = as.name(input$secondary_exp_var))) +
          scale_color_brewer(type = "qual")
      }

      p <- p +
        geom_jitter(alpha = 0.1) +
        geom_smooth() +
        theme_bw(base_size = 18) +
        ylim(0, 1) +
        labs(y = "Probability of falling to selected class")

      if (log_the_x())
        p <- p + scale_x_log10(labels = scales::comma)

      p
    })
  })
}
