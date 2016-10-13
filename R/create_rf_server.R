#' @import shiny
#' @import ggplot2
create_rf_server <- function(rf, data) {

  shinyServer(function(input, output, session) {

    rf_votes <- rf[["votes"]]

    classes <- reactive({
      rf[["classes"]]
    })

    terms <- reactive({
      attr(rf$terms, "term.labels")
    })

    output$primary_term_buttons <- renderUI({
      radioButtons("primary_exp_var", label = "Term", choices = terms())
    })

    output$secondary_term_buttons <- renderUI({
      radioButtons("secondary_exp_var", label = "Term", choices = c("(none)", terms()), selected = "(none)")
    })

    is_primary_continuous <- reactive({
      is.numeric(term_data()[[input$primary_exp_var]])
    })

    output$log_the_x <- renderUI({
      if (is_primary_continuous()) {
        checkboxInput("log_x_axis", "Log-transform the x-axis?", value = FALSE)
      }
    })

    log_the_x <- reactive({
      if (is.null(input$log_x_axis)) {
        FALSE
      } else {
        input$log_x_axis
      }
    })

    output$class_checklist <- renderUI({
      checkboxGroupInput("class_var", label = "Classes to compare", choices = classes(), selected = classes()[1])
    })

    term_data <- reactive({
      joined_data <- dplyr::bind_cols(data, as.data.frame(rf_votes))
      tidyr::gather_(joined_data,
                     key_col = "class",
                     value_col = "votes",
                     gather_cols = input$class_var)
    })

    output$influence_plot <- renderPlot({
      p <- ggplot(term_data(), aes_(x = as.name(input$primary_exp_var), y = ~votes, color = ~votes)) +
        geom_jitter(alpha = 0.5) +
        scale_color_continuous(guide = FALSE) +
        theme_bw(base_size = 18)

      if (log_the_x())
        p <- p + scale_x_log10(labels = scales::comma)

      if (input$secondary_exp_var == "(none)") {
        p + facet_wrap("class", labeller = label_both)
      } else {
        p + facet_grid(paste0(input$secondary_exp_var, " ~ class"), labeller = label_both)
      }
    })
  })
}
