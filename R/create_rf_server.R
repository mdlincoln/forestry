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

    output$log_the_x <- renderUI({
      checkboxInput("log_x_axis", "Log-transform the x-axis? (ignored for categorical variables)", value = FALSE)
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
      d <- tidyr::gather_(joined_data,
                     key_col = "class",
                     value_col = "votes",
                     gather_cols = input$class_var)

      if (input$secondary_exp_var != "(none)" & is.numeric(d[[input$secondary_exp_var]])) {
        mdots <- list(lazyeval::interp(~cut(var2, breaks = quantile(var2, probs = seq(0, 1, length.out = 5))), var2 = as.name(input$secondary_exp_var)))
        d <- dplyr::mutate_(iris, .dots = setNames(mdots, input$secondary_exp_var))
      }

      d
    })

    is_primary_continuous <- reactive({
      is.numeric(term_data()[[input$primary_exp_var]])
    })

    output$influence_plot <- renderPlot({

      p <- ggplot(term_data(), aes_(x = as.name(input$primary_exp_var), y = ~votes, color = ~profit_type)) +
        geom_jitter(alpha = 0.5) +
        theme_bw(base_size = 18)

      if (log_the_x())
        p <- p + scale_x_log10(labels = scales::comma)

      if (input$secondary_exp_var == "(none)") {
        p + facet_wrap("class", labeller = label_both)
      } else {
        p + facet_grid(paste0(input$secondary_exp_var, " ~ class"), labeller = label_both)
      }

      p
    })
  })
}
