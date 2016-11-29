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
      d <- bind_cols(data, as.data.frame(rf_votes)) %>%
        tidyr::gather_(key_col = "predicted", value_col = "votes", gather_cols = colnames(rf_votes))

      if (input$secondary_exp_var != "(none)" && is.numeric(d[[input$secondary_exp_var]])) {
        mdots <- list(lazyeval::interp(
          ~cut(var2, breaks = quantile(var2, probs = seq(0, 1, length.out = min(n_distinct(var2), 5)))),
          var2 = as.name(input$secondary_exp_var)
        ))

        d <- mutate_(d, .dots = setNames(mdots, input$secondary_exp_var))

        if (input$tertiary_exp_var != "(none)" && is.numeric(d[[input$tertiary_exp_var]])) {
          mdots <- list(lazyeval::interp(
            ~cut(var3, breaks = quantile(var3, probs = seq(0, 1, length.out = min(n_distinct(var3), 5)))),
            var3 = as.name(input$tertiary_exp_var)
          ))
          d <- mutate_(d, .dots = setNames(mdots, input$tertiary_exp_var))
        }
      }

      d
    })

    is_primary_continuous <- reactive({
      is.numeric(term_data()[[input$primary_exp_var]])
    })

    output$influence_plot <- renderPlot({

      p <- ggplot(term_data(), aes_(x = as.name(input$primary_exp_var), y = ~votes, color = ~predicted)) +
        scale_color_brewer(type = "qual") +
        geom_jitter(alpha = 0.1) +
        geom_smooth() +
        theme_bw(base_size = 18) +
        ylim(0, 1) +
        labs(y = "Probability of falling to selected class")

      if (log_the_x())
        p <- p + scale_x_log10(labels = scales::comma)

      if (input$secondary_exp_var != "(none)") {
        if (input$tertiary_exp_var != "(none)") {
          p <- p + facet_grid(paste0(input$tertiary_exp_var, " ~ ", input$secondary_exp_var), labeller = label_both)
        } else {
          p <- p + facet_wrap(input$secondary_exp_var)
        }
      }

      p
    })
  })
}
