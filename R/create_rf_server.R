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

    output$term_buttons <- renderUI({
      radioButtons("exp_var", label = "Term", choices = terms())
    })

    output$class_checklist <- renderUI({
      checkboxGroupInput("class_var", label = "Classes to compare", choices = classes(), selected = classes()[1])
    })

    term_data <- reactive({
      original_data <- data
      joined_data <- dplyr::bind_cols(original_data, as.data.frame(rf_votes))
      tidyr::gather_(joined_data,
                     key_col = "class",
                     value_col = "votes",
                     gather_cols = input$class_var)
    })

    output$influence_plot <- renderPlot({
      names(term_data())
      ggplot(term_data(), aes_(x = as.name(input$exp_var), y = ~votes, color = ~votes)) +
        geom_jitter(alpha = 0.5) +
        facet_wrap(~ class)
    })
  })
}
