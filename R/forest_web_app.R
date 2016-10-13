#' @import shiny
create_rf_app <- function(rf, rf_name) {
  shinyApp(
    ui = create_rf_ui(rf_name),
    server = create_rf_server(rf))
}

#' @import shiny
#' @import ggplot2
create_rf_server <- function(rf, rf_name) {

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

    term_data <- reactive({
      original_data <- rf_data(rf)
      joined_data <- dplyr::bind_cols(original_data, as.data.frame(rf_votes))
      tidyr::gather_(joined_data,
                     key_col = "class",
                     value_col = "certainty",
                     gather_cols = classes())
    })

    output$influence_plot <- renderPlot({
      names(term_data())
      ggplot(term_data(), aes_(x = as.name(input$exp_var), y = ~certainty, color = ~certainty)) +
        geom_jitter(alpha = 0.5) +
        facet_wrap(~ class)
    })
  })
}

#' @import shiny
create_rf_ui <- function(rf, rf_name) {
  shinyUI(
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          uiOutput("term_buttons")
        ),
        mainPanel(
          plotOutput("influence_plot")
        )
      )
    )
  )
}
