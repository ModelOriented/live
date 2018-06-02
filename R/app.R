#' Function that starts a Shiny app which helps use LIVE.
#'
#' @param train_data dataset from which observations will be sampled.
#' @param black_box_model Pre-trained  model with predict interface.
#' @param target character, name of the response variable.
#' @param explained_data Data frame with predictions to explain.
#'
#' @import shiny
#'
#' @export
#'
#' @return shiny app
#'

live_shiny <- function(train_data, black_box_model, target, explained_data = train_data) {
  shinyApp(
    ui = fluidPage(
      column(3,
             sliderInput("instance", "Explained prediction (row number)",
                         min = 1, max = nrow(explained_data),
                         step = 1, round = T, value = 1),
             sliderInput("size", "Size of simulated dataset",
                         min = 100, max = 10000, step = 100,
                         round = T, value = 1000),
             selectInput("method", "Sampling method", choices = c("live", "lime"),
                         selected = "live"),
             checkboxInput("standardize", "Center predictors"),
             checkboxInput("selection", "Variable selection"),
             selectInput("whitebox", "Explanation model",
                         choices = paste(mlr::listLearners(warn.missing.packages = F)$type,
                                         mlr::listLearners(warn.missing.packages = F)$short.name,
                                         sep = "."),
                         selected = "regr.lm"),
             selectInput("fixed", "Fixed variables", choices = colnames(train_data),
                         selected = NULL, multiple = TRUE)

      ),
      column(9, plotOutput("main_plot"))
    ),
    server = function(input, output) {
      similars <- reactive({
        sample_locally2(train_data, explained_data[input$instance, ],
                        target, input$size, input$method,
                        input$fixed)
      })

      similars2 <- reactive({
        add_predictions2(similars(), black_box_model)
      })
      expl <- reactive({
        fit_explanation2(similars2(), input$whitebox, standardize = input$standardize,
                         selection = input$selection)
      })
      output$main_plot <- renderPlot(plot(expl(), type = "waterfall"))
    }
  )
}

