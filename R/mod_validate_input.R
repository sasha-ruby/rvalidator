#' Validate Input Module UI
#'
#' @param id The module id.
#' @param input_type The type of input (e.g., "textInput", "numericInput").
#' @param label The label of the input.
#' @param ... Additional arguments passed to the input function.
#' @return Shiny UI element.
validate_input_ui <- function(id, input_type = "textInput", label, ...) {
  ns <- NS(id)
  tagList(
    shinyFeedback::useShinyFeedback(),
    do.call(input_type, c(list(inputId = ns("input"), label = label), list(...)))
  )
}

#' Validate Input Module Server
#'
#' @param id The module id.
#' @param validators A list of validator functions.
#' @return Reactive value of the input.
validate_input_server <- function(id, validators) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$input, {
      validation <- validate_input(input$input, validators)
      if (!validation$is_valid) {
        shinyFeedback::showFeedbackDanger(
          "input",
          paste(validation$messages, collapse = " ")
        )
      } else {
        shinyFeedback::hideFeedback("input")
      }
    })
    return(reactive(input$input))
  })
}
