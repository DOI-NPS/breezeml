highLevelInput <- function(id) {
  layout_columns(
    card(
      card_header("Title"),
      textInput(NS(id, "title"), label = NULL, width = "100%", updateOn = "blur"),
      helpText("Here is some guidance on selecting a good title")
    ),
    card(
      card_header("Abstract")
    ),
    card(
      card_header("Methods")
    ),
    card(
      card_header("Additional notes")
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

highLevelServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(title = input$title)
    })
  })
}