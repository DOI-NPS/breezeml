highLevelInput <- function(id) {
  layout_columns(
    card(
      card_header("Title"),
      textInput(NS(id, "title"), label = NULL, width = "100%", updateOn = "blur"),
      helpText("A good title should address What the data are, Where they are from and When they were collected. Avoid acronyms: spell out park and network units.")
    ),
    card(
      card_header("Abstract"),
      textInput(NS(id, "abstract"), label = NULL, width = "100%", updateOn = "blur"),
      helpText("An abstract should allow a non-expert to understand Why the study was conducted as well as How, Where, and When it was conducted and what data were collected. An abstract is typically about 250 words.")
    ),
    card(
      card_header("Methods"),
      textInput(NS(id, "methods"), label = NULL, width = "100%", updateOn = "blur"),
      helpText("Methods should contain sufficient detail that an expert in the field could repeat the study. The Methods likely require citing references such as SOPs and Protocols; however, simply citing these sources is insufficient. Methods should include not only experimental design and data collection protocols but also data quality assurance and control methods as well as any deviations from the cited resources.")
    ),
    card(
      card_header("Additional notes"),
      textInput(NS(id, "additional_notes"), label = NULL, width = "100%", updateOn = "blur"),
      helpText("Additional Notes include any information that may be useful to a data user that are not included elsewhere. If there are citations in the Methods section the Additional Notes section is a great place to include the full citations and URLs to cited resources.")
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