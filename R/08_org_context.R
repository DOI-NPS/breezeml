# 08_org_context.R v1
#
# RENAMED from 09_org_context.R to match actual tab order (Units & Project
# is tab 8, not tab 9) as part of converting the app into the breezeml
# R package. Content otherwise unchanged from 09_org_context.R's last
# version - see prior conversation history for full change log.
#
# Corresponds to skeleton.Rmd's "Add content unit links" (park units,
# EMLeditor::set_content_units()), "Add the Producing Unit(s)"
# (EMLeditor::set_producing_units()), "Add your data package to a
# DataStore project" (EMLeditor::set_project()), and "Add cross
# references" (EMLeditor::set_cross_reference()) sections.
#
# All four are optional per skeleton.Rmd - a data package may have no
# specific location (species list, lab samples), no project link, and no
# cross-references. Producing unit(s) ARE the one field here skeleton.Rmd
# treats as generally expected (who made this package), but EMLeditor
# doesn't strictly require it either, so it's left optional here too
# rather than blocking generation over it.
#
# Like 07_permissions.R, this tab's state is applied to the EML object via
# EMLeditor::set_*() calls in run_generation() (09_generate.R), not
# written to a .txt template.

# NPS park unit 4-letter codes - a real implementation should pull this
# from a live NPS unit list/API rather than a hardcoded sample. Flagged
# for follow-up; using a short illustrative set for now so the multi-select
# UI has real choices to demonstrate the pattern.
NPS_UNIT_CHOICES <- c(
  "ROMO - Rocky Mountain National Park",
  "GRSA - Great Sand Dunes National Park and Preserve",
  "YELL - Yellowstone National Park",
  "BICA - Bighorn Canyon National Recreation Area",
  "EVER - Everglades National Park"
)

org_context_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    card(
      card_header("Park Units (content) - optional"),
      helpText("Park units where the DATA were collected. If data span ",
               "multiple units within a network, list each unit ",
               "individually rather than the network - bounding box ",
               "coordinates are generated per unit."),
      selectInput(ns("content_units"), NULL, choices = NPS_UNIT_CHOICES,
                  multiple = TRUE, width = "100%")
    ),
    card(
      card_header("Producing Unit(s)"),
      helpText("The unit(s) responsible for generating the data package - ",
               "may be a single park, a network, or several. May overlap ",
               "with, or differ entirely from, the content units above."),
      selectInput(ns("producing_units"), NULL, choices = NPS_UNIT_CHOICES,
                  multiple = TRUE, width = "100%")
    ),
    card(
      card_header("DataStore Project (optional)"),
      helpText("DataStore only supports one project connection per data ",
               "package. The project must already exist and be public."),
      numericInput(ns("project_id"), "DataStore Project Reference ID",
                   value = NA, width = "100%")
    ),
    card(
      card_header("Cross-references (optional)"),
      helpText("Reference IDs for related DataStore items (e.g. a sampling ",
               "scheme diagram) not included in the data package itself."),
      layout_columns(
        numericInput(ns("new_xref"), NULL, value = NA, width = "100%"),
        actionButton(ns("add_xref"), "Add", class = "btn-primary btn-sm"),
        col_widths = c(10, 2)
      ),
      DT::DTOutput(ns("xref_table"))
    ),
    col_widths = c(-2, 8, -2), fill = FALSE
  )
}

#' @return reactive() list:
#'   $content_units, $producing_units - character vectors of 4-letter codes (may be empty)
#'   $project_id - integer or NA
#'   $cross_references - integer vector (may be empty)
#'   $valid, $errors
org_contextServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    extract_code <- function(x) sub(" -.*$", "", x)
    
    xrefs <- reactiveVal(integer(0))
    
    observeEvent(input$add_xref, {
      req(input$new_xref, !is.na(input$new_xref))
      ref <- as.integer(input$new_xref)
      current <- xrefs()
      if (ref %in% current) {
        showNotification("That reference ID is already in the list.", type = "warning")
      } else {
        xrefs(c(current, ref))
      }
      updateNumericInput(session, "new_xref", value = NA)
    })
    
    output$xref_table <- DT::renderDT({
      refs <- xrefs()
      display_df <- tibble::tibble(reference_id = refs)
      if (nrow(display_df) > 0) {
        display_df$remove <- vapply(seq_len(nrow(display_df)), function(i) {
          as.character(
            actionButton(session$ns(paste0("remove_xref_", i)), "Remove",
                         class = "btn-danger btn-sm",
                         onclick = sprintf(
                           'Shiny.setInputValue(\"%s\", %d, {priority: \"event\"})',
                           session$ns("remove_xref"), i
                         ))
          )
        }, character(1))
      } else {
        display_df$remove <- character(0)
      }
      DT::datatable(display_df, rownames = FALSE, selection = "none",
                    colnames = c("Reference ID", ""), escape = FALSE,
                    options = list(dom = 't', pageLength = -1))
    })
    
    observeEvent(input$remove_xref, {
      idx <- input$remove_xref
      current <- xrefs()
      req(idx >= 1, idx <= length(current))
      xrefs(current[-idx])
    })
    
    reactive({
      project_id <- if (!is.null(input$project_id) && !is.na(input$project_id)) {
        as.integer(input$project_id)
      } else {
        NA_integer_
      }
      
      list(
        content_units = extract_code(input$content_units %||% character(0)),
        producing_units = extract_code(input$producing_units %||% character(0)),
        project_id = project_id,
        cross_references = xrefs(),
        valid = TRUE,   # everything here is optional - nothing currently blocks generation
        errors = character(0)
      )
    })
  })
}

#' Apply this tab's state to an in-memory EML object. Called from
#' run_generation() alongside apply_permissions_to_eml(), after make_eml()
#' and before write_eml(). Every piece here is optional, so each call is
#' skipped if its corresponding state is empty/NA.
#'
#' force = TRUE / NPS = TRUE on every call, matching apply_permissions_to_eml()
#' - see that function's docstring in 07_permissions.R for the rationale.
apply_org_context_to_eml <- function(my_metadata, state) {
  if (length(state$content_units) > 0) {
    my_metadata <- EMLeditor::set_content_units(my_metadata, state$content_units, force = TRUE, NPS = TRUE)
  }
  if (length(state$producing_units) > 0) {
    my_metadata <- EMLeditor::set_producing_units(my_metadata, state$producing_units, force = TRUE, NPS = TRUE)
  }
  if (!is.na(state$project_id)) {
    my_metadata <- EMLeditor::set_project(my_metadata, state$project_id, force = TRUE, NPS = TRUE)
  }
  for (ref in state$cross_references) {
    my_metadata <- EMLeditor::set_cross_reference(my_metadata, ref, force = TRUE, NPS = TRUE)
  }
  my_metadata
}