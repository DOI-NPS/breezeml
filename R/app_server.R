# app_server.R v6
#
# v6: removed the v5 TEMP diagnostic block (str()/dput() on
# saved$geography after load_app_state()). That diagnostic did its job -
# it PROVED lat_col/lon_col were correct and clean coming out of
# load_app_state(), exonerating app_state.R/jsonlite and localizing the
# lon_col restore bug entirely to 05_geography.R, where it was then
# root-caused and fixed (see 05_geography.R v13: a pending restore value
# was being discarded by an intermediate/stale table-change observer
# firing before the correct-table firing could apply it, because
# updateSelectInput(selected=) is asynchronous). No functional change in
# this file vs v4; diagnostic removed only.
#
# v4: fixed a real class of bug found in testing - jsonlite::write_json's
# auto_unbox = TRUE (needed so plain scalars like metadata_id/project_id
# serialize as bare values, not 1-element arrays) has a side effect:
# it ALSO collapses any length-1 or length-0 VECTOR field to a bare
# scalar/null, not a 1-element/empty array. Any "list of things" field
# saved with exactly 0 or 1 items therefore round-tripped with the wrong
# shape on load - confirmed via testing with keywords (one keyword saved
# -> kw$keyword came back as a length-1 character scalar and
# kw$keywordThesaurus as NULL instead of NA, silently producing an empty
# keywords tibble on restore).
#
# FIX: every field below that represents "a list of things" (as opposed
# to a genuine single scalar value) is now wrapped in I() before being
# assigned into `state`. I() (base R's "as-is" marker) tells
# jsonlite::write_json to always serialize that value as a JSON array,
# regardless of length - overriding auto_unbox for that specific field
# without having to disable it globally (which would break every genuine
# scalar field instead). This means every restore() can now trust it's
# always receiving an array/list shape for these fields, at any length
# (0, 1, or many) - the defensive NULL/length-1 handling added to Tab 1's
# restore() for keywords as a first patch is no longer strictly necessary
# but is left in place as harmless extra defense.
#
# Fields wrapped in I(): high_level_info$keywords' keyword/keywordThesaurus,
# people's per-category columns, data_tables$metadata's columns,
# fields$tables' per-table attributes/catvars columns, taxonomy$pairs'
# table/column, org_context's content_units/producing_units/
# cross_references/cross_reference_titles. Left UNWRAPPED (genuine
# scalars): metadata_id, package_title, data_status, abstract, methods,
# additional_notes, start_date, end_date, project_id, access_level,
# legal_authority_id, contact_email, authority_designator, int_rights,
# language, has_coords, lat_table/lat_col/lon_table/lon_col/site_table/
# site_col, has_taxa, doi_state's reference_id/doi.
#
# Also fixed: taxonomy pairs$column was built with purrr::map() (list of
# length-1 vectors) instead of purrr::map_chr() (flat vector) - wrong
# shape independent of the I()/auto_unbox issue, confirmed as the cause
# of taxonomy not repopulating after CSV re-upload in testing.
#
# This required updating EVERY tab module call site from v2/v3, because
# every tab module's return shape changed from a bare reactive() to
# list(data = <reactive>, restore = <function>, ...) as part of adding
# restore() support:
#   high_level()      -> high_level$data()
#   people()          -> people$data()
#   tables()          -> tables$data()
#   fields()          -> fields$data()      (and fields()$tables -> fields$data()$tables)
#   geography()       -> geography$data()
#   taxonomy()        -> taxonomy$data()
#   permissions()     -> permissions$data()
#   org_context()     -> org_context$data()
#
# SAVE: output$save_session is a downloadHandler() that collects every
# tab's CURRENT $data() into app_state.R's default_app_state() shape and
# calls save_app_state(). Filename includes a timestamp so repeated saves
# don't silently overwrite each other in the browser's downloads folder.
#
# LOAD: observeEvent(input$load_session, ...) reads the uploaded JSON via
# load_app_state(), then:
#   1. immediately calls restore() on the four tabs with no upload
#      dependency: high_level, people, permissions, org_context
#   2. calls restore() ONCE on each of the four dependent tabs (tables,
#      fields, geography, taxonomy) - each of those modules internally
#      handles re-attempting as matching files are re-uploaded (see each
#      tab's own v9+ header comments), so app_server.R does NOT need its
#      own retry loop
#   3. stores doi_state from the save file, if present
#
# PENDING-FILES BANNER: pending_upload_files() aggregates
# unmatched_saved_files() from tables/fields (geography/taxonomy don't
# expose their own - they depend on the SAME uploaded tables tracked by
# Tab 3, so Tab 3's list is the authoritative one; Tab 4's is included
# separately since field-level saved data could theoretically reference a
# table Tab 3's own metadata restore already matched, if e.g. a save file
# were hand-edited - defensive, not expected in normal use). Rendered as
# a persistent notice via output$pending_files_banner, placed in the
# sidebar (see app_ui.R) so it's visible regardless of which tab is open.
#
# FIXED: fieldsServer() (04_fields.R v20) now returns
# list(tables=, valid=, errors=) instead of just the bare named list of
# tables - it previously computed no validation at all, meaning Tab 4
# (numeric columns missing units, Date columns missing format strings,
# categorical codes missing definitions, blank attribute definitions)
# could NEVER block Generate, regardless of content. Confirmed via live
# testing: a numeric column with no unit was silently accepted, "Generate"
# reported everything complete, and the resulting EML silently dropped
# that entire data table and failed schema validation.
#
# This required two changes here:
#   1. all_errors() now includes fields()'s $valid/$errors, same pattern
#      as every other tab.
#   2. Every other place that consumed fields() expecting the OLD bare
#      list-of-tables shape now uses fields()$tables instead (the preview
#      script handler and do_generate()'s call into run_generation()).
#
# The top-level Shiny server function, converted from app.R's top-level
# `server <- function(input, output, session) {...}` script variable -
# same packaging rationale as app_ui.R.

#' bReezEML server logic
#' @param input,output,session standard Shiny server arguments
#' @keywords internal
app_server <- function(input, output, session) {
  high_level <- highLevelServer("high_level")
  people <- peopleServer("people")
  tables <- tableMetadataServer("table_metadata")

  # shared reactive of just the parsed data.frames, keyed by file name -
  # this is what downstream modules (fields, geography, taxonomy) consume
  table_data <- shiny::reactive({
    tables$data()$data
  })

  fields <- fieldsServer("fields", table_data)
  geography <- geographyServer("geography", table_data)
  taxonomy <- taxonomyServer("taxonomy", table_data)
  permissions <- permissionsServer("permissions")
  org_context <- org_contextServer("org_context")

  # Tracks the most recent successfully-created DataStore draft reference
  # this session, if any. NULL until a create-new-DOI generate succeeds
  # once. Persists across regenerates so the same draft/DOI is reused
  # rather than silently creating duplicates. Also restorable from a
  # saved session file (see Load handling below).
  doi_state <- shiny::reactiveVal(NULL)

  # Collect every tab's validation errors in one place so "Generate" has a
  # single, clear gate rather than failing deep inside make_eml().
  all_errors <- shiny::reactive({
    hl <- high_level$data()
    pp <- people$data()
    tb <- tables$data()
    fl <- fields$data()
    pm <- permissions$data()
    oc <- org_context$data()
    c(
      if (!isTRUE(hl$valid)) hl$errors,
      if (!isTRUE(pp$valid)) pp$errors,
      if (!isTRUE(tb$valid)) tb$errors,
      if (!isTRUE(fl$valid)) fl$errors,
      if (!isTRUE(pm$valid)) pm$errors,
      if (!isTRUE(oc$valid)) oc$errors
    )
  })

  output$generate_status <- shiny::renderUI({
    errs <- all_errors()
    if (length(errs) == 0) {
      return(shiny::tags$div(class = "text-success", "All required information is complete. Ready to generate."))
    }
    shiny::tagList(
      shiny::tags$div(class = "text-danger", "Resolve the following before generating:"),
      shiny::tags$ul(lapply(errs, shiny::tags$li))
    )
  })

  shiny::observeEvent(input$preview_script, {
    script <- build_generation_script(
      high_level$data(), people$data(), tables$data(), fields$data()$tables,
      geography$data(), taxonomy$data(), permissions$data(), org_context$data()
    )
    shiny::showModal(shiny::modalDialog(
      title = "Generated script (preview)",
      shiny::tags$pre(script, style = "white-space: pre-wrap; max-height: 500px; overflow-y: auto;"),
      size = "l", easyClose = TRUE, footer = shiny::modalButton("Close")
    ))
  })

  output$generation_result <- shiny::renderUI({ NULL })

  # Actually runs the generation pipeline exactly once.
  # @param create_new_doi passed straight through to run_generation() -
  #   TRUE only right after the user has confirmed creating/replacing a
  #   DataStore draft reference; FALSE (the default) reuses whatever
  #   doi_state() already holds, if anything.
  do_generate <- function(create_new_doi = FALSE) {
    wf <- input$working_folder
    if (!nzchar(wf) || !dir.exists(wf)) {
      shiny::showNotification("Working folder does not exist or is not accessible.", type = "error")
      return(invisible(NULL))
    }

    shiny::withProgress(message = "Generating EML...", value = 0.3, {
      result <- run_generation(
        parent_folder = wf,
        high_level_state = high_level$data(),
        people_state = people$data(),
        tables_state = tables$data(),
        fields_state = fields$data()$tables,
        geo_state = geography$data(),
        taxonomy_state = taxonomy$data(),
        permissions_state = permissions$data(),
        org_context_state = org_context$data(),
        doi_state = doi_state(),
        create_new_doi = create_new_doi
      )
      shiny::incProgress(0.7)

      if (isTRUE(result$success) && !is.null(result$doi_state)) {
        doi_state(result$doi_state)
      }

      output$generation_result <- shiny::renderUI({
        if (isTRUE(result$success)) {
          shiny::tagList(
            shiny::tags$div(class = "text-success mt-2", result$message),
            shiny::tags$div(class = "text-muted", paste0("Written to: ", result$xml_path)),
            if (!is.null(result$doi_state) && !is.na(result$doi_state$doi)) {
              shiny::tags$div(class = "text-muted", paste0("DataStore DOI: ", result$doi_state$doi))
            },
            if (!is.null(result$content_issues)) {
              shiny::tagList(
                shiny::tags$hr(),
                shiny::tags$details(
                  shiny::tags$summary(
                    style = "cursor: pointer; font-weight: 600;",
                    "Review notes from EMLassemblyline::issues() (click to expand)"
                  ),
                  shiny::tags$p(class = "text-muted mt-2",
                                "This always runs and often includes expected notes (e.g. no ",
                                "Principal Investigator listed, which NPS data packages don't ",
                                "require). Skim for anything indicating a field was skipped or ",
                                "not understood - if something you entered in Tabs 4-6 isn't ",
                                "reflected here, you can go fix it and generate again without ",
                                "losing any of your other entries."),
                  shiny::tags$pre(
                    style = "white-space: pre-wrap; max-height: 300px; overflow-y: auto;",
                    result$content_issues
                  )
                )
              )
            }
          )
        } else {
          shiny::tagList(
            shiny::tags$div(class = "text-danger mt-2", result$message),
            if (!is.null(result$validation_errors)) {
              shiny::tags$ul(lapply(result$validation_errors, shiny::tags$li))
            }
          )
        }
      })
    })
  }

  shiny::observeEvent(input$generate_script, {
    errs <- all_errors()
    if (length(errs) > 0) {
      shiny::showNotification("Cannot generate: required information is missing. See the list above.",
                              type = "error")
      return(invisible(NULL))
    }

    # DataStore draft reference creation is a real, non-idempotent side
    # effect - always confirm before creating or replacing one, even if
    # this is the very first generate this session. Regenerating with the
    # SAME draft (set_doi(), a pure local edit with no API call - see
    # 09_generate.R) is offered as a distinct option from replacing the
    # draft outright (set_datastore_doi(), a real DataStore API call) -
    # restored after briefly being removed; the removal was based on a
    # misdiagnosis of set_doi()'s "unused argument" error as evidence the
    # reuse path was broken, when actually set_doi() correctly never
    # accepts `dev` (it makes no API call at all).
    existing <- doi_state()

    if (is.null(existing)) {
      confirmation <- build_doi_confirmation(NULL)
      shiny::showModal(shiny::modalDialog(
        title = confirmation$title,
        confirmation$message,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_create_doi", "Create draft & generate", class = "btn-primary")
        )
      ))
    } else {
      # a draft already exists - let the user choose to just regenerate
      # locally (reusing the existing DOI, no DataStore call at all) or
      # explicitly replace the draft
      shiny::showModal(shiny::modalDialog(
        title = "Regenerate metadata",
        paste0("You already have a draft reference (ID: ", existing$reference_id,
               ", DOI: ", existing$doi, "). How would you like to proceed?"),
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("confirm_reuse_doi", "Regenerate (keep existing draft)", class = "btn-secondary"),
          shiny::actionButton("confirm_replace_doi", "Replace draft reference", class = "btn-danger")
        )
      ))
    }
  })

  shiny::observeEvent(input$confirm_create_doi, {
    shiny::removeModal()
    do_generate(create_new_doi = TRUE)
  })

  shiny::observeEvent(input$confirm_replace_doi, {
    shiny::removeModal()
    do_generate(create_new_doi = TRUE)
  })

  shiny::observeEvent(input$confirm_reuse_doi, {
    shiny::removeModal()
    do_generate(create_new_doi = FALSE)
  })

  # ---- Save/Load session (Save/Load Progress, sidebar) ----

  output$save_session <- shiny::downloadHandler(
    filename = function() {
      paste0("breezeml_session_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      state <- default_app_state()

      # I() marks a value as "as-is" for jsonlite::write_json(), forcing
      # it to always serialize as a JSON array regardless of length (0,
      # 1, or many) - without this, auto_unbox = TRUE (needed for genuine
      # scalars elsewhere in this same state list) collapses any length-1
      # vector to a bare scalar and any length-0 vector to `[]` vs `null`
      # inconsistently, which silently breaks restore() on the read side
      # for any "list of things" field that happens to have exactly one
      # (or zero) items saved. See file header note - this was a
      # confirmed bug (keywords, taxonomy pairs) found in testing.

      hl <- high_level$data()
      state$high_level_info <- list(
        metadata_id = hl$metadata_id,
        package_title = hl$package_title,
        data_status = hl$data_status,
        abstract = hl$abstract,
        methods = hl$methods,
        additional_notes = hl$additional_notes,
        keywords = list(
          keyword = I(hl$keywords$keyword),
          keywordThesaurus = I(hl$keywords$keywordThesaurus)
        ),
        start_date = if (!is.null(hl$start_date) && !is.na(hl$start_date)) format(hl$start_date, "%Y-%m-%d") else NA_character_,
        end_date = if (!is.null(hl$end_date) && !is.na(hl$end_date)) format(hl$end_date, "%Y-%m-%d") else NA_character_
      )

      pp <- people$data()
      as_person_list <- function(df) {
        if (nrow(df) == 0) return(list())
        purrr::map(as.list(df), I)
      }
      state$people <- list(
        authors = as_person_list(pp$authors),
        contacts = as_person_list(pp$contacts),
        contributors = as_person_list(pp$contributors),
        editors = as_person_list(pp$editors)
      )

      tb <- tables$data()
      state$data_tables <- list(
        metadata = if (nrow(tb$metadata) == 0) list() else purrr::map(
          as.list(tb$metadata[, c("file_name", "table_name", "description")]), I
        )
      )

      fl <- fields$data()
      state$fields <- list(
        tables = purrr::map(fl$tables, function(tbl_state) {
          list(
            attributes = purrr::map(as.list(tbl_state$attributes), I),
            catvars = purrr::map(as.list(tbl_state$catvars), I)
          )
        })
      )

      geo <- geography$data()
      state$geography <- list(
        has_coords = isTRUE(geo$enabled),
        lat_table = geo$table %||% NA_character_,
        lat_col = geo$lat_col %||% NA_character_,
        lon_table = geo$table %||% NA_character_,
        lon_col = geo$lon_col %||% NA_character_,
        site_table = geo$table %||% NA_character_,
        site_col = geo$site_col %||% NA_character_
      )

      tax <- taxonomy$data()
      state$taxonomy <- list(
        has_taxa = isTRUE(tax$enabled),
        pairs = if (isTRUE(tax$enabled) && length(tax$pairs) > 0) {
          list(
            table = I(purrr::map_chr(tax$pairs, "table")),
            column = I(purrr::map_chr(tax$pairs, "column"))
          )
        } else {
          list(table = I(character(0)), column = I(character(0)))
        },
        authorities = I(if (isTRUE(tax$enabled)) {
          names(TAXA_AUTHORITIES)[TAXA_AUTHORITIES %in% tax$authorities]
        } else {
          character(0)
        })
      )

      pm <- permissions$data()
      state$permissions <- list(
        access_level = pm$access_level,
        legal_authority_id = pm$legal_authority_id,
        contact_email = pm$contact_email,
        authority_designator = pm$authority_designator,
        int_rights = pm$int_rights,
        language = pm$language
      )

      oc <- org_context$data()
      state$org_context <- list(
        content_units = I(oc$content_units),
        producing_units = I(oc$producing_units),
        project_id = oc$project_id,
        project_title = oc$project_title,
        cross_references = I(oc$cross_references),
        cross_reference_titles = I(oc$cross_reference_titles)
      )

      state$doi_state <- doi_state()

      save_app_state(state, file)
    }
  )

  # Aggregated "please re-upload" targets from every dependent tab, for
  # the sidebar banner. Tables (Tab 3) is authoritative for file-level
  # matching; Fields (Tab 4) is included too since it tracks its own
  # pending list independently (see 04_fields.R's restore()) - in normal
  # use these two lists will be identical (both keyed off the same
  # uploaded files), but kept separate/unioned defensively rather than
  # assumed identical.
  pending_upload_files <- shiny::reactive({
    union(tables$unmatched_saved_files(), fields$unmatched_saved_files())
  })

  output$load_session_status <- shiny::renderUI({
    pending <- pending_upload_files()
    if (length(pending) == 0) return(NULL)
    shiny::tags$div(
      class = "alert alert-warning mt-2",
      style = "font-size: 0.85em;",
      shiny::tags$strong("Re-load on Tab 3 to finish restoring: "),
      paste(pending, collapse = ", ")
    )
  })

  shiny::observeEvent(input$load_session, {
    shiny::req(input$load_session)

    saved <- tryCatch(
      load_app_state(input$load_session$datapath),
      error = function(e) {
        shiny::showNotification(
          paste0("Could not load session file: ", conditionMessage(e)),
          type = "error", duration = 10
        )
        NULL
      }
    )
    shiny::req(saved)

    # Immediate-restore tabs - no upload dependency, safe to apply now.
    high_level$restore(saved$high_level_info)
    people$restore(saved$people)
    permissions$restore(saved$permissions)
    org_context$restore(saved$org_context)

    # Dependent tabs - each module internally re-attempts as matching
    # files are re-uploaded (see 03/04/05/06's own restore()
    # implementations). Called once here; no retry loop needed at this level.
    tables$restore(saved$data_tables$metadata)
    fields$restore(saved$fields$tables)
    geography$restore(saved$geography)
    taxonomy$restore(saved$taxonomy)

    if (!is.null(saved$doi_state)) {
      doi_state(saved$doi_state)
    }

    shiny::showNotification(
      paste0("Session loaded (saved ", saved$saved_at %||% "unknown time", "). ",
             "Re-upload your data files on Tab 3 to finish restoring tables, fields, ",
             "geography, and taxonomy."),
      type = "message", duration = 8
    )
  })

  `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
}
