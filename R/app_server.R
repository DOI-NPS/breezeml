# app_server.R v27
#
# v27: creation_dir() needs ONLY the metadata filename (Tab 1). The
# parent output folder falls back to getwd() - the same default Tab 9's
# own box carries - instead of being required, so the "Save template"
# button on Tab 5 works without visiting the Generate tab. input$
# working_folder is NULL until that tab has been rendered, which is why
# v26 refused.
#
# ---- v26 ----
#
# v26: computes the data_package_creation path (parent output folder from
# Tab 9 + package name from Tab 1) and passes it to geographyServer(), so
# Tab 5's "Save template" button can write the geography template there
# and tell the user where it went. derive_package_folder_name() from
# 09_generate.R is reused so the path cannot drift from the one
# setup_package_dirs() actually creates.
#
# ---- v25 ----
#
# v25: the saved session now records Tab 5's coordinate SOURCE
# (geography$coord_source), so a reloaded session returns to the
# geography-template option rather than silently to the data-table
# option. The coordinates themselves are still not saved - see
# 05_geography.R v17.
#
# v24: REMOVED A DUPLICATE, DEAD DEFINITION OF app_server().
#
# This file previously contained the whole server twice, end to end:
# a v22 copy on lines 1-1875 and this v23 copy on lines 1876-3846,
# each preceded by its own copy of the TAB*_NAV_VALUE constants. Both
# were top-level assignments to the same name, so R evaluated the file
# in order and the SECOND definition silently replaced the first. Only
# the v23 copy was ever called; the v22 copy was unreachable code.
#
# The duplication was not harmless. It made the file's line numbers
# ambiguous: three of breezeml's file boxes, among much else, appeared
# twice, and an edit to the first copy had no effect whatsoever on the
# running app. It also doubled every search hit in this 158 KB file.
#
# What was removed: lines 1-1875 in their entirety - the v22 header,
# the first set of TAB*_NAV_VALUE constants, and the v22 app_server()
# body. Nothing else changed. The two sets of constants were verified
# identical before removal, and the v22 body was verified to contain
# nothing absent from the v23 body except the one superseded line
#
#   congruenceServer("congruence", shiny::reactive(last_package_dir()))
#
# which v23 replaced with the three-argument form that passes
# on_revise. The full v22 version history is preserved below, since
# this copy's header already carried it.
#
# v23: check-to-revise callback. Import the checked XML in revise_draft
# mode and
# transactionally replace CSVs from that directory. Retain the checked
# package DOI/reference, never create a draft during this transition.
#
# v22: devtools::document() hygiene. Every roxygen comment (#') that
# sat INSIDE a function body is now a plain comment (#). The text is
# unchanged and still documents the inner closures for a reader; it is
# simply no longer addressed to roxygen2.
#
# Why it had to change: a #' block is only meaningful directly above a
# TOP-LEVEL object. Inside a body there is nothing for roxygen2 to attach
# it to, so it appends the text to the last tag of the preceding
# top-level block - which produced errors such as "@keywords must be only
# 1 line long, not 3" against a perfectly correct @keywords several
# hundred lines earlier. See 02_people.R's header for the full account.
#
# v21: the About control looks and sits the same on every screen, and
# the sidebar's state now follows the user rather than the screen.
#
#   * Default: closed on the start screen, open on every other screen.
#   * A user's own choice overrides that default and carries to every
#     screen afterwards, in whichever direction they chose. Close it
#     once and it stays closed as they move on; open it once and it
#     stays open.
#
# about_user_pref holds that choice. It starts NULL, meaning "no choice
# yet, use the screen's default", and is set from the browser by
# start_screen.R's onAboutClick(), which reports every real click on the
# control as input$about_user_pref. It has to come from the browser
# because the control is bslib's own and is not a Shiny input; the
# sidebar state input bslib offers is not arriving in this app, which is
# what v19 tried and v20 abandoned.
#
# go_to_screen() applies the preference when there is one and the
# screen's default otherwise.
#
# v20: the About fix no longer depends on this file. v19 added an
# observer on input$app_sidebar that called accordion_panel_open() when
# the sidebar opened; that did not surface the text, because the input
# bslib reports for a sidebar's state is not arriving here. The Overview
# panel is now expanded in the markup instead (app_ui.R v12:
# accordion(open = "overview")), which needs no round trip at all.
#
# The observer is KEPT below, because it still helps in one case: a user
# who collapses Overview by hand, closes the sidebar and presses About
# again gets it re-opened. Where the input never arrives the observer
# simply never fires, so it is harmless either way.
#
# v18: the sidebar is opened and closed with the screen. It is declared
# closed in app_ui.R (id "app_sidebar"), so the app opens on the start
# screen with the accordion collapsed, as the reference design shows.
# go_to_screen() opens it when the user enters the numbered tabs (the
# Session card with Save Progress is in it) and closes it again on a
# return to the start screen. The user can still open it anywhere with
# the "About" control at the lower left - that is bslib's own sidebar
# collapse control, renamed; see about_toggle_script() in start_screen.R.
#
# v17: no code change. The folder-window chain the revise screen reports
# is now the no-shell-script one in choose_directory.R v3 - Tcl/Tk and,
# where present, the desktop's own dialog - after the PowerShell method
# of v2 was withdrawn on security and cross-platform grounds. The
# messages and $attempts handling here are unchanged and work with any
# method list.
#
# v16: the revise screen's folder button now reports WHICH folder-window
# methods were tried and what each one did, using the $attempts the
# rewritten choose_directory_native() returns (choose_directory.R v2).
# Testing on Windows found utils::choose.dir() returning NA with no window
# ever drawn, which v15 could only describe as "no folder was selected";
# v2 treats that as "no window" and falls through to a PowerShell folder
# dialog, and this file now shows the user the chain when every method
# fails. No other change.
#
# v15: the "Select data package folder" button on the revise screen gave
# no feedback at all when the operating-system folder window did not
# appear or was closed without a choice, so it looked like the button did
# nothing. Three changes, all in the "Revise an existing data package"
# block:
#
#   - Every outcome of choose_directory_native() is now reported.
#     "cancelled" (the window was closed, or never appeared) says so and
#     opens the typed-path box; "unavailable" (no folder window can be
#     opened on this computer) says so and opens the typed-path box.
#     Before, only "unavailable" said anything and "cancelled" was a
#     silent no-op - the symptom reported from testing.
#   - The typed-path box is confirmed with its own "Use this folder"
#     button, matching the Congruence tab. Previously it reacted to every
#     keystroke, so a half-typed path cleared the selection.
#   - A plain "Enter the folder path instead" link is always available
#     under the button, so the screen is never a dead end whatever the
#     folder window does.
#
# The folder window itself is unchanged (choose_directory_native() in
# choose_directory.R, shared with the Congruence tab). If it does not
# appear on Windows, look for it behind the browser window: it is a modal
# window opened by R, and R waits for it, so the page will not respond
# until it is answered.
#
# v14: start-screen option c, "Revise an existing data package", now works
# on a package FOLDER instead of a single .xml file. All of the new code is
# in the block headed "Revise an existing data package", after the start
# screen block. Two routes, both ending in the same load:
#
#   local      the user selects the folder with the operating system's
#              folder window (choose_directory_native(), shared with the
#              Congruence tab), or types a path if no window can open.
#   datastore  the user enters a draft reference id. check_revise_ref
#              runs ds_check_draft_reference() (exists / is a data package
#              / is in DRAFT / user is an editor), the screen then shows
#              where files will be written and asks for confirmation, and
#              only then does ds_download_data_package() run
#              NPSutils::get_data_package(secure = TRUE, force = TRUE).
#              Downloads land in <working folder>/data/<reference id>.
#              Checks and download: datastore_download.R.
#
# Loading the folder (do_revise_load) imports its single *_metadata.xml
# with the same import_eml_state(mode = "revise_draft") call the .xml
# screen uses, so the DOI/reference is reused exactly as before, and then
# hands the folder's .csv files to the Tables module's new add_files()
# (03_data_tables.R v18). That is what makes the re-load notice empty
# itself: table, field, geography and taxonomy metadata restore without
# the user selecting any file a second time.
#
# The .xml screen (import_xml) still exists and is unchanged; it now
# serves only option d, "Create a new data package from existing
# metadata", where there are no data files to find.
#
# v13: wording and pending-file reporting.
#   - "upload" is no longer used for local files anywhere in the messages
#     (the start-screen help text, the import notification, and the
#     session-loaded notification and result).
#   - The sidebar banner (load_session_status), the pending_upload_files
#     reactive that fed it, and its go_to_tab3 click handler are removed.
#     The green result message on the .json screen now reads
#     "Session loaded (saved <time>). Re-load to finish restoring:
#     <file names>" with a "Re-load files now" link that opens the
#     data-table tab (go_to_reload). The file names come from the
#     session file via the Tables and Fields tabs' unmatched_saved_files(),
#     exactly as the old banner did, and shrink as files are re-loaded.
#     The .xml screen's result shows the same line, since the banner was
#     its only pointer to the files still needed.
#
# v12: START SCREEN. The sidebar import/restore card is gone; the app now
# opens on a start screen (app_ui.R v7, start_screen.R) with six options.
# All of the new code is in the block headed "Start screen and
# navigation", just before the XML import section:
#   - go_to_screen() / go_to_tab() move between the start screens and the
#     numbered tabs (a hidden navset "app_screen" wraps the tabs).
#   - Option a -> Tab 1; b -> the .json upload screen; c and d -> the .xml
#     upload screen; e -> Tab 10 (Congruence checks); f -> Tab 11 (Upload
#     and Publish).
#   - xml_import_mode (a reactiveVal set by options c and d) replaces the
#     old sidebar radio input$startup_mode, which no longer exists. The
#     XML import observer reads it, so "revise" keeps the DOI/reference
#     and "new package" does not, exactly as in v11.
#   - The file boxes load_session and import_eml are now created by
#     renderUI (output$load_session_input, output$import_eml_input) so
#     each visit to an upload screen starts with an empty box. Their
#     input ids are unchanged, so the load and import observers are as
#     before except for the points below.
#   - The JSON load now records its outcome in load_session_result so
#     the upload screen can show it (it only showed a pop-up before).
#   - import_eml_result is now created in the start-screen block (moved
#     from the import section); nothing else about it changed.
#   - The clickable "Re-load on Tab 3" banner now also leaves the start
#     screens (go_to_tab3 calls go_to_tab()).
#   - Choosing option a does NOT clear anything the user already entered
#     or loaded this session; it just opens Tab 1. Restart the app to
#     begin from a blank state.
#
# v11: fixed the XML import ignoring the user's startup choice. The import
# observer read input$import_eml_mode, an input that does not exist - the
# sidebar radio in app_ui.R is input$startup_mode. The NULL value always
# fell back to "new_package", so "Revise an existing draft data package"
# never retained the imported DOI/reference. The observer now reads
# input$startup_mode and accepts only "revise_draft" or "new_package"
# (anything else falls back to "new_package", which never reuses a DOI).
# The choice is read when the file is imported: changing the radio
# afterwards does not change an import that already happened - import the
# file again. No other behavior changed vs v10.
#
# v10: the orange "Re-load on Tab 3 to finish restoring: ..." sidebar banner
# (output$load_session_status) is now CLICKABLE. Clicking it switches the
# main navset (id "main_panel", defined in app_ui.R) to Tab 3
# ("3. Data tables"), where the user re-uploads the listed files. Details:
#   - The banner TEXT and its construction are unchanged. Only a pointer
#     cursor (style) and an onclick handler were added to the same div.
#   - onclick sends Shiny.setInputValue("go_to_tab3", <timestamp>,
#     {priority: "event"}); the "event" priority makes EVERY click fire the
#     observer, even repeated clicks while already on another tab.
#   - observeEvent(input$go_to_tab3) calls bslib::nav_select("main_panel",
#     selected = TAB3_NAV_VALUE). The tab has no explicit `value=` in
#     app_ui.R, so its value is its TITLE, "3. Data tables" - if that tab's
#     title is ever renamed in app_ui.R, update TAB3_NAV_VALUE below too.
#   - The banner is NOT dismissed on click. It still disappears only when
#     every listed file has been re-loaded (pending_upload_files() empties).
#   No other behavior changed vs v9.
#
# v9: wired in the Upload & Publish tab (11_upload_publish.R v1). Additive
# only - no existing behavior changed:
#   1. uploadPublishServer("upload_publish", ...) instantiated, passed:
#        - a reactive wrapper around last_package_dir() (the data_package/
#          directory the most recent successful Generate produced - the
#          directory EMLeditor::upload_data_package() needs)
#        - a reactive wrapper around doi_state() (prefills the reference_id
#          field with the draft reference created during Generate)
#        - a reactive giving the package title (for the reviewer-invite email)
#   2. SAVE: state$upload_publish is written, persisting only the
#      reference_id OVERRIDE from the tab (see app_state.R v4 / the tab's
#      restore() note - the self-certification checkbox is intentionally
#      not persisted).
#   3. LOAD: upload_publish$restore(saved$upload_publish) is called in the
#      immediate-restore group (no upload dependency - it only pushes a
#      single text input value).
# Tab 11 is NOT part of all_errors() - like the congruence tab it runs
# after Generate and never gates it.
#
# v8: made the generation-finished result far more obvious in the Generate
# tab's result area (output$generation_result). Previously success showed
# a small green text line and failure a small red text line - easy to miss
# after clicking Generate. Now BOTH outcomes lead with a large, prominent
# banner, visually distinct by outcome:
#   - success: green banner "Metadata generated"
#   - success-but-not-schema-valid (permissive gate in 09_generate.R v8
#     writes the .xml even when EML::eml_validate() failed): the green
#     "generated" banner carries an amber caveat strip noting it did not
#     pass schema validation and pointing to the Congruence checks tab.
#   - failure (the pipeline could not produce a file at all): red banner
#     "Generation failed".
# All the existing detail (written-to path, DOI, issues() output,
# validation errors) is retained BELOW the banner, unchanged. This is the
# only change vs v7.
#
# v7: wired in the congruence checks tab (10_congruence.R v2). Three
# additions vs v6, all additive - no existing behavior changed:
#   1. last_package_dir (reactiveVal) - holds the directory of the most
#      recently generated data package, set inside do_generate() on
#      success from dirname(result$xml_path).
#   2. congruenceServer("congruence", ...) instantiated, passed a reactive
#      wrapper around last_package_dir() so the module auto-clears its
#      results whenever a new Generate writes a new package (see
#      10_congruence.R's observeEvent(package_dir(), ...) reset).
#   3. (app_ui.R, separately) a nav_panel("Congruence checks",
#      congruenceUI("congruence")) after the Generate panel.
# The congruence tab is purely informational and runs AFTER Generate (it
# checks the finalized data_package/ directory including the DOI-finalized
# .xml), on a button press - it never gates Generate or anything else, so
# it is deliberately NOT part of all_errors().
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

# Value of the "3. Data tables" nav_panel in app_ui.R's navset
# (id = "main_panel"). That panel has no explicit `value=`, so its value is
# its title. Keep in sync with app_ui.R if the tab is renamed.
TAB3_NAV_VALUE <- "3. Data tables"

# Values of the other nav_panels the start screen can open (v12). Like
# TAB3_NAV_VALUE, each is the panel's title in app_ui.R because the panels
# have no explicit `value=`. Keep in sync with app_ui.R if a tab is renamed.
TAB1_NAV_VALUE  <- "1. High-level info"
TAB10_NAV_VALUE <- "10. Congruence checks"
TAB11_NAV_VALUE <- "11. Upload and Publish"

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

  # The data_package_creation folder for THIS package, computed the same
  # way run_generation() does: parent output folder + package name (Tab
  # 1's metadata_id) + "data_package_creation". Tab 5's "Save template"
  # button writes the geography template here and shows the user the
  # path, so this must stay in step with setup_package_dirs() in
  # 09_generate.R - hence derive_package_folder_name() is reused rather
  # than the name being rebuilt by hand.
  #
  # v27: the METADATA FILENAME ALONE is enough. The parent folder falls
  # back to getwd() when Tab 9's box has not been reached, which is not a
  # guess: getwd() is that box's own default value (app_ui.R, textInput
  # "working_folder", value = getwd()), so until the user edits it the
  # fallback IS what a Generate would use. revise_download_parent() in
  # this file already resolves the same input the same way.
  #
  # Before v27 this required input$working_folder too, and that input is
  # NULL until the Generate tab has been rendered - so a user who had
  # filled in Tab 1 and gone straight to Tab 5 was told to visit a tab
  # that had nothing to do with their task.
  #
  # NA_character_ only when the metadata filename is still empty, which
  # Tab 5 reports by naming that one field. The folder need not exist:
  # Tab 5 creates it on demand, so the template can be saved before the
  # first Generate.
  creation_dir <- shiny::reactive({
    # one_string(): collapse anything to a single trimmed string, or "".
    # This file's %||% does not test length(a) == 0, so a zero-length
    # value would survive it and then make nzchar() return logical(0),
    # which `if` rejects with "argument is of length zero". Guard here
    # rather than relying on the operator.
    one_string <- function(x) {
      if (is.null(x) || length(x) != 1 || is.na(x)) return("")
      trimws(as.character(x))
    }

    package_name <- one_string(derive_package_folder_name(high_level$data()))
    if (!nzchar(package_name)) return(NA_character_)

    parent <- one_string(input$working_folder)
    if (!nzchar(parent)) parent <- getwd()

    file.path(parent, package_name, "data_package_creation")
  })

  geography <- geographyServer("geography", table_data,
                               creation_dir_reactive = creation_dir)
  taxonomy <- taxonomyServer("taxonomy", table_data)
  permissions <- permissionsServer("permissions")
  org_context <- org_contextServer("org_context")

  # Tracks the most recent successfully-created DataStore draft reference
  # this session, if any. NULL until a create-new-DOI generate succeeds
  # once. Persists across regenerates so the same draft/DOI is reused
  # rather than silently creating duplicates. Also restorable from a
  # saved session file (see Load handling below).
  doi_state <- shiny::reactiveVal(NULL)
  checked_revision_identity <- shiny::reactiveVal(NULL)

  # Directory of the most recently generated data package (the folder
  # holding the .csv data files + the finalized *_metadata.xml). NULL
  # until a generate succeeds. Set inside do_generate() on success from
  # dirname(result$xml_path), and consumed by the congruence tab
  # (10_congruence.R), which runs DPchecker's checks against it on demand.
  # The congruence module auto-clears its own prior results whenever this
  # changes (a fresh Generate), so results are never shown against a stale
  # package.
  last_package_dir <- shiny::reactiveVal(NULL)

  # Congruence checks tab - purely informational, runs AFTER Generate on
  # the finalized package directory, on a button press. Deliberately NOT
  # part of all_errors() (it never gates Generate). Passed a reactive
  # wrapper around last_package_dir() per 10_congruence.R's contract.
  congruenceServer(
    "congruence", shiny::reactive(last_package_dir()),
    on_revise = function(dir) revise_checked_package(dir)
  )

  # Upload & Publish tab (Tab 11) - the final step: upload the generated
  # data package to its DataStore draft reference, draft a reviewer-invite
  # email, and (self-certified) activate the reference. Like the congruence
  # tab it runs AFTER Generate and never gates it, so it is NOT part of
  # all_errors(). Passed:
  #   - last_package_dir() : the data_package/ dir upload_data_package()
  #     uploads from (same directory the congruence tab checks)
  #   - doi_state()        : prefills the reference_id field with the draft
  #     reference created during Generate
  #   - package title      : for the reviewer-invite email
  upload_publish <- uploadPublishServer(
    "upload_publish",
    package_dir_reactive = shiny::reactive(last_package_dir()),
    doi_state_reactive = shiny::reactive(doi_state()),
    package_title_reactive = shiny::reactive(high_level$data()$package_title)
  )

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

      # On a successful generate, record the package directory (the folder
      # containing the written .xml and the data files) so the congruence
      # tab can check it. Setting this also auto-clears any prior
      # congruence results (see 10_congruence.R). Guarded on a non-empty
      # xml_path so a failed generate never points the congruence tab at a
      # stale/nonexistent directory.
      if (isTRUE(result$success) && !is.null(result$xml_path) && nzchar(result$xml_path)) {
        last_package_dir(dirname(result$xml_path))
      }

      output$generation_result <- shiny::renderUI({
        if (isTRUE(result$success)) {
          # Permissive gate (09_generate.R v8): a successful result can
          # still carry validation_errors (the .xml was written but is not
          # schema-valid). Reflect that in the banner rather than showing a
          # flat "success" that would imply the EML is valid.
          not_valid <- !is.null(result$validation_errors)

          success_banner <- shiny::tags$div(
            style = paste0(
              "margin: 0.5rem 0 1rem 0; padding: 1rem 1.25rem; border-radius: 6px; ",
              "background-color: #d1e7dd; border: 1px solid #a3cfbb; color: #0a3622;"
            ),
            shiny::tags$div(
              style = "font-size: 1.25rem; font-weight: 700;",
              "\u2713 Metadata generated"
            ),
            shiny::tags$div(
              style = "margin-top: 0.25rem;",
              result$message
            ),
            if (not_valid) {
              shiny::tags$div(
                style = paste0(
                  "margin-top: 0.75rem; padding: 0.5rem 0.75rem; border-radius: 4px; ",
                  "background-color: #fff3cd; border: 1px solid #ffe69c; color: #664d03; ",
                  "font-size: 0.9em;"
                ),
                shiny::tags$strong("Note: "),
                "the written EML did not pass schema validation. Open the ",
                shiny::tags$strong("Congruence checks"),
                " tab (last tab) and run the checks to see exactly what to fix. ",
                "The file was still written so you can inspect/troubleshoot it."
              )
            }
          )

          shiny::tagList(
            success_banner,
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
            shiny::tags$div(
              style = paste0(
                "margin: 0.5rem 0 1rem 0; padding: 1rem 1.25rem; border-radius: 6px; ",
                "background-color: #f8d7da; border: 1px solid #f1aeb5; color: #58151c;"
              ),
              shiny::tags$div(
                style = "font-size: 1.25rem; font-weight: 700;",
                "\u2717 Generation failed"
              ),
              shiny::tags$div(
                style = "margin-top: 0.25rem;",
                result$message
              )
            ),
            if (!is.null(result$validation_errors)) {
              shiny::tags$ul(lapply(result$validation_errors, shiny::tags$li))
            }
          )
        }
      })
    })
  }

  shiny::observeEvent(input$generate_script, {
    locked <- checked_revision_identity()
    if (!is.null(locked)) {
      doi_state(locked)
      do_generate(create_new_doi = FALSE)
      return(invisible(NULL))
    }
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
    if (!is.null(checked_revision_identity())) {
      shiny::removeModal()
      doi_state(checked_revision_identity())
      do_generate(create_new_doi = FALSE)
      return(invisible(NULL))
    }
    shiny::removeModal()
    do_generate(create_new_doi = TRUE)
  })

  shiny::observeEvent(input$confirm_replace_doi, {
    if (!is.null(checked_revision_identity())) {
      shiny::removeModal()
      doi_state(checked_revision_identity())
      do_generate(create_new_doi = FALSE)
      return(invisible(NULL))
    }
    shiny::removeModal()
    do_generate(create_new_doi = TRUE)
  })

  shiny::observeEvent(input$confirm_reuse_doi, {
    shiny::removeModal()
    do_generate(create_new_doi = FALSE)
  })

  # ---- Start screen and navigation (v12) ----
  #
  # The main area is a hidden navset (app_ui.R: id "app_screen") with the
  # screens "start", "load_json", "import_xml" and "workflow" (the
  # numbered tabs, navset id "main_panel").

  # Open or close the sidebar. toggle_sidebar() is the current bslib
  # function; sidebar_toggle() was deprecated in bslib 0.9.1 and is used
  # only as a fallback for an older installed bslib.
  set_sidebar <- function(open) {
    if (!is.null(session)) {
      if (exists("toggle_sidebar", asNamespace("bslib"), inherits = FALSE)) {
        bslib::toggle_sidebar("app_sidebar", open = open, session = session)
      } else if (exists("sidebar_toggle", asNamespace("bslib"),
                        inherits = FALSE)) {
        get("sidebar_toggle", asNamespace("bslib"))(
          "app_sidebar", open = open, session = session
        )
      }
    }
  }

  # The user's own choice of sidebar state, or NULL until they make one.
  # Set from the browser - see the header of this file and
  # start_screen.R's reportPreference().
  about_user_pref <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$about_user_pref, {
    choice <- input$about_user_pref
    if (is.character(choice) && length(choice) == 1L &&
        choice %in% c("open", "closed")) {
      about_user_pref(choice)
    }
  })

  # The sidebar's default state, used only until the user chooses for
  # themselves: closed on the start screen, so the hero is uncluttered
  # and the About pill invites a first press; open on every other
  # screen, where the Session card with Save Progress is wanted and
  # there is room for it.
  default_sidebar_state <- function(screen) {
    if (identical(screen, "start")) "closed" else "open"
  }

  # Switch screens, then set the sidebar. A choice the user has already
  # made wins over the screen's default, and keeps winning on every
  # screen after that, which is what makes the state follow them.
  go_to_screen <- function(screen) {
    bslib::nav_select("app_screen", selected = screen, session = session)

    chosen <- about_user_pref()
    set_sidebar(
      if (is.null(chosen)) default_sidebar_state(screen) else chosen
    )
  }

  # Open a numbered tab: leave the start screens, then pick the tab.
  go_to_tab <- function(tab) {
    go_to_screen("workflow")
    bslib::nav_select("main_panel", selected = tab, session = session)
  }

  # Mode for the single-.xml import screen. Option d is the only thing
  # that reaches that screen now, so this stays "new_package"; the
  # revise-a-package screen passes "revise_draft" directly to
  # import_eml_state(). Kept as a reactiveVal so the screen's wording and
  # the import observer still read one value.
  xml_import_mode <- shiny::reactiveVal("new_package")

  # Outcome of the last XML import / JSON load, shown on the upload
  # screens. Cleared whenever an upload screen is opened.
  import_eml_result <- shiny::reactiveVal(NULL)
  load_session_result <- shiny::reactiveVal(NULL)

  # Bumped each time an upload screen is opened; the file boxes depend on
  # it, so each visit starts with an empty box (a browser will not report
  # a change when the same file is chosen twice in a row).
  file_input_nonce <- shiny::reactiveVal(0)

  open_upload_screen <- function(screen) {
    import_eml_result(NULL)
    load_session_result(NULL)
    file_input_nonce(file_input_nonce() + 1)
    go_to_screen(screen)
  }

  # Secondary measure only. The Overview panel is expanded in the markup
  # (app_ui.R: accordion(open = "overview")), so pressing "About" shows
  # text without any help from here. This re-opens Overview if the user
  # collapsed it by hand and then closed and re-opened the sidebar.
  #
  # input$app_sidebar is TRUE when the sidebar is open, where the
  # installed bslib reports that state; sidebar() was given an id for
  # this. It is NOT arriving in this app, which is why the preference
  # above is reported from the browser instead. The observer is left in
  # place because it costs nothing and would help on an installation
  # where the input does arrive. Wrapped in try() so that an older bslib
  # without accordion_panel_open() cannot stop the session.
  shiny::observeEvent(input$app_sidebar, ignoreInit = TRUE, {
    if (isTRUE(input$app_sidebar)) {
      try(
        bslib::accordion_panel_open("app_about", "overview",
                                    session = session),
        silent = TRUE
      )
    }
  })

  # a) start a new EML metadata file
  shiny::observeEvent(input$start_new, {
    checked_revision_identity(NULL)
    go_to_tab(TAB1_NAV_VALUE)
  })

  # b) load progress from a .json session file
  shiny::observeEvent(input$start_load_json, {
    open_upload_screen("load_json")
  })

  # c) revise an existing data package: select its folder, or download a
  #    draft from DataStore (v14). The DOI/reference is reused.
  shiny::observeEvent(input$start_revise, {
    checked_revision_identity(NULL)
    reset_revise_screen()
    go_to_screen("revise_package")
  })

  # d) create a new data package from existing metadata: import .xml,
  #    create a new DOI/reference
  shiny::observeEvent(input$start_new_from_xml, {
    checked_revision_identity(NULL)
    xml_import_mode("new_package")
    open_upload_screen("import_xml")
  })

  # e) review or check an existing data package -> Congruence checks tab
  shiny::observeEvent(input$start_review, {
    go_to_tab(TAB10_NAV_VALUE)
  })

  # f) upload and publish a data package -> Upload and Publish tab
  shiny::observeEvent(input$start_upload, {
    go_to_tab(TAB11_NAV_VALUE)
  })

  # Back to the start screen (from the sidebar and from each upload screen).
  shiny::observeEvent(input$back_to_start, { go_to_screen("start") })
  shiny::observeEvent(input$back_from_json, { go_to_screen("start") })
  shiny::observeEvent(input$back_from_xml, { go_to_screen("start") })
  shiny::observeEvent(input$back_from_revise, { go_to_screen("start") })

  # After a successful load/import, continue to the first metadata tab.
  shiny::observeEvent(input$continue_json, { go_to_tab(TAB1_NAV_VALUE) })
  shiny::observeEvent(input$continue_xml, { go_to_tab(TAB1_NAV_VALUE) })
  shiny::observeEvent(input$continue_revise, { go_to_tab(TAB1_NAV_VALUE) })

  # ---- Files still needed after a load or import (v13) ----
  #
  # File names saved in a .json session (or found in an imported .xml)
  # whose .csv has not been re-loaded yet. Tables (Tab 3) is authoritative
  # for file-level matching; Fields (Tab 4) tracks its own list, so the two
  # are unioned. The list shrinks as the files are re-loaded.
  pending_import_files <- shiny::reactive({
    union(tables$unmatched_saved_files(), fields$unmatched_saved_files())
  })

  # "Re-load to finish restoring: a.csv, b.csv. [Re-load files now]" -
  # NULL when nothing is pending. Shown inside the green result messages.
  reload_notice <- function() {
    pending <- pending_import_files()
    if (length(pending) == 0) return(NULL)
    shiny::tags$div(
      style = "margin-top: 0.35rem;",
      shiny::tags$strong("Re-load to finish restoring: "),
      paste(pending, collapse = ", "),
      shiny::tags$span(" \u00b7 "),
      shiny::actionLink("go_to_reload", "Re-load files now")
    )
  }

  # The link opens the data-table tab, where the file box and the list of
  # files still needed are at the top.
  shiny::observeEvent(input$go_to_reload, {
    go_to_tab(TAB3_NAV_VALUE)
  })

  # ---- Import screens: file boxes and messages ----

  output$load_session_input <- shiny::renderUI({
    file_input_nonce()
    shiny::fileInput(
      "load_session",
      "Session file",
      accept = ".json",
      width = "100%",
      buttonLabel = "Browse...",
      placeholder = "*.json"
    )
  })

  output$import_eml_input <- shiny::renderUI({
    file_input_nonce()
    shiny::fileInput(
      "import_eml",
      "Metadata file",
      accept = ".xml",
      width = "100%",
      buttonLabel = "Browse...",
      placeholder = "*.xml"
    )
  })

  output$xml_screen_title <- shiny::renderText({
    if (identical(xml_import_mode(), "revise_draft")) {
      "Revise an existing data package"
    } else {
      "Create a new data package from existing metadata"
    }
  })

  output$xml_screen_intro <- shiny::renderUI({
    if (identical(xml_import_mode(), "revise_draft")) {
      shiny::helpText(
        "Choose the metadata .xml file of the data package you want to ",
        "revise. Its DataStore DOI and reference id are kept and reused, ",
        "so generating updates that same draft reference. Data tables, ",
        "fields, geography and taxonomy also need your .csv files to be ",
        "re-imported."
      )
    } else {
      shiny::helpText(
        "Choose a metadata .xml file to use as a starting point. Its DOI ",
        "and reference id are not reused: generating creates a new ",
        "DataStore draft reference. Data tables, fields, geography and ",
        "taxonomy also need your .csv files to be re-imported."
      )
    }
  })

  output$load_session_result <- shiny::renderUI({
    r <- load_session_result()
    if (is.null(r)) return(NULL)
    if (!isTRUE(r$success)) {
      return(shiny::tags$div(
        class = "alert alert-danger mt-2",
        style = "font-size: 0.9em;",
        shiny::tags$strong("Could not load the session file: "),
        r$message
      ))
    }
    shiny::tags$div(
      class = "alert alert-success mt-2",
      style = "font-size: 0.9em;",
      shiny::tags$strong("Session loaded "),
      paste0("(saved ", r$saved_at, ")."),
      reload_notice()
    )
  })

  output$load_session_continue <- shiny::renderUI({
    r <- load_session_result()
    if (is.null(r) || !isTRUE(r$success)) return(NULL)
    shiny::actionButton("continue_json", "Continue to metadata entry",
                        icon = shiny::icon("arrow-right"),
                        class = "btn-primary")
  })

  output$import_continue <- shiny::renderUI({
    r <- import_eml_result()
    if (is.null(r) || !isTRUE(r$success)) return(NULL)
    shiny::actionButton("continue_xml", "Continue to metadata entry",
                        icon = shiny::icon("arrow-right"),
                        class = "btn-primary")
  })
  # --- End start screen and navigation ---

  # ---- Revise an existing data package (v14) ----
  #
  # Screen "revise_package" (revise_screen_panel() in start_screen.R).
  # A data package is a folder: one *_metadata.xml plus its .csv files.

  revise_dir <- shiny::reactiveVal(NULL)        # folder chosen or downloaded
  revise_typed <- shiny::reactiveVal(FALSE)     # show the typed-path box?
  revise_ref_check <- shiny::reactiveVal(NULL)  # ds_check_draft_reference()
  revise_download <- shiny::reactiveVal(NULL)   # ds_download_data_package()
  revise_result <- shiny::reactiveVal(NULL)     # outcome of loading it

  reset_revise_screen <- function() {
    revise_dir(NULL)
    revise_typed(FALSE)
    revise_ref_check(NULL)
    revise_download(NULL)
    revise_result(NULL)
  }

  # What the chosen folder holds. resolve_package_dir() (10_congruence.R)
  # steps into a data_package/ subfolder when the parent was selected.
  revise_resolved <- shiny::reactive({
    d <- revise_dir()
    if (!congruence_dir_is_valid(d)) {
      return(list(dir = NULL, note = NULL, metadata = character(0),
                  csvs = character(0)))
    }
    res <- resolve_package_dir(d)
    list(
      dir = res$dir,
      note = res$note,
      metadata = list.files(res$dir, pattern = "metadata\\.xml$",
                            ignore.case = TRUE, full.names = FALSE),
      csvs = list.files(res$dir, pattern = "\\.csv$",
                        ignore.case = TRUE, full.names = FALSE)
    )
  })

  # Exactly one metadata .xml is required: with none there is nothing to
  # import, and with several the app cannot tell which describes the package.
  revise_ready <- shiny::reactive({
    length(revise_resolved()$metadata) == 1
  })

  # ---- A. a folder on this computer ----

  # Every outcome of the folder window is reported (v15). A click that
  # produces no visible window and no message is indistinguishable from a
  # broken button, which is what testing found.
  shiny::observeEvent(input$revise_browse, {
    res <- choose_directory_native(
      caption = "Select the data package folder to revise",
      default = revise_resolved()$dir
    )

    if (identical(res$status, "ok")) {
      revise_result(NULL)
      revise_dir(res$path)
      return(invisible(NULL))
    }

    # No method could draw a window. Name what was tried, so this is
    # diagnosable rather than mysterious.
    if (identical(res$status, "unavailable")) {
      revise_typed(TRUE)
      tried <- choose_directory_attempts_text(res)
      shiny::showNotification(
        shiny::tagList(
          shiny::tags$strong("No folder window could be opened on this computer."),
          shiny::tags$div(
            style = "margin-top: 0.35rem;",
            "Enter the folder path in the box below instead."
          ),
          if (nzchar(tried)) {
            shiny::tags$div(
              style = "margin-top: 0.35rem; font-size: 0.85em;",
              "Methods tried - ", tried
            )
          }
        ),
        type = "warning", duration = NULL
      )
      return(invisible(NULL))
    }

    # "cancelled": a window was answered without choosing a folder. The
    # picker now distinguishes this from a method that never drew one, so
    # this really is a closed window.
    revise_typed(TRUE)
    shiny::showNotification(
      paste0("No folder was selected. Select the button again, or enter ",
             "the folder path in the box below."),
      type = "message", duration = 10
    )
  })

  # Reveal the typed-path box without waiting for the folder window to
  # fail, so this screen is never a dead end.
  shiny::observeEvent(input$revise_show_typed, {
    revise_typed(TRUE)
  })

  # Confirmed with its own button, as on the Congruence tab: reacting to
  # every keystroke made a half-typed path clear the selection.
  shiny::observeEvent(input$revise_use_typed, {
    d <- trimws(input$revise_typed_dir %||% "")
    if (!nzchar(d)) {
      shiny::showNotification("Enter a folder path first.", type = "error")
      return(invisible(NULL))
    }
    if (!dir.exists(d)) {
      shiny::showNotification(
        paste0("That folder does not exist on the computer running ",
               "breezeml: ", d),
        type = "error", duration = 10
      )
      return(invisible(NULL))
    }
    revise_result(NULL)
    revise_dir(normalizePath(d, winslash = "/", mustWork = FALSE))
  })

  output$revise_folder_ui <- shiny::renderUI({
    local_ok <- congruence_is_local_session(session)
    show_typed <- revise_typed() || !local_ok

    shiny::tagList(
      if (local_ok) {
        shiny::tagList(
          shiny::actionButton(
            "revise_browse",
            if (is.null(revise_dir())) "Select data package folder..."
            else "Select a different folder...",
            icon = shiny::icon("folder-open"),
            class = "btn-primary"
          ),
          shiny::helpText(
            "The folder window is opened by R on this computer. On Windows ",
            "it can open behind the browser; the page will not respond ",
            "until it is answered."
          )
        )
      },
      if (!show_typed) {
        shiny::actionLink("revise_show_typed",
                          "Enter the folder path instead")
      },
      if (show_typed) {
        shiny::tagList(
          shiny::helpText(
            if (!local_ok) {
              paste0("This app is being viewed from another computer, so a ",
                     "folder window cannot be opened on your screen. Enter ",
                     "the folder path as the computer running breezeml sees it.")
            } else {
              paste0("Enter the full path of the data package folder, then ",
                     "select Use this folder.")
            }
          ),
          shiny::textInput("revise_typed_dir", "Data package folder",
                           value = revise_dir() %||% "", width = "100%"),
          shiny::actionButton("revise_use_typed", "Use this folder",
                              class = "btn-primary",
                              icon = shiny::icon("folder-open"))
        )
      }
    )
  })

  # ---- B. download a draft from DataStore ----

  # Where a download will be written: the Generate tab's parent output
  # folder, so downloads sit beside the user's other breezeml output.
  revise_download_parent <- shiny::reactive({
    wf <- trimws(input$working_folder %||% "")
    if (nzchar(wf)) wf else getwd()
  })

  output$revise_datastore_ui <- shiny::renderUI({
    chk <- revise_ref_check()
    dl <- revise_download()

    shiny::tagList(
      shiny::helpText(
        "Enter the DataStore reference id of the draft data package. ",
        "Downloading needs the NPS VPN (or an NPS office network), and you ",
        "must be listed as an editor on the reference."
      ),
      shiny::textInput("revise_ref_id", "DataStore reference id",
                       value = "", placeholder = "e.g. 2300498",
                       width = "100%"),
      shiny::actionButton("revise_check_ref", "Check this reference",
                          icon = shiny::icon("magnifying-glass"),
                          class = "btn-primary"),

      # Check outcome, then the download warning and confirmation.
      if (!is.null(chk)) revise_check_report(chk),

      if (!is.null(chk) && isTRUE(chk$can_download) && is.null(dl)) {
        dest <- ds_download_dir_for(revise_download_parent(),
                                    chk$reference_id)
        shiny::tagList(
          shiny::tags$div(
            class = "alert alert-warning mt-2",
            style = "font-size: 0.9em;",
            shiny::tags$strong("Files will be written to your computer."),
            shiny::tags$div(
              style = "margin-top: 0.35rem;",
              "The data package's files are downloaded to:"
            ),
            shiny::tags$div(
              style = "margin-top: 0.2rem; font-family: monospace; word-break: break-all;",
              dest
            ),
            shiny::tags$div(
              style = "margin-top: 0.35rem;",
              "If that folder already exists, its contents are replaced. ",
              "Nothing on DataStore is changed by downloading."
            )
          ),
          shiny::actionButton("revise_do_download",
                              "Download this data package",
                              icon = shiny::icon("download"),
                              class = "btn-primary")
        )
      },

      if (!is.null(dl)) revise_download_report(dl)
    )
  })

  # What the pre-download checks found.
  revise_check_report <- function(chk) {
    rows <- list()

    add <- function(rows, ok, text) {
      mark <- if (isTRUE(ok)) "\u2713" else if (isTRUE(!ok)) "\u2717" else "?"
      c(rows, list(shiny::tags$li(paste0(mark, " ", text))))
    }

    rows <- add(rows, chk$exists, "Reference exists on DataStore")
    rows <- add(rows, chk$is_package, "Reference is a data package")
    rows <- add(
      rows, chk$is_draft,
      paste0("Status is draft",
             if (!is.na(chk$lifecycle)) paste0(" (reported: ", chk$lifecycle, ")") else
               " (status could not be read)")
    )
    rows <- add(
      rows,
      switch(chk$editor_status, match = TRUE, no_match = FALSE, NA),
      switch(chk$editor_status,
             match = "You are listed as an editor",
             no_match = "You are not listed as an editor",
             "Your editor access could not be confirmed")
    )

    cls <- if (isTRUE(chk$can_download)) "alert alert-success mt-2"
    else "alert alert-danger mt-2"

    shiny::tags$div(
      class = cls,
      style = "font-size: 0.9em;",
      if (!is.na(chk$title)) {
        shiny::tags$div(shiny::tags$strong("Title: "), chk$title)
      },
      shiny::tags$ul(style = "margin: 0.35rem 0 0; padding-left: 1.2rem;", rows),
      if (length(chk$blockers) > 0) {
        shiny::tags$div(
          style = "margin-top: 0.35rem;",
          shiny::tags$strong("Cannot download: "),
          shiny::tags$ul(style = "margin: 0.2rem 0 0; padding-left: 1.2rem;",
                         lapply(chk$blockers, shiny::tags$li))
        )
      },
      if (length(chk$warnings) > 0) {
        shiny::tags$div(
          style = "margin-top: 0.35rem;",
          shiny::tags$strong("Note: "),
          shiny::tags$ul(style = "margin: 0.2rem 0 0; padding-left: 1.2rem;",
                         lapply(chk$warnings, shiny::tags$li))
        )
      },
      if (length(chk$editors) > 0) {
        shiny::tags$details(
          style = "margin-top: 0.4rem;",
          shiny::tags$summary(style = "cursor: pointer;",
                              paste0("Editors on this reference (",
                                     length(chk$editors), ")")),
          shiny::tags$ul(style = "margin: 0.2rem 0 0; padding-left: 1.2rem;",
                         lapply(chk$editors, shiny::tags$li))
        )
      }
    )
  }

  # What the download produced.
  revise_download_report <- function(dl) {
    if (!isTRUE(dl$ok)) {
      return(shiny::tags$div(
        class = "alert alert-danger mt-2",
        style = "font-size: 0.9em;",
        shiny::tags$strong("Download failed. "),
        dl$message
      ))
    }
    shiny::tags$div(
      class = "alert alert-success mt-2",
      style = "font-size: 0.9em;",
      shiny::tags$strong("Download complete. "),
      paste0(dl$metadata_count, " metadata .xml and ", dl$csv_count,
             " .csv file", if (dl$csv_count == 1) "" else "s", " written to:"),
      shiny::tags$div(
        style = "margin-top: 0.2rem; font-family: monospace; word-break: break-all;",
        dl$dir
      )
    )
  }

  shiny::observeEvent(input$revise_check_ref, {
    revise_download(NULL)
    revise_result(NULL)
    revise_dir(NULL)

    parsed <- ds_parse_reference_id(input$revise_ref_id)
    if (!isTRUE(parsed$ok)) {
      revise_ref_check(NULL)
      shiny::showNotification(parsed$message, type = "error", duration = 8)
      return(invisible(NULL))
    }

    chk <- shiny::withProgress(
      message = "Checking the reference on DataStore...", value = 0.5,
      {
        ds_check_draft_reference(reference_id = parsed$id,
                                 dev = is_datastore_dev())
      }
    )
    chk$reference_id <- parsed$id
    revise_ref_check(chk)
  })

  shiny::observeEvent(input$revise_do_download, {
    chk <- revise_ref_check()
    shiny::req(chk, isTRUE(chk$can_download))

    dl <- shiny::withProgress(
      message = "Downloading the data package from DataStore...", value = 0.4,
      {
        ds_download_data_package(
          reference_id = chk$reference_id,
          path = revise_download_parent(),
          dev = is_datastore_dev()
        )
      }
    )
    revise_download(dl)
    revise_result(NULL)
    revise_dir(if (isTRUE(dl$ok)) dl$dir else NULL)
  })

  # ---- Shared: what is in the folder, and loading it ----

  output$revise_package_summary <- shiny::renderUI({
    info <- revise_resolved()
    if (is.null(info$dir)) return(NULL)

    n_xml <- length(info$metadata)
    problem <- if (n_xml == 0) {
      paste0("This folder has no file ending in _metadata.xml, so there is ",
             "no metadata to read. Select the folder that holds the data ",
             "package itself.")
    } else if (n_xml > 1) {
      paste0("This folder has ", n_xml, " files ending in _metadata.xml, so ",
             "breezeml cannot tell which one describes the package.")
    } else NULL

    shiny::tagList(
      shiny::tags$div(
        class = "mt-2",
        style = "font-size: 0.9em;",
        shiny::tags$strong("Folder: "),
        shiny::tags$span(style = "font-family: monospace; word-break: break-all;",
                         info$dir)
      ),
      if (!is.null(info$note)) {
        shiny::tags$div(class = "text-muted", style = "font-size: 0.85em;",
                        info$note)
      },
      shiny::tags$div(
        style = "font-size: 0.9em; margin-top: 0.2rem;",
        paste0(n_xml, " metadata .xml, ", length(info$csvs), " .csv file",
               if (length(info$csvs) == 1) "" else "s", " found.")
      ),
      if (length(info$csvs) > 0) {
        shiny::tags$details(
          style = "font-size: 0.85em;",
          shiny::tags$summary(style = "cursor: pointer;", "Data files found"),
          shiny::tags$ul(style = "margin: 0.2rem 0 0; padding-left: 1.2rem;",
                         lapply(info$csvs, shiny::tags$li))
        )
      },
      if (!is.null(problem)) {
        shiny::tags$div(class = "alert alert-warning mt-2",
                        style = "font-size: 0.9em;", problem)
      }
    )
  })

  output$revise_load_button <- shiny::renderUI({
    info <- revise_resolved()
    if (is.null(info$dir)) return(NULL)
    if (!revise_ready()) {
      return(congruence_unavailable_button(
        "revise_load", "Load this data package",
        why = "Select a folder containing exactly one *_metadata.xml file."
      ))
    }
    if (!is.null(revise_result())) return(NULL)
    shiny::actionButton("revise_load", "Load this data package",
                        icon = shiny::icon("file-import"),
                        class = "btn-primary")
  })

  output$revise_result <- shiny::renderUI({
    r <- revise_result()
    if (is.null(r)) return(NULL)
    if (!isTRUE(r$success)) {
      return(shiny::tags$div(
        class = "alert alert-danger mt-2",
        style = "font-size: 0.9em;",
        shiny::tags$strong("Could not read this data package: "),
        r$message
      ))
    }
    shiny::tags$div(
      class = "alert alert-success mt-2",
      style = "font-size: 0.9em;",
      shiny::tags$strong("Data package loaded. "),
      paste0(r$table_count, " table(s) and ", r$field_count,
             " field(s) from ", r$xml_name, "; ",
             r$csv_loaded, " data file",
             if (r$csv_loaded == 1) "" else "s", " read."),
      shiny::tags$div(style = "margin-top: 0.35rem;", r$doi_text),
      reload_notice(),
      if (length(r$warnings) > 0) {
        shiny::tags$details(
          style = "margin-top: 0.5rem;",
          shiny::tags$summary(style = "cursor: pointer; font-weight: 600;",
                              paste0(length(r$warnings), " import note",
                                     if (length(r$warnings) == 1) "" else "s")),
          shiny::tags$ul(style = "margin-top: 0.35rem; margin-bottom: 0;",
                         lapply(r$warnings, shiny::tags$li))
        )
      }
    )
  })

  output$revise_continue <- shiny::renderUI({
    r <- revise_result()
    if (is.null(r) || !isTRUE(r$success)) return(NULL)
    shiny::actionButton("continue_revise", "Continue to metadata entry",
                        icon = shiny::icon("arrow-right"),
                        class = "btn-primary")
  })

  # Import the folder's metadata .xml in revise mode, then hand its .csv
  # files straight to the Tables module so nothing has to be selected
  # again. Same importer and same mode as the single-.xml screen, so the
  # DOI/reference is reused in exactly the same way.
  shiny::observeEvent(input$revise_load, {
    checked_revision_identity(NULL)
    info <- revise_resolved()
    shiny::req(info$dir, length(info$metadata) == 1)

    xml_path <- file.path(info$dir, info$metadata[1])

    imported <- tryCatch(
      import_eml_state(file = xml_path, mode = "revise_draft"),
      error = function(e) {
        revise_result(list(success = FALSE,
                           message = conditionMessage(e)))
        shiny::showNotification(
          paste0("Could not read the metadata .xml: ", conditionMessage(e)),
          type = "error", duration = 10
        )
        NULL
      }
    )
    shiny::req(imported)

    saved <- imported$state

    high_level$restore(saved$high_level_info)
    people$restore(saved$people)
    permissions$restore(saved$permissions)
    org_context$restore(saved$org_context)

    tables$restore(saved$data_tables$metadata)
    fields$restore(saved$fields$tables)
    geography$restore(saved$geography)
    taxonomy$restore(saved$taxonomy)

    doi_state(saved$doi_state)

    # The package came from a folder, not from a Generate run in this
    # session, so there is no newly generated package for Tabs 10 and 11.
    last_package_dir(NULL)

    # Read the folder's data files as data tables. restore() above
    # recorded what to watch for, so this is what actually restores the
    # table, field, geography and taxonomy selections.
    csv_paths <- file.path(info$dir, info$csvs)
    added <- tables$add_files(csv_paths)
    added <- if (is.numeric(added) && length(added) == 1 && !is.na(added)) added else 0L

    summary <- imported$summary
    doi_text <- if (isTRUE(summary$retained_doi)) {
      paste0("Existing DOI/reference retained: ", summary$imported_doi,
             " (reference ", summary$imported_reference_id,
             "). Generating updates that draft reference.")
    } else {
      paste0("No usable DataStore DOI/reference was found in this metadata, ",
             "so there is nothing to reuse. Generating will create a new ",
             "draft reference.")
    }

    revise_result(list(
      success = TRUE,
      warnings = imported$warnings,
      table_count = summary$table_count,
      field_count = summary$field_count,
      xml_name = info$metadata[1],
      csv_loaded = added,
      doi_text = doi_text
    ))

    shiny::showNotification(
      paste0("Data package loaded: ", summary$table_count,
             " table(s), ", added, " data file(s) read."),
      type = "message", duration = 8
    )
  })
  # Enter the existing revise mode directly at metadata entry, bypassing
  # the folder-selection screen. Called only by the congruence module.
  revise_checked_package <- function(dir) {
    tryCatch({
      if (!congruence_dir_is_valid(dir)) stop("The checked package folder is unavailable.")
      dir <- normalizePath(dir, winslash = "/", mustWork = TRUE)
      xmls <- list.files(dir, pattern = "metadata\\.xml$",
                         ignore.case = TRUE, full.names = TRUE)
      if (length(xmls) != 1) stop("The checked package must contain exactly one metadata XML.")
      imported <- import_eml_state(file = xmls[1], mode = "revise_draft")
      saved <- imported$state
      retained <- saved$doi_state
      if (!is.list(retained) || length(retained$reference_id) != 1L ||
          length(retained$doi) != 1L || is.na(retained$reference_id) ||
          is.na(retained$doi) || !nzchar(retained$doi)) {
        stop("The checked metadata has no reusable DataStore DOI/reference. Automatic revision was not started; use the standard revision workflow to review its identity.")
      }
      csvs <- list.files(dir, pattern = "\\.csv$", ignore.case = TRUE, full.names = TRUE)
      # Parse before updating other modules. Same-name tables are replaced,
      # not skipped. Field defaults are explicitly rebuilt on restoration.
      csv_count <- tables$replace_files(csvs, saved$data_tables$metadata)
      doi_state(retained)
      checked_revision_identity(retained)
      high_level$restore(saved$high_level_info)
      people$restore(saved$people)
      permissions$restore(saved$permissions)
      org_context$restore(saved$org_context)
      fields$restore(saved$fields$tables, rebuild = TRUE)
      geography$restore(saved$geography)
      taxonomy$restore(saved$taxonomy)
      table_count <- imported$summary$table_count
      field_count <- imported$summary$field_count
      notes <- imported$warnings
      if (csv_count == 0) notes <- c(notes, "No CSVs were present; missing data files must be supplied before generation.")
      reset_revise_screen()
      revise_dir(dir)
      revise_result(list(
        success = TRUE, warnings = notes, table_count = table_count,
        field_count = field_count, xml_name = basename(xmls[1]), csv_loaded = csv_count,
        doi_text = paste0("Existing DOI/reference retained: ", retained$doi,
                          " (reference ", retained$reference_id, ").")
      ))
      # The checked package remains available for checking/uploading. Merely
      # opening it does not imply that corrections have been generated yet.
      last_package_dir(dir)
      upload_publish$restore(list(upload_dir = dir))
      output$generation_result <- shiny::renderUI({
        shiny::tags$div(class = "alert alert-info", "Revision opened from congruence findings. Regenerate metadata with the existing draft before checking or uploading corrections.")
      })
      go_to_tab(TAB1_NAV_VALUE)
      shiny::showNotification(
        paste0("Package opened for revision. DOI ", retained$doi,
               " and draft reference ", retained$reference_id,
               " retained. Review the findings, edit, and select Generate EML; this revision always reuses the existing draft."),
        type = "message", duration = 15
      )
      invisible(TRUE)
    }, error = function(e) {
      shiny::showNotification(paste0("Could not open the checked package for revision: ", conditionMessage(e)),
                              type = "error", duration = 15)
      invisible(FALSE)
    })
  }
  # --- End revise an existing data package ---

  # ---- Import existing EML XML ----
  #
  # XML import is separate from JSON session restoration:
  #
  #   - JSON restores state previously saved by breezeml.
  #   - XML translates an EML document into the same canonical app-state
  #     shape and then passes that state through the existing restore()
  #     functions.
  #
  # The actual parser/converter lives in import_eml.R. It reads
  # schema-invalid but well-formed XML with xml2 and returns:
  #
  #   list(
  #     state = <default_app_state()-shaped list>,
  #     warnings = <character vector>,
  #     summary = <import summary>
  #   )
  #
  # Tabs 3-6 still use their existing deferred restore behavior. Imported
  # table and field metadata remain pending until matching CSV files are
  # uploaded on Tab 3.
  # import_eml_result is created in the start-screen block above.

  output$import_eml_status <- shiny::renderUI({
    result <- import_eml_result()
    if (is.null(result)) return(NULL)

    if (!isTRUE(result$success)) {
      return(
        shiny::tags$div(
          class = "alert alert-danger mt-2",
          style = "font-size: 0.85em;",
          shiny::tags$strong("Import failed: "),
          result$message
        )
      )
    }

    summary <- result$summary
    warnings <- result$warnings %||% character(0)

    mode_text <- if (identical(summary$import_mode, "revise_draft")) {
      if (isTRUE(summary$retained_doi)) {
        paste0(
          "Existing DOI/reference retained: ",
          summary$imported_doi,
          " (reference ",
          summary$imported_reference_id,
          ")."
        )
      } else {
        paste0(
          "Revise-existing-draft mode was selected, but no usable ",
          "DataStore DOI/reference was found."
        )
      }
    } else {
      "Imported DOI/reference was not retained; Generate will create a new draft."
    }

    shiny::tags$div(
      class = "alert alert-success mt-2",
      style = "font-size: 0.85em;",

      shiny::tags$strong("EML import complete."),

      shiny::tags$div(
        style = "margin-top: 0.35rem;",
        paste0(
          summary$table_count,
          " table(s) and ",
          summary$field_count,
          " field(s) imported. "
        ),
        mode_text
      ),

      reload_notice(),

      if (length(warnings) > 0) {
        shiny::tags$details(
          style = "margin-top: 0.5rem;",
          shiny::tags$summary(
            style = "cursor: pointer; font-weight: 600;",
            paste0(
              length(warnings),
              " import note",
              if (length(warnings) == 1) "" else "s"
            )
          ),
          shiny::tags$ul(
            style = "margin-top: 0.35rem; margin-bottom: 0;",
            lapply(warnings, shiny::tags$li)
          )
        )
      }
    )
  })

  shiny::observeEvent(input$import_eml, {
    checked_revision_identity(NULL)
    shiny::req(input$import_eml)

    # Clear the previous result immediately so its status cannot be
    # mistaken for the newly selected file.
    import_eml_result(NULL)

    # The mode is set by the start-screen option the user chose:
    # "Revise an existing data package" -> "revise_draft", "Create a new
    # data package from existing metadata" -> "new_package". Any other
    # value falls back to "new_package", the safe choice, because it
    # never reuses a DOI.
    mode <- xml_import_mode() %||% "new_package"
    if (!mode %in% c("revise_draft", "new_package")) {
      mode <- "new_package"
    }

    imported <- tryCatch(
      import_eml_state(
        file = input$import_eml$datapath,
        mode = mode
      ),
      error = function(e) {
        import_eml_result(list(
          success = FALSE,
          message = conditionMessage(e)
        ))

        shiny::showNotification(
          paste0(
            "Could not import EML XML: ",
            conditionMessage(e)
          ),
          type = "error",
          duration = 10
        )

        NULL
      }
    )

    shiny::req(imported)

    saved <- imported$state

    # Immediate-restore modules have no dependency on uploaded CSV data.
    high_level$restore(saved$high_level_info)
    people$restore(saved$people)
    permissions$restore(saved$permissions)
    org_context$restore(saved$org_context)

    # Dependent modules record imported state and re-attempt restoration
    # when matching CSV files are uploaded on Tab 3.
    tables$restore(saved$data_tables$metadata)
    fields$restore(saved$fields$tables)
    geography$restore(saved$geography)
    taxonomy$restore(saved$taxonomy)

    # This assignment must also occur when saved$doi_state is NULL.
    #
    # In create-new-package mode, NULL deliberately clears any DOI from
    # the current session so Generate cannot accidentally reuse a draft
    # that belonged to work performed before this XML was imported.
    doi_state(saved$doi_state)

    # An imported XML is not itself a newly generated local package.
    # Clear the last-generated directory so Tabs 10 and 11 cannot
    # accidentally operate on a package produced before this import.
    last_package_dir(NULL)

    import_eml_result(list(
      success = TRUE,
      warnings = imported$warnings,
      summary = imported$summary
    ))

    pending <- union(
      tables$unmatched_saved_files(),
      fields$unmatched_saved_files()
    )

    notification_text <- paste0(
      "EML metadata imported. ",
      imported$summary$table_count,
      " table(s) and ",
      imported$summary$field_count,
      " field(s) were found.",
      if (length(pending) > 0) {
        paste0(
          " Re-load the matching CSV file",
          if (length(pending) == 1) "" else "s",
          " to restore table-dependent metadata."
        )
      } else {
        ""
      }
    )

    shiny::showNotification(
      notification_text,
      type = "message",
      duration = 10
    )
  })
  # --- End import existing EML XML ---

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
      # coord_source records WHICH source Tab 5 is using; the uploaded
      # coordinates themselves are never written to a session file (see
      # 05_geography.R v17 and the comment in default_app_state()). On
      # load the user re-uploads the template, as they re-upload CSVs.
      state$geography <- list(
        has_coords = isTRUE(geo$enabled),
        # NA when the user ticked the box but has not chosen a source yet
        # (05_geography.R v19). %||% is length/NA-aware, so NA_character_
        # from geo$source would be replaced by its right-hand side - the
        # test must therefore be explicit, or an unchosen source would be
        # saved as "table" and silently restore the wrong option.
        coord_source = if (is.null(geo$source) || length(geo$source) != 1 ||
                           is.na(geo$source)) {
          NA_character_
        } else {
          geo$source
        },
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

      # Tab 11 - Upload & Publish. Only the reference_id override is
      # persisted (the self-certification checkbox is intentionally not -
      # see 11_upload_publish.R's restore() note). Stored as a genuine
      # scalar (NA_integer_ when unset), so no I() wrapper.
      up <- upload_publish$data()
      state$upload_publish <- list(
        reference_id = up$reference_id
      )

      save_app_state(state, file)
    }
  )

  # (The sidebar "re-load" banner and its click handler were removed in v13;
  # pending file names are reported on the import screens - see
  # pending_import_files() and reload_notice() in the start-screen block.)

  shiny::observeEvent(input$load_session, {
    checked_revision_identity(NULL)
    shiny::req(input$load_session)
    load_session_result(NULL)

    saved <- tryCatch(
      load_app_state(input$load_session$datapath),
      error = function(e) {
        load_session_result(list(success = FALSE,
                                 message = conditionMessage(e)))
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
    upload_publish$restore(saved$upload_publish)

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

    load_session_result(list(success = TRUE,
                             saved_at = saved$saved_at %||% "unknown time"))

    pending <- pending_import_files()
    shiny::showNotification(
      paste0("Session loaded (saved ", saved$saved_at %||% "unknown time", ").",
             if (length(pending) > 0) {
               paste0(" Re-load to finish restoring: ",
                      paste(pending, collapse = ", "))
             }),
      type = "message", duration = 8
    )
  })

  `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
}
