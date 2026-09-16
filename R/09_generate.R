# 09_generate.R v3
#
# Added missing @param tags across every documented-but-incomplete
# function (derive_package_folder_name, setup_package_dirs,
# build_generation_script, reference_id_from_doi, cleanup_old_doi,
# run_generation) to resolve R CMD check's "Undocumented arguments"
# warnings, which would otherwise fail R-CMD-check.yaml on GitHub.
#
# RENAMED from 07_generate.R to match actual tab order (Generate is tab 9,
# the last tab) as part of converting the app into the breezeml R
# package. Content otherwise unchanged from 07_generate.R's last version
# (v22) - see prior conversation history for full change log, including
# the fix for the stray "Currently used only..." text that had broken
# parsing.
#
# The finish line: orchestrates writing every EMLassemblyline .txt template
# to disk from app state, then runs the same make_eml() -> eml_validate() ->
# write_eml() sequence as skeleton.Rmd, ending in a properly named
# <metadata_id>_metadata.xml.
#
# This ALSO applies NPS-specific EMLeditor::set_*() edits to the in-memory
# EML object between make_eml() and write_eml() - this is the "generate
# the R object behind the scenes and edit it before writing to disk"
# workflow, replacing the CLI pattern of generating EML first and
# separately post-processing it. See apply_permissions_to_eml() (07) and
# apply_org_context_to_eml() (08).
#
# DataStore draft reference creation (EMLeditor::set_datastore_doi()) is a
# real, non-idempotent external side effect - re-running it creates a
# duplicate draft. It is therefore NOT silently re-run on every generate;
# it requires explicit user confirmation, handled as a distinct step
# BEFORE the rest of run_generation() executes (see build_doi_confirmation()
# and its wiring in app_server.R). Once a DOI exists, generation reuses it
# automatically (via set_doi(), NOT set_datastore_doi() again) unless the
# user explicitly chooses to create a new one, in which case the
# superseded draft is deleted via NPSdatastore::delete_inactive_ref()
# (which itself refuses to delete anything that was ever active, as a
# safety backstop). set_datastore_doi() and set_doi() are never both
# called within the same run_generation() call - see create_new_doi
# parameter below.

# Output directory structure (per package, folder name = metadata_id for
# now - will become dynamically generated once more package info exists):
#
#   <parent working folder>/
#     <metadata_id>/
#       data_package/              <- what actually gets uploaded to DataStore
#         <data files>.csv
#         <metadata_id>_metadata.xml
#       data_package_creation/     <- EMLassemblyline's working directory:
#         generation_script.R         all .txt templates + a COPY of the
#         abstract.txt                data files (EMLassemblyline's
#         methods.txt                 template_*()/make_eml() functions
#         keywords.txt                need the data files physically
#         personnel.txt               present alongside the templates to
#         attributes_*.txt            read attribute info from them)
#         catvars_*.txt
#         geographic_coverage.txt
#         taxonomic_coverage.txt
#         <data files>.csv          <- working copy, not the deliverable
#
# Only the final .xml is copied from data_package_creation into
# data_package/ - the data files in data_package/ are the ORIGINAL uploads,
# not round-tripped through the creation folder.

#' Derive the package folder name. Currently just the metadata_id - a
#' placeholder until enough package info exists to generate something more
#' descriptive (e.g. incorporating park unit + year).
#'
#' @param high_level_state the list returned by highLevelServer()'s
#'   reactive, evaluated (i.e. state <- high_level_reactive())
#' @return character - the folder name to use for this package
derive_package_folder_name <- function(high_level_state) {
  high_level_state$metadata_id
}

#' Ensure the three-directory structure exists under `parent_folder`.
#' Returns a list of the resolved paths.
#'
#' @param parent_folder existing, writable directory under which the
#'   package folder will be created
#' @param package_name name of the package subfolder to create/use under
#'   parent_folder
#' @return list(package_root=, data_package_dir=, creation_dir=) - the
#'   resolved absolute paths to each directory
setup_package_dirs <- function(parent_folder, package_name) {
  package_root <- file.path(parent_folder, package_name)
  data_package_dir <- file.path(package_root, "data_package")
  creation_dir <- file.path(package_root, "data_package_creation")

  dir.create(data_package_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(creation_dir, recursive = TRUE, showWarnings = FALSE)

  list(
    package_root = package_root,
    data_package_dir = data_package_dir,
    creation_dir = creation_dir
  )
}

#' Assemble the full reproducible R script from every tab's state, in
#' skeleton.Rmd order. Pure function. Paths in the emitted script reference
#' the data_package_creation working directory, since that's where
#' EMLassemblyline's template functions and make_eml() actually operate.
#'
#' @param high_level_state the list returned by highLevelServer()'s reactive
#' @param people_state the list returned by peopleServer()'s reactive
#' @param tables_state the list returned by tableMetadataServer()'s reactive
#' @param fields_state the list returned by fieldsServer()'s reactive
#' @param geo_state the list returned by geographyServer()'s reactive
#' @param taxonomy_state the list returned by taxonomyServer()'s reactive
#' @param working_folder_var name of the R variable holding the working
#'   folder path in the generated script (default "working_folder")
#' @return character - the full generated R script as a single string
build_generation_script <- function(high_level_state, people_state, tables_state,
                                    fields_state, geo_state, taxonomy_state,
                                    working_folder_var = "working_folder") {
  paste(
    '# --- Generated by bReezEML ---',
    'library(NPSdataverse)',
    'library(tidyverse)',
    '',
    '# working_folder should be the data_package_creation directory - see',
    '# the app\'s output folder structure. Data files must be copied here',
    '# alongside the .txt templates for EMLassemblyline to read them.',
    glue::glue('{working_folder_var} <- getwd()'),
    glue::glue('data_files <- c({paste(sprintf("\\"%s\\"", tables_state$metadata$file_name), collapse = ", ")})'),
    glue::glue('data_names <- c({paste(sprintf("\\"%s\\"", tables_state$metadata$table_name), collapse = ", ")})'),
    glue::glue('data_descriptions <- c({paste(sprintf("\\"%s\\"", tables_state$metadata$description), collapse = ", ")})'),
    'data_urls <- c(rep("temporary URL", length(data_files)))',
    '',
    emit_high_level_chunk(high_level_state, working_folder_var),
    emit_people_chunk(people_state, working_folder_var),
    emit_fields_chunk(fields_state, working_folder_var),
    emit_geo_chunk(geo_state, working_folder_var),
    emit_taxonomy_chunk(taxonomy_state, working_folder_var),
    '',
    'my_metadata <- EMLassemblyline::make_eml(',
    glue::glue('  path = {working_folder_var},'),
    '  dataset.title = package_title,',
    '  data.table = data_files,',
    '  data.table.name = data_names,',
    '  data.table.description = data_descriptions,',
    '  data.table.url = data_urls,',
    '  temporal.coverage = c(startdate, enddate),',
    '  maintenance.description = data_type,',
    '  package.id = metadata_id,',
    '  return.obj = TRUE,',
    '  write.file = FALSE',
    ')',
    '',
    'EML::eml_validate(my_metadata)',
    '',
    '# Final .xml is written here, then copied into the data_package/',
    '# directory alongside the original (not working-copy) data files.',
    glue::glue('EML::write_eml(my_metadata, file.path({working_folder_var}, paste0(metadata_id, "_metadata.xml")))'),
    sep = "\n"
  )
}

#' Determine what confirmation message (if any) should be shown before
#' creating a DataStore draft reference. Pure function - no side effects.
#'
#' @param existing_doi_state NULL, or a list(reference_id = <7-digit int>,
#'   doi = <character>) from a prior successful creation this session
#' @return list(needs_confirmation = TRUE, title = ..., message = ...)
build_doi_confirmation <- function(existing_doi_state) {
  if (is.null(existing_doi_state)) {
    return(list(
      needs_confirmation = TRUE,
      title = "Create DataStore draft reference?",
      message = paste0(
        "This will create a new DRAFT reference on DataStore, assign it a DOI, ",
        "and update each data table's online URL to point at the draft's landing ",
        "page. The draft is not public until reviewed and activated. Continue?"
      )
    ))
  }

  list(
    needs_confirmation = TRUE,
    title = "Replace existing draft reference?",
    message = paste0(
      "You already have a draft reference (ID: ", existing_doi_state$reference_id,
      ", DOI: ", existing_doi_state$doi, "). Creating a new one will delete ",
      "that draft from DataStore (if it is still inactive) and replace it ",
      "with a new draft, DOI, and set of data table URLs. This cannot be undone. Continue?"
    )
  )
}

#' Derive the 7-digit DataStore reference ID from a DOI string returned by
#' EMLeditor::get_doi() - the reference ID is always the last 7 digits of
#' the DOI. Returns NA_integer_ if doi is NA/empty.
#'
#' @param doi character - a DOI string as returned by EMLeditor::get_doi()
#' @return integer - the 7-digit reference ID, or NA_integer_ if doi is
#'   NA, empty, or too short to contain a valid reference ID
reference_id_from_doi <- function(doi) {
  if (is.na(doi) || !nzchar(doi)) return(NA_integer_)
  digits_only <- gsub("[^0-9]", "", doi)
  if (nchar(digits_only) < 7) return(NA_integer_)
  as.integer(substr(digits_only, nchar(digits_only) - 6, nchar(digits_only)))
}

#' Delete a superseded DataStore draft reference. Non-fatal if it fails
#' (e.g. the reference was already active, in which case
#' delete_inactive_ref() itself refuses as a safety backstop) - the NEW
#' draft has already been created successfully by the time this runs, so a
#' cleanup failure shouldn't block the user. Returns a message to surface,
#' or NULL if cleanup wasn't needed/nothing to report.
#'
#' @param old_doi_state NULL, or a list(reference_id = <7-digit int>,
#'   doi = <character>) identifying the draft reference to delete
#' @return character - a message to surface to the user if cleanup failed,
#'   or NULL if cleanup wasn't needed or succeeded silently
cleanup_old_doi <- function(old_doi_state) {
  if (is.null(old_doi_state) || is.na(old_doi_state$reference_id)) return(NULL)

  result <- tryCatch({
    NPSdatastore::delete_inactive_ref(
      reference_id = old_doi_state$reference_id,
      dev = is_datastore_dev(),
      interactive = FALSE
    )
    TRUE
  }, error = function(e) e)

  if (inherits(result, "condition")) {
    paste0(
      "Note: the previous draft reference (ID: ", old_doi_state$reference_id,
      ") could not be automatically deleted: ", conditionMessage(result),
      ". You may need to remove it manually on DataStore."
    )
  } else {
    NULL
  }
}

#' Actually execute the pipeline:
#'   1. create <package_name>/data_package and .../data_package_creation
#'   2. copy uploaded data files into data_package_creation (EMLassemblyline's
#'      working directory) AND into data_package (the deliverable)
#'   3. write every .txt template into data_package_creation
#'   4. save the human-readable generation script into data_package_creation
#'   5. apply NPS-specific EMLeditor edits (permissions, org context, DOI)
#'   6. call make_eml() / eml_validate() / write_eml(), writing the .xml into
#'      data_package_creation
#'   7. copy the final .xml into data_package
#'
#' @param parent_folder existing, writable directory under which the
#'   package folder will be created
#' @param high_level_state the list returned by highLevelServer()'s reactive
#' @param people_state the list returned by peopleServer()'s reactive
#' @param tables_state the list returned by tableMetadataServer()'s reactive
#' @param fields_state the list returned by fieldsServer()'s reactive
#' @param geo_state the list returned by geographyServer()'s reactive
#' @param taxonomy_state the list returned by taxonomyServer()'s reactive
#' @param permissions_state the list returned by permissionsServer()'s reactive
#' @param org_context_state the list returned by org_contextServer()'s reactive
#' @param doi_state NULL, or list(reference_id=, doi=) from a prior
#'   create-DOI step this session
#' @param create_new_doi logical - if TRUE, calls
#'   EMLeditor::set_datastore_doi() to create a brand-new DataStore draft
#'   reference (which in one call: creates the draft, attaches its DOI to
#'   the EML object, AND updates every data table's onlineURL to the
#'   draft's landing page). If an old doi_state is passed in, the
#'   superseded draft is deleted afterward via
#'   NPSdatastore::delete_inactive_ref(). If FALSE and doi_state is
#'   non-NULL, calls EMLeditor::set_doi() instead, which re-attaches the
#'   EXISTING DOI/URLs without creating a new draft - this is what happens
#'   on every regenerate after a draft has already been created once,
#'   avoiding the redundant/duplicate work of calling both functions.
#' @return list(success = logical, message = character,
#'              xml_path = character or NULL (final location in data_package),
#'              validation_errors = character vector or NULL,
#'              doi_state = list(reference_id=, doi=) or NULL - the
#'                resulting DOI state after this call, for the caller to
#'                persist and pass back in on the next generate)
run_generation <- function(parent_folder, high_level_state, people_state,
                           tables_state, fields_state, geo_state, taxonomy_state,
                           permissions_state, org_context_state,
                           doi_state = NULL, create_new_doi = FALSE) {

  if (!dir.exists(parent_folder)) {
    return(list(success = FALSE, message = paste0("Working folder does not exist: ", parent_folder)))
  }

  package_name <- derive_package_folder_name(high_level_state)
  if (!nzchar(package_name)) {
    return(list(success = FALSE, message = "Cannot determine a package folder name - metadata_id is empty."))
  }

  dirs <- tryCatch(
    setup_package_dirs(parent_folder, package_name),
    error = function(e) NULL
  )
  if (is.null(dirs)) {
    return(list(success = FALSE, message = paste0("Failed to create output directories under: ", parent_folder)))
  }

  # 1. copy uploaded data files into BOTH data_package (the deliverable)
  #    and data_package_creation (EMLassemblyline's working directory)
  copy_ok <- purrr::map_lgl(seq_len(nrow(tables_state$metadata)), function(i) {
    row <- tables_state$metadata[i, ]
    ok_deliverable <- tryCatch(
      file.copy(row$file_loc, file.path(dirs$data_package_dir, row$file_name), overwrite = TRUE),
      error = function(e) FALSE
    )
    ok_working <- tryCatch(
      file.copy(row$file_loc, file.path(dirs$creation_dir, row$file_name), overwrite = TRUE),
      error = function(e) FALSE
    )
    ok_deliverable && ok_working
  })
  if (!all(copy_ok)) {
    return(list(success = FALSE, message = "Failed to copy one or more data files into the output directories."))
  }

  working_folder <- dirs$creation_dir

  # 2. write core metadata templates + overwrite with captured content
  result <- tryCatch({
    EMLassemblyline::template_core_metadata(path = working_folder, license = "CC0")

    writeLines(high_level_state$abstract, file.path(working_folder, "abstract.txt"))
    writeLines(high_level_state$methods, file.path(working_folder, "methods.txt"))
    writeLines(high_level_state$additional_notes, file.path(working_folder, "additional_info.txt"))

    keywords_df <- tibble::tibble(
      keyword = high_level_state$keywords,
      keywordThesaurus = "NPS Data Package"
    )
    readr::write_tsv(keywords_df, file.path(working_folder, "keywords.txt"), na = "")

    # 3. personnel.txt
    to_personnel_rows <- function(df, role) {
      if (nrow(df) == 0) return(NULL)
      tibble::tibble(
        givenName = df$givenName,
        middleInitial = "",
        surName = df$surName,
        organizationName = df$organizationName,
        electronicMailAddress = df$email,
        userId = df$userId,
        role = if ("role" %in% names(df)) df$role else role,
        projectTitle = "",
        fundingAgency = "",
        fundingNumber = ""
      )
    }
    personnel_df <- dplyr::bind_rows(
      to_personnel_rows(people_state$authors, "creator"),
      to_personnel_rows(people_state$contacts, "contact"),
      to_personnel_rows(people_state$contributors, NA_character_)
    )
    readr::write_tsv(personnel_df, file.path(working_folder, "personnel.txt"), na = "")

    # 4. attributes + catvars, per data table. EMLassemblyline strips the
    # file extension before naming these - "BICA_Herps.csv" ->
    # "attributes_BICA_Herps.txt", not "attributes_BICA_Herps.csv.txt".
    strip_extension <- function(file_name) sub("\\.[^.]*$", "", file_name)

    for (file_name in names(fields_state)) {
      tbl_state <- fields_state[[file_name]]
      base_name <- strip_extension(file_name)
      readr::write_tsv(normalize_missing_value_pair(tbl_state$attributes),
                       file.path(working_folder, paste0("attributes_", base_name, ".txt")),
                       na = "")
      if (nrow(tbl_state$catvars) > 0) {
        readr::write_tsv(tbl_state$catvars,
                         file.path(working_folder, paste0("catvars_", base_name, ".txt")),
                         na = "")
      }
    }
    EMLassemblyline::template_table_attributes(path = working_folder, data.table = tables_state$metadata$file_name, write.file = FALSE)

    # 5. geographic coverage (optional). Delete any existing
    # geographic_coverage.txt first - EMLassemblyline's template functions
    # are documented to skip writing if the file already exists (this is
    # intentional for hand-edited templates in the original CLI workflow,
    # but wrong for this app: every regenerate should reflect current Tab 5
    # state, not silently keep a stale file from an earlier run).
    geo_file <- file.path(working_folder, "geographic_coverage.txt")
    if (file.exists(geo_file)) file.remove(geo_file)
    if (isTRUE(geo_state$enabled)) {
      EMLassemblyline::template_geographic_coverage(
        path = working_folder, data.path = working_folder,
        data.table = geo_state$table, lat.col = geo_state$lat_col,
        lon.col = geo_state$lon_col, site.col = geo_state$site_col,
        write.file = TRUE
      )
    }

    # 6. taxonomic coverage (optional). Same reasoning as geographic
    # coverage above - delete any existing file first so edits in Tab 6
    # always take effect on regenerate.
    taxa_file <- file.path(working_folder, "taxonomic_coverage.txt")
    if (file.exists(taxa_file)) file.remove(taxa_file)
    if (isTRUE(taxonomy_state$enabled)) {
      EMLassemblyline::template_taxonomic_coverage(
        path = working_folder, data.path = working_folder,
        taxa.table = purrr::map_chr(taxonomy_state$pairs, "table"),
        taxa.col = purrr::map_chr(taxonomy_state$pairs, "column"),
        taxa.authority = taxonomy_state$authorities,
        taxa.name.type = "scientific", write.file = TRUE
      )
    }

    list(ok = TRUE)
  }, error = function(e) {
    list(ok = FALSE, message = paste0("Failed while writing metadata templates: ", conditionMessage(e)))
  })

  if (!isTRUE(result$ok)) {
    return(list(success = FALSE, message = result$message))
  }

  # 7. save the human-readable generation script alongside the templates
  tryCatch({
    script <- build_generation_script(
      high_level_state, people_state, tables_state, fields_state, geo_state, taxonomy_state
    )
    writeLines(script, file.path(working_folder, "generation_script.R"))
  }, error = function(e) {
    # non-fatal - the script is a convenience artifact, not required for
    # the .xml itself to be produced
    NULL
  })

  # 8. make_eml()
  my_metadata <- tryCatch({
    EMLassemblyline::make_eml(
      path = working_folder,
      dataset.title = high_level_state$package_title,
      data.table = tables_state$metadata$file_name,
      data.table.name = tables_state$metadata$table_name,
      data.table.description = tables_state$metadata$description,
      data.table.url = rep("temporary URL", nrow(tables_state$metadata)),
      temporal.coverage = c(high_level_state$start_date, high_level_state$end_date),
      maintenance.description = high_level_state$data_status,
      package.id = high_level_state$metadata_id,
      return.obj = TRUE,
      write.file = FALSE
    )
  }, error = function(e) {
    e
  })

  if (inherits(my_metadata, "error") || inherits(my_metadata, "condition")) {
    return(list(success = FALSE, message = paste0("make_eml() failed: ", conditionMessage(my_metadata))))
  }

  # 8a. Apply NPS-specific EMLeditor edits to the in-memory object BEFORE
  # validation/writing - this is the "edit before writing to disk" step
  # that replaces the CLI workflow's separate post-hoc editing pass.
  #
  # DOI handling is exactly one of two mutually-exclusive calls, never
  # both, to avoid redundant work:
  #   - create_new_doi = TRUE  -> set_datastore_doi() (creates draft +
  #     attaches DOI + updates data table URLs, all in one call), then
  #     clean up any superseded old draft
  #   - create_new_doi = FALSE and doi_state already exists -> set_doi()
  #     (re-attaches the EXISTING DOI + updates URLs, no new draft)
  #   - neither -> no DOI handling at all (package not yet linked to DataStore)
  edit_result <- tryCatch({
    my_metadata <- apply_permissions_to_eml(my_metadata, permissions_state)
    my_metadata <- apply_org_context_to_eml(my_metadata, org_context_state)

    new_doi_state <- doi_state
    cleanup_message <- NULL

    if (isTRUE(create_new_doi)) {
      my_metadata <- EMLeditor::set_datastore_doi(my_metadata, force = TRUE, NPS = TRUE)
      new_doi <- EMLeditor::get_doi(my_metadata)
      new_doi_state <- list(reference_id = reference_id_from_doi(new_doi), doi = new_doi)
      cleanup_message <- cleanup_old_doi(doi_state)  # doi_state here is the OLD one being superseded
    } else if (!is.null(doi_state) && !is.na(doi_state$reference_id)) {
      my_metadata <- EMLeditor::set_doi(my_metadata, doi_state$reference_id, force = TRUE, NPS = TRUE)
    }

    list(ok = TRUE, my_metadata = my_metadata, doi_state = new_doi_state, cleanup_message = cleanup_message)
  }, error = function(e) {
    list(ok = FALSE, message = paste0("Failed while applying NPS-specific metadata edits: ", conditionMessage(e)))
  })

  if (!isTRUE(edit_result$ok)) {
    return(list(success = FALSE, message = edit_result$message))
  }
  my_metadata <- edit_result$my_metadata

  # 9. validate
  validation <- tryCatch(EML::eml_validate(my_metadata), error = function(e) e)
  if (inherits(validation, "condition")) {
    return(list(success = FALSE, message = paste0("Validation failed to run: ", conditionMessage(validation))))
  }
  if (!isTRUE(validation)) {
    return(list(
      success = FALSE,
      message = "EML did not pass schema validation.",
      validation_errors = attr(validation, "errors")
    ))
  }

  # 9b. Run EMLassemblyline::issues() and surface it verbatim. issues()
  # takes NO arguments - it inspects state left behind by the most recent
  # make_eml() call in this R session, so it must be called immediately
  # after make_eml() succeeds, in the same process. It is expected to
  # report SOMETHING on essentially every run (e.g. NPS data packages
  # never have a Principal Investigator, which issues() flags as missing)
  # - it is not itself a failure signal. issues() prints its report
  # directly to the console as a side effect rather than returning it as a
  # value, so capture.output() is used to grab the printed text, with a
  # withCallingHandlers() wrapper in case issues() uses message() instead
  # of cat()/print(). issues() failing to run at all is non-fatal - it's a
  # diagnostic aid, not a gate on whether the .xml was written.
  content_issues <- tryCatch({
    lines <- character(0)
    withCallingHandlers(
      {
        printed <- utils::capture.output(EMLassemblyline::issues())
        lines <- c(lines, printed)
      },
      message = function(m) {
        lines <<- c(lines, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    lines <- lines[nzchar(trimws(lines))]
    paste(lines, collapse = "\n")
  }, error = function(e) NULL)

  # 10. write final .xml into data_package_creation, then copy into
  #     data_package (the deliverable) - always named
  #     <metadata_id>_metadata.xml, enforced here rather than trusting
  #     user input to include the suffix.
  xml_filename <- paste0(high_level_state$metadata_id, "_metadata.xml")
  creation_xml_path <- file.path(working_folder, xml_filename)
  deliverable_xml_path <- file.path(dirs$data_package_dir, xml_filename)

  write_result <- tryCatch({
    EML::write_eml(my_metadata, creation_xml_path)
    file.copy(creation_xml_path, deliverable_xml_path, overwrite = TRUE)
    TRUE
  }, error = function(e) e)

  if (!isTRUE(write_result)) {
    return(list(success = FALSE, message = paste0("Failed to write .xml: ", conditionMessage(write_result))))
  }

  list(
    success = TRUE,
    message = paste0(
      "Successfully wrote ", xml_filename,
      if (!is.null(edit_result$cleanup_message)) paste0(" (", edit_result$cleanup_message, ")") else ""
    ),
    xml_path = deliverable_xml_path,
    content_issues = content_issues,
    doi_state = edit_result$doi_state
  )
}
