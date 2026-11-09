# import_eml.R v1
#
# Import an existing Ecological Metadata Language XML document into the
# canonical bReezEML application-state structure.
#
# This importer deliberately uses xml2 directly rather than requiring
# EML::read_eml() to succeed. As a result, an XML document may be imported
# even when it is not EML-schema-valid, provided it is still well-formed
# XML.
#
# The importer returns state shaped like default_app_state(). That state
# is intended to be passed through the existing module restore() methods
# in app_server.R. This preserves the carefully tested deferred-restore
# behavior already implemented by Tabs 3-6.
#
# Import modes:
#
#   revise_draft
#     Retains a DataStore DOI/reference found in the imported XML by
#     populating state$doi_state. A later Generate can reuse that reference
#     through the existing EMLeditor::set_doi() path.
#
#   new_package
#     Imports editable metadata but deliberately leaves state$doi_state
#     NULL. A later Generate therefore creates a new DataStore draft and
#     DOI through the existing create-new-DOI workflow.
#
# Conservative behavior:
#
# - Data tables and fields are restored by their XML objectName and
#   attributeName values. Existing Tab 3/4 logic performs exact-name
#   matching when current CSV files are uploaded.
# - File and column renames are not inferred.
# - Tab 5 geography source table/column selections are not reconstructed
#   from calculated geographicCoverage output.
# - Tab 6 taxonomy source table/column selections are not reconstructed
#   from calculated taxonomicCoverage output.
# - metadataProvider is not assumed to be an NPS producing-unit code.
# - Projects and cross-references are imported only when a future,
#   explicitly recognized serialization is implemented.
# - A permissions legal-authority numeric ID is not guessed from its
#   label, description, marking, or URL.
#
# The returned object contains:
#
#   $state
#     A list matching default_app_state().
#
#   $warnings
#     Human-readable import limitations and fields requiring review.
#
#   $summary
#     Counts and key identifiers suitable for an import-result message.
#
# xml2 must be listed in DESCRIPTION Imports.

#' Read an EML XML file and convert it to bReezEML application state
#'
#' @param file path to a local XML file
#' @param mode one of "revise_draft" or "new_package"
#'
#' @return list with state, warnings, and summary
#' @keywords internal
import_eml_state <- function(file,
                             mode = c("revise_draft", "new_package")) {
  mode <- match.arg(mode)

  if (is.null(file) || length(file) != 1 || is.na(file) || !nzchar(file)) {
    stop("No EML XML file was supplied.")
  }

  if (!file.exists(file)) {
    stop("The selected EML XML file does not exist.")
  }

  doc <- tryCatch(
    xml2::read_xml(
      file,
      options = c("NONET", "NOBLANKS")
    ),
    error = function(e) {
      stop(
        paste0(
          "Could not parse the selected file as well-formed XML: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  root <- xml2::xml_root(doc)
  root_name <- xml2::xml_name(root)

  if (!identical(root_name, "eml")) {
    stop(
      paste0(
        "The selected XML document does not have an EML root element. ",
        "Found root element: ",
        root_name,
        "."
      ),
      call. = FALSE
    )
  }

  dataset <- xml_find_first_local(root, "dataset")
  if (is_missing_xml(dataset)) {
    stop(
      "The selected EML document does not contain a dataset element.",
      call. = FALSE
    )
  }

  warnings <- character(0)
  state <- default_app_state()

  high_level_result <- extract_eml_high_level(root, dataset)
  state$high_level_info <- high_level_result$value
  warnings <- c(warnings, high_level_result$warnings)

  people_result <- extract_eml_people(dataset)
  state$people <- people_result$value
  warnings <- c(warnings, people_result$warnings)

  tables_result <- extract_eml_tables(dataset)
  state$data_tables <- tables_result$data_tables
  state$fields <- tables_result$fields
  warnings <- c(warnings, tables_result$warnings)

  permissions_result <- extract_eml_permissions(root, dataset)
  state$permissions <- permissions_result$value
  warnings <- c(warnings, permissions_result$warnings)

  org_result <- extract_eml_org_context(dataset)
  state$org_context <- org_result$value
  warnings <- c(warnings, org_result$warnings)

  geography_result <- extract_eml_geography_import(dataset)
  state$geography <- geography_result$value
  warnings <- c(warnings, geography_result$warnings)

  taxonomy_result <- extract_eml_taxonomy_import(dataset)
  state$taxonomy <- taxonomy_result$value
  warnings <- c(warnings, taxonomy_result$warnings)

  doi_result <- extract_eml_doi_state(dataset)

  if (identical(mode, "revise_draft")) {
    if (is.null(doi_result$value)) {
      warnings <- c(
        warnings,
        paste0(
          "Revise-existing-draft mode was selected, but no usable ",
          "DataStore DOI/reference ID was found in the imported EML. ",
          "The active DOI state was left empty."
        )
      )
      state$doi_state <- NULL
    } else {
      state$doi_state <- doi_result$value
    }
  } else {
    state$doi_state <- NULL

    if (!is.null(doi_result$value)) {
      warnings <- c(
        warnings,
        paste0(
          "The imported EML contains DOI ",
          doi_result$value$doi,
          " and DataStore reference ",
          doi_result$value$reference_id,
          ". They were intentionally not placed into active DOI state ",
          "because create-new-package mode was selected."
        )
      )
    }
  }

  state$upload_publish <- default_app_state()$upload_publish

  warnings <- unique(warnings[nzchar(trimws(warnings))])

  table_metadata <- state$data_tables$metadata
  table_count <- length(table_metadata$file_name %||% character(0))
  field_count <- sum(
    vapply(
      state$fields$tables,
      function(x) length(x$attributes$attributeName %||% character(0)),
      integer(1)
    )
  )

  list(
    state = state,
    warnings = warnings,
    summary = list(
      import_mode = mode,
      package_id = state$high_level_info$metadata_id,
      package_title = state$high_level_info$package_title,
      table_count = table_count,
      field_count = field_count,
      imported_doi = doi_result$doi,
      imported_reference_id = doi_result$reference_id,
      retained_doi = !is.null(state$doi_state)
    )
  )
}

#' Extract high-level package metadata from EML
#'
#' @param root EML document root
#' @param dataset dataset node
#'
#' @return list(value = high-level state, warnings = character)
#' @keywords internal
extract_eml_high_level <- function(root, dataset) {
  warnings <- character(0)

  metadata_id <- xml2::xml_attr(root, "packageId") %||% ""
  package_title <- xml_text_first_local(dataset, "title", default = "")
  data_status <- xml_text_path_local(
    dataset,
    c("maintenance", "description"),
    default = "complete"
  )

  data_status <- tolower(trimws(data_status))
  if (!data_status %in% c("complete", "ongoing")) {
    warnings <- c(
      warnings,
      paste0(
        "The imported maintenance description was '",
        data_status,
        "'. bReezEML supports 'complete' or 'ongoing'; ",
        "'complete' was used as the import default."
      )
    )
    data_status <- "complete"
  }

  abstract <- xml_text_descendant_path(
    dataset,
    c("abstract", "para"),
    collapse = "\n\n",
    default = ""
  )

  method_nodes <- xml_find_all_path_local(
    dataset,
    c("methods", "methodStep", "description", "para")
  )
  methods <- xml_nodes_text(method_nodes, collapse = "\n\n")

  additional_notes <- xml_text_descendant_path(
    dataset,
    c("additionalInfo", "para"),
    collapse = "\n\n",
    default = ""
  )

  keyword_sets <- xml_find_all_local(dataset, "keywordSet")
  keyword_values <- character(0)
  thesaurus_values <- character(0)

  if (length(keyword_sets) > 0) {
    for (keyword_set in keyword_sets) {
      thesaurus <- xml_text_first_local(
        keyword_set,
        "keywordThesaurus",
        default = NA_character_
      )
      keyword_nodes <- xml_find_all_local(keyword_set, "keyword")

      if (length(keyword_nodes) == 0) next

      values <- trim_xml_text(xml2::xml_text(keyword_nodes))
      keep <- nzchar(values)

      if (!any(keep)) next

      keyword_values <- c(keyword_values, values[keep])
      thesaurus_values <- c(
        thesaurus_values,
        rep(
          if (is_blank_xml_value(thesaurus)) NA_character_ else thesaurus,
          sum(keep)
        )
      )
    }
  }

  begin_date <- xml_text_path_local(
    dataset,
    c(
      "coverage",
      "temporalCoverage",
      "rangeOfDates",
      "beginDate",
      "calendarDate"
    ),
    default = NA_character_
  )

  end_date <- xml_text_path_local(
    dataset,
    c(
      "coverage",
      "temporalCoverage",
      "rangeOfDates",
      "endDate",
      "calendarDate"
    ),
    default = NA_character_
  )

  if (is_blank_xml_value(begin_date)) begin_date <- NA_character_
  if (is_blank_xml_value(end_date)) end_date <- NA_character_

  list(
    value = list(
      metadata_id = metadata_id,
      package_title = package_title,
      data_status = data_status,
      abstract = abstract,
      methods = methods,
      additional_notes = additional_notes,
      keywords = list(
        keyword = keyword_values,
        keywordThesaurus = thesaurus_values
      ),
      start_date = begin_date,
      end_date = end_date
    ),
    warnings = warnings
  )
}

#' Extract creators, contacts, contributors, and editors from EML
#'
#' @param dataset dataset node
#'
#' @return list(value = people state, warnings = character)
#' @keywords internal
extract_eml_people <- function(dataset) {
  warnings <- character(0)

  creators <- xml_find_children_local(dataset, "creator")
  contacts <- xml_find_children_local(dataset, "contact")
  associated <- xml_find_children_local(dataset, "associatedParty")

  authors_tbl <- eml_party_nodes_to_tbl(creators)
  contacts_tbl <- eml_party_nodes_to_tbl(contacts)

  contributors_tbl <- empty_import_people_tbl(with_role = TRUE)
  editors_tbl <- empty_import_people_tbl(with_role = FALSE)

  if (length(associated) > 0) {
    associated_rows <- lapply(associated, function(node) {
      role <- xml_text_first_local(node, "role", default = "")
      party <- eml_party_node_to_row(node)

      list(
        role = role,
        party = party
      )
    })

    editor_rows <- associated_rows[
      vapply(
        associated_rows,
        function(x) identical(tolower(trimws(x$role)), "editor"),
        logical(1)
      )
    ]

    contributor_rows <- associated_rows[
      !vapply(
        associated_rows,
        function(x) identical(tolower(trimws(x$role)), "editor"),
        logical(1)
      )
    ]

    if (length(editor_rows) > 0) {
      editors_tbl <- dplyr::bind_rows(
        lapply(editor_rows, function(x) x$party)
      )
    }

    if (length(contributor_rows) > 0) {
      contributors_tbl <- dplyr::bind_rows(
        lapply(contributor_rows, function(x) {
          row <- x$party
          row$role <- x$role
          row
        })
      )
    }
  }

  list(
    value = list(
      authors = people_tbl_to_state_columns(authors_tbl),
      contacts = people_tbl_to_state_columns(contacts_tbl),
      contributors = people_tbl_to_state_columns(contributors_tbl),
      editors = people_tbl_to_state_columns(editors_tbl)
    ),
    warnings = warnings
  )
}

#' Extract data-table and field metadata from EML
#'
#' @param dataset dataset node
#'
#' @return list with data_tables, fields, and warnings
#' @keywords internal
extract_eml_tables <- function(dataset) {
  warnings <- character(0)
  data_table_nodes <- xml_find_children_local(dataset, "dataTable")

  if (length(data_table_nodes) == 0) {
    return(list(
      data_tables = list(metadata = list()),
      fields = list(tables = list()),
      warnings = "The imported EML contains no dataTable elements."
    ))
  }

  metadata_rows <- list()
  fields_by_file <- list()

  for (i in seq_along(data_table_nodes)) {
    table_node <- data_table_nodes[[i]]

    file_name <- xml_text_path_local(
      table_node,
      c("physical", "objectName"),
      default = ""
    )
    table_name <- xml_text_first_local(
      table_node,
      "entityName",
      default = ""
    )
    description <- xml_text_first_local(
      table_node,
      "entityDescription",
      default = ""
    )

    if (!nzchar(file_name)) {
      file_name <- paste0("imported_table_", i, ".csv")
      warnings <- c(
        warnings,
        paste0(
          "Data table ",
          i,
          " had no physical/objectName. It was assigned the placeholder ",
          "filename '",
          file_name,
          "'."
        )
      )
    }

    if (file_name %in% vapply(
      metadata_rows,
      function(x) x$file_name,
      character(1)
    )) {
      warnings <- c(
        warnings,
        paste0(
          "The imported EML contains more than one dataTable with ",
          "objectName '",
          file_name,
          "'. Only one field-state entry can be retained under that name."
        )
      )
    }

    metadata_rows[[length(metadata_rows) + 1]] <- list(
      file_name = file_name,
      table_name = table_name,
      description = description
    )

    attribute_result <- extract_eml_table_fields(table_node)
    fields_by_file[[file_name]] <- attribute_result$value
    warnings <- c(warnings, attribute_result$warnings)
  }

  metadata_tbl <- dplyr::bind_rows(metadata_rows)

  list(
    data_tables = list(
      metadata = list(
        file_name = as.character(metadata_tbl$file_name),
        table_name = as.character(metadata_tbl$table_name),
        description = as.character(metadata_tbl$description)
      )
    ),
    fields = list(tables = fields_by_file),
    warnings = warnings
  )
}

#' Extract one data table's attributes and categorical codes
#'
#' @param table_node one EML dataTable node
#'
#' @return list(value = table field state, warnings = character)
#' @keywords internal
extract_eml_table_fields <- function(table_node) {
  warnings <- character(0)
  attribute_nodes <- xml_find_all_path_local(
    table_node,
    c("attributeList", "attribute")
  )

  if (length(attribute_nodes) == 0) {
    return(list(
      value = list(
        attributes = empty_import_attributes_state(),
        catvars = empty_import_catvars_state()
      ),
      warnings = "An imported dataTable contains no attribute elements."
    ))
  }

  attribute_rows <- list()
  catvar_rows <- list()

  for (attribute_node in attribute_nodes) {
    attribute_name <- xml_text_first_local(
      attribute_node,
      "attributeName",
      default = ""
    )
    attribute_definition <- xml_text_first_local(
      attribute_node,
      "attributeDefinition",
      default = ""
    )

    class_result <- infer_imported_eml_class(attribute_node)

    unit <- xml_text_path_local(
      attribute_node,
      c("measurementScale", "ratio", "unit", "standardUnit"),
      default = NA_character_
    )

    if (is_blank_xml_value(unit)) {
      unit <- xml_text_path_local(
        attribute_node,
        c("measurementScale", "interval", "unit", "standardUnit"),
        default = NA_character_
      )
    }

    date_format <- xml_text_path_local(
      attribute_node,
      c("measurementScale", "dateTime", "formatString"),
      default = NA_character_
    )

    missing_pair <- extract_eml_missing_value_pair(attribute_node)

    attribute_rows[[length(attribute_rows) + 1]] <- tibble::tibble(
      attributeName = attribute_name,
      attributeDefinition = attribute_definition,
      class = class_result,
      unit = normalize_optional_xml_text(unit),
      dateTimeFormatString = normalize_optional_xml_text(date_format),
      missingValueCode = missing_pair$code,
      missingValueCodeExplanation = missing_pair$explanation
    )

    code_nodes <- xml_find_all_path_local(
      attribute_node,
      c(
        "measurementScale",
        "nominal",
        "nonNumericDomain",
        "enumeratedDomain",
        "codeDefinition"
      )
    )

    if (length(code_nodes) > 0) {
      for (code_node in code_nodes) {
        catvar_rows[[length(catvar_rows) + 1]] <- tibble::tibble(
          attributeName = attribute_name,
          code = xml_text_first_local(code_node, "code", default = ""),
          definition = xml_text_first_local(
            code_node,
            "definition",
            default = ""
          )
        )
      }
    }

    if (!nzchar(attribute_name)) {
      warnings <- c(
        warnings,
        "An imported attribute had no attributeName."
      )
    }
  }

  attributes_tbl <- dplyr::bind_rows(attribute_rows)
  catvars_tbl <- if (length(catvar_rows) == 0) {
    tibble::tibble(
      attributeName = character(0),
      code = character(0),
      definition = character(0)
    )
  } else {
    dplyr::bind_rows(catvar_rows)
  }

  list(
    value = list(
      attributes = tibble_to_state_columns(attributes_tbl),
      catvars = tibble_to_state_columns(catvars_tbl)
    ),
    warnings = warnings
  )
}

#' Extract permission, rights, and language state
#'
#' @param root EML document root
#' @param dataset dataset node
#'
#' @return list(value = permissions state, warnings = character)
#' @keywords internal
extract_eml_permissions <- function(root, dataset) {
  warnings <- character(0)

  access_level <- xml_text_path_local(
    root,
    c(
      "additionalMetadata",
      "metadata",
      "distribution",
      "accessLevel"
    ),
    default = ""
  )

  if (!nzchar(access_level)) {
    deny_public <- xml2::xml_find_first(
      root,
      paste0(
        ".//*[local-name()='access']",
        "/*[local-name()='deny']",
        "[*[local-name()='principal' and normalize-space(text())='public']]"
      )
    )

    access_level <- if (is_missing_xml(deny_public)) {
      "PUBLIC"
    } else {
      "RESTRICTED"
    }

    warnings <- c(
      warnings,
      paste0(
        "No NPS permissions accessLevel extension was found. Access level ",
        "was inferred as ",
        access_level,
        " from the document-level access element and should be reviewed."
      )
    )
  }

  access_level <- toupper(trimws(access_level))
  if (!access_level %in% c("PUBLIC", "INTERNAL", "RESTRICTED")) {
    warnings <- c(
      warnings,
      paste0(
        "Imported access level '",
        access_level,
        "' is not supported by bReezEML. PUBLIC was used."
      )
    )
    access_level <- "PUBLIC"
  }

  authority_designator <- xml_text_path_local(
    root,
    c(
      "additionalMetadata",
      "metadata",
      "distribution",
      "authorityDesignator"
    ),
    default = ""
  )

  contact_email <- xml_text_path_local(
    root,
    c(
      "additionalMetadata",
      "metadata",
      "distribution",
      "contactEmail"
    ),
    default = ""
  )

  legal_authority_id <- NA_integer_

  if (access_level != "PUBLIC") {
    warnings <- c(
      warnings,
      paste0(
        "The imported EML contains restricted-access authority metadata, ",
        "but it does not provide the numeric legal_authority_id required ",
        "by bReezEML. Select the appropriate justification on Tab 7."
      )
    )
  }

  license_name <- xml_text_path_local(
    dataset,
    c("licensed", "licenseName"),
    default = ""
  )
  intellectual_rights <- xml_text_first_local(
    dataset,
    "intellectualRights",
    default = ""
  )

  int_rights <- infer_imported_intellectual_rights(
    license_name = license_name,
    intellectual_rights = intellectual_rights,
    access_level = access_level
  )

  language_raw <- xml_text_first_local(
    dataset,
    "language",
    default = "eng"
  )
  language <- normalize_imported_language(language_raw)

  list(
    value = list(
      access_level = access_level,
      legal_authority_id = legal_authority_id,
      contact_email = contact_email,
      authority_designator = authority_designator,
      int_rights = int_rights,
      language = language,
      language_other = ""
    ),
    warnings = warnings
  )
}

#' Extract organizational-context state that can be identified safely
#'
#' @param dataset dataset node
#'
#' @return list(value = org-context state, warnings = character)
#' @keywords internal
extract_eml_org_context <- function(dataset) {
  warnings <- character(0)

  content_nodes <- xml2::xml_find_all(
    dataset,
    paste0(
      ".//*[local-name()='geographicCoverage'",
      " and @system='content unit link']",
      "/*[local-name()='geographicDescription']"
    )
  )

  content_descriptions <- trim_xml_text(xml2::xml_text(content_nodes))
  content_units <- sub(
    "^NPS\\s+Content\\s+Unit\\s+Link:\\s*",
    "",
    content_descriptions,
    ignore.case = TRUE
  )
  content_units <- toupper(trimws(content_units))
  content_units <- unique(content_units[nzchar(content_units)])

  metadata_provider_nodes <- xml_find_all_path_local(
    dataset,
    c("metadataProvider", "organizationName")
  )

  if (length(metadata_provider_nodes) > 0) {
    warnings <- c(
      warnings,
      paste0(
        "The imported EML contains metadataProvider organization values, ",
        "but they were not assumed to be NPS producing-unit codes. Review ",
        "Tab 8 and add the producing unit or units."
      )
    )
  }

  list(
    value = list(
      content_units = content_units,
      producing_units = character(0),
      project_id = NA_integer_,
      project_title = NA_character_,
      cross_references = integer(0),
      cross_reference_titles = character(0)
    ),
    warnings = warnings
  )
}

#' Build conservative geography state for imported EML
#'
#' The EML stores geographic coverage output but generally does not retain
#' the source table/latitude/longitude/site-column selections that created
#' that output. Those inputs are therefore not guessed.
#'
#' @param dataset dataset node
#'
#' @return list(value = geography state, warnings = character)
#' @keywords internal
extract_eml_geography_import <- function(dataset) {
  geographic_nodes <- xml_find_all_path_local(
    dataset,
    c("coverage", "geographicCoverage")
  )

  non_content_nodes <- geographic_nodes[
    vapply(
      geographic_nodes,
      function(node) {
        system <- xml2::xml_attr(node, "system")
        is.na(system) || !identical(system, "content unit link")
      },
      logical(1)
    )
  ]

  warnings <- character(0)

  if (length(non_content_nodes) > 0) {
    warnings <- c(
      warnings,
      paste0(
        "The imported EML contains site-level geographic coverage, but EML ",
        "does not preserve the source table, latitude column, longitude ",
        "column, and site-name column selections required by Tab 5. ",
        "Select those fields again after uploading the current data files."
      )
    )
  }

  list(
    value = list(
      has_coords = FALSE,
      lat_table = NA_character_,
      lat_col = NA_character_,
      lon_table = NA_character_,
      lon_col = NA_character_,
      site_table = NA_character_,
      site_col = NA_character_
    ),
    warnings = warnings
  )
}

#' Build conservative taxonomy state for imported EML
#'
#' Expanded taxonomicCoverage does not reliably identify the data table
#' and source column used to generate it, so the source selections are not
#' guessed.
#'
#' @param dataset dataset node
#'
#' @return list(value = taxonomy state, warnings = character)
#' @keywords internal
extract_eml_taxonomy_import <- function(dataset) {
  taxonomy_nodes <- xml_find_all_path_local(
    dataset,
    c("coverage", "taxonomicCoverage")
  )

  warnings <- character(0)

  if (length(taxonomy_nodes) > 0) {
    warnings <- c(
      warnings,
      paste0(
        "The imported EML contains taxonomic coverage, but EML does not ",
        "reliably preserve the source data table and scientific-name ",
        "column required by Tab 6. Select those fields again after ",
        "uploading the current data files."
      )
    )
  }

  list(
    value = list(
      has_taxa = FALSE,
      pairs = list(
        table = character(0),
        column = character(0)
      ),
      authorities = character(0)
    ),
    warnings = warnings
  )
}

#' Extract a DataStore DOI and reference ID
#'
#' @param dataset dataset node
#'
#' @return list with value, doi, and reference_id
#' @keywords internal
extract_eml_doi_state <- function(dataset) {
  alternate_nodes <- xml_find_children_local(
    dataset,
    "alternateIdentifier"
  )
  alternate_values <- trim_xml_text(xml2::xml_text(alternate_nodes))

  doi_candidates <- alternate_values[
    grepl(
      "10\\.57830/[0-9]{7}",
      alternate_values,
      perl = TRUE
    )
  ]

  doi <- NA_character_
  reference_id <- NA_integer_

  if (length(doi_candidates) > 0) {
    match <- regmatches(
      doi_candidates[[1]],
      regexpr(
        "10\\.57830/[0-9]{7}",
        doi_candidates[[1]],
        perl = TRUE
      )
    )

    if (length(match) == 1 && nzchar(match)) {
      doi <- match
      reference_match <- regmatches(
        doi,
        regexpr("[0-9]{7}$", doi, perl = TRUE)
      )

      if (length(reference_match) == 1 && nzchar(reference_match)) {
        reference_id <- as.integer(reference_match)
      }
    }
  }

  value <- if (!is.na(doi) && !is.na(reference_id)) {
    list(
      reference_id = reference_id,
      doi = doi
    )
  } else {
    NULL
  }

  list(
    value = value,
    doi = doi,
    reference_id = reference_id
  )
}

#' Infer a bReezEML field class from one EML attribute
#'
#' @param attribute_node EML attribute node
#'
#' @return one of numeric, character, categorical, or Date
#' @keywords internal
infer_imported_eml_class <- function(attribute_node) {
  measurement_scale <- xml_find_first_local(
    attribute_node,
    "measurementScale"
  )

  if (!is_missing_xml(
    xml_find_first_local(measurement_scale, "dateTime")
  )) {
    return("Date")
  }

  if (!is_missing_xml(
    xml_find_first_local(measurement_scale, "enumeratedDomain")
  )) {
    return("categorical")
  }

  if (
    !is_missing_xml(xml_find_first_local(measurement_scale, "ratio")) ||
    !is_missing_xml(xml_find_first_local(measurement_scale, "interval"))
  ) {
    return("numeric")
  }

  storage_type <- tolower(
    xml_text_first_local(
      attribute_node,
      "storageType",
      default = ""
    )
  )

  if (storage_type %in% c(
    "byte", "short", "integer", "int", "long",
    "float", "double", "decimal", "numeric", "number"
  )) {
    return("numeric")
  }

  if (storage_type %in% c("date", "datetime", "date-time")) {
    return("Date")
  }

  "character"
}

#' Extract one missing-value code and explanation pair
#'
#' bReezEML currently stores one pair per attribute. If an imported
#' attribute contains multiple missing-value entries, the first complete
#' pair is used.
#'
#' @param attribute_node EML attribute node
#'
#' @return list(code = character or NA, explanation = character or NA)
#' @keywords internal
extract_eml_missing_value_pair <- function(attribute_node) {
  missing_nodes <- xml_find_children_local(
    attribute_node,
    "missingValueCode"
  )

  if (length(missing_nodes) == 0) {
    return(list(
      code = NA_character_,
      explanation = NA_character_
    ))
  }

  for (missing_node in missing_nodes) {
    code <- xml_text_first_local(
      missing_node,
      "code",
      default = NA_character_
    )
    explanation <- xml_text_first_local(
      missing_node,
      "codeExplanation",
      default = NA_character_
    )

    if (
      !is_blank_xml_value(code) ||
      !is_blank_xml_value(explanation)
    ) {
      return(list(
        code = normalize_optional_xml_text(code),
        explanation = normalize_optional_xml_text(explanation)
      ))
    }
  }

  list(
    code = NA_character_,
    explanation = NA_character_
  )
}

#' Infer the bReezEML intellectual-rights option
#'
#' @param license_name EML licenseName text
#' @param intellectual_rights intellectualRights text
#' @param access_level imported access level
#'
#' @return one of CC0, public, or restricted
#' @keywords internal
infer_imported_intellectual_rights <- function(
    license_name,
    intellectual_rights,
    access_level) {

  combined <- tolower(
    paste(
      license_name %||% "",
      intellectual_rights %||% ""
    )
  )

  if (
    grepl("unlicensed", combined, fixed = TRUE) ||
    grepl("restricted", combined, fixed = TRUE) ||
    grepl("controlled unclassified information", combined, fixed = TRUE)
  ) {
    return("restricted")
  }

  if (
    grepl("creative commons zero", combined, fixed = TRUE) ||
    grepl("cc0", combined, fixed = TRUE)
  ) {
    return("CC0")
  }

  if (grepl("public domain", combined, fixed = TRUE)) {
    return("public")
  }

  if (!identical(access_level, "PUBLIC")) {
    return("restricted")
  }

  "CC0"
}

#' Convert imported EML language values to bReezEML display values
#'
#' @param x language value from EML
#'
#' @return character scalar
#' @keywords internal
normalize_imported_language <- function(x) {
  value <- tolower(trimws(x %||% ""))

  known <- c(
    "eng" = "English",
    "en" = "English",
    "english" = "English",
    "spa" = "Spanish",
    "es" = "Spanish",
    "spanish" = "Spanish",
    "fra" = "French",
    "fre" = "French",
    "fr" = "French",
    "french" = "French",
    "deu" = "German",
    "ger" = "German",
    "de" = "German",
    "german" = "German",
    "nav" = "Navajo",
    "navajo" = "Navajo",
    "haw" = "Hawaiian",
    "hawaiian" = "Hawaiian"
  )

  if (value %in% names(known)) {
    return(unname(known[[value]]))
  }

  if (!nzchar(value)) {
    return("English")
  }

  trimws(x)
}

#' Convert EML party nodes to an import people tibble
#'
#' @param nodes XML node set
#'
#' @return tibble
#' @keywords internal
eml_party_nodes_to_tbl <- function(nodes) {
  if (length(nodes) == 0) {
    return(empty_import_people_tbl(with_role = FALSE))
  }

  dplyr::bind_rows(
    lapply(nodes, eml_party_node_to_row)
  )
}

#' Convert one EML party node to an import people row
#'
#' @param node creator, contact, or associatedParty node
#'
#' @return one-row tibble
#' @keywords internal
eml_party_node_to_row <- function(node) {
  user_id <- xml_text_first_local(
    node,
    "userId",
    default = ""
  )

  orcid <- normalize_imported_orcid(user_id)

  tibble::tibble(
    email = xml_text_first_local(
      node,
      "electronicMailAddress",
      default = ""
    ),
    givenName = xml_text_path_local(
      node,
      c("individualName", "givenName"),
      default = ""
    ),
    surName = xml_text_path_local(
      node,
      c("individualName", "surName"),
      default = ""
    ),
    organizationName = xml_text_first_local(
      node,
      "organizationName",
      default = ""
    ),
    userId = orcid,
    ad_found = FALSE,
    orcid_status = if (nzchar(orcid)) "unchecked" else ""
  )
}

#' Normalize an imported ORCID URL or identifier
#'
#' @param x character scalar
#'
#' @return bare ORCID identifier or empty string
#' @keywords internal
normalize_imported_orcid <- function(x) {
  value <- trimws(x %||% "")
  if (!nzchar(value)) return("")

  match <- regmatches(
    value,
    regexpr(
      "[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9Xx]",
      value,
      perl = TRUE
    )
  )

  if (length(match) == 1 && nzchar(match)) {
    match
  } else {
    value
  }
}

#' Empty people tibble used during XML import
#'
#' @param with_role logical
#'
#' @return tibble
#' @keywords internal
empty_import_people_tbl <- function(with_role = FALSE) {
  result <- tibble::tibble(
    email = character(0),
    givenName = character(0),
    surName = character(0),
    organizationName = character(0),
    userId = character(0)
  )

  if (with_role) {
    result$role <- character(0)
  }

  result$ad_found <- logical(0)
  result$orcid_status <- character(0)

  result
}

#' Convert a people tibble to the list-of-columns restore shape
#'
#' @param x tibble or data frame
#'
#' @return list
#' @keywords internal
people_tbl_to_state_columns <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(list())
  }

  as.list(x)
}

#' Convert any tibble to the list-of-columns restore shape
#'
#' @param x tibble or data frame
#'
#' @return list
#' @keywords internal
tibble_to_state_columns <- function(x) {
  if (is.null(x)) return(list())
  as.list(x)
}

#' Construct empty attribute restore state
#'
#' @return list-of-columns
#' @keywords internal
empty_import_attributes_state <- function() {
  list(
    attributeName = character(0),
    attributeDefinition = character(0),
    class = character(0),
    unit = character(0),
    dateTimeFormatString = character(0),
    missingValueCode = character(0),
    missingValueCodeExplanation = character(0)
  )
}

#' Construct empty categorical-code restore state
#'
#' @return list-of-columns
#' @keywords internal
empty_import_catvars_state <- function() {
  list(
    attributeName = character(0),
    code = character(0),
    definition = character(0)
  )
}

# ---- Namespace-insensitive XML utilities ----

#' Find the first descendant having a local element name
#'
#' @param node XML node
#' @param name local element name
#'
#' @return xml node
#' @keywords internal
xml_find_first_local <- function(node, name) {
  if (is_missing_xml(node)) {
    return(xml2::xml_missing())
  }

  xml2::xml_find_first(
    node,
    paste0(".//*[local-name()='", name, "']")
  )
}

#' Find all descendants having a local element name
#'
#' @param node XML node
#' @param name local element name
#'
#' @return xml node set
#' @keywords internal
xml_find_all_local <- function(node, name) {
  if (is_missing_xml(node)) {
    return(xml2::xml_nodeset())
  }

  xml2::xml_find_all(
    node,
    paste0(".//*[local-name()='", name, "']")
  )
}

#' Find direct children having a local element name
#'
#' @param node XML node
#' @param name local element name
#'
#' @return xml node set
#' @keywords internal
xml_find_children_local <- function(node, name) {
  if (is_missing_xml(node)) {
    return(xml2::xml_nodeset())
  }

  xml2::xml_find_all(
    node,
    paste0("./*[local-name()='", name, "']")
  )
}

#' Find the first node following a direct-child local-name path
#'
#' @param node XML node
#' @param path character vector of local element names
#'
#' @return xml node
#' @keywords internal
xml_find_first_path_local <- function(node, path) {
  if (is_missing_xml(node) || length(path) == 0) {
    return(xml2::xml_missing())
  }

  xpath <- paste0(
    "./",
    paste0(
      "*[local-name()='",
      path,
      "']",
      collapse = "/"
    )
  )

  xml2::xml_find_first(node, xpath)
}

#' Find all nodes following a direct-child local-name path
#'
#' @param node XML node
#' @param path character vector of local element names
#'
#' @return xml node set
#' @keywords internal
xml_find_all_path_local <- function(node, path) {
  if (is_missing_xml(node) || length(path) == 0) {
    return(xml2::xml_nodeset())
  }

  xpath <- paste0(
    "./",
    paste0(
      "*[local-name()='",
      path,
      "']",
      collapse = "/"
    )
  )

  xml2::xml_find_all(node, xpath)
}

#' Extract text from the first descendant with a local name
#'
#' @param node XML node
#' @param name local element name
#' @param default value returned when absent or blank
#'
#' @return character scalar
#' @keywords internal
xml_text_first_local <- function(node, name, default = "") {
  found <- xml_find_first_local(node, name)

  if (is_missing_xml(found)) {
    return(default)
  }

  value <- trim_xml_text(xml2::xml_text(found))
  if (!nzchar(value)) default else value
}

#' Extract text from the first node found at a local-name path
#'
#' @param node XML node
#' @param path character vector of local element names
#' @param default value returned when absent or blank
#'
#' @return character scalar
#' @keywords internal
xml_text_path_local <- function(node, path, default = "") {
  found <- xml_find_first_path_local(node, path)

  if (is_missing_xml(found)) {
    return(default)
  }

  value <- trim_xml_text(xml2::xml_text(found))
  if (!nzchar(value)) default else value
}

#' Extract and combine text from all nodes at a local-name path
#'
#' @param node XML node
#' @param path character vector of local element names
#' @param collapse separator
#' @param default value returned when no usable text exists
#'
#' @return character scalar
#' @keywords internal
xml_text_descendant_path <- function(node,
                                     path,
                                     collapse = "\n",
                                     default = "") {
  nodes <- xml_find_all_path_local(node, path)
  value <- xml_nodes_text(nodes, collapse = collapse)

  if (!nzchar(value)) default else value
}

#' Trim, remove empty values, and combine XML node text
#'
#' @param nodes XML node set
#' @param collapse separator
#'
#' @return character scalar
#' @keywords internal
xml_nodes_text <- function(nodes, collapse = "\n") {
  if (length(nodes) == 0) return("")

  values <- trim_xml_text(xml2::xml_text(nodes))
  values <- values[nzchar(values)]

  paste(values, collapse = collapse)
}

#' Normalize XML text whitespace without collapsing internal paragraphs
#'
#' @param x character vector
#'
#' @return character vector
#' @keywords internal
trim_xml_text <- function(x) {
  if (length(x) == 0) return(character(0))

  x <- gsub("\r\n?", "\n", x)
  x <- gsub("[ \t]+", " ", x)
  x <- gsub(" *\n *", "\n", x)
  trimws(x)
}

#' Return TRUE for xml_missing or NULL
#'
#' @param node XML node
#'
#' @return logical scalar
#' @keywords internal
is_missing_xml <- function(node) {
  is.null(node) || inherits(node, "xml_missing")
}

#' Return TRUE for a missing or blank XML value
#'
#' @param x character value
#'
#' @return logical scalar
#' @keywords internal
is_blank_xml_value <- function(x) {
  is.null(x) ||
    length(x) == 0 ||
    is.na(x[[1]]) ||
    !nzchar(trimws(x[[1]]))
}

#' Convert optional XML text to NA_character_ when blank
#'
#' @param x character scalar
#'
#' @return character scalar
#' @keywords internal
normalize_optional_xml_text <- function(x) {
  if (is_blank_xml_value(x)) NA_character_ else trimws(x[[1]])
}

`%||%` <- function(a, b) {
  if (
    is.null(a) ||
    length(a) == 0 ||
    (length(a) == 1 && is.na(a))
  ) {
    b
  } else {
    a
  }
}
