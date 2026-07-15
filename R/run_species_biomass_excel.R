#' Run Species Biomass Reconstruction from an Excel File
#'
#' Reads an Excel file containing age-specific fertility, body mass, and a
#' single Weibull beta value, reconstructs the stable demographic profile,
#' derives biomass quantities, and writes the results to a new Excel file.
#'
#' The input sheet must contain columns named `age`, `mx`, `body_mass`, and
#' `beta`. The `beta` column may contain one numeric value and empty cells in
#' the remaining rows.
#'
#' @param input_file Character string. Path to the input Excel file.
#' @param output_file Character string. Path to the output Excel file. When
#'   `NULL`, the output file is created next to `input_file` with suffix
#'   `_biomass_output.xlsx`.
#' @param sheet Sheet name or position passed to [readxl::read_excel()].
#'   Default is `1`.
#' @param age_classes Optional data frame defining age-class intervals, with
#'   columns `age_class`, `min_age`, and `max_age`. When supplied, an
#'   `Age_Class_Summary` sheet is added to the output workbook.
#' @param overwrite Logical. Controls replacement of an existing output file.
#'   Default is `TRUE`.
#'
#' @return Invisibly returns a list containing the output file path, beta
#'   value, demographic profile, biomass profile, biomass summary, and
#'   optional age-class summary.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_species_biomass_excel(
#'   input_file = "species_input.xlsx",
#'   output_file = "species_biomass_output.xlsx"
#' )
#' }
run_species_biomass_excel <- function(input_file,
                                      output_file = NULL,
                                      sheet = 1,
                                      age_classes = NULL,
                                      overwrite = TRUE) {
  # Validate file paths and output overwrite behaviour.
  if (!is.character(input_file) || length(input_file) != 1L) {
    stop("`input_file` must be one file path.", call. = FALSE)
  }

  if (!file.exists(input_file)) {
    stop("`input_file` does not exist: ", input_file, call. = FALSE)
  }

  if (is.null(output_file)) {
    output_file <- sub("\\.xlsx$", "_biomass_output.xlsx", input_file, ignore.case = TRUE)

    if (identical(output_file, input_file)) {
      output_file <- paste0(input_file, "_biomass_output.xlsx")
    }
  }

  if (!is.character(output_file) || length(output_file) != 1L) {
    stop("`output_file` must be one file path.", call. = FALSE)
  }

  if (file.exists(output_file) && !isTRUE(overwrite)) {
    stop("`output_file` already exists and `overwrite = FALSE`: ", output_file, call. = FALSE)
  }

  # Read the selected Excel sheet into a plain data frame.
  input_data <- readxl::read_excel(input_file, sheet = sheet)
  input_data <- as.data.frame(input_data)

  # Check the expected species-level input columns.
  required_cols <- c("age", "mx", "body_mass", "beta")
  missing_cols <- setdiff(required_cols, names(input_data))

  if (length(missing_cols) > 0) {
    stop(
      "The input sheet must contain these columns: ",
      paste(required_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Keep rows containing the age-specific demographic and body-mass data.
  keep_rows <- !(
    is.na(input_data$age) &
      is.na(input_data$mx) &
      is.na(input_data$body_mass)
  )

  input_data <- input_data[keep_rows, , drop = FALSE]

  if (nrow(input_data) == 0L) {
    stop("The input sheet must contain at least one demographic row.", call. = FALSE)
  }

  # Coerce the three age-specific columns to numeric vectors.
  age <- suppressWarnings(as.numeric(input_data$age))
  fertility_rates <- suppressWarnings(as.numeric(input_data$mx))
  body_mass_values <- suppressWarnings(as.numeric(input_data$body_mass))

  if (any(!is.finite(age))) {
    stop("The `age` column must contain finite numeric values.", call. = FALSE)
  }

  if (any(!is.finite(fertility_rates))) {
    stop("The `mx` column must contain finite numeric values.", call. = FALSE)
  }

  if (any(!is.finite(body_mass_values))) {
    stop("The `body_mass` column must contain finite numeric values.", call. = FALSE)
  }

  if (any(fertility_rates < 0)) {
    stop("The `mx` column must contain non-negative fertility values.", call. = FALSE)
  }

  if (any(body_mass_values < 0)) {
    stop("The `body_mass` column must contain non-negative body mass values.", call. = FALSE)
  }

  # Use consecutive age-class indices to stay aligned with StablePopulation.
  expected_age <- seq_along(fertility_rates) - 1L

  if (!isTRUE(all.equal(age, expected_age, check.attributes = FALSE))) {
    stop(
      "For this workflow, the `age` column must be consecutive age-class indices: ",
      paste(expected_age, collapse = ", "),
      call. = FALSE
    )
  }

  # Extract one species-level beta value from the beta column.
  beta_values <- suppressWarnings(as.numeric(input_data$beta))
  beta_values <- unique(beta_values[is.finite(beta_values)])

  if (length(beta_values) == 0L) {
    stop("The `beta` column must contain one numeric beta value.", call. = FALSE)
  }

  if (length(beta_values) > 1L) {
    stop(
      "The `beta` column must contain one unique beta value per sheet. Values found: ",
      paste(beta_values, collapse = ", "),
      call. = FALSE
    )
  }

  beta <- beta_values[[1L]]

  if (beta <= 0) {
    stop("The beta value must be positive.", call. = FALSE)
  }

  # Build the body-mass table expected by derive_biomass_from_fertility().
  body_mass <- data.frame(
    age = age,
    body_mass = body_mass_values
  )

  # Run the demographic reconstruction and biomass derivation.
  result <- derive_biomass_from_fertility(
    fertility_rates = fertility_rates,
    beta = beta,
    body_mass = body_mass
  )

  # Extract the main tables returned by the R workflow.
  demographic_profile <- result$demographic_profile$table
  biomass_profile <- result$biomass_profile
  biomass_summary <- summarise_biomass_profile(biomass_profile)

  # Add optional age-class summaries when the user provides intervals.
  age_class_summary <- NULL

  if (!is.null(age_classes)) {
    age_class_summary <- summarise_age_classes(
      biomass_profile = biomass_profile,
      age_classes = age_classes
    )
  }

  # Record key run settings and reconstructed parameters.
  metadata <- data.frame(
    field = c(
      "input_file",
      "sheet",
      "beta",
      "n_age_classes",
      "alpha",
      "reproductive_output"
    ),
    value = c(
      normalizePath(input_file, winslash = "/", mustWork = FALSE),
      as.character(sheet),
      as.character(beta),
      as.character(length(fertility_rates)),
      as.character(result$reconstruction$alpha),
      as.character(result$reconstruction$reproductive_output)
    )
  )

  # Create the output workbook with one worksheet per result table.
  workbook <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(workbook, "Input")
  openxlsx::writeData(workbook, "Input", input_data)

  openxlsx::addWorksheet(workbook, "Demographic_Profile")
  openxlsx::writeData(workbook, "Demographic_Profile", demographic_profile)

  openxlsx::addWorksheet(workbook, "Biomass_Profile")
  openxlsx::writeData(workbook, "Biomass_Profile", biomass_profile)

  openxlsx::addWorksheet(workbook, "Biomass_Summary")
  openxlsx::writeData(workbook, "Biomass_Summary", biomass_summary)

  if (!is.null(age_class_summary)) {
    openxlsx::addWorksheet(workbook, "Age_Class_Summary")
    openxlsx::writeData(workbook, "Age_Class_Summary", age_class_summary)
  }

  openxlsx::addWorksheet(workbook, "Metadata")
  openxlsx::writeData(workbook, "Metadata", metadata)

  # Apply simple spreadsheet formatting to every output sheet.
  sheet_names <- names(workbook)

  for (sheet_name in sheet_names) {
    openxlsx::freezePane(workbook, sheet_name, firstRow = TRUE)
    sheet_data <- openxlsx::readWorkbook(workbook, sheet = sheet_name)
    openxlsx::addFilter(workbook, sheet_name, rows = 1, cols = 1:ncol(sheet_data))
  }

  # Save the workbook and return the same objects invisibly for R users.
  openxlsx::saveWorkbook(workbook, output_file, overwrite = overwrite)

  invisible(list(
    output_file = output_file,
    beta = beta,
    demographic_profile = demographic_profile,
    biomass_profile = biomass_profile,
    biomass_summary = biomass_summary,
    age_class_summary = age_class_summary,
    metadata = metadata
  ))
}
