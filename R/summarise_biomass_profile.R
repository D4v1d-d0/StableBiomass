#' Summarise a Biomass Profile
#'
#' Summarises an age-structured biomass profile into species-level biomass
#' quantities. The function reports total live biomass, total mortality
#' biomass, total birth biomass, weighted mean body masses, and the age
#' classes contributing the highest live and mortality biomass.
#'
#' @param biomass_profile A data frame produced by
#'   [derive_biomass_profile()] or another table with equivalent columns.
#' @param age_col Character string. Name of the age column.
#'   Default is `"age"`.
#' @param mass_col Character string. Name of the body mass column.
#'   Default is `"body_mass"`.
#' @param live_biomass_col Character string. Name of the live biomass column.
#'   Default is `"live_biomass"`.
#' @param mortality_biomass_col Character string. Name of the mortality
#'   biomass column. Default is `"mortality_biomass"`.
#' @param birth_biomass_col Character string. Name of the birth biomass
#'   column. Default is `"birth_biomass"`.
#' @param structure_col Character string. Name of the stable structure column.
#'   Default is `"R"`.
#' @param deaths_col Character string. Name of the deaths column.
#'   Default is `"D"`.
#' @param births_col Character string. Name of the births column.
#'   Default is `"B"`.
#'
#' @return A one-row data frame with species-level biomass summaries:
#'   \describe{
#'     \item{total_live_biomass}{Total live biomass across age classes.}
#'     \item{total_mortality_biomass}{Total mortality biomass across age classes.}
#'     \item{total_birth_biomass}{Total birth biomass across age classes.}
#'     \item{mean_live_body_mass}{Body mass averaged with live structure weights.}
#'     \item{mean_mortality_body_mass}{Body mass averaged with death weights.}
#'     \item{mean_birth_body_mass}{Body mass averaged with birth weights.}
#'     \item{age_of_max_live_biomass}{Age class contributing the highest live biomass.}
#'     \item{age_of_max_mortality_biomass}{Age class contributing the highest mortality biomass.}
#'   }
#'
#' @export
#'
#' @examples
#' biomass_profile <- data.frame(
#'   age = 0:3,
#'   R = c(1.00, 0.50, 0.25, 0.10),
#'   D = c(0.50, 0.25, 0.15, 0.10),
#'   B = c(0.00, 0.00, 0.20, 0.30),
#'   body_mass = c(10, 20, 40, 80),
#'   live_biomass = c(10, 10, 10, 8),
#'   mortality_biomass = c(5, 5, 6, 8),
#'   birth_biomass = c(0, 0, 8, 24)
#' )
#'
#' summarise_biomass_profile(biomass_profile)
summarise_biomass_profile <- function(biomass_profile,
                                      age_col = "age",
                                      mass_col = "body_mass",
                                      live_biomass_col = "live_biomass",
                                      mortality_biomass_col = "mortality_biomass",
                                      birth_biomass_col = "birth_biomass",
                                      structure_col = "R",
                                      deaths_col = "D",
                                      births_col = "B") {
  # Validate the biomass table and its required columns.
  if (!is.data.frame(biomass_profile)) {
    stop("`biomass_profile` must be a data frame.", call. = FALSE)
  }

  required_cols <- c(
    age_col, mass_col, live_biomass_col, mortality_biomass_col,
    birth_biomass_col, structure_col, deaths_col, births_col
  )

  missing_cols <- setdiff(required_cols, names(biomass_profile))

  if (length(missing_cols) > 0) {
    stop(
      "`biomass_profile` must contain these columns: ",
      paste(required_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Validate numeric columns used in totals and weighted means.
  numeric_cols <- c(
    mass_col, live_biomass_col, mortality_biomass_col,
    birth_biomass_col, structure_col, deaths_col, births_col
  )

  for (col in numeric_cols) {
    values <- biomass_profile[[col]]

    if (!is.numeric(values)) {
      stop("`", col, "` must be numeric.", call. = FALSE)
    }

    if (any(!is.finite(values))) {
      stop("`", col, "` must contain finite values.", call. = FALSE)
    }

    if (any(values < 0)) {
      stop("`", col, "` must contain non-negative values.", call. = FALSE)
    }
  }

  # Extract working vectors with user-selected column names.
  age <- biomass_profile[[age_col]]
  body_mass <- biomass_profile[[mass_col]]
  live_biomass <- biomass_profile[[live_biomass_col]]
  mortality_biomass <- biomass_profile[[mortality_biomass_col]]
  birth_biomass <- biomass_profile[[birth_biomass_col]]
  structure_values <- biomass_profile[[structure_col]]
  deaths_values <- biomass_profile[[deaths_col]]
  births_values <- biomass_profile[[births_col]]

  # Compute total biomass pools.
  total_live_biomass <- sum(live_biomass)
  total_mortality_biomass <- sum(mortality_biomass)
  total_birth_biomass <- sum(birth_biomass)

  # Compute demographic denominators for body-mass weighted means.
  total_structure <- sum(structure_values)
  total_deaths <- sum(deaths_values)
  total_births <- sum(births_values)

  # Estimate body mass experienced by live individuals, deaths, and births.
  mean_live_body_mass <- if (total_structure > 0) {
    sum(body_mass * structure_values) / total_structure
  } else {
    NA_real_
  }

  mean_mortality_body_mass <- if (total_deaths > 0) {
    sum(body_mass * deaths_values) / total_deaths
  } else {
    NA_real_
  }

  mean_birth_body_mass <- if (total_births > 0) {
    sum(body_mass * births_values) / total_births
  } else {
    NA_real_
  }

  # Identify the ages contributing the largest biomass pools.
  age_of_max_live_biomass <- age[which.max(live_biomass)]
  age_of_max_mortality_biomass <- age[which.max(mortality_biomass)]

  # Return a one-row species-level summary table.
  data.frame(
    total_live_biomass = total_live_biomass,
    total_mortality_biomass = total_mortality_biomass,
    total_birth_biomass = total_birth_biomass,
    mean_live_body_mass = mean_live_body_mass,
    mean_mortality_body_mass = mean_mortality_body_mass,
    mean_birth_body_mass = mean_birth_body_mass,
    age_of_max_live_biomass = age_of_max_live_biomass,
    age_of_max_mortality_biomass = age_of_max_mortality_biomass
  )
}
