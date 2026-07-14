#' Derive a Biomass Profile from a Stable Demographic Profile
#'
#' Combines an age-structured stable demographic profile with body mass
#' values by age to estimate live biomass, mortality biomass, and birth
#' biomass across age classes.
#'
#' @param demographic_profile A data frame, or a list containing a data frame
#'   named `table`. The demographic table must contain age, stable structure,
#'   deaths, and births columns. By default, these columns are `age`, `R`, `D`,
#'   and `B`.
#' @param body_mass A data frame containing age-specific body mass values.
#'   By default, it must contain columns named `age` and `body_mass`.
#' @param age_col Character string. Name of the age column shared by both
#'   input tables. Default is `"age"`.
#' @param structure_col Character string. Name of the stable structure column.
#'   Default is `"R"`.
#' @param deaths_col Character string. Name of the deaths column.
#'   Default is `"D"`.
#' @param births_col Character string. Name of the births column.
#'   Default is `"B"`.
#' @param mass_col Character string. Name of the body mass column.
#'   Default is `"body_mass"`.
#'
#' @return A data frame with age, demographic quantities, body mass, and
#'   derived biomass quantities:
#'   \describe{
#'     \item{live_biomass}{Stable structure multiplied by body mass.}
#'     \item{mortality_biomass}{Deaths multiplied by body mass.}
#'     \item{birth_biomass}{Births multiplied by body mass.}
#'     \item{relative_live_biomass}{Proportion of total live biomass in each age class.}
#'     \item{relative_mortality_biomass}{Proportion of total mortality biomass in each age class.}
#'   }
#'
#' @export
#'
#' @examples
#' demographic_profile <- data.frame(
#'   age = 0:4,
#'   R = c(1.00, 0.70, 0.45, 0.25, 0.10),
#'   D = c(0.30, 0.25, 0.20, 0.15, 0.10),
#'   B = c(0.00, 0.00, 0.20, 0.50, 0.30)
#' )
#'
#' body_mass <- data.frame(
#'   age = 0:4,
#'   body_mass = c(5, 20, 45, 70, 80)
#' )
#'
#' derive_biomass_profile(demographic_profile, body_mass)
derive_biomass_profile <- function(demographic_profile,
                                   body_mass,
                                   age_col = "age",
                                   structure_col = "R",
                                   deaths_col = "D",
                                   births_col = "B",
                                   mass_col = "body_mass") {
  # Accept the list structure returned by StablePopulation-style workflows.
  if (is.list(demographic_profile) &&
      !is.data.frame(demographic_profile) &&
      !is.null(demographic_profile$table)) {
    demographic_profile <- demographic_profile$table
  }

  # Validate input table types.
  if (!is.data.frame(demographic_profile)) {
    stop("`demographic_profile` must be a data frame or a list containing a data frame named `table`.", call. = FALSE)
  }

  if (!is.data.frame(body_mass)) {
    stop("`body_mass` must be a data frame.", call. = FALSE)
  }

  required_demo_cols <- c(age_col, structure_col, deaths_col, births_col)
  missing_demo_cols <- setdiff(required_demo_cols, names(demographic_profile))

  if (length(missing_demo_cols) > 0) {
    stop(
      "`demographic_profile` must contain these columns: ",
      paste(required_demo_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_demo_cols, collapse = ", "),
      call. = FALSE
    )
  }

  required_mass_cols <- c(age_col, mass_col)
  missing_mass_cols <- setdiff(required_mass_cols, names(body_mass))

  if (length(missing_mass_cols) > 0) {
    stop(
      "`body_mass` must contain these columns: ",
      paste(required_mass_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_mass_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Match body mass values to the age order of the demographic profile.
  match_index <- match(demographic_profile[[age_col]], body_mass[[age_col]])

  if (anyNA(match_index)) {
    missing_ages <- demographic_profile[[age_col]][is.na(match_index)]
    stop(
      "`body_mass` must provide body mass values for all demographic ages. Missing ages: ",
      paste(missing_ages, collapse = ", "),
      call. = FALSE
    )
  }

  structure_values <- demographic_profile[[structure_col]]
  deaths_values <- demographic_profile[[deaths_col]]
  births_values <- demographic_profile[[births_col]]
  mass_values <- body_mass[[mass_col]][match_index]

  numeric_inputs <- list(
    structure = structure_values,
    deaths = deaths_values,
    births = births_values,
    body_mass = mass_values
  )

  for (nm in names(numeric_inputs)) {
    values <- numeric_inputs[[nm]]

    if (!is.numeric(values)) {
      stop("`", nm, "` values must be numeric.", call. = FALSE)
    }

    if (any(!is.finite(values))) {
      stop("`", nm, "` values must be finite.", call. = FALSE)
    }

    if (any(values < 0)) {
      stop("`", nm, "` values must be non-negative.", call. = FALSE)
    }
  }

  live_biomass <- structure_values * mass_values
  mortality_biomass <- deaths_values * mass_values
  birth_biomass <- births_values * mass_values

  total_live_biomass <- sum(live_biomass)
  total_mortality_biomass <- sum(mortality_biomass)

  relative_live_biomass <- if (total_live_biomass > 0) {
    live_biomass / total_live_biomass
  } else {
    rep(NA_real_, length(live_biomass))
  }

  relative_mortality_biomass <- if (total_mortality_biomass > 0) {
    mortality_biomass / total_mortality_biomass
  } else {
    rep(NA_real_, length(mortality_biomass))
  }

  data.frame(
    age = demographic_profile[[age_col]],
    R = structure_values,
    D = deaths_values,
    B = births_values,
    body_mass = mass_values,
    live_biomass = live_biomass,
    mortality_biomass = mortality_biomass,
    birth_biomass = birth_biomass,
    relative_live_biomass = relative_live_biomass,
    relative_mortality_biomass = relative_mortality_biomass
  )
}
