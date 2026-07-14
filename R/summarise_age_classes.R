#' Summarise Biomass and Demography by Age Class
#'
#' Groups an age-structured biomass profile into user-defined age classes
#' and computes demographic and biomass totals for each class.
#'
#' @param biomass_profile A data frame produced by
#'   [derive_biomass_profile()] or another table with equivalent columns.
#' @param age_classes A data frame defining age-class intervals. It must
#'   contain `age_class`, `min_age`, and `max_age` columns.
#' @param age_col Character string. Name of the age column.
#'   Default is `"age"`.
#' @param structure_col Character string. Name of the stable structure column.
#'   Default is `"R"`.
#' @param deaths_col Character string. Name of the deaths column.
#'   Default is `"D"`.
#' @param births_col Character string. Name of the births column.
#'   Default is `"B"`.
#' @param live_biomass_col Character string. Name of the live biomass column.
#'   Default is `"live_biomass"`.
#' @param mortality_biomass_col Character string. Name of the mortality
#'   biomass column. Default is `"mortality_biomass"`.
#' @param birth_biomass_col Character string. Name of the birth biomass
#'   column. Default is `"birth_biomass"`.
#'
#' @return A data frame with one row per age class and columns summarising
#'   demographic structure, deaths, births, live biomass, mortality biomass,
#'   birth biomass, and their relative contributions.
#'
#' @export
#'
#' @examples
#' biomass_profile <- data.frame(
#'   age = 0:5,
#'   R = c(1.00, 0.70, 0.45, 0.25, 0.12, 0.05),
#'   D = c(0.30, 0.25, 0.20, 0.13, 0.07, 0.05),
#'   B = c(0.00, 0.00, 0.20, 0.40, 0.30, 0.10),
#'   body_mass = c(5, 20, 45, 70, 80, 82),
#'   live_biomass = c(5, 14, 20.25, 17.5, 9.6, 4.1),
#'   mortality_biomass = c(1.5, 5, 9, 9.1, 5.6, 4.1),
#'   birth_biomass = c(0, 0, 9, 28, 24, 8.2)
#' )
#'
#' age_classes <- data.frame(
#'   age_class = c("juvenile", "subadult", "adult"),
#'   min_age = c(0, 2, 4),
#'   max_age = c(1, 3, 5)
#' )
#'
#' summarise_age_classes(biomass_profile, age_classes)
summarise_age_classes <- function(biomass_profile,
                                  age_classes,
                                  age_col = "age",
                                  structure_col = "R",
                                  deaths_col = "D",
                                  births_col = "B",
                                  live_biomass_col = "live_biomass",
                                  mortality_biomass_col = "mortality_biomass",
                                  birth_biomass_col = "birth_biomass") {
  if (!is.data.frame(biomass_profile)) {
    stop("`biomass_profile` must be a data frame.", call. = FALSE)
  }

  if (!is.data.frame(age_classes)) {
    stop("`age_classes` must be a data frame.", call. = FALSE)
  }

  required_profile_cols <- c(
    age_col, structure_col, deaths_col, births_col,
    live_biomass_col, mortality_biomass_col, birth_biomass_col
  )

  missing_profile_cols <- setdiff(required_profile_cols, names(biomass_profile))

  if (length(missing_profile_cols) > 0) {
    stop(
      "`biomass_profile` must contain these columns: ",
      paste(required_profile_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_profile_cols, collapse = ", "),
      call. = FALSE
    )
  }

  required_class_cols <- c("age_class", "min_age", "max_age")
  missing_class_cols <- setdiff(required_class_cols, names(age_classes))

  if (length(missing_class_cols) > 0) {
    stop(
      "`age_classes` must contain these columns: ",
      paste(required_class_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_class_cols, collapse = ", "),
      call. = FALSE
    )
  }

  numeric_profile_cols <- c(
    age_col, structure_col, deaths_col, births_col,
    live_biomass_col, mortality_biomass_col, birth_biomass_col
  )

  for (col in numeric_profile_cols) {
    values <- biomass_profile[[col]]

    if (!is.numeric(values)) {
      stop("`", col, "` must be numeric.", call. = FALSE)
    }

    if (any(!is.finite(values))) {
      stop("`", col, "` must contain finite values.", call. = FALSE)
    }
  }

  for (col in c("min_age", "max_age")) {
    values <- age_classes[[col]]

    if (!is.numeric(values)) {
      stop("`", col, "` in `age_classes` must be numeric.", call. = FALSE)
    }

    if (any(!is.finite(values))) {
      stop("`", col, "` in `age_classes` must contain finite values.", call. = FALSE)
    }
  }

  if (any(age_classes$min_age > age_classes$max_age)) {
    stop("Each age-class interval must have `min_age` less than or equal to `max_age`.", call. = FALSE)
  }

  ages <- biomass_profile[[age_col]]
  assigned_class <- rep(NA_character_, length(ages))

  for (i in seq_len(nrow(age_classes))) {
    in_class <- ages >= age_classes$min_age[i] & ages <= age_classes$max_age[i]

    if (any(in_class & !is.na(assigned_class))) {
      overlapping_ages <- ages[in_class & !is.na(assigned_class)]
      stop(
        "Age-class intervals assign some ages to more than one class: ",
        paste(unique(overlapping_ages), collapse = ", "),
        call. = FALSE
      )
    }

    assigned_class[in_class] <- as.character(age_classes$age_class[i])
  }

  if (anyNA(assigned_class)) {
    unassigned_ages <- ages[is.na(assigned_class)]
    stop(
      "Age-class intervals must assign every age in `biomass_profile`. Unassigned ages: ",
      paste(unique(unassigned_ages), collapse = ", "),
      call. = FALSE
    )
  }

  biomass_profile$.age_class <- assigned_class

  total_structure <- sum(biomass_profile[[structure_col]])
  total_deaths <- sum(biomass_profile[[deaths_col]])
  total_births <- sum(biomass_profile[[births_col]])
  total_live_biomass <- sum(biomass_profile[[live_biomass_col]])
  total_mortality_biomass <- sum(biomass_profile[[mortality_biomass_col]])
  total_birth_biomass <- sum(biomass_profile[[birth_biomass_col]])

  result <- lapply(seq_len(nrow(age_classes)), function(i) {
    class_name <- as.character(age_classes$age_class[i])
    rows <- biomass_profile$.age_class == class_name

    structure_sum <- sum(biomass_profile[[structure_col]][rows])
    deaths_sum <- sum(biomass_profile[[deaths_col]][rows])
    births_sum <- sum(biomass_profile[[births_col]][rows])
    live_biomass_sum <- sum(biomass_profile[[live_biomass_col]][rows])
    mortality_biomass_sum <- sum(biomass_profile[[mortality_biomass_col]][rows])
    birth_biomass_sum <- sum(biomass_profile[[birth_biomass_col]][rows])

    data.frame(
      age_class = class_name,
      min_age = age_classes$min_age[i],
      max_age = age_classes$max_age[i],
      structure = structure_sum,
      deaths = deaths_sum,
      births = births_sum,
      live_biomass = live_biomass_sum,
      mortality_biomass = mortality_biomass_sum,
      birth_biomass = birth_biomass_sum,
      relative_structure = if (total_structure > 0) structure_sum / total_structure else NA_real_,
      relative_deaths = if (total_deaths > 0) deaths_sum / total_deaths else NA_real_,
      relative_births = if (total_births > 0) births_sum / total_births else NA_real_,
      relative_live_biomass = if (total_live_biomass > 0) live_biomass_sum / total_live_biomass else NA_real_,
      relative_mortality_biomass = if (total_mortality_biomass > 0) mortality_biomass_sum / total_mortality_biomass else NA_real_,
      relative_birth_biomass = if (total_birth_biomass > 0) birth_biomass_sum / total_birth_biomass else NA_real_
    )
  })

  do.call(rbind, result)
}
