#' Classify Body Mass Values into Mass Classes
#'
#' Assigns body mass values to user-defined mass classes. This function
#' provides the categorical bridge between age-structured biomass profiles
#' and prey-size classes used in community or trophic analyses.
#'
#' Mass-class intervals are interpreted as left-closed and right-open:
#' `min_mass <= body_mass < max_mass`. The final class also includes its
#' upper boundary, and classes with `max_mass = Inf` are open-ended.
#'
#' @param data A data frame containing a body mass column.
#' @param mass_classes A data frame defining body-mass intervals. It must
#'   contain `mass_class`, `min_mass`, and `max_mass` columns.
#' @param mass_col Character string. Name of the body mass column in `data`.
#'   Default is `"body_mass"`.
#'
#' @return The input data frame with one additional column, `mass_class`,
#'   indicating the body-mass category assigned to each row.
#'
#' @export
#'
#' @examples
#' body_mass <- data.frame(
#'   age = 0:5,
#'   body_mass = c(5, 20, 25, 70, 100, 180)
#' )
#'
#' mass_classes <- data.frame(
#'   mass_class = c("small", "medium", "large"),
#'   min_mass = c(0, 25, 100),
#'   max_mass = c(25, 100, Inf)
#' )
#'
#' classify_body_mass(body_mass, mass_classes)
classify_body_mass <- function(data,
                               mass_classes,
                               mass_col = "body_mass") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (!is.data.frame(mass_classes)) {
    stop("`mass_classes` must be a data frame.", call. = FALSE)
  }

  if (!mass_col %in% names(data)) {
    stop("`data` must contain the body mass column: ", mass_col, call. = FALSE)
  }

  required_class_cols <- c("mass_class", "min_mass", "max_mass")
  missing_class_cols <- setdiff(required_class_cols, names(mass_classes))

  if (length(missing_class_cols) > 0) {
    stop(
      "`mass_classes` must contain these columns: ",
      paste(required_class_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_class_cols, collapse = ", "),
      call. = FALSE
    )
  }

  mass_values <- data[[mass_col]]

  if (!is.numeric(mass_values)) {
    stop("`", mass_col, "` must be numeric.", call. = FALSE)
  }

  if (any(!is.finite(mass_values))) {
    stop("`", mass_col, "` must contain finite values.", call. = FALSE)
  }

  if (any(mass_values < 0)) {
    stop("`", mass_col, "` must contain non-negative values.", call. = FALSE)
  }

  if (!is.character(mass_classes$mass_class)) {
    mass_classes$mass_class <- as.character(mass_classes$mass_class)
  }

  if (!is.numeric(mass_classes$min_mass)) {
    stop("`min_mass` in `mass_classes` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(mass_classes$max_mass)) {
    stop("`max_mass` in `mass_classes` must be numeric.", call. = FALSE)
  }

  if (any(!is.finite(mass_classes$min_mass))) {
    stop("`min_mass` in `mass_classes` must contain finite values.", call. = FALSE)
  }

  if (any(is.na(mass_classes$max_mass))) {
    stop("`max_mass` in `mass_classes` must contain defined values.", call. = FALSE)
  }

  if (any(mass_classes$min_mass < 0)) {
    stop("`min_mass` in `mass_classes` must contain non-negative values.", call. = FALSE)
  }

  if (any(mass_classes$min_mass >= mass_classes$max_mass)) {
    stop("Each mass-class interval must have `min_mass` lower than `max_mass`.", call. = FALSE)
  }

  assigned_class <- rep(NA_character_, length(mass_values))

  for (i in seq_len(nrow(mass_classes))) {
    lower <- mass_classes$min_mass[i]
    upper <- mass_classes$max_mass[i]
    is_final_class <- i == nrow(mass_classes)

    if (is.infinite(upper) || is_final_class) {
      in_class <- mass_values >= lower & mass_values <= upper
    } else {
      in_class <- mass_values >= lower & mass_values < upper
    }

    if (any(in_class & !is.na(assigned_class))) {
      overlapping_masses <- mass_values[in_class & !is.na(assigned_class)]
      stop(
        "Mass-class intervals assign some body masses to more than one class: ",
        paste(unique(overlapping_masses), collapse = ", "),
        call. = FALSE
      )
    }

    assigned_class[in_class] <- mass_classes$mass_class[i]
  }

  if (anyNA(assigned_class)) {
    unassigned_masses <- mass_values[is.na(assigned_class)]
    stop(
      "Mass-class intervals must assign every body mass in `data`. Unassigned body masses: ",
      paste(unique(unassigned_masses), collapse = ", "),
      call. = FALSE
    )
  }

  result <- data
  result$mass_class <- assigned_class
  result
}
