#' Derive Biomass Directly from Fertility Rates
#'
#' Reconstructs a stable survivorship profile from age-specific fertility
#' rates using exported functions from \pkg{StablePopulation}, then
#' derives a demographic table and combines it with age-specific body mass
#' values using \code{derive_biomass_profile()}.
#'
#' @param fertility_rates A numeric vector of age-specific fertility values
#'   \eqn{m_x}.
#' @param beta A positive numeric value. Weibull shape parameter.
#' @param body_mass A data frame containing age-specific body mass values.
#'   By default, it must contain columns named \code{age} and
#'   \code{body_mass}.
#' @param age_col Character string. Name of the age column in
#'   \code{body_mass}. Default is \code{"age"}.
#' @param mass_col Character string. Name of the body mass column.
#'   Default is \code{"body_mass"}.
#'
#' @return A list with three elements:
#'   \describe{
#'     \item{reconstruction}{A list containing the reconstructed
#'       Weibull parameters, survivorship profile, ages, and reproductive
#'       output.}
#'     \item{demographic_profile}{A list containing the demographic table
#'       derived from the reconstructed survivorship profile.}
#'     \item{biomass_profile}{A data frame returned by
#'       \code{derive_biomass_profile()}.}
#'   }
#'
#' @export
#'
#' @examples
#' fertility_rates <- c(0, 0, 0.30, 0.75, 0.60, 0.20)
#'
#' body_mass <- data.frame(
#'   age = 0:5,
#'   body_mass = c(5, 20, 45, 70, 80, 82)
#' )
#'
#' result <- derive_biomass_from_fertility(
#'   fertility_rates = fertility_rates,
#'   beta = 1.2,
#'   body_mass = body_mass
#' )
#'
#' result$biomass_profile
derive_biomass_from_fertility <- function(fertility_rates,
                                          beta,
                                          body_mass,
                                          age_col = "age",
                                          mass_col = "body_mass") {
  if (!is.numeric(fertility_rates)) {
    stop("`fertility_rates` must be numeric.", call. = FALSE)
  }

  if (any(!is.finite(fertility_rates))) {
    stop("`fertility_rates` must contain finite values.", call. = FALSE)
  }

  if (any(fertility_rates < 0)) {
    stop("`fertility_rates` must contain non-negative values.", call. = FALSE)
  }

  if (!is.numeric(beta) || length(beta) != 1L || !is.finite(beta) || beta <= 0) {
    stop("`beta` must be one positive finite numeric value.", call. = FALSE)
  }

  ages <- seq_along(fertility_rates) - 1L

  alpha <- StablePopulation::find_alphas(
    beta = beta,
    fertility_rates = fertility_rates
  )

  lx <- StablePopulation::weibull_survival(
    alpha = alpha,
    beta = beta,
    age = ages
  )

  reproductive_output <- sum(lx * fertility_rates)

  deaths <- c(
    lx[-length(lx)] - lx[-1],
    lx[length(lx)]
  )

  deaths[deaths < 0 & abs(deaths) < sqrt(.Machine$double.eps)] <- 0

  stable_structure <- lx / sum(lx)
  relative_deaths <- deaths / sum(deaths)
  births <- lx * fertility_rates

  demographic_table <- data.frame(
    age = ages,
    lx = lx,
    mx = fertility_rates,
    R = stable_structure,
    D = deaths,
    D_relative = relative_deaths,
    B = births
  )

  biomass_profile <- derive_biomass_profile(
    demographic_profile = demographic_table,
    body_mass = body_mass,
    age_col = age_col,
    mass_col = mass_col
  )

  list(
    reconstruction = list(
      alpha = alpha,
      beta = beta,
      age = ages,
      lx = lx,
      reproductive_output = reproductive_output
    ),
    demographic_profile = list(
      table = demographic_table
    ),
    biomass_profile = biomass_profile
  )
}
