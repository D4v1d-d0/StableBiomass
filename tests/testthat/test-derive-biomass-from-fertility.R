test_that("derive_biomass_from_fertility links StablePopulation and StableBiomass", {
  fertility_rates <- c(0, 0, 0.30, 0.75, 0.60, 0.20)

  body_mass <- data.frame(
    age = 0:5,
    body_mass = c(5, 20, 45, 70, 80, 82)
  )

  result <- derive_biomass_from_fertility(
    fertility_rates = fertility_rates,
    beta = 1.2,
    body_mass = body_mass
  )

  expect_type(result, "list")
  expect_named(result, c("reconstruction", "demographic_profile", "biomass_profile"))

  expect_true(is.numeric(result$reconstruction$alpha))
  expect_equal(result$reconstruction$beta, 1.2)
  expect_equal(length(result$reconstruction$lx), length(fertility_rates))
  expect_equal(result$reconstruction$age, 0:5)

  expect_s3_class(result$demographic_profile$table, "data.frame")
  expect_s3_class(result$biomass_profile, "data.frame")
  expect_equal(nrow(result$biomass_profile), length(fertility_rates))

  expect_true(all(c(
    "age", "R", "D", "B", "body_mass",
    "live_biomass", "mortality_biomass", "birth_biomass"
  ) %in% names(result$biomass_profile)))

  expect_equal(result$biomass_profile$body_mass, body_mass$body_mass)
})

test_that("derive_biomass_from_fertility validates beta", {
  fertility_rates <- c(0, 0, 0.30, 0.75)

  body_mass <- data.frame(
    age = 0:3,
    body_mass = c(5, 20, 45, 70)
  )

  expect_error(
    derive_biomass_from_fertility(fertility_rates, beta = 0, body_mass = body_mass),
    "`beta` must be one positive finite numeric value"
  )
})

test_that("derive_biomass_from_fertility validates fertility rates", {
  body_mass <- data.frame(
    age = 0:3,
    body_mass = c(5, 20, 45, 70)
  )

  expect_error(
    derive_biomass_from_fertility(c(0, -1, 0.3, 0.7), beta = 1.2, body_mass = body_mass),
    "`fertility_rates` must contain non-negative values"
  )
})
