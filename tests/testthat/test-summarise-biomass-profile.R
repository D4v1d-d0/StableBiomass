test_that("summarise_biomass_profile computes species-level summaries", {
  biomass_profile <- data.frame(
    age = 0:3,
    R = c(1.00, 0.50, 0.25, 0.10),
    D = c(0.50, 0.25, 0.15, 0.10),
    B = c(0.00, 0.00, 0.20, 0.30),
    body_mass = c(10, 20, 40, 80),
    live_biomass = c(10, 10, 10, 8),
    mortality_biomass = c(5, 5, 6, 8),
    birth_biomass = c(0, 0, 8, 24)
  )

  result <- summarise_biomass_profile(biomass_profile)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$total_live_biomass, 38)
  expect_equal(result$total_mortality_biomass, 24)
  expect_equal(result$total_birth_biomass, 32)
  expect_equal(result$mean_live_body_mass, (10 * 1 + 20 * 0.5 + 40 * 0.25 + 80 * 0.1) / 1.85)
  expect_equal(result$mean_mortality_body_mass, (10 * 0.5 + 20 * 0.25 + 40 * 0.15 + 80 * 0.1) / 1.0)
  expect_equal(result$mean_birth_body_mass, (40 * 0.2 + 80 * 0.3) / 0.5)
  expect_equal(result$age_of_max_live_biomass, 0)
  expect_equal(result$age_of_max_mortality_biomass, 3)
})

test_that("summarise_biomass_profile works after derive_biomass_profile", {
  demographic_profile <- data.frame(
    age = 0:3,
    R = c(1.00, 0.50, 0.25, 0.10),
    D = c(0.50, 0.25, 0.15, 0.10),
    B = c(0.00, 0.00, 0.20, 0.30)
  )

  body_mass <- data.frame(
    age = 0:3,
    body_mass = c(10, 20, 40, 80)
  )

  biomass_profile <- derive_biomass_profile(demographic_profile, body_mass)
  result <- summarise_biomass_profile(biomass_profile)

  expect_equal(result$total_live_biomass, sum(biomass_profile$live_biomass))
  expect_equal(result$total_mortality_biomass, sum(biomass_profile$mortality_biomass))
  expect_equal(result$total_birth_biomass, sum(biomass_profile$birth_biomass))
})

test_that("summarise_biomass_profile validates required columns", {
  biomass_profile <- data.frame(
    age = 0:2,
    body_mass = c(10, 20, 30)
  )

  expect_error(
    summarise_biomass_profile(biomass_profile),
    "Missing:"
  )
})
