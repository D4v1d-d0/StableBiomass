test_that("derive_biomass_profile computes biomass quantities", {
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

  result <- derive_biomass_profile(demographic_profile, body_mass)

  expect_s3_class(result, "data.frame")
  expect_equal(result$live_biomass, c(10, 10, 10, 8))
  expect_equal(result$mortality_biomass, c(5, 5, 6, 8))
  expect_equal(result$birth_biomass, c(0, 0, 8, 24))
  expect_equal(sum(result$relative_live_biomass), 1)
  expect_equal(sum(result$relative_mortality_biomass), 1)
})

test_that("derive_biomass_profile preserves demographic age order", {
  demographic_profile <- data.frame(
    age = c(2, 0, 1),
    R = c(0.25, 1.00, 0.50),
    D = c(0.15, 0.50, 0.25),
    B = c(0.20, 0.00, 0.00)
  )

  body_mass <- data.frame(
    age = c(0, 1, 2),
    body_mass = c(10, 20, 40)
  )

  result <- derive_biomass_profile(demographic_profile, body_mass)

  expect_equal(result$age, c(2, 0, 1))
  expect_equal(result$body_mass, c(40, 10, 20))
})

test_that("derive_biomass_profile accepts list input with table element", {
  demographic_table <- data.frame(
    age = 0:2,
    R = c(1.00, 0.50, 0.25),
    D = c(0.50, 0.25, 0.15),
    B = c(0.00, 0.10, 0.20)
  )

  body_mass <- data.frame(
    age = 0:2,
    body_mass = c(10, 20, 40)
  )

  result <- derive_biomass_profile(
    demographic_profile = list(table = demographic_table),
    body_mass = body_mass
  )

  expect_equal(result$live_biomass, c(10, 10, 10))
})

test_that("derive_biomass_profile validates required body mass ages", {
  demographic_profile <- data.frame(
    age = 0:2,
    R = c(1.00, 0.50, 0.25),
    D = c(0.50, 0.25, 0.15),
    B = c(0.00, 0.10, 0.20)
  )

  body_mass <- data.frame(
    age = c(0, 1),
    body_mass = c(10, 20)
  )

  expect_error(
    derive_biomass_profile(demographic_profile, body_mass),
    "Missing ages: 2"
  )
})
