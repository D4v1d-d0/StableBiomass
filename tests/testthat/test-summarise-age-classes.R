test_that("summarise_age_classes computes class-level summaries", {
  biomass_profile <- data.frame(
    age = 0:5,
    R = c(1.00, 0.70, 0.45, 0.25, 0.12, 0.05),
    D = c(0.30, 0.25, 0.20, 0.13, 0.07, 0.05),
    B = c(0.00, 0.00, 0.20, 0.40, 0.30, 0.10),
    body_mass = c(5, 20, 45, 70, 80, 82),
    live_biomass = c(5, 14, 20.25, 17.5, 9.6, 4.1),
    mortality_biomass = c(1.5, 5, 9, 9.1, 5.6, 4.1),
    birth_biomass = c(0, 0, 9, 28, 24, 8.2)
  )

  age_classes <- data.frame(
    age_class = c("juvenile", "subadult", "adult"),
    min_age = c(0, 2, 4),
    max_age = c(1, 3, 5)
  )

  result <- summarise_age_classes(biomass_profile, age_classes)

  expect_s3_class(result, "data.frame")
  expect_equal(result$age_class, c("juvenile", "subadult", "adult"))
  expect_equal(result$structure, c(1.70, 0.70, 0.17))
  expect_equal(result$deaths, c(0.55, 0.33, 0.12))
  expect_equal(result$births, c(0.00, 0.60, 0.40))
  expect_equal(result$live_biomass, c(19, 37.75, 13.7))
  expect_equal(result$mortality_biomass, c(6.5, 18.1, 9.7))
  expect_equal(result$birth_biomass, c(0, 37, 32.2))
  expect_equal(sum(result$relative_structure), 1)
  expect_equal(sum(result$relative_deaths), 1)
  expect_equal(sum(result$relative_births), 1)
  expect_equal(sum(result$relative_live_biomass), 1)
  expect_equal(sum(result$relative_mortality_biomass), 1)
  expect_equal(sum(result$relative_birth_biomass), 1)
})

test_that("summarise_age_classes works after derive_biomass_profile", {
  demographic_profile <- data.frame(
    age = 0:5,
    R = c(1.00, 0.70, 0.45, 0.25, 0.12, 0.05),
    D = c(0.30, 0.25, 0.20, 0.13, 0.07, 0.05),
    B = c(0.00, 0.00, 0.20, 0.40, 0.30, 0.10)
  )

  body_mass <- data.frame(
    age = 0:5,
    body_mass = c(5, 20, 45, 70, 80, 82)
  )

  age_classes <- data.frame(
    age_class = c("juvenile", "subadult", "adult"),
    min_age = c(0, 2, 4),
    max_age = c(1, 3, 5)
  )

  biomass_profile <- derive_biomass_profile(demographic_profile, body_mass)
  result <- summarise_age_classes(biomass_profile, age_classes)

  expect_equal(sum(result$live_biomass), sum(biomass_profile$live_biomass))
  expect_equal(sum(result$mortality_biomass), sum(biomass_profile$mortality_biomass))
  expect_equal(sum(result$birth_biomass), sum(biomass_profile$birth_biomass))
})

test_that("summarise_age_classes validates complete age-class assignment", {
  biomass_profile <- data.frame(
    age = 0:2,
    R = c(1, 0.5, 0.25),
    D = c(0.5, 0.25, 0.15),
    B = c(0, 0.1, 0.2),
    live_biomass = c(10, 10, 10),
    mortality_biomass = c(5, 5, 6),
    birth_biomass = c(0, 2, 8)
  )

  age_classes <- data.frame(
    age_class = "juvenile",
    min_age = 0,
    max_age = 1
  )

  expect_error(
    summarise_age_classes(biomass_profile, age_classes),
    "Unassigned ages: 2"
  )
})

test_that("summarise_age_classes validates overlapping age classes", {
  biomass_profile <- data.frame(
    age = 0:2,
    R = c(1, 0.5, 0.25),
    D = c(0.5, 0.25, 0.15),
    B = c(0, 0.1, 0.2),
    live_biomass = c(10, 10, 10),
    mortality_biomass = c(5, 5, 6),
    birth_biomass = c(0, 2, 8)
  )

  age_classes <- data.frame(
    age_class = c("juvenile", "subadult"),
    min_age = c(0, 1),
    max_age = c(1, 2)
  )

  expect_error(
    summarise_age_classes(biomass_profile, age_classes),
    "more than one class"
  )
})
