test_that("classify_body_mass assigns mass classes", {
  body_mass <- data.frame(
    age = 0:5,
    body_mass = c(5, 20, 45, 70, 120, 180)
  )

  mass_classes <- data.frame(
    mass_class = c("small", "medium", "large"),
    min_mass = c(0, 25, 100),
    max_mass = c(24.999, 99.999, Inf)
  )

  result <- classify_body_mass(body_mass, mass_classes)

  expect_s3_class(result, "data.frame")
  expect_equal(result$mass_class, c("small", "small", "medium", "medium", "large", "large"))
  expect_equal(result$body_mass, body_mass$body_mass)
})

test_that("classify_body_mass works with biomass profiles", {
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

  mass_classes <- data.frame(
    mass_class = c("small", "medium"),
    min_mass = c(0, 25),
    max_mass = c(24.999, Inf)
  )

  biomass_profile <- derive_biomass_profile(demographic_profile, body_mass)
  result <- classify_body_mass(biomass_profile, mass_classes)

  expect_equal(result$mass_class, c("small", "small", "medium", "medium"))
  expect_true(all(c("live_biomass", "mortality_biomass", "birth_biomass") %in% names(result)))
})

test_that("classify_body_mass validates complete mass-class assignment", {
  body_mass <- data.frame(
    age = 0:2,
    body_mass = c(10, 40, 120)
  )

  mass_classes <- data.frame(
    mass_class = c("small", "medium"),
    min_mass = c(0, 25),
    max_mass = c(24.999, 99.999)
  )

  expect_error(
    classify_body_mass(body_mass, mass_classes),
    "Unassigned body masses: 120"
  )
})

test_that("classify_body_mass validates overlapping mass classes", {
  body_mass <- data.frame(
    age = 0:2,
    body_mass = c(10, 25, 40)
  )

  mass_classes <- data.frame(
    mass_class = c("small", "medium"),
    min_mass = c(0, 25),
    max_mass = c(25, 100)
  )

  expect_error(
    classify_body_mass(body_mass, mass_classes),
    "more than one class"
  )
})
