test_that("run_species_biomass_excel writes an output workbook", {
  input_file <- tempfile(fileext = ".xlsx")
  output_file <- tempfile(fileext = ".xlsx")

  input_data <- data.frame(
    age = 0:5,
    mx = c(0, 0, 0.30, 0.75, 0.60, 0.20),
    body_mass = c(5, 20, 45, 70, 80, 82),
    beta = c(1.2, NA, NA, NA, NA, NA)
  )

  openxlsx::write.xlsx(input_data, input_file, overwrite = TRUE)

  result <- run_species_biomass_excel(
    input_file = input_file,
    output_file = output_file
  )

  expect_true(file.exists(output_file))
  expect_equal(result$beta, 1.2)

  sheets <- openxlsx::getSheetNames(output_file)

  expect_true(all(c(
    "Input",
    "Demographic_Profile",
    "Biomass_Profile",
    "Biomass_Summary",
    "Metadata"
  ) %in% sheets))

  biomass_profile <- openxlsx::read.xlsx(output_file, sheet = "Biomass_Profile")

  expect_true(all(c(
    "age",
    "R",
    "D",
    "B",
    "body_mass",
    "live_biomass",
    "mortality_biomass",
    "birth_biomass"
  ) %in% names(biomass_profile)))
})

test_that("run_species_biomass_excel accepts age-class summaries", {
  input_file <- tempfile(fileext = ".xlsx")
  output_file <- tempfile(fileext = ".xlsx")

  input_data <- data.frame(
    age = 0:5,
    mx = c(0, 0, 0.30, 0.75, 0.60, 0.20),
    body_mass = c(5, 20, 45, 70, 80, 82),
    beta = c(1.2, NA, NA, NA, NA, NA)
  )

  age_classes <- data.frame(
    age_class = c("juvenile", "subadult", "adult"),
    min_age = c(0, 2, 4),
    max_age = c(1, 3, 5)
  )

  openxlsx::write.xlsx(input_data, input_file, overwrite = TRUE)

  run_species_biomass_excel(
    input_file = input_file,
    output_file = output_file,
    age_classes = age_classes
  )

  sheets <- openxlsx::getSheetNames(output_file)
  expect_true("Age_Class_Summary" %in% sheets)
})

test_that("run_species_biomass_excel validates unique beta", {
  input_file <- tempfile(fileext = ".xlsx")
  output_file <- tempfile(fileext = ".xlsx")

  input_data <- data.frame(
    age = 0:3,
    mx = c(0, 0, 0.30, 0.75),
    body_mass = c(5, 20, 45, 70),
    beta = c(1.2, 1.4, NA, NA)
  )

  openxlsx::write.xlsx(input_data, input_file, overwrite = TRUE)

  expect_error(
    run_species_biomass_excel(input_file, output_file),
    "one unique beta value"
  )
})
