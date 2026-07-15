
<!-- README.md is generated from README.Rmd. Please edit that file -->

# StableBiomass

<!-- badges: start -->

<!-- badges: end -->

`StableBiomass` estimates live biomass, mortality biomass, birth
biomass, and prey-size categories from age-structured stable demographic
profiles. It is designed as an ecological and paleobiological extension
of [`StablePopulation`](https://github.com/D4v1d-d0/StablePopulation).

The basic idea is:

``` text
fertility rates + Weibull beta + body mass by age
  -> stable survivorship and demographic profile
  -> live biomass by age
  -> mortality biomass by age
  -> birth biomass by age
  -> summaries by species, age class, and body-mass class
```

## Installation

You can install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("D4v1d-d0/StableBiomass")
```

`StableBiomass` imports `StablePopulation`, which provides the stable
population reconstruction functions used by the biomass workflow.

## Main workflow

The species-level workflow starts with fertility rates, a fixed Weibull
shape parameter, and a body-mass table by age.

``` r
library(StableBiomass)

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

biomass_profile <- result$biomass_profile
biomass_profile
```

The main biomass quantities are:

``` text
live_biomass      = R * body_mass
mortality_biomass = D * body_mass
birth_biomass     = B * body_mass
```

where `R` is the stable structure, `D` is the deaths profile, and `B` is
the reproductive contribution by age.

## Species summaries

``` r
biomass_summary <- summarise_biomass_profile(biomass_profile)
biomass_summary
```

Age classes can be defined by the user:

``` r
age_classes <- data.frame(
  age_class = c("juvenile", "subadult", "adult"),
  min_age = c(0, 2, 4),
  max_age = c(1, 3, 5)
)

summarise_age_classes(biomass_profile, age_classes)
```

Body-mass classes prepare the output for community and trophic analyses:

``` r
mass_classes <- data.frame(
  mass_class = c("small", "medium", "large"),
  min_mass = c(0, 25, 75),
  max_mass = c(25, 75, Inf)
)

classify_body_mass(biomass_profile, mass_classes)
```

## Excel workflow

`run_species_biomass_excel()` runs the species-level workflow from an
Excel file. The input file contains one sheet with these columns:

``` text
age | mx | body_mass | beta
```

The `beta` column can contain a single numeric value in the first row
and empty cells below it:

``` text
age | mx   | body_mass | beta
0   | 0.00 | 5         | 1.2
1   | 0.00 | 20        |
2   | 0.30 | 45        |
3   | 0.75 | 70        |
4   | 0.60 | 80        |
5   | 0.20 | 82        |
```

``` r
run_species_biomass_excel(
  input_file = "species_input.xlsx",
  output_file = "species_biomass_output.xlsx"
)
```

The output workbook contains:

- `Input`
- `Demographic_Profile`
- `Biomass_Profile`
- `Biomass_Summary`
- `Age_Class_Summary`, when age classes are supplied
- `Metadata`

## Current scope

The current development version focuses on the species-level block:

``` text
stable demography -> biomass by age -> species summaries -> class summaries
```

The next development block will extend this foundation toward community
biomass, prey availability, and predator allocation workflows.
