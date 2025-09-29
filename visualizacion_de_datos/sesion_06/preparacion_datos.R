library(tidyverse)
library(jsonlite)

year_data <- 2020

## Emisiones de CO2 per cápita
co2 <- read_csv("https://ourworldindata.org/grapher/co-emissions-per-capita.csv?v=1&csvType=full&useColumnShortNames=true") |>
  janitor::clean_names()
names(co2)
co2 <- co2 |> 
  filter(year == year_data & !is.na(code)) |>
  rename(annual_co2_emissions_per_capita = emissions_total_per_capita) |>
  select(code, annual_co2_emissions_per_capita)
co2

## Continentes
continents <- read_csv("https://ourworldindata.org/grapher/continents-according-to-our-world-in-data.csv?v=1&csvType=full&useColumnShortNames=true") |>
  janitor::clean_names()
names(continents)
continents <- continents |>
  rename(continent = owid_region) |>
  select(-year)
data <- co2 |>
  left_join(continents, by = "code")

## GDP
gdp <- read_csv("https://ourworldindata.org/grapher/gdp-worldbank.csv?v=1&csvType=full&useColumnShortNames=true") |>
  janitor::clean_names()
names(gdp)
gdp <- gdp |>
  filter(year == year_data) |>
  rename(gdp = ny_gdp_mktp_pp_kd ) |>
  select(code, gdp)
data <- data |>
  left_join(gdp, by = "code")

## Gasto en salud y mortalidad infantil
health <- read_csv("https://ourworldindata.org/grapher/child-mortality-vs-health-expenditure.csv?v=1&csvType=full&useColumnShortNames=true") |>
  janitor::clean_names()
names(health)
health <- health |> 
  filter(year == year_data & !is.na(code)) |>
  rename(
    child_mortality = observation_value_indicator_child_mortality_rate_sex_total_wealth_quintile_total_unit_of_measure_deaths_per_100_live_births,
    health_spending_per_capita = sh_xpd_chex_pp_cd
  ) |>
  select(code, child_mortality, health_spending_per_capita)
data <- data |> 
  left_join(health, by = "code")

## Población
population <- read_csv("https://ourworldindata.org/grapher/population.csv?v=1&csvType=full&useColumnShortNames=true") |>
  janitor::clean_names()
names(population)
population <- population |> 
  filter(year == year_data) |>
  rename(
    population = population_historical
  ) |>
  select(code, population)
data <- data |> 
  left_join(population, by = "code")

## Esperanza de vida
life_exp <- read_csv("https://ourworldindata.org/grapher/life-expectancy.csv?v=1&csvType=full&useColumnShortNames=true") |>
  janitor::clean_names()
names(life_exp)
life_exp <- life_exp |>
  filter(year == year_data) |>
  rename(life_expectancy = life_expectancy_0) |>
  select(code, life_expectancy)

data <- data |>
  left_join(life_exp, by = "code")

## Residuos
wasteJSON <- fromJSON("https://datacatalogapi.worldbank.org/ddhxext/v3/resources/DR0049199/data?&top=300")

waste <- as_tibble(as.data.frame(wasteJSON)) |> 
  janitor::clean_names() |>
  rename_with(~ str_remove(.x, "^value_"))
names(waste)

waste <- waste |> 
  mutate(
    waste_per_capita_tons_year = as.numeric(total_msw_total_msw_generated_tons_year) / as.numeric(population_population_number_of_people)
  ) |>
  select(iso3c, waste_per_capita_tons_year, income_id) |>
  rename(
    code = iso3c,
    income = income_id
  )
waste

data <- data |>
  left_join(waste)

## Arreglar tabla de datos final
data <- data |>
  mutate(
    gdp_per_capita = gdp / population
  ) |>
  select(
    entity, 
    code, 
    continent, 
    population, 
    annual_co2_emissions_per_capita, 
    waste_per_capita_tons_year, 
    gdp_per_capita, 
    income,
    life_expectancy,
    child_mortality, 
    health_spending_per_capita) |>
  rename(
    country = entity
  ) |>
  filter(!is.na(country))

## Escribir archivo
write_csv(data, "misc_world_data_2020.csv")