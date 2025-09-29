library(tidyverse)
library(jsonlite)

start <- 1900
end <- 2023

## Emisiones de CO2 per cápita
co2 <- read_csv("https://ourworldindata.org/grapher/co-emissions-per-capita.csv?v=1&csvType=full&useColumnShortNames=true") |>
  janitor::clean_names()
names(co2)
co2 <- co2 |> 
  filter(year >= start & year <= end & !is.na(code)) |>
  rename(annual_co2_emissions_per_capita = emissions_total_per_capita) |>
  select(code, annual_co2_emissions_per_capita, year)
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
  filter(year >= start & year <= end & !is.na(code)) |>
  rename(gdp = ny_gdp_mktp_pp_kd ) |>
  select(code, gdp, year)
data <- data |>
  left_join(gdp, by = c("code", "year"))

## Gasto en salud y mortalidad infantil
health <- read_csv("https://ourworldindata.org/grapher/child-mortality-vs-health-expenditure.csv?v=1&csvType=full&useColumnShortNames=true") |>
  janitor::clean_names()
names(health)
health <- health |> 
  filter(year >= start & year <= end & !is.na(code)) |>
  rename(
    child_mortality = observation_value_indicator_child_mortality_rate_sex_total_wealth_quintile_total_unit_of_measure_deaths_per_100_live_births,
    health_spending_per_capita = sh_xpd_chex_pp_cd
  ) |>
  select(code, child_mortality, health_spending_per_capita, year)
data <- data |> 
  left_join(health, by = c("code", "year"))

## Población
population <- read_csv("https://ourworldindata.org/grapher/population.csv?v=1&csvType=full&useColumnShortNames=true") |>
  janitor::clean_names()
names(population)
population <- population |> 
  filter(year >= start & year <= end & !is.na(code)) |>
  rename(
    population = population_historical
  ) |>
  select(code, population, year)
data <- data |> 
  left_join(population, by = c("code", "year"))

## Esperanza de vida
life_exp <- read_csv("https://ourworldindata.org/grapher/life-expectancy.csv?v=1&csvType=full&useColumnShortNames=true") |>
  janitor::clean_names()
names(life_exp)
life_exp <- life_exp |>
  filter(year >= start & year <= end & !is.na(code)) |>
  rename(life_expectancy = life_expectancy_0) |>
  select(code, life_expectancy, year)

data <- data |>
  left_join(life_exp, by = c("code", "year"))

## Uso de fertilizantes
fertilizantes <- read_csv("https://ourworldindata.org/grapher/fertilizer-use-per-hectare-of-cropland.csv?v=1&csvType=full&useColumnShortNames=true") |>
  janitor::clean_names()
names(fertilizantes)
fertilizantes <- fertilizantes |>
  filter(year >= start & year <= end & !is.na(code)) |>
  rename(average_fertilizer_per_ha = all_fertilizers_per_cropland) |>
  select(code, average_fertilizer_per_ha, year)
data <- data |>
  left_join(fertilizantes, by = c("code", "year"))

## Ingreso del banco mundial
income <- read_csv("https://ourworldindata.org/grapher/world-bank-income-groups.csv?v=1&csvType=full&useColumnShortNames=true") |>
  janitor::clean_names()
names(income)
income <- income |>
  filter(year >= start & year <= end & !is.na(code)) |>
  rename(income = classification) |>
  select(code, income, year)
data <- data |>
  left_join(income, by = c("code", "year"))
  
## Arreglar tabla de datos final
data <- data |>
  mutate(
    gdp_per_capita = gdp / population
  ) |>
  select(
    entity, 
    code, 
    continent, 
    year,
    population, 
    annual_co2_emissions_per_capita, 
    average_fertilizer_per_ha,
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
write_csv(data, paste0("misc_world_data_", start ,"_", end, ".csv"))
