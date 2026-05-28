#-------------------------------------------------

# Lab 3 - Mapping Census Data
# Name: Isabel McCormick
# Date: May 2, 2026

#-------------------------------------------------

# Libraries
library(tidycensus)
library(tidyverse)
library(tigris)
library(tmap)
library(tmaptools)
library(sf)

options(tigris_use_cache = TRUE)
tmap_mode("plot")

# Obtaining Data : Cook County, IL

###race data
cook_race <- get_acs(
  geography = "tract",
  variables = c(
    White = "B02001_002",
    Black = "B02001_003",
    Asian = "B02001_005"
  ),
  state = "IL",
  county = "Cook",
  year = 2019,
  survey = "acs5",
  geometry = TRUE
) %>%
  shift_geometry()

###age data
cook_young <- get_acs(
  geography = "tract",
  variables = c(
    male_under18 = "B01001_003",
    female_under18 = "B01001_027"
  ),
  state = "IL",
  county = "Cook",
  year = 2019,
  survey = "acs5",
  geometry = TRUE
) %>%
  shift_geometry()

###income data
cook_income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "IL",
  county = "Cook",
  year = 2019,
  survey = "acs5",
  geometry = TRUE
) %>%
  shift_geometry()

#------------------------------------------------------------

# Submission
## Different Maps Illistrating Race in Cook County IL
# Data Source: ACS 5-Year Estimates (2015–2019)

#------------------------------------------------------------

# 1 Dot Density Map

cook_dots <- cook_race %>%
  mutate(dot_count = round(estimate / 50)) %>%
  group_by(variable) %>%
  slice(rep(1:n(), dot_count)) %>%
  st_as_sf()

background_tracts <- cook_race

tm_shape(background_tracts) +
  tm_polygons(
    col = "white",
    border.col = "grey"
  ) +
  tm_shape(cook_dots) +
  tm_dots(
    col = "variable",
    palette = "Set1",
    size = 0.005,
    title = "1 dot = 100 people"
  ) +
  tm_layout(
    legend.outside = TRUE,
    title = "Race/Ethnicity – Cook County, IL"
  )

#-----------------------------------------

# 2 Graduated Symbol Map

##merge tables
cook_young_sum <- cook_young %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(young_pop = sum(estimate, na.rm = TRUE))

cook_young_map <- cook_young %>%
  select(GEOID, geometry) %>%
  distinct() %>%
  left_join(cook_young_sum, by = "GEOID")

##graph
tm_shape(cook_young_map) +
  tm_symbols(
    col = "young_pop",
    size = "young_pop",
    palette = "Blues",
    title.col = "Population (0–17)"
  ) +
  tm_layout(
    title = "Young Population – Cook County, IL\nACS 2015–2019 (B01001)",
    legend.outside = TRUE
  )

#-----------------------------------------

# 3 Choropleth Map

ggplot(cook_income) +
  geom_sf(aes(fill = estimate), color = NA) +
  scale_fill_viridis_c(option = "viridis") +
  labs(
    title = "Median Household Income in Cook County, IL",
    fill = "Income ($)"
  ) +
  theme_minimal()
