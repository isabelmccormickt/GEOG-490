#------------------------------------
# Replication Study: Charlotte, NC
# Isabel McCormick
## 15th May 2026
#-----------------------------------


#-----------------------------------
# Libraries

library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
library(tmap)
library(ggplot2)
#-----------------------------------


#-----------------------------------------
###-----Landscape Map-----####
#-----------------------------------------


# Obtain Data: Charlotte, NC+SC Census Data

### North Carolina
nc_tracts <- get_acs(
  geography = "tract",
  variables = "B01003_001",
  state = "NC",
  year = 2020,
  geometry = TRUE
)
### South Carolina
sc_tracts <- get_acs(
  geography = "tract",
  variables = "B01003_001",
  state = "SC",
  year = 2020,
  geometry = TRUE
)

all_tracts <- rbind(nc_tracts, sc_tracts)


# Defining Boundary

### all metro areas
cbsa <- core_based_statistical_areas(year = 2020)
### just charlotte's metro
charlotte <- cbsa %>%
  filter(str_detect(NAME, "Charlotte"))
#---------check
charlotte$NAME
### keeping only the census tracts that fall inside boundary
charlotte <- st_transform(charlotte, st_crs(all_tracts))
charlotte_tracts <- st_filter(all_tracts, charlotte)
#---------check
nrow(charlotte_tracts)


# Define Population Density

### project CRS
charlotte_tracts <- st_transform(charlotte_tracts, 32119)
### set area
charlotte_tracts <- charlotte_tracts %>%
  mutate(
    area_km2 = as.numeric(st_area(.) / 1000000),
    pop_density = estimate / area_km2
  )
### landscape categories
charlotte_tracts <- charlotte_tracts %>%
  mutate(
    landscape = case_when(
      pop_density < 100 ~ "Exurban Low",
      pop_density >= 100 & pop_density < 250 ~ "Exurban High",
      pop_density >= 250 & pop_density < 550 ~ "Suburban Low",
      pop_density >= 550 & pop_density < 800 ~ "Suburban High",
      pop_density >= 800 & pop_density < 1900 ~ "Urban Low",
      pop_density >= 1900 ~ "Urban High"
    )
  )


# Landscape Graph

landscape_map <- ggplot(charlotte_tracts) +
  geom_sf(aes(fill = landscape), color = NA) +
  labs(
    title = "Urban Landscapes of the Charlotte Metro Area",
    fill = "Landscape Type"
  ) +
  theme_minimal()

landscape_map


#---------------------------------------------
###-----ACS Variable Maps-----####
#---------------------------------------------


# Obtain Data: ACS Variables

### Poverty
poverty <- get_acs(
  geography = "tract",
  variables = "B17001_002",
  state = c("NC", "SC"),
  year = 2020,
  geometry = TRUE
)
### Median Household Income
income <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = c("NC", "SC"),
  year = 2020,
  geometry = TRUE
)


#Defining Boundary

income <- st_transform(income, st_crs(charlotte))
poverty <- st_transform(poverty, st_crs(charlotte))

income_charlotte <- st_filter(income, charlotte)
poverty_charlotte <- st_filter(poverty, charlotte)


#Spatial Join: Landscapes & ACS Variables

income_charlotte <- income_charlotte %>%
  left_join(
    st_drop_geometry(
      charlotte_tracts %>%
        select(GEOID, landscape)
    ),
    by = "GEOID"
  )
poverty_charlotte <- poverty_charlotte %>%
  left_join(
    st_drop_geometry(
      charlotte_tracts %>%
        select(GEOID, landscape)
    ),
    by = "GEOID"
  )


# Poverty Graph

poverty_graph <- ggplot(
  poverty_charlotte,
  aes(x = landscape, y = estimate)
) +
  geom_boxplot() +
  labs(
    title = "Population Below Poverty Line by Landscape Type",
    x = "Landscape",
    y = "Population Below Poverty Line"
  ) +
  theme_minimal()
poverty_graph


# Median Income Map

income_map <- ggplot(income_charlotte) +
  geom_sf(aes(fill = estimate), color = NA) +
  labs(
    title = "Median Household Income in the Charlotte Metro Area",
    fill = "Income ($)"
  ) +
  theme_minimal()
income_map


#---------------------------------------------
###-----Population Pyramids-----####
#---------------------------------------------

# Obtaining Data: Population Characteristics
charlotte_genders <- get_estimates(
  geography = "cbsa",
  cbsa = "16740",   # Charlotte-Concord-Gastonia CBSA
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2020
)

### filter data to sex data
charlotte_filtered <- charlotte_genders %>%
  filter(
    str_detect(AGEGROUP, "Age"),
    SEX != "Both sexes"
  ) %>%
  mutate(
    value = ifelse(SEX == "Male", -value, value)
  )

# Graph Population Pyramid
ggplot(charlotte_filtered,
       aes(x = value, y = AGEGROUP, fill = SEX)) +
  
  geom_col() +
  
  scale_x_continuous(
    labels = function(x) abs(x)
  ) +
  
  scale_fill_manual(values = c(
    "Male" = "#4F81BD",
    "Female" = "#C0504D"
  )) +
  
  labs(
    title = "Charlotte Metro Population Pyramid (2020)",
    x = "Population",
    y = "Age Group",
    fill = "Sex"
  ) +
  
  theme_minimal()


