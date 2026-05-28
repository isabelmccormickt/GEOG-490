
#-------------------------------------------------
# Lab 2 
# Isabel McCormick
# April 30, 2026
#-------------------------------------------------

# Libraries
library(tidyverse)
library(tidycensus)
library(dplyr)
library(ggplot2)
library(stringr)
library(scales)

####plot1
TotalPopulation_2020 <- read_csv("/Users/isabelmccormick/Documents/R_studio/Lab_2/nhgis0002_csv/nhgis0002_ds258_2020_state.csv")
ggplot() +
  geom_col(
    data = TotalPopulation_2020,
    aes(x = reorder(STATE, U7H001), y = U7H001),
    fill = "darkred"
  ) +
  coord_flip() +
  labs(
    x = "State",
    y = "Total Population",
    title = "Total Population by State"
  )

####plot2
TotalPopulation_2020_TC <- get_decennial(
  geography = "state",
  variables = "P1_001N",
  year = 2020,
  geometry = FALSE
)

ggplot() +
  geom_col(
    data = TotalPopulation_2020_TC,
    aes(x = reorder(NAME, value), y = value),
    fill = "darkred"
  ) +
  coord_flip() +
  labs(
    x = "State",
    y = "Total Population",
    title = "Total Population by State (Tidycensus)"
  )

#------------------------------------------------------------

# Submission
## Number of People above the Age of 25 with a Bachelor’s Degree
# Data Source: ACS 5-Year Estimates (2015–2019)

#------------------------------------------------------------

#--- Obtaining Data : Oregon Counties

edu <- get_acs(
  geography = "county",
  state = "OR",
  variables = "DP02_0068P",
  year = 2019,
  survey = "acs5",
  geometry = FALSE
)

edu_clean <- edu %>%
  select(NAME, estimate) %>%
  rename(bachelors_pct = estimate)

# 1. County with highest %
edu_clean %>% arrange(desc(bachelors_pct)) %>% slice(1)

#----Benton County, Oregon : 54.1%


# 2. County with the lowest %
edu_clean %>% arrange(bachelors_pct) %>% slice(1)

#----Morrow County, Oregon : 9%


# 3. Median Value
median(edu_clean$bachelors_pct, na.rm = TRUE)

#---- Median Value: 20.95


#-----------------------------------------

# 5. Margin of Error

#--- Obtaining Data : Poverty Rate Variable
income <- get_acs(
  geography = "county",
  state = "OR",
  variables = "S1701_C03_001",
  year = 2019,
  survey = "acs5"
)

## Margin of Error Code
ggplot(income, aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_point() +
  geom_errorbarh(aes(xmin = estimate - moe,
                     xmax = estimate + moe)) +
  labs(
    title = "Margin of Error: Poverty Rate",
    x = "Income ($)",
    y = "County"
  )


#-----------------------------------------

# 6. Population Pyramid

#--- Obtaining & Filtering Data : Oregon's Genders

oregon <- get_estimates(
  geography = "state",
  state = "OR",
  product = "characteristics",
  breakdown = c("SEX", "AGEGROUP"),
  breakdown_labels = TRUE,
  year = 2019
)

#### converting male population values to negative numbers
oregon_filtered <- oregon %>%
  filter(str_detect(AGEGROUP, "Age"),
         SEX != "Both sexes") %>%
  mutate(value = ifelse(SEX == "Male", -value, value))

#### graphing
ggplot(oregon_filtered, aes(x = value, y = AGEGROUP, fill = SEX)) +
  geom_col()

oregon_pyramid <- ggplot(oregon_filtered,
                         aes(x = value,
                             y = AGEGROUP,
                             fill = SEX)) +
  
  geom_col(width = 0.95, alpha = 0.75) +
  
  scale_fill_manual(values = c("#4575b4", "#d73027")) +
  
  scale_x_continuous(
    labels = function(x) paste0(abs(x)/1000, "k")
  ) +
  
  scale_y_discrete(labels = ~ str_remove_all(.x, "Age\\s|\\syears")) +
  
  theme_minimal(base_family = "Verdana", base_size = 12) +
  
  labs(
    x = "",
    y = "2019 Population Estimate",
    title = "Population Structure in Oregon",
    fill = "",
    caption = "Source: U.S. Census Bureau PEP"
  )
oregon_pyramid

### This analysis uses population estimates for Oregon to create a population pyramid showing the distribution of age groups by sex.  