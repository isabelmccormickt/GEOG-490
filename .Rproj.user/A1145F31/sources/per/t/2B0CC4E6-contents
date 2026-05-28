
#------------------------------------
# Lab 8 
# Isabel McCormick
## 27th May 2026
#-----------------------------------


# Libraries
library(tidycensus)
library(sf)
library(tidyverse)
library(patchwork)
library(viridis)

# Obtaining Data: Portland Counties
portland_counties <- c(
  "Multnomah",
  "Washington",
  "Clackamas"
)

# Obtaining Data: Portland Variables 
variables_to_get <- c(
  median_value = "B25077_001",
  median_rooms = "B25018_001",
  median_income = "DP03_0062",
  total_population = "B01003_001",
  median_age = "B01002_001",
  pct_college = "DP02_0068P",
  pct_foreign_born = "DP02_0094P",
  pct_white = "DP05_0077P",
  median_year_built = "B25037_001",
  percent_ooh = "DP04_0046P"
)

###combing data
portland_data <- get_acs(
  geography = "tract",
  variables = variables_to_get,
  state = "OR",
  county = portland_counties,
  geometry = TRUE,
  output = "wide",
  year = 2020
) %>%
  select(-NAME) %>%
  st_transform(2992)

# Regression model
mhv_map <- ggplot(portland_data, aes(fill = median_valueE)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(labels = scales::label_dollar()) +
  theme_void() +
  labs(fill = "Median home value")


# Assumption testing
model1 <- lm(
  log(median_valueE) ~ median_incomeE +
    pct_collegeE +
    median_roomsE +
    median_ageE,
  data = portland_data
)
summary(model1)
plot(model1)

#-----------Submission-----------

# 1 

# Obtaining Data: Median Income
portland_data <- portland_data %>%
  mutate(log_income = log(median_incomeE))

# Income Map
income_map <- ggplot(portland_data, aes(fill = log_income)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "magma") +
  theme_void() +
  labs(
    title = "Logged Median Household Income",
    fill = "Log income"
  )

# Income History
income_hist <- ggplot(portland_data, aes(x = log_income)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(
    title = "Distribution of Logged Median Household Income",
    x = "Logged median household income",
    y = "Number of census tracts"
  )

### combind
income_map + income_hist



# 2

# Prepare data for PCA
portland_data_for_model <- portland_data %>%
  mutate(
    pop_density = total_populationE / as.numeric(st_area(.)),
    median_structure_age = 2020 - median_year_builtE
  ) %>%
  rename(
    median_value = median_valueE,
    median_rooms = median_roomsE,
    median_income = median_incomeE,
    total_population = total_populationE,
    median_age = median_ageE,
    pct_college = pct_collegeE,
    pct_foreign_born = pct_foreign_bornE,
    pct_white = pct_whiteE,
    percent_ooh = percent_oohE
  ) %>%
  select(
    GEOID,
    median_value,
    median_rooms,
    total_population,
    median_age,
    median_income,
    pct_college,
    pct_foreign_born,
    pct_white,
    percent_ooh,
    pop_density,
    median_structure_age,
    geometry
  ) %>%
  drop_na()

###isolate Predictor variables for PCA
portland_estimates <- portland_data_for_model %>%
  st_drop_geometry() %>%
  select(
    median_rooms,
    total_population,
    median_age,
    median_income,
    pct_college,
    pct_foreign_born,
    pct_white,
    percent_ooh,
    pop_density,
    median_structure_age
  )

# Run PCA
pca <- prcomp(
  formula = ~ .,
  data = portland_estimates,
  scale. = TRUE,
  center = TRUE
)

summary(pca)

###create loading tibble
pca_tibble <- pca$rotation %>%
  as_tibble(rownames = "predictor")

# Plot first five principal components
pca_plot <- pca_tibble %>%
  select(predictor:PC5) %>%
  pivot_longer(
    PC1:PC5,
    names_to = "component",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value, y = predictor)) +
  geom_col(fill = "darkgreen", color = "darkgreen", alpha = 0.5) +
  facet_wrap(~ component, nrow = 1) +
  labs(
    title = "Loadings for First Five Principal Components",
    x = "Value",
    y = NULL
  ) +
  theme_minimal()

### combind PCA to spatial data
components <- predict(pca, portland_estimates)

portland_pca <- portland_data_for_model %>%
  select(GEOID, median_value, geometry) %>%
  cbind(components)

# Map PC1
pc1_map <- ggplot(portland_pca, aes(fill = PC1)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c() +
  theme_void() +
  labs(
    title = "Map of Principal Component 1",
    fill = "PC1"
  )
pca_plot / pc1_map

