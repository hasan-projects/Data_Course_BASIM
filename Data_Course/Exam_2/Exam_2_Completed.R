#### Loading the libraries needed for this assignment ####
library(tidyverse)
library(broom)

#### Reading in the UNICEF data ####
unicef <- read_csv("unicef-u5mr.csv")

#### Turning the data into tidy format ####
unicef_tidy <- unicef %>%
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to = "year",
    values_to = "u5mr"
  ) %>%
  mutate(
    country = as.factor(CountryName),
    continent = as.factor(Continent),
    year = as.numeric(gsub("U5MR.", "", year))
  ) %>%
  select(country, continent, year, u5mr)

#### Plotting each country’s U5MR over time ####
plot1 <- unicef_tidy %>%
  ggplot(aes(x = year, y = u5mr, group = country, color = continent)) +
  geom_line(alpha = 0.7) +
  labs(
    title = "Under-5 Mortality Rate Over Time by Country",
    x = "Year",
    y = "U5MR (deaths per 1000 live births)"
  ) +
  theme_minimal()

# Saving the first plot using the required filename structure
ggsave("Basim_Plot_1.png", plot1, width = 8, height = 5, dpi = 300)

#### Calculating mean U5MR for each continent ####
continent_means <- unicef_tidy %>%
  group_by(continent, year) %>%
  summarize(mean_u5mr = mean(u5mr, na.rm = TRUE), .groups = "drop")

# Plotting the average U5MR trend for each continent
plot2 <- continent_means %>%
  ggplot(aes(x = year, y = mean_u5mr, color = continent)) +
  geom_line(size = 1.2) +
  labs(
    title = "Mean U5MR per Continent Over Time",
    x = "Year",
    y = "Mean U5MR"
  ) +
  theme_minimal()

# Saving the second required plot
ggsave("Basim_Plot_2.png", plot2, width = 8, height = 5, dpi = 300)

#### Building three different models of U5MR ####
# Model 1: U5MR predicted by year only
model1 <- lm(u5mr ~ year, data = unicef_tidy)

# Model 2: U5MR predicted by year and continent
model2 <- lm(u5mr ~ year + continent, data = unicef_tidy)

# Model 3: U5MR predicted by the interaction between year and continent
model3 <- lm(u5mr ~ year * continent, data = unicef_tidy)

#### Comparing the three models ####
# Comparing models using AIC, BIC, and R-squared to evaluate performance
model_performance <- tibble(
  model = c("Model 1: year",
            "Model 2: year + continent",
            "Model 3: year * continent"),
  AIC = c(AIC(model1), AIC(model2), AIC(model3)),
  BIC = c(BIC(model1), BIC(model2), BIC(model3)),
  R2  = c(summary(model1)$r.squared,
          summary(model2)$r.squared,
          summary(model3)$r.squared)
)

# Displaying model comparison table
print(model_performance)

#### Plotting predictions from all three models ####
# Creating a grid of all combinations of years and continents for predictions
prediction_grid <- unicef_tidy %>%
  distinct(continent, year) %>%
  complete(continent,
           year = seq(min(unicef_tidy$year),
                      max(unicef_tidy$year)))

# Adding model predictions to the prediction grid
prediction_grid <- prediction_grid %>%
  mutate(
    model1 = predict(model1, newdata = prediction_grid),
    model2 = predict(model2, newdata = prediction_grid),
    model3 = predict(model3, newdata = prediction_grid)
  )

# Reshaping prediction data so it can be plotted nicely
prediction_long <- prediction_grid %>%
  pivot_longer(
    cols = starts_with("model"),
    names_to = "model",
    values_to = "pred_u5mr"
  )

# Plotting predicted U5MR trends for each model by continent
plot3 <- prediction_long %>%
  ggplot(aes(x = year, y = pred_u5mr, color = model)) +
  geom_line(size = 1.1) +
  facet_wrap(~ continent) +
  labs(
    title = "Predicted U5MR Over Time for Each Model",
    x = "Year",
    y = "Predicted U5MR"
  ) +
  theme_minimal()

# Saving the optional prediction plot
ggsave("Basim_Model_Predictions.png", plot3, width = 10, height = 6, dpi = 300)

#### BONUS: Predicting Ecuador’s U5MR in 2020 ####
# Finding the continent associated with Ecuador in the dataset
ecuador_continent <- unicef_tidy %>%
  filter(country == "Ecuador") %>%
  pull(continent) %>%
  unique() %>%
  .[1]

# Creating a small data frame for Ecuador in the year 2020
ecuador_2020 <- tibble(
  country = "Ecuador",
  continent = ecuador_continent,
  year = 2020
)

# Getting the model’s predicted U5MR value for Ecuador in 2020
ecuador_pred <- predict(model3, newdata = ecuador_2020)

# Real U5MR value provided by the assignment
real_value <- 13

# Printing the predicted value, the real value, and the difference
cat("Predicted U5MR for Ecuador in 2020 (Model 3):",
    round(ecuador_pred, 1), "\n")
cat("Real U5MR for Ecuador in 2020:",
    real_value, "\n")
cat("Difference (prediction - real):",
    round(ecuador_pred - real_value, 1), "\n")
