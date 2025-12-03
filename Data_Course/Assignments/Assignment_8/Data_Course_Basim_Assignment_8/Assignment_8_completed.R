#### Load Libraries ####
library(tidyverse)
library(modelr)
library(broom)

#### Load Mushroom Data ####
mush <- read_csv("Data/mushroom_growth.csv")

# Look at the data so I know what's inside.
glimpse(mush)

#### Clean Data ####
# Make GrowthRate numeric and turn character columns into factors.
mush <- mush %>%
  mutate(
    GrowthRate = as.numeric(GrowthRate),
    Species = as.factor(Species),
    Humidity = as.factor(Humidity)
  )

#### Explore Data With Plots ####

# GrowthRate vs Nitrogen
ggplot(mush, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# GrowthRate vs Light
ggplot(mush, aes(x = Light, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

# GrowthRate vs Species
ggplot(mush, aes(x = Species, y = GrowthRate)) +
  geom_boxplot() +
  theme_minimal()

# GrowthRate vs Humidity
ggplot(mush, aes(x = Humidity, y = GrowthRate)) +
  geom_boxplot() +
  theme_minimal()

# Nitrogen vs GrowthRate colored by Species
ggplot(mush, aes(x = Nitrogen, y = GrowthRate, color = Species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

#### Build Models ####

# Model 1: only Nitrogen
mod1 <- lm(GrowthRate ~ Nitrogen, data = mush)

# Model 2: Nitrogen + Light
mod2 <- lm(GrowthRate ~ Nitrogen + Light, data = mush)

# Model 3: Nitrogen + Species
mod3 <- lm(GrowthRate ~ Nitrogen + Species, data = mush)

# Model 4: Nitrogen * Species + Light + Humidity
mod4 <- lm(GrowthRate ~ Nitrogen * Species + Light + Humidity, data = mush)

#### Calculate Mean Squared Error ####
# Lower MSE means the model fits better.

mse1 <- mean(residuals(mod1)^2)
mse2 <- mean(residuals(mod2)^2)
mse3 <- mean(residuals(mod3)^2)
mse4 <- mean(residuals(mod4)^2)

mse_results <- tibble(
  model = c("mod1", "mod2", "mod3", "mod4"),
  mse   = c(mse1, mse2, mse3, mse4)
)

mse_results

#### Pick Best Model ####
# I choose the model with the lowest MSE.
best_model_name <- mse_results$model[which.min(mse_results$mse)]
best_model_name

if (best_model_name == "mod1") {
  best_mod <- mod1
} else if (best_model_name == "mod2") {
  best_mod <- mod2
} else if (best_model_name == "mod3") {
  best_mod <- mod3
} else {
  best_mod <- mod4
}

# Look at a summary of the best model.
summary(best_mod)

#### Add Predictions to Real Data ####
mush_pred <- mush %>%
  add_predictions(best_mod)

head(mush_pred)

# Plot actual vs predicted values using Nitrogen on x-axis.
ggplot(mush_pred, aes(x = Nitrogen)) +
  geom_point(aes(y = GrowthRate), color = "black") +
  geom_point(aes(y = pred), color = "red") +
  theme_minimal() +
  labs(y = "GrowthRate (Observed and Predicted)")

#### Make Hypothetical Predictions ####

hypo <- expand.grid(
  Nitrogen = seq(min(mush$Nitrogen), max(mush$Nitrogen), length.out = 5),
  Light = seq(min(mush$Light), max(mush$Light), length.out = 3),
  Species = levels(mush$Species),
  Humidity = levels(mush$Humidity)
)

# Use the best model to predict GrowthRate for these hypothetical settings.
hypo$GrowthRate_pred <- predict(best_mod, newdata = hypo)

#### Combine Real and Hypothetical Data ####
# Mark real and hypothetical rows.

real_df <- mush_pred %>%
  mutate(Type = "Real") %>%
  select(Nitrogen, Light, Species, Humidity, GrowthRate, pred, Type)

hypo_df <- hypo %>%
  rename(pred = GrowthRate_pred) %>%
  mutate(Type = "Hypothetical",
         GrowthRate = NA) %>%
  select(Nitrogen, Light, Species, Humidity, GrowthRate, pred, Type)

all_preds <- bind_rows(real_df, hypo_df)

#### Plot Real and Hypothetical Predictions ####

ggplot(all_preds, aes(x = Nitrogen, y = pred, color = Type)) +
  geom_point() + 
  geom_point(aes(y = GrowthRate), color = "black") +
  facet_grid(Species ~ Humidity) +
  theme_minimal() +
  labs(y = "GrowthRate (Observed and Predicted)",
       x = "Nitrogen")

#### Model Non-Linear Relationship Data ####

nonlin <- read_csv("Data/non_linear_relationship.csv")

# check the column names to know what to use.
colnames(nonlin)

# The dataset uses "predictor", so I make a squared version.
nonlin <- nonlin %>%
  mutate(
    predictor2 = predictor^2
  )

# Fit a linear model with both predictor and predictor^2.
nonlin_mod <- lm(response ~ predictor + predictor2, data = nonlin)

# Look at the model results.
summary(nonlin_mod)
