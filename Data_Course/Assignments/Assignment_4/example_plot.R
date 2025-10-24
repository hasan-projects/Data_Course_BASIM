# Load libraries
library(ggplot2)

# Read in the fake data
smoke_data <- read.csv("fake_smoking_data.csv")

# Make a ggplot
ggplot(smoke_data, aes(x = Year, y = Smoking_Prevalence, color = Campaign)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Trend in U.S. Adult Smoking Rates (2008–2018)",
    subtitle = "Fake data simulating impact of anti-smoking campaigns",
    x = "Year",
    y = "Smoking Prevalence (%)",
    color = "Campaign Active?"
  ) +
  theme_minimal(base_size = 14)

  