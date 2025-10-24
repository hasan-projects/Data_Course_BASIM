# BIOL3100 Exam 1 — Hasan Basim

# Load Libraries 
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(forcats)


# Load data
covid <- read_csv("cleaned_covid_data.csv")

covid <- covid %>%
  mutate(
    Date = as.Date(Last_Update),
    Deaths = suppressWarnings(as.numeric(Deaths)),
    Case_Fatality_Ratio = suppressWarnings(as.numeric(Case_Fatality_Ratio))
  )

# Subset to states that start with "A"
A_states <- covid %>%
  filter(grepl("^A", Province_State))

# Plot (A-states)

p1 <- ggplot(A_states, aes(x = Date, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") +
  labs(title = "Deaths Over Time (States Starting With 'A')",
       x = "Date", y = "Deaths") +
  theme_minimal()
print(p1)

# Peak CFR By State 

state_max_fatality_rate <- covid %>%
  group_by(Province_State) %>%
  summarise(
    Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)
  ) %>%
  arrange(desc(Maximum_Fatality_Ratio))

# quick look
head(state_max_fatality_rate)

# Bar Plot of Peak CFR

p2 <- ggplot(
  state_max_fatality_rate %>%
    mutate(Province_State = fct_reorder(Province_State, Maximum_Fatality_Ratio, .desc = TRUE)),
  aes(x = Province_State, y = Maximum_Fatality_Ratio)
) +
  geom_col() +
  labs(title = "Peak Case Fatality Ratio by State",
       x = "Province_State", y = "Maximum_Fatality_Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
print(p2)

# BONUS 

us_cum <- covid %>%
  group_by(Date) %>%
  summarise(US_Cumulative_Deaths = sum(Deaths, na.rm = TRUE))

p3 <- ggplot(us_cum, aes(x = Date, y = US_Cumulative_Deaths)) +
  geom_line() +
  labs(title = "Cumulative US Deaths Over Time",
       x = "Date", y = "US Cumulative Deaths") +
  theme_minimal()
print(p3)
