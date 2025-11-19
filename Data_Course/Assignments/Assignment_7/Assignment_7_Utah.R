#### Libraries ####
library(tidyverse)
library(janitor)

#### Import data ####
# I’m assuming the .R file and the CSV are in the same folder
utah_religions <- read.csv("~/Data_Course_BASIM/Data_Course/Assignments/Assignment_7/Utah_Religions_by_County.csv")

#### Peek at raw data ####
head(utah_religions)
str(utah_religions)

#### Clean column names ####
# This makes names lowercase and replaces spaces with _
utah_religions <- clean_names(utah_religions)

#### Check cleaned names ####
names(utah_religions)

#### Decide which columns are “religion” columns ####
# county and pop_2010 are NOT religions
# everything else is some kind of proportion
religion_cols <- names(utah_religions)[
  !(names(utah_religions) %in% c("county", "pop_2010"))
]

religion_cols   # just to see what we are treating as religion-like variables

#### Make data tidy (county | religion | proportion) ####
utah_religions_tidy <- utah_religions %>%
  pivot_longer(
    cols = all_of(religion_cols),
    names_to = "religion",
    values_to = "proportion"
  )

#### Convert proportion to numeric (just in case) ####
utah_religions_tidy$proportion <- as.numeric(utah_religions_tidy$proportion)

#### Look at tidy data ####
head(utah_religions_tidy)


#### Basic exploration plots (just looking) ####

#### Plot population by county ####
ggplot(utah_religions, aes(x = reorder(county, pop_2010), y = pop_2010)) +
  geom_col() +
  coord_flip() +
  labs(title = "Population by County (2010)",
       x = "County",
       y = "Population (2010)")

#### Religious vs non-religious by county ####
# Here I just plot the two big summary categories
ggplot(utah_religions, aes(x = county)) +
  geom_col(aes(y = religious, fill = "Religious"), position = "dodge") +
  geom_col(aes(y = non_religious, fill = "Non-Religious"), position = "dodge") +
  coord_flip() +
  labs(title = "Religious vs Non-Religious by County",
       x = "County",
       y = "Proportion") +
  scale_fill_manual(values = c("Religious" = "gray40", "Non-Religious" = "gray70"))

#### Heatmap of religions by county ####
# This lets me see patterns of which religions are bigger in which counties
ggplot(utah_religions_tidy, aes(x = religion, y = county, fill = proportion)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Heatmap of Religious Proportions by County",
       x = "Religion",
       y = "County")

#### Q1: Population vs specific religious group ####
#### “Does population of a county correlate with ####
####  the proportion of any specific religious group?” ####

#### Filter out overall summary columns for this question ####
# I want “specific” groups, not the big totals “religious” or “non_religious”
pop_religions <- utah_religions_tidy %>%
  filter(!(religion %in% c("religious", "non_religious")))

#### Scatterplots: population vs religion proportion ####
ggplot(pop_religions, aes(x = pop_2010, y = proportion)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ religion, scales = "free_y") +
  labs(title = "Population vs Religion Proportion by County",
       x = "Population (2010)",
       y = "Religion Proportion")

#### Correlation between population and each religion ####
cor_pop_by_religion <- pop_religions %>%
  group_by(religion) %>%
  summarize(
    correlation = cor(pop_2010, proportion, use = "complete.obs")
  )

cor_pop_by_religion

#### Thought: I would look at this table and see which religions have strong + or - correlations with population ####
#### Q2: Specific religions vs non-religious proportion ####
#### “Does proportion of any specific religion in a given ####
####  county correlate with the proportion of non-religious?” ####

#### Get non-religious proportion as its own column ####
# I’ll pull it out from the tidy version first
nonrelig_df <- utah_religions_tidy %>%
  filter(religion == "non_religious") %>%
  select(county, non_religious_prop = proportion)

#### Join non-religious proportions onto religion data ####
relig_vs_non <- pop_religions %>%
  left_join(nonrelig_df, by = "county")

#### Check merged data ####
head(relig_vs_non)

#### Scatterplots: religion proportion vs non-religious proportion ####
ggplot(relig_vs_non, aes(x = non_religious_prop, y = proportion)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  facet_wrap(~ religion, scales = "free_y") +
  labs(title = "Specific Religion vs Non-Religious Proportion by County",
       x = "Non-Religious Proportion",
       y = "Religion Proportion")

#### Correlation between each religion and non-religious ####
cor_nonrelig_by_religion <- relig_vs_non %>%
  group_by(religion) %>%
  summarize(
    correlation = cor(non_religious_prop, proportion, use = "complete.obs")
  )

cor_nonrelig_by_religion

#### Thought: Here I would look for strong negative correlations ####
#### (if a religion goes up when non-religious goes down, or vice versa) ####
