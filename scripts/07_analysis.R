
# Three Checks ------------------------------------------------------------
dim(my_dataset)
names(my_dataset)
summary(my_dataset)


# Create sub for each year with and then the ----------------------------

# Create tidy subsets for each year ------------------------------------------

# 1997 Subset
tidy_1997 <- my_dataset |>
  filter(year == 1997) |>
  select(
    sports_participation, age, female, weight,
    fight, binge_drinking, marijuana_use, cocaine_use,
    race_group
  )

# 2007 Subset
tidy_2007 <- my_dataset |>
  filter(year == 2007) |>
  select(
    sports_participation, age, female, weight,
    fight, binge_drinking, marijuana_use, cocaine_use,
    race_group
  )

# 2017 Subset
tidy_2017 <- my_dataset |>
  filter(year == 2017) |>
  select(
    sports_participation, age, female, weight,
    fight, binge_drinking, marijuana_use, cocaine_use,
    race_group
  )

# 2023 Subset
tidy_2023 <- my_dataset |>
  filter(year == 2023) |>
  select(
    sports_participation, age, female, weight,
    fight, binge_drinking, marijuana_use, cocaine_use,
    race_group
  )

# Create factor variable for race in each year ----------------------------
tidy_1997 <- tidy_1997 |>
  mutate(race_group = factor(race_group)) |>
  mutate(race_group = relevel(race_group, ref = "White"))

tidy_2007 <- tidy_2007 |>
  mutate(race_group = factor(race_group)) |>
  mutate(race_group = relevel(race_group, ref = "White"))

tidy_2017 <- tidy_2017 |>
  mutate(race_group = factor(race_group)) |>
  mutate(race_group = relevel(race_group, ref = "White"))

tidy_2023 <- tidy_2023 |>
  mutate(race_group = factor(race_group)) |>
  mutate(race_group = relevel(race_group, ref = "White"))

# Logistic regression predicting sports participation by year -------------


# 1997
# set reference to white for race group
glm_sports_1997 <- glm(sports_participation ~ 
    age + 
    female  + 
    race_group, 
  data = tidy_1997, 
  family = binomial(link = "logit"))
summary(glm_sports_1997)

# 2007
# set reference to white for race group
glm_sports_2007 <- glm(sports_participation ~ 
    age + 
    female  + 
    race_group, 
  data = tidy_2007, 
  family = binomial(link = "logit"))
summary(glm_sports_2007)

# 2017
glm_sports_2017 <- glm(sports_participation ~ 
    age + 
    female  + 
      race_group,
  data = tidy_2017, 
  family = binomial(link = "logit"))
summary(glm_sports_2017)

# 2023
glm_sports_2023 <- glm(sports_participation ~ 
    age + 
    female  + 
    race_group, 
  data = tidy_2023, 
  family = binomial(link = "logit"))
summary(glm_sports_2023)
  
# Graph predicted probability of sports participation over cohort ------------------------------------------------------

# Graph predicted probability of sports participation over cohort ----

# Predict at a fixed reference profile so years are directly comparable:
#   average age, male, White (reference race group)
ref_profile <- bind_rows(
  list(
    age = mean(tidy_1997$age, na.rm = TRUE),
    age = mean(tidy_2007$age, na.rm = TRUE),
    age = mean(tidy_2017$age, na.rm = TRUE),
    age = mean(tidy_2023$age, na.rm = TRUE)
  ) |> mutate(year = c(1997, 2007, 2017, 2023),
              female = 0,
              race_group = factor("White", levels = levels(tidy_1997$race_group)))
)

predicted_probs <- ref_profile |>
  mutate(
    year = as.numeric(year),
    prob = map_dbl(
      list(glm_sports_1997, glm_sports_2007, glm_sports_2017, glm_sports_2023),
      ~ plogis(predict(.x, newdata = ref_profile, type = "link"))[1]
    )
  )

ggplot(predicted_probs, aes(x = year, y = prob)) |>
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Predicted Probability of Sports Participation by Cohort",
    subtitle = "Predicted at mean age, male, White (reference)",
    x = "Survey Year (Cohort)",
    y = "Predicted Probability"
  ) +
  scale_x_continuous(breaks = c(1997, 2007, 2017, 2023)) +
  theme_minimal()

