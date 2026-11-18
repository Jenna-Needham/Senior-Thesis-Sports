############## DEVIANT BEHAVIORS: SPORT VS. NO SPORT ##########################

# Question: Within each survey year, how do rates of fighting, binge drinking, 
# marijuana use, and cocaine use compare between students who participate in sports
# and students who do not participate in sports? 

# STEP 1: Prepare the data for the comparison 
# We reuse deviance_data from Graph 1 

# this is because it already contains the four behavior variables in long format 
# also includes the year and sports_participation group, excludes missing behavior 
# responses, and excldues missing or invalid survey weights 

# We only need to remove respondents whose sports participation group 
# is unknown because we cannot classify them as Sport or No Sport 
# This leaves us with valid behavior responses from respondents whose sports 
# participation status is also known 

# STEP 2: Calcualte the weighted percentage within each subgroup 

deviance_by_sports <- deviance_data %>%
  filter(!is.na(sports_group)) %>%
  group_by(year, sports_group, behavior) %>%
  summarise(
    n_valid = n(),
    percent = weighted.mean(response, weight) * 100,
    n_eff = sum(weight)^2 / sum(weight^2),
    .groups = "drop"
  ) %>%
  mutate(
    se = sqrt((percent / 100) * (1 - percent / 100) / n_eff),
    moe = 1.96 * se * 100,
    lower = pmax(0, percent - moe),
    upper = pmin(100, percent + moe)
  )

# STEP 3: Examine the subgroup calculations before making the graphs 

deviance_by_sports %>%
  select(year, sports_group, behavior, n_valid,
         n_eff, percent, moe, lower, upper) %>%
  arrange(year, sports_group, behavior)

############## DEVIANT BEHAVIORS: SPORT VS. NO SPORT ##########################

# STEP 1: Prepare the data for the comparison 
# We reuse deviance_data from Graph 1; we remove respondents whose sports participation 
# is unknown

# STEP 2: Calculate the weighted percentage within each subgroup

deviance_by_sports <- deviance_data %>%
  filter(!is.na(sports_group)) %>%
  group_by(year, sports_group, behavior) %>%
  summarise(
    n_valid = n(),
    percent = weighted.mean(response, weight) * 100,
    n_eff = sum(weight)^2 / sum(weight^2),
    .groups = "drop"
  ) %>%
  mutate(
    se = sqrt((percent / 100) * (1 - percent / 100) / n_eff),
    moe = 1.96 * se * 100,
    lower = pmax(0, percent - moe),
    upper = pmin(100, percent + moe)
  )

# STEP 3: Examine the subgroup calculations

deviance_by_sports %>%
  select(year, sports_group, behavior, n_valid,
         n_eff, percent, moe, lower, upper) %>%
  arrange(year, sports_group, behavior)

# STEP 4: Calculate the difference between Sport and No Sport 
# Put Sport and No Sport percentages into separate columns
# This allows us to subtract the two percentages for each behavior and year 

deviance_differences <- deviance_by_sports %>%
  select(year, behavior, sports_group, percent) %>%
  pivot_wider(
    names_from = sports_group,
    values_from = percent
  ) %>%
  mutate(
    difference = Sport - `No Sport`
  )

# STEP 5: Examine the differences between Sport and No Sport
# A negative difference means the percentage is higher among No Sport students
# A positive difference means the percentage is higher among Sport students
# A difference of zero means both groups have the same percentage of students engaging in the behavior

deviance_differences %>%
  select(year, behavior, Sport, `No Sport`, difference) %>%
  arrange(year, behavior)

# STEP 6: Create the graph of difference scores 

# Each line represents one deviant behavior 
# The horizontal line at zero indicates no difference between Sport and No Sport students

p_deviance_difference <- ggplot(
  deviance_differences,
  aes(x = year, y = difference,
      color = behavior, group = behavior)
) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Differences in Deviant Behaviors by Sports Participation",
    subtitle = "Sport minus No Sport",
    x = "Year",
    y = "Difference in Percentage Points",
    color = "Behavior"
  ) +
  scale_x_continuous(breaks = c(1997, 2007, 2017, 2023)) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

# View the difference graph
p_deviance_difference


deviance_differences
