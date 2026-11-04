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

# STEP 4: Create a graph for students who participate in sports 

# Keep only the Sport group, each line represents one behavior 
# Each point represents the weighted percentage of Sport students 
# reporting that behavior in a particular survey year 

p_deviance_sport <- ggplot(
  deviance_by_sports %>% filter(sports_group == "Sport"),
  aes(x = year, y = percent, color = behavior, group = behavior)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 1) +
  labs(
    title = "Deviant Behaviors: Sport",
    x = "Year",
    y = "Percent Reporting Behavior",
    color = "Behavior"
  ) +
  scale_x_continuous(breaks = c(1997, 2007, 2017, 2023)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

# STEP 5: Create a graph for students who do not participate in sports 

p_deviance_no_sport <- ggplot(
  deviance_by_sports %>% filter(sports_group == "No Sport"),
  aes(x = year, y = percent, color = behavior, group = behavior)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 1) +
  labs(
    title = "Deviant Behaviors: No Sport",
    x = "Year",
    y = "Percent Reporting Behavior",
    color = "Behavior"
  ) +
  scale_x_continuous(breaks = c(1997, 2007, 2017, 2023)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

# View the two sports-group graphs separately
p_deviance_sport
p_deviance_no_sport

# Show the Sport and No Sport graphs side by side for easier comparison
combined_deviance_sports <- p_deviance_sport | p_deviance_no_sport
combined_deviance_sports
