
############## DEVIANT BEHAVIORS: SPORT VS. NO SPORT ##########################

# The next graph summaries answer a different question:
# Within each year, what percentage of Sport and No Sport students report each
# of the four behaviors? We calculate this separately for every year, group,
# and behavior. This gives a fair comparison between the two sports groups.

# Step 1: Summarize by year, sports group, and behavior.
# We reuse deviance_data, which already has one row per person/behavior and
# valid weights. Exclude respondents whose sports group is unknown.

deviance_by_sports <- deviance_data %>%
  filter(!is.na(sports_group)) %>%
  group_by(year, sports_group, behavior) %>%
  summarise(
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

# Step 2: Graph deviant behaviors among students who participated in sports.

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

# Step 3: Graph deviant behaviors among students who did not participate in sports.

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

