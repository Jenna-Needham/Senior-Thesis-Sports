




############              PHASE 4: FIGURES AND TRENDS                  ############


# The percentages below use the YRBS weight variable
# The margins of error below are approximate 95% confidence intervals. The sample size
# does not fully account for the YRBS's complex survey design (e.g., clustering, stratification). Therefore, these intervals are likely too narrow.
#
# percent = weighted percentage for a group/year
# n_eff   = approximate effective sample size after accounting for unequal weights
# moe     = margin of error in percentage points
# lower   = percent minus margin of error (not below 0%)
# upper   = percent plus margin of error (not above 100%)
# geom_errorbar() draws the vertical line from lower to upper around each point.

######################### GRAPH 1: DEVIANT BEHAVIORS ###########################

# Step 1: Put the four deviant-behavior variables into one column.
# This makes it possible to draw four lines on the same graph.
# Each variable is coded 0 = no and 1 = yes.

deviance_data <- my_dataset %>%
  select(year, sports_group, weight, fight, binge_drinking,
         marijuana_use, cocaine_use) %>%
  pivot_longer(
    cols = c(fight, binge_drinking, marijuana_use, cocaine_use),
    names_to = "behavior",
    values_to = "response"
  ) %>%
  filter(!is.na(response), !is.na(weight), weight > 0)

# Step 2: Calculate the weighted percentage for each behavior in each year.
# The effective sample size is calculated using the weights for the valid
# responses to that behavior and year.

deviance_trends <- deviance_data %>%
  group_by(year, behavior) %>%
  summarise(
    percent = weighted.mean(response, weight) * 100,
    n_eff = sum(weight)^2 / sum(weight^2),
    .groups = "drop"
  ) %>%
  mutate(
    # Approximate standard error, converted to percentage points
    se = sqrt((percent / 100) * (1 - percent / 100) / n_eff),
    # 1.96 is used for an approximate 95% margin of error
    moe = 1.96 * se * 100,
    lower = pmax(0, percent - moe),
    upper = pmin(100, percent + moe)
  )

# This seems to inflate power because we've multiplied n x 4 for year and then by 4 again for each group
# double check errors; perhaps use lm predictions

# Step 3: Draw the weighted percentages and their error bars.

p_deviance <- ggplot(
  deviance_trends,
  aes(x = year, y = percent, color = behavior, group = behavior)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 1) +
  labs(
    title = "Deviant Behaviors",
    x = "Year",
    y = "Percent Reporting Behavior",
    color = "Behavior"
  ) +
  scale_x_continuous(breaks = c(1997, 2007, 2017, 2023)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

# View the first graph
p_deviance

###################### GRAPH 2: SPORT VS. NO SPORT ############################

# Step 1: Keep only respondents with a known sports group and valid weight.
# First, calculate the effective sample size for each year.

sports_valid <- my_dataset %>%
  filter(!is.na(sports_group), !is.na(weight), weight > 0)

sports_year_n <- sports_valid %>%
  group_by(year) %>%
  summarise(
    n_eff = sum(weight)^2 / sum(weight^2),
    .groups = "drop"
  )

# Step 2: Add the weights within Sport and No Sport.
# The percent for each category is its share of the total weighted count
# for that year. Because there are two categories, they should add to 100%.

sports_trends <- sports_valid %>%
  group_by(year, sports_group) %>%
  summarise(
    weighted_n = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(percent = weighted_n / sum(weighted_n) * 100) %>%
  ungroup() %>%
  left_join(sports_year_n, by = "year") %>%
  mutate(
    se = sqrt((percent / 100) * (1 - percent / 100) / n_eff),
    moe = 1.96 * se * 100,
    lower = pmax(0, percent - moe),
    upper = pmin(100, percent + moe)
  )

# Step 3: Plot the weighted percentages for Sport and No Sport.

p_sports <- ggplot(
  sports_trends,
  aes(x = year, y = percent, color = sports_group, group = sports_group)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 1) +
  labs(
    title = "Sports Participation",
    x = "Year",
    y = "Percent of Students",
    color = "Sports Participation"
  ) +
  scale_x_continuous(breaks = c(1997, 2007, 2017, 2023)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  theme_minimal()

# View the second graph
p_sports

######################## GRAPH 3: NICOTINE USE ################################

# Note about measurement:
# In 1997 and 2007, nicotine_use represents cigarette use only.
# In 2017 and 2023, nicotine_use represents cigarette OR vaping use.
# Therefore, avg_nic is not measured exactly the same way in every year.
# Vaping is NA in 1997 and 2007 because it was not measured in those datasets.

# Step 1: Put the three nicotine measures into one column to create three lines.

nicotine_data <- my_dataset %>%
  select(year, weight, nicotine_use, cig, vaping) %>%
  rename(avg_nic = nicotine_use) %>%
  pivot_longer(
    cols = c(avg_nic, cig, vaping),
    names_to = "nicotine_type",
    values_to = "response"
  ) %>%
  filter(!is.na(response), !is.na(weight), weight > 0)

# Step 2: Calculate weighted percentages and approximate margins of error.
# Missing vaping values in 1997 and 2007 are excluded, not counted as 0%.

nicotine_trends <- nicotine_data %>%
  group_by(year, nicotine_type) %>%
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

# Step 3: Plot the three measures together.

p_nicotine <- ggplot(
  nicotine_trends,
  aes(x = year, y = percent, color = nicotine_type, group = nicotine_type)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 1) +
  labs(
    title = "Nicotine Use",
    x = "Year",
    y = "Percent Reporting Use",
    color = "Nicotine Type"
  ) +
  scale_x_continuous(breaks = c(1997, 2007, 2017, 2023)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal()

# View the third graph
p_nicotine

######################## GRAPH 4: RACE / ETHNICITY ############################

# STEP 1: Calculate the percentage of respondents in each race group

race_trends <- my_dataset %>%
  filter(!is.na(race_group)) %>%
  group_by(year, race_group) %>%
  summarise(
    weighted_n = sum(weight),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(
    percent = weighted_n / sum(weighted_n) * 100
  ) %>%
  ungroup()

# STEP 2: Check that percentages add up to 100% each year

race_trends %>%
  group_by(year) %>%
  summarise(total_percent = sum(percent))

# STEP 3: Create the race composition graph

p_race <- ggplot(
  race_trends,
  aes(
    x = year,
    y = percent,
    fill = race_group
  )
) +
  geom_area(position = "stack") +
  labs(
    title = "Race/Ethnicity Composition",
    x = "Year",
    y = "Percent",
    fill = "Race/Ethnicity"
  ) +
  scale_x_continuous(
    breaks = c(1997, 2007, 2017, 2023)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20)
  ) +
  coord_cartesian(
    ylim = c(0, 100)
  ) +
  scale_fill_brewer(
    palette = "Set2"
  ) +
  theme_minimal()

p_race

######################### PATCHWORK: FOUR GRAPHS ##############################

# This creates the main 2-by-2 figure:
# top left = deviant behaviors; top right = sports participation;
# bottom left = nicotine measures; bottom right = race/ethnicity.

combined_trends <- (
  p_deviance | p_sports
) / (
  p_nicotine | p_race
)

# Display the four graphs together
combined_trends