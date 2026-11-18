
############              PHASE 3: DESCRIPTIVE STATISTICS               ############


### TABLE 1: Weighted descriptive statistics for 1997

descriptives_1997 <- yrbs_complete_case_1997 %>%
  summarise(
    age_mean = weighted.mean(age, weight),
    female_pct = weighted.mean(female, weight) * 100,
    sports_pct = weighted.mean(sports_participation, weight) * 100,
    fight_pct = weighted.mean(fight, weight) * 100,
    binge_drinking_pct = weighted.mean(binge_drinking, weight) * 100,
    marijuana_use_pct = weighted.mean(marijuana_use, weight) * 100,
    cocaine_use_pct = weighted.mean(cocaine_use, weight) * 100,
    nicotine_use_pct = weighted.mean(nicotine_use, weight) * 100
  )

descriptives_1997


### TABLE 2: Weighted descriptive statistics for 2007

descriptives_2007 <- yrbs_complete_case_2007 %>%
  summarise(
    age_mean = weighted.mean(age, weight),
    female_pct = weighted.mean(female, weight) * 100,
    sports_pct = weighted.mean(sports_participation, weight) * 100,
    fight_pct = weighted.mean(fight, weight) * 100,
    binge_drinking_pct = weighted.mean(binge_drinking, weight) * 100,
    marijuana_use_pct = weighted.mean(marijuana_use, weight) * 100,
    cocaine_use_pct = weighted.mean(cocaine_use, weight) * 100,
    nicotine_use_pct = weighted.mean(nicotine_use, weight) * 100
  )

descriptives_2007


### TABLE 3: Weighted descriptive statistics for 2017

descriptives_2017 <- yrbs_complete_case_2017 %>%
  summarise(
    age_mean = weighted.mean(age, weight),
    female_pct = weighted.mean(female, weight) * 100,
    sports_pct = weighted.mean(sports_participation, weight) * 100,
    fight_pct = weighted.mean(fight, weight) * 100,
    binge_drinking_pct = weighted.mean(binge_drinking, weight) * 100,
    marijuana_use_pct = weighted.mean(marijuana_use, weight) * 100,
    cocaine_use_pct = weighted.mean(cocaine_use, weight) * 100,
    nicotine_use_pct = weighted.mean(nicotine_use, weight) * 100
  )

descriptives_2017


### TABLE 4: Weighted descriptive statistics for 2023

descriptives_2023 <- yrbs_complete_case_2023 %>%
  summarise(
    age_mean = weighted.mean(age, weight),
    female_pct = weighted.mean(female, weight) * 100,
    sports_pct = weighted.mean(sports_participation, weight) * 100,
    fight_pct = weighted.mean(fight, weight) * 100,
    binge_drinking_pct = weighted.mean(binge_drinking, weight) * 100,
    marijuana_use_pct = weighted.mean(marijuana_use, weight) * 100,
    cocaine_use_pct = weighted.mean(cocaine_use, weight) * 100,
    nicotine_use_pct = weighted.mean(nicotine_use, weight) * 100
  )

descriptives_2023