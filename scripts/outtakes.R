



####################################################################################
############     Compare Whole Sample vs. Selected Complete-Case Sample    ############
####################################################################################

# Look at weighted racial and gender composition in the whole YRBS sample
# and compare it to the weighted racial and gender composition
# of our selected complete-case analytic sample 


############        COMPARE OUR DEMOGRAPHICS WITH BENCHMARKS            ############


# To what extent is our sample different from the national population
# of high school students in the U.S.?




# Trends in deviant participation by sports involvement -------------------



# Any gender differencse here? --------------------------------------------




# Any age differences here? -----------------------------------------------



# Any race differences here? ----------------------------------------------





# Logistic regression models predicting pr(dev_behavior)  -----------------




# Figure showing pr() by IVs (especially sports, controlling for demographic factors --------





# measures of intensity of sport use? -------------------------------------




####################################################################################
############ 1) COMPARE WHOLE SAMPLE VS. SELECTED COMPLETE-CASE SAMPLE ############
####################################################################################

# Look at weighted racial and gender composition in the whole YRBS sample
# and compare it to the weighted racial and gender composition
# of our selected complete-case analytic sample.


# STEP 1: Combine original samples and complete-case samples (only weight, female, race_group)

demographic_samples <- bind_rows(
  yrbs1997 %>%
    select(weight, female, race_group) %>%
    mutate(year = 1997, sample = "Original"),
  
  yrbs_complete_case_1997 %>%
    select(weight, female, race_group) %>%
    mutate(year = 1997, sample = "Complete cases"),
  
  yrbs2007 %>%
    select(weight, female, race_group) %>%
    mutate(year = 2007, sample = "Original"),
  
  yrbs_complete_case_2007 %>%
    select(weight, female, race_group) %>%
    mutate(year = 2007, sample = "Complete cases"),
  
  yrbs2017 %>%
    select(weight, female, race_group) %>%
    mutate(year = 2017, sample = "Original"),
  
  yrbs_complete_case_2017 %>%
    select(weight, female, race_group) %>%
    mutate(year = 2017, sample = "Complete cases"),
  
  yrbs2023 %>%
    select(weight, female, race_group) %>%
    mutate(year = 2023, sample = "Original"),
  
  yrbs_complete_case_2023 %>%
    select(weight, female, race_group) %>%
    mutate(year = 2023, sample = "Complete cases")
)



# STEP 2: Calculate weighted gender and race percentages (group by year and sample type)
# this gives us separate percentages for 1997 original, 1997 complete cases, etc.
# weighted.mean(female, weight) gives the weighted proportion female 
# weighted.mean(1 - female, weight) gives us the proportion male 

demographic_comparison <- demographic_samples %>%
  group_by(year, sample) %>%
  summarise(
    female_pct = weighted.mean(female, weight, na.rm = TRUE) * 100,
    male_pct = weighted.mean(1 - female, weight, na.rm = TRUE) * 100,
    
    white_pct = weighted.mean(
      as.numeric(race_group == "White"),
      weight,
      na.rm = TRUE
    ) * 100,
    
    black_pct = weighted.mean(
      as.numeric(race_group == "Black"),
      weight,
      na.rm = TRUE
    ) * 100,
    
    hispanic_pct = weighted.mean(
      as.numeric(race_group == "Hispanic"),
      weight,
      na.rm = TRUE
    ) * 100,
    
    asian_pacific_pct = weighted.mean(
      as.numeric(race_group == "Asian/Pacific Islander"),
      weight,
      na.rm = TRUE
    ) * 100,
    
    american_indian_pct = weighted.mean(
      as.numeric(race_group == "American Indian/Alaska Native"),
      weight,
      na.rm = TRUE
    ) * 100,
    
    other_multiracial_pct = weighted.mean(
      as.numeric(race_group == "Other/Multiracial"),
      weight,
      na.rm = TRUE
    ) * 100,
    
    .groups = "drop"
  )


# STEP 3: View weighted demographic percentages

demographic_comparison


# STEP 4: Check that weighted race percentages add to approximately 100%

demographic_comparison <- demographic_comparison %>%
  mutate(
    race_total =
      white_pct +
      black_pct +
      hispanic_pct +
      asian_pacific_pct +
      american_indian_pct +
      other_multiracial_pct
  )

demographic_comparison %>%
  select(year, sample, race_total)


# STEP 5: Calculate the difference between the original sample and the complete-case sample
# we use pivot_longer() so that each demographic group has its own row
# then we use pivot_wider() so Original and Complete cases are in separate columns

sample_comparison <- demographic_comparison %>%
  select(
    year,
    sample,
    female_pct,
    male_pct,
    white_pct,
    black_pct,
    hispanic_pct,
    asian_pacific_pct,
    american_indian_pct,
    other_multiracial_pct
  ) %>%
  pivot_longer(
    cols = -c(year, sample),
    names_to = "demographic_group",
    values_to = "percent"
  ) %>%
  pivot_wider(
    names_from = sample,
    values_from = percent
  ) %>%
  mutate(
    Difference = `Complete cases` - Original
  )

sample_comparison

####################################################################################
############         Compare our demographics with benchmarks      ############
####################################################################################

# To what extent is our sample different from the national population
# of high school students in the U.S.? 

demographic_comparison


# 2) Compare our weighted racial composition in the whole sample and our selected subsample
# to a benchmark, like the American Community Survey or the nearest census 
# i.e., the census for 2000, 2010, 2020.
# primiarly compare race and gender for benchmarking

####################################################################################
############                   Predict missingness for sports + fighting               ############
####################################################################################

# create a valid variable equal to 1 when sports is missing and 0 when sports is not missing

# assess whether any of your other covariates are related to this missingness

# Is the data missing completely at random (MCAR), missing at random (MAR), or missing not at random (MNAR)?



