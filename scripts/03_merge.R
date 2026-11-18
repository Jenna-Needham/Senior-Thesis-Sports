
############               CREATE MY DATASET in complete case                    ############


## STEP 1: Create a list of variables to keep across all four years

my_varlist <- c(
  "sports_participation", "age", "white", "black", "hispanic", "asian", 
  "american_indian", "fight", "binge_drinking", "marijuana_use", 
  "cocaine_use", "cig", "vaping", "race_group", "weight", "female", "sports_group", "nicotine_use"
)


# Variables specific to 1997
my_varlist_1997 <- c(
  "other_race"
)

# Variables used in 2007, 2017, and 2023
my_varlist_2007_2017_2023 <- c(
  "native_hawaiian",
  "multiple_hispanic",
  "multiple_nonhispanic"
)


### STEP 2: Create a dataset for each year

yrbs_data_1997 <- yrbs1997 %>%
  select(all_of(c(my_varlist, my_varlist_1997)))

yrbs_data_2007 <- yrbs2007 %>%
  select(all_of(c(my_varlist, my_varlist_2007_2017_2023)))

yrbs_data_2017 <- yrbs2017 %>%
  select(all_of(c(my_varlist, my_varlist_2007_2017_2023)))

yrbs_data_2023 <- yrbs2023 %>%
  select(all_of(c(my_varlist, my_varlist_2007_2017_2023)))

### STEP 3: Combine all four years into one dataset

my_dataset <- bind_rows(
  yrbs_data_1997 %>% mutate(year = 1997),
  yrbs_data_2007 %>% mutate(year = 2007),
  yrbs_data_2017 %>% mutate(year = 2017),
  yrbs_data_2023 %>% mutate(year = 2023)
)

# Check that all four years are present
table(my_dataset$year)

## STEP 3: Create separate complete-case datasets for the
## common variables used in the main regression analyses.
## These do NOT require vaping, since vaping is unavailable in 1997/2007.

complete_case_vars <- c(
  "sports_participation", "age", "female", "race_group", "fight",
  "binge_drinking", "marijuana_use", "cocaine_use", "weight"
)

yrbs_complete_case_1997 <- yrbs_data_1997 %>%
  filter(complete.cases(select(., all_of(complete_case_vars))))

yrbs_complete_case_2007 <- yrbs_data_2007 %>%
  filter(complete.cases(select(., all_of(complete_case_vars))))

yrbs_complete_case_2017 <- yrbs_data_2017 %>%
  filter(complete.cases(select(., all_of(complete_case_vars))))

yrbs_complete_case_2023 <- yrbs_data_2023 %>%
  filter(complete.cases(select(., all_of(complete_case_vars))))

## Check sample sizes

nrow(yrbs_data_1997)
nrow(yrbs_data_2007)
nrow(yrbs_data_2017)
nrow(yrbs_data_2023)

nrow(yrbs_complete_case_1997)
nrow(yrbs_complete_case_2007)
nrow(yrbs_complete_case_2017)
nrow(yrbs_complete_case_2023)

## Proportion excluded from the selected complete-case sample

missing_97 <- 1 - nrow(yrbs_complete_case_1997) / nrow(yrbs_data_1997)
missing_07 <- 1 - nrow(yrbs_complete_case_2007) / nrow(yrbs_data_2007)
missing_17 <- 1 - nrow(yrbs_complete_case_2017) / nrow(yrbs_data_2017)
missing_23 <- 1 - nrow(yrbs_complete_case_2023) / nrow(yrbs_data_2023)