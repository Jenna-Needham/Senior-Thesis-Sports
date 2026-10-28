
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############

# follow three steps of cleaning data for each variable for each year
# step 1: examine the variable
# Step 2: Clean the variable (always create a new variable!)
# Step 3: Confirm cleaning was correct


############       INDEPENDENT VARIABLE: Sports Participation                     ############
## 1997

## AMOUNT OF TEAMS PLAYED FOR RUN BY SCHOOL

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q83, useNA = "ifany")

# STEP 2: Create binary sports participation variables 

yrbs1997 <- mutate(
  yrbs1997,
  sports_participation_school = ifelse(Q83 == 1, 0, 1)
)

# STEP 3: Confirm creation

table(yrbs1997$sports_participation_school, useNA = "ifany")
table(yrbs1997$Q83, yrbs1997$sports_participation_school, useNA = "ifany")

## AMOUNT OF TEAMS PLAYED FOR ORGS OUTSIDE OF SCHOOL

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q84, useNA = "ifany")

# STEP 2: Create binary sports participation variables 

yrbs1997 <- mutate(yrbs1997, sports_participation_org = ifelse(Q84 == 1, 0, 1))

## COMBINE SCHOOL AND OUTSIDE SPORTS

# STEP 3: Create overall sports participation variable 

yrbs1997 <- mutate(
  yrbs1997,
  sports_participation = case_when(
    sports_participation_school == 1 |
      sports_participation_org == 1 ~ 1,
    sports_participation_school == 0 &
      sports_participation_org == 0 ~ 0,
    TRUE ~ NA_real_
  )
)

# STEP 4: Confirm creation

table(yrbs1997$sports_participation, useNA = "ifany")

table(
  yrbs1997$sports_participation_school,
  yrbs1997$sports_participation_org,
  useNA = "ifany"
)

## Create binary sports category 

# STEP 1: Create a binary sports category variable for 1997

yrbs1997 <- mutate(
  yrbs1997,
  sports_group = ifelse(
    sports_participation == 1,
    "Sport",
    "No Sport"
  )
)

# STEP 2: Confirm creation

table(yrbs1997$sports_group, useNA = "ifany")

table(
  yrbs1997$sports_participation,
  yrbs1997$sports_group,
  useNA = "ifany"
)

## 2007

# STEP 1: Examine variable and coding schema 

table(yrbs2007$q84, useNA = "ifany")

# STEP 2: Create binary sports participation variables 

yrbs2007 <- mutate(yrbs2007, sports_participation = ifelse(q84 == 1, 0, 1))

# STEP 3: Confirm creation

table(yrbs2007$sports_participation, useNA = "ifany")
table(yrbs2007$q84, yrbs2007$sports_participation, useNA = "ifany")


# Create a binary sports participation category 

# STEP 1: Create a binary sports category variable for 2007

yrbs2007 <- mutate(
  yrbs2007,
  sports_group = ifelse(
    sports_participation == 1,
    "Sport",
    "No Sport"
  )
)

# STEP 2: Confirm creation

table(yrbs2007$sports_group, useNA = "ifany")

table(
  yrbs2007$sports_participation,
  yrbs2007$sports_group,
  useNA = "ifany"
)


## 2017 
# STEP 1: Examine variable and coding schema

table(yrbs2017$q83, useNA = "ifany")

# STEP 2: Create binary sports participation variables 

yrbs2017 <- mutate(yrbs2017, sports_participation = ifelse(q83 == 1, 0, 1))

# STEP 3: Confirm creation

table(yrbs2017$sports_participation, useNA = "ifany")
table(yrbs2017$q83, yrbs2017$sports_participation, useNA = "ifany")

# Create a binary sports participation category 

# STEP 1: Create a binary sports category variable for 2017

yrbs2017 <- mutate(
  yrbs2017,
  sports_group = ifelse(
    sports_participation == 1,
    "Sport",
    "No Sport"
  )
)

# STEP 2: Confirm creation

table(yrbs2017$sports_group, useNA = "ifany")

table(
  yrbs2017$sports_participation,
  yrbs2017$sports_group,
  useNA = "ifany"
)


## 2023
# STEP 1: Examine variable and coding schema

table(yrbs2023$q78, useNA = "ifany")

# STEP 2: Create binary sports participation variables 

yrbs2023 <- mutate(yrbs2023, sports_participation = ifelse(q78 == 1, 0, 1))

# STEP 3: Confirm creation

table(yrbs2023$sports_participation, useNA = "ifany")
table(yrbs2023$q78, yrbs2023$sports_participation, useNA = "ifany")

# Create a binary sports participation category 

# STEP 1: Create a binary sports category variable for 2023

yrbs2023 <- mutate(
  yrbs2023,
  sports_group = ifelse(
    sports_participation == 1,
    "Sport",
    "No Sport"
  )
)

# STEP 2: Confirm creation

table(yrbs2023$sports_group, useNA = "ifany")

table(
  yrbs2023$sports_participation,
  yrbs2023$sports_group,
  useNA = "ifany"
)


############                    DEMOGRAPHIC VARIABLES                   ############

### AGE ###

## 1997 ##

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q1, useNA = "ifany")

# STEP 2: Recode age as a numeric variable

yrbs1997 <- mutate(yrbs1997, age = case_when(
  Q1 == 1 ~ 12,
  Q1 == 2 ~ 13,
  Q1 == 3 ~ 14,
  Q1 == 4 ~ 15,
  Q1 == 5 ~ 16,
  Q1 == 6 ~ 17,
  Q1 == 7 ~ 18
))

# STEP 3: Confirm recoding

table(yrbs1997$Q1, yrbs1997$age, useNA = "ifany")
table(yrbs1997$age, useNA = "ifany")

## 2007 ##

# STEP 1: Examine variable and coding schema

table(yrbs2007$q1, useNA = "ifany")

# STEP 2: Recode age as a numeric variable

yrbs2007 <- mutate(yrbs2007, age = case_when(
  q1 == 1 ~ 12,
  q1 == 2 ~ 13,
  q1 == 3 ~ 14,
  q1 == 4 ~ 15,
  q1 == 5 ~ 16,
  q1 == 6 ~ 17,
  q1 == 7 ~ 18
))

# STEP 3: Confirm recording

table(yrbs2007$q1, yrbs2007$age, useNA = "ifany")
table(yrbs2007$age, useNA = "ifany")

## 2017 ##

# STEP 1: Examine variable and coding schema

table(yrbs2017$q1, useNA = "ifany")

# STEP 2: Recode age as a numeric variable

yrbs2017 <- mutate(yrbs2017, age = case_when(
  q1 == 1 ~ 12,
  q1 == 2 ~ 13,
  q1 == 3 ~ 14,
  q1 == 4 ~ 15,
  q1 == 5 ~ 16,
  q1 == 6 ~ 17,
  q1 == 7 ~ 18
))

# STEP 3: Confirm recoding

table(yrbs2017$q1, yrbs2017$age, useNA = "ifany")
table(yrbs2017$age, useNA = "ifany")

## 2023 ##

# STEP 1: Examine variable and coding schema

table(yrbs2023$q1, useNA = "ifany")

# STEP 2: Recode age as a numeric variable

yrbs2023 <- mutate(yrbs2023, age = case_when(
  q1 == 1 ~ 12,
  q1 == 2 ~ 13,
  q1 == 3 ~ 14,
  q1 == 4 ~ 15,
  q1 == 5 ~ 16,
  q1 == 6 ~ 17,
  q1 == 7 ~ 18
))

# STEP 3: Confirm recoding

table(yrbs2023$q1, yrbs2023$age, useNA = "ifany")
table(yrbs2023$age, useNA = "ifany")


### GENDER ###

## 1997

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q2, useNA = "ifany")

# STEP 2: Create female dummy variable
# Reference category = male

yrbs1997 <- mutate(yrbs1997, female = ifelse(Q2 == 1, 1, 0))

# STEP 3: Confirm creation

table(yrbs1997$Q2, yrbs1997$female, useNA = "ifany")


## 2007

# STEP 1: Examine variable and coding schema

table(yrbs2007$q2, useNA = "ifany")

# STEP 2: Create female dummy variable
# Reference category = male

yrbs2007 <- mutate(yrbs2007, female = ifelse(q2 == 1, 1, 0))

# STEP 3: Confirm creation

table(yrbs2007$q2, yrbs2007$female, useNA = "ifany")


## 2017

# STEP 1: Examine variable and coding schema

table(yrbs2017$q2, useNA = "ifany")

# STEP 2: Create female dummy variable
# Reference category = male

yrbs2017 <- mutate(yrbs2017, female = ifelse(q2 == 1, 1, 0))

# STEP 3: Confirm creation

table(yrbs2017$q2, yrbs2017$female, useNA = "ifany")


## 2023

# STEP 1: Examine variable and coding schema

table(yrbs2023$q2, useNA = "ifany")

# STEP 2: Create female dummy variable
# Reference category = male

yrbs2023 <- mutate(yrbs2023, female = ifelse(q2 == 1, 1, 0))

# STEP 3: Confirm creation

table(yrbs2023$q2, yrbs2023$female, useNA = "ifany")

### RACE ###

## 1997

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q4, useNA = "ifany")

# STEP 2: Create race dummy variables

yrbs1997 <- mutate(yrbs1997, white = ifelse(Q4 == 1, 1, 0))
yrbs1997 <- mutate(yrbs1997, black = ifelse(Q4 == 2, 1, 0))
yrbs1997 <- mutate(yrbs1997, hispanic = ifelse(Q4 == 3, 1, 0))
yrbs1997 <- mutate(yrbs1997, asian = ifelse(Q4 == 4, 1, 0))
yrbs1997 <- mutate(yrbs1997, american_indian = ifelse(Q4 == 5, 1, 0))
yrbs1997 <- mutate(yrbs1997, other_race = ifelse(Q4 == 6, 1, 0))

# STEP 3: Confirm creation

table(yrbs1997$white, useNA = "ifany")
table(yrbs1997$black, useNA = "ifany")
table(yrbs1997$hispanic, useNA = "ifany")
table(yrbs1997$asian, useNA = "ifany")
table(yrbs1997$american_indian, useNA = "ifany")
table(yrbs1997$other_race, useNA = "ifany")

table(yrbs1997$Q4, yrbs1997$white, useNA = "ifany")
table(yrbs1997$Q4, yrbs1997$black, useNA = "ifany")
table(yrbs1997$Q4, yrbs1997$hispanic, useNA = "ifany")
table(yrbs1997$Q4, yrbs1997$asian, useNA = "ifany")
table(yrbs1997$Q4, yrbs1997$american_indian, useNA = "ifany")
table(yrbs1997$Q4, yrbs1997$other_race, useNA = "ifany")

## 2007

# STEP 1: Examine variable and coding schema

table(yrbs2007$raceeth, useNA = "ifany")

# STEP 2: Create race dummy variables

yrbs2007 <- mutate(yrbs2007, american_indian = ifelse(raceeth == 1, 1, 0))
yrbs2007 <- mutate(yrbs2007, asian = ifelse(raceeth == 2, 1, 0))
yrbs2007 <- mutate(yrbs2007, black = ifelse(raceeth == 3, 1, 0))
yrbs2007 <- mutate(yrbs2007, native_hawaiian = ifelse(raceeth == 4, 1, 0))
yrbs2007 <- mutate(yrbs2007, white = ifelse(raceeth == 5, 1, 0))
yrbs2007 <- mutate(yrbs2007, hispanic = ifelse(raceeth == 6, 1, 0))
yrbs2007 <- mutate(yrbs2007, multiple_hispanic = ifelse(raceeth == 7, 1, 0))
yrbs2007 <- mutate(yrbs2007, multiple_nonhispanic = ifelse(raceeth == 8, 1, 0))


# STEP 3: Confirm creation

table(yrbs2007$american_indian, useNA = "ifany")
table(yrbs2007$asian, useNA = "ifany")
table(yrbs2007$black, useNA = "ifany")
table(yrbs2007$native_hawaiian, useNA = "ifany")
table(yrbs2007$white, useNA = "ifany")
table(yrbs2007$hispanic, useNA = "ifany")
table(yrbs2007$multiple_hispanic, useNA = "ifany")
table(yrbs2007$multiple_nonhispanic, useNA = "ifany")

table(yrbs2007$raceeth, yrbs2007$american_indian, useNA = "ifany")
table(yrbs2007$raceeth, yrbs2007$asian, useNA = "ifany")
table(yrbs2007$raceeth, yrbs2007$black, useNA = "ifany")
table(yrbs2007$raceeth, yrbs2007$native_hawaiian, useNA = "ifany")
table(yrbs2007$raceeth, yrbs2007$white, useNA = "ifany")
table(yrbs2007$raceeth, yrbs2007$hispanic, useNA = "ifany")
table(yrbs2007$raceeth, yrbs2007$multiple_hispanic, useNA = "ifany")
table(yrbs2007$raceeth, yrbs2007$multiple_nonhispanic, useNA = "ifany")

## 2017

# STEP 1: Examine variable and coding schema

table(yrbs2017$raceeth, useNA = "ifany")

# STEP 2: Create race dummy variables

yrbs2017 <- mutate(yrbs2017, american_indian = ifelse(raceeth == 1, 1, 0))
yrbs2017 <- mutate(yrbs2017, asian = ifelse(raceeth == 2, 1, 0))
yrbs2017 <- mutate(yrbs2017, black = ifelse(raceeth == 3, 1, 0))
yrbs2017 <- mutate(yrbs2017, native_hawaiian = ifelse(raceeth == 4, 1, 0))
yrbs2017 <- mutate(yrbs2017, white = ifelse(raceeth == 5, 1, 0))
yrbs2017 <- mutate(yrbs2017, hispanic = ifelse(raceeth == 6, 1, 0))
yrbs2017 <- mutate(yrbs2017, multiple_hispanic = ifelse(raceeth == 7, 1, 0))
yrbs2017 <- mutate(yrbs2017, multiple_nonhispanic = ifelse(raceeth == 8, 1, 0))

# STEP 3: Confirm creation

table(yrbs2017$american_indian, useNA = "ifany")
table(yrbs2017$asian, useNA = "ifany")
table(yrbs2017$black, useNA = "ifany")
table(yrbs2017$native_hawaiian, useNA = "ifany")
table(yrbs2017$white, useNA = "ifany")
table(yrbs2017$hispanic, useNA = "ifany")
table(yrbs2017$multiple_hispanic, useNA = "ifany")
table(yrbs2017$multiple_nonhispanic, useNA = "ifany")

table(yrbs2017$raceeth, yrbs2017$american_indian, useNA = "ifany")
table(yrbs2017$raceeth, yrbs2017$asian, useNA = "ifany")
table(yrbs2017$raceeth, yrbs2017$black, useNA = "ifany")
table(yrbs2017$raceeth, yrbs2017$native_hawaiian, useNA = "ifany")
table(yrbs2017$raceeth, yrbs2017$white, useNA = "ifany")
table(yrbs2017$raceeth, yrbs2017$hispanic, useNA = "ifany")
table(yrbs2017$raceeth, yrbs2017$multiple_hispanic, useNA = "ifany")
table(yrbs2017$raceeth, yrbs2017$multiple_nonhispanic, useNA = "ifany")

## 2023

# STEP 1: Examine variable and coding schema

table(yrbs2023$raceeth, useNA = "ifany")

# STEP 2: Create race dummy variables

yrbs2023 <- mutate(yrbs2023, american_indian = ifelse(raceeth == 1, 1, 0))
yrbs2023 <- mutate(yrbs2023, asian = ifelse(raceeth == 2, 1, 0))
yrbs2023 <- mutate(yrbs2023, black = ifelse(raceeth == 3, 1, 0))
yrbs2023 <- mutate(yrbs2023, native_hawaiian = ifelse(raceeth == 4, 1, 0))
yrbs2023 <- mutate(yrbs2023, white = ifelse(raceeth == 5, 1, 0))
yrbs2023 <- mutate(yrbs2023, hispanic = ifelse(raceeth == 6, 1, 0))
yrbs2023 <- mutate(yrbs2023, multiple_hispanic = ifelse(raceeth == 7, 1, 0))
yrbs2023 <- mutate(yrbs2023, multiple_nonhispanic = ifelse(raceeth == 8, 1, 0))

# STEP 3: Confirm creation

table(yrbs2023$american_indian, useNA = "ifany")
table(yrbs2023$asian, useNA = "ifany")
table(yrbs2023$black, useNA = "ifany")
table(yrbs2023$native_hawaiian, useNA = "ifany")
table(yrbs2023$white, useNA = "ifany")
table(yrbs2023$hispanic, useNA = "ifany")
table(yrbs2023$multiple_hispanic, useNA = "ifany")
table(yrbs2023$multiple_nonhispanic, useNA = "ifany")

table(yrbs2023$raceeth, yrbs2023$american_indian, useNA = "ifany")
table(yrbs2023$raceeth, yrbs2023$asian, useNA = "ifany")
table(yrbs2023$raceeth, yrbs2023$black, useNA = "ifany")
table(yrbs2023$raceeth, yrbs2023$native_hawaiian, useNA = "ifany")
table(yrbs2023$raceeth, yrbs2023$white, useNA = "ifany")
table(yrbs2023$raceeth, yrbs2023$hispanic, useNA = "ifany")
table(yrbs2023$raceeth, yrbs2023$multiple_hispanic, useNA = "ifany")
table(yrbs2023$raceeth, yrbs2023$multiple_nonhispanic, useNA = "ifany")

############              DEPENDENT VARIABLE: Fighting                   ############

## 1997

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q18, useNA = "ifany")

# STEP 2: Create binary variable for fighting

yrbs1997 <- mutate(yrbs1997, fight = ifelse(Q18 >= 2 & Q18 <= 8, 1, 0))

# STEP 3: Confirm creation

table(yrbs1997$Q18, yrbs1997$fight, useNA = "ifany")


## 2007

# STEP 1: Examine variable and coding schema

table(yrbs2007$q18, useNA = "ifany")

# STEP 2: Create binary variable for fighting

yrbs2007 <- mutate(yrbs2007, fight = ifelse(q18 >= 2 & q18 <= 8, 1, 0))

# STEP 3: Confirm creation

table(yrbs2007$q18, yrbs2007$fight, useNA = "ifany")


## 2017

# STEP 1: Examine variable and coding schema

table(yrbs2017$q17, useNA = "ifany")

# STEP 2: Create binary variable for fighting

yrbs2017 <- mutate(yrbs2017, fight = ifelse(q17 >= 2 & q17 <= 8, 1, 0))

# STEP 3: Confirm creation

table(yrbs2017$q17, yrbs2017$fight, useNA = "ifany")


## 2023

# STEP 1: Examine variable and coding schema

table(yrbs2023$q16, useNA = "ifany")

# STEP 2: Create binary variable for fighting

yrbs2023 <- mutate(yrbs2023, fight = ifelse(q16 >= 2 & q16 <= 8, 1, 0))

# STEP 3: Confirm creation

table(yrbs2023$q16, yrbs2023$fight, useNA = "ifany")

############              DEPENDENT VARIABLE: Alcohol Use                    ############
## 1997

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q39, useNA = "ifany")

# STEP 2: Create binary variable for binge drinking

yrbs1997 <- mutate(yrbs1997, binge_drinking = ifelse(Q39 >= 2 & Q39 <= 7, 1, 0))

# STEP 3: Confirm creation

table(yrbs1997$Q39, yrbs1997$binge_drinking, useNA = "ifany")


## 2007

# STEP 1: Examine variable and coding schema

table(yrbs2007$q42, useNA = "ifany")

# STEP 2: Create binary variable for binge drinking

yrbs2007 <- mutate(yrbs2007, binge_drinking = ifelse(q42 >= 2 & q42 <= 7, 1, 0))

# STEP 3: Confirm creation

table(yrbs2007$q42, yrbs2007$binge_drinking, useNA = "ifany")


## 2017

# STEP 1: Examine variable and coding schema

table(yrbs2017$q44, useNA = "ifany")

# STEP 2: Create binary variable for binge drinking

yrbs2017 <- mutate(yrbs2017, binge_drinking = ifelse(q44 >= 2 & q44 <= 7, 1, 0))

# STEP 3: Confirm creation

table(yrbs2017$q44, yrbs2017$binge_drinking, useNA = "ifany")


## 2023

# STEP 1: Examine variable and coding schema

table(yrbs2023$q43, useNA = "ifany")

# STEP 2: Create binary variable for binge drinking

yrbs2023 <- mutate(yrbs2023, binge_drinking = ifelse(q43 >= 2 & q43 <= 7, 1, 0))

# STEP 3: Confirm creation

table(yrbs2023$q43, yrbs2023$binge_drinking, useNA = "ifany")

############              DEPENDENT VARIABLE: Marijuana Use                    ############

## 1997

# STEP 1: Examine variable and coding schema
table(yrbs1997$Q43, useNA = "ifany")

# STEP 2: Create binary variable for marijuana use
yrbs1997 <- mutate(yrbs1997, marijuana_use = ifelse(Q43 >= 2 & Q43 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs1997$marijuana_use, useNA = "ifany")
table(yrbs1997$Q43, yrbs1997$marijuana_use, useNA = "ifany")


## 2007

# STEP 1: Examine variable and coding schema
table(yrbs2007$q47, useNA = "ifany")

# STEP 2: Create binary variable for marijuana use
yrbs2007 <- mutate(yrbs2007, marijuana_use = ifelse(q47 >= 2 & q47 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs2007$marijuana_use, useNA = "ifany")
table(yrbs2007$q47, yrbs2007$marijuana_use, useNA = "ifany")


## 2017

# STEP 1: Examine variable and coding schema
table(yrbs2017$q48, useNA = "ifany")

# STEP 2: Create binary variable for marijuana use
yrbs2017 <- mutate(yrbs2017, marijuana_use = ifelse(q48 >= 2 & q48 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs2017$marijuana_use, useNA = "ifany")
table(yrbs2017$q48, yrbs2017$marijuana_use, useNA = "ifany")


## 2023

# STEP 1: Examine variable and coding schema
table(yrbs2023$q48, useNA = "ifany")

# STEP 2: Create binary variable for marijuana use
yrbs2023 <- mutate(yrbs2023, marijuana_use = ifelse(q48 >= 2 & q48 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs2023$marijuana_use, useNA = "ifany")
table(yrbs2023$q48, yrbs2023$marijuana_use, useNA = "ifany")


############          DEPENDENT VARIABLE: Lifetime Cocaine Use                      ############

## 1997

# STEP 1: Examine variable and coding schema
table(yrbs1997$Q48, useNA = "ifany")

# STEP 2: Create binary variable for lifetime cocaine use
yrbs1997 <- mutate(yrbs1997, cocaine_use = ifelse(Q48 >= 2 & Q48 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs1997$cocaine_use, useNA = "ifany")
table(yrbs1997$Q48, yrbs1997$cocaine_use, useNA = "ifany")


## 2007

# STEP 1: Examine variable and coding schema
table(yrbs2007$q49, useNA = "ifany")

# STEP 2: Create binary variable for lifetime cocaine use
yrbs2007 <- mutate(yrbs2007, cocaine_use = ifelse(q49 >= 2 & q49 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs2007$cocaine_use, useNA = "ifany")
table(yrbs2007$q49, yrbs2007$cocaine_use, useNA = "ifany")


## 2017

# STEP 1: Examine variable and coding schema
table(yrbs2017$q49, useNA = "ifany")

# STEP 2: Create binary variable for lifetime cocaine use
yrbs2017 <- mutate(yrbs2017, cocaine_use = ifelse(q49 >= 2 & q49 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs2017$cocaine_use, useNA = "ifany")
table(yrbs2017$q49, yrbs2017$cocaine_use, useNA = "ifany")


## 2023

# STEP 1: Examine variable and coding schema
table(yrbs2023$q50, useNA = "ifany")

# STEP 2: Create binary variable for lifetime cocaine use
yrbs2023 <- mutate(yrbs2023, cocaine_use = ifelse(q50 >= 2 & q50 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs2023$cocaine_use, useNA = "ifany")
table(yrbs2023$q50, yrbs2023$cocaine_use, useNA = "ifany")

############              DEPENDENT VARIABLE: Nicotine Use                      ############

## 1997

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q28, useNA = "ifany")

# STEP 2: Create binary variable for nicotine use

yrbs1997 <- mutate(
  yrbs1997,
  cig = ifelse(Q28 >= 2 & Q28 <= 7, 1, 0),
  vaping = NA_real_,
  nicotine_use = cig
)

# STEP 3: Confirm creation

table(yrbs1997$nicotine_use, useNA = "ifany")
table(yrbs1997$Q28, yrbs1997$nicotine_use, useNA = "ifany")


## 2007

# STEP 1: Examine variable and coding schema

table(yrbs2007$q30, useNA = "ifany")

# STEP 2: Create binary variable for nicotine use

yrbs2007 <- mutate(
  yrbs2007,
  cig = ifelse(q30 >= 2 & q30 <= 7, 1, 0),
  vaping = NA_real_,
  nicotine_use = cig
)
# STEP 3: Confirm creation

table(yrbs2007$nicotine_use, useNA = "ifany")
table(yrbs2007$q30, yrbs2007$nicotine_use, useNA = "ifany")

## 2017

# STEP 1: Examine variable and coding schema

table(yrbs2017$q32, useNA = "ifany")
table(yrbs2017$q35, useNA = "ifany")

# STEP 2: Create binary variable for nicotine use
# 1 = cigarette OR vaping use
# 0 = neither cigarette nor vaping use

yrbs2017 <- mutate(
  yrbs2017,
  cig = ifelse(q32 >= 2 & q32 <= 7, 1, 0),
  vaping = ifelse(q35 >= 2 & q35 <= 7, 1, 0),
  nicotine_use = ifelse(
    (q32 >= 2 & q32 <= 7) | (q35 >= 2 & q35 <= 7),
    1, 0
  )
)

# STEP 3: Confirm creation

table(yrbs2017$nicotine_use)
table(yrbs2017$q32, yrbs2017$nicotine_use, useNA = "ifany")
table(yrbs2017$q35, yrbs2017$nicotine_use, useNA = "ifany")

## 2023

# STEP 1: Examine variable and coding schema

table(yrbs2023$q33, useNA = "ifany")
table(yrbs2023$q36, useNA = "ifany")

# STEP 2: Create binary variable for nicotine use
# 1 = cigarette OR vaping use
# 0 = neither cigarette nor vaping use

yrbs2023 <- mutate(
  yrbs2023,
  cig = ifelse(q33 >= 2 & q33 <= 7, 1, 0),
  vaping = ifelse(q36 >= 2 & q36 <= 7, 1, 0),
  nicotine_use = ifelse(
    (q33 >= 2 & q33 <= 7) | (q36 >= 2 & q36 <= 7),
    1, 0
  )
)

# STEP 3: Confirm creation

table(yrbs2023$nicotine_use)
table(yrbs2023$q33, yrbs2023$nicotine_use, useNA = "ifany")
table(yrbs2023$q36, yrbs2023$nicotine_use, useNA = "ifany")

############              WEIGHTS          ############

# Standardize the weight variable name for 1997

# check names of yrbs1997

yrbs1997 <- rename(
  yrbs1997,
  weight = WEIGHT
)

# Check the weight variables for each year

# Weight

summary(yrbs1997$weight)
summary(yrbs2007$weight)
summary(yrbs2017$weight)
summary(yrbs2023$weight)


# Create a common race/ethnicity variable for 1997

yrbs1997 <- mutate(
  yrbs1997,
  race_group = case_when(
    Q4 == 1 ~ "White",
    Q4 == 2 ~ "Black",
    Q4 == 3 ~ "Hispanic",
    Q4 == 4 ~ "Asian/Pacific Islander",
    Q4 == 5 ~ "American Indian/Alaska Native",
    Q4 == 6 ~ "Other/Multiracial",
    TRUE ~ NA_character_
  )
)

# Create a common race/ethnicity variable for 2007

yrbs2007 <- mutate(
  yrbs2007,
  race_group = case_when(
    raceeth == 5 ~ "White",
    raceeth == 3 ~ "Black",
    raceeth %in% c(6, 7) ~ "Hispanic",
    raceeth %in% c(2, 4) ~ "Asian/Pacific Islander",
    raceeth == 1 ~ "American Indian/Alaska Native",
    raceeth == 8 ~ "Other/Multiracial",
    TRUE ~ NA_character_
  )
)

# Create a common race/ethnicity variable for 2017

yrbs2017 <- mutate(
  yrbs2017,
  race_group = case_when(
    raceeth == 5 ~ "White",
    raceeth == 3 ~ "Black",
    raceeth %in% c(6, 7) ~ "Hispanic",
    raceeth %in% c(2, 4) ~ "Asian/Pacific Islander",
    raceeth == 1 ~ "American Indian/Alaska Native",
    raceeth == 8 ~ "Other/Multiracial",
    TRUE ~ NA_character_
  )
)

# Create a common race/ethnicity variable for 2023

yrbs2023 <- mutate(
  yrbs2023,
  race_group = case_when(
    raceeth == 5 ~ "White",
    raceeth == 3 ~ "Black",
    raceeth %in% c(6, 7) ~ "Hispanic",
    raceeth %in% c(2, 4) ~ "Asian/Pacific Islander",
    raceeth == 1 ~ "American Indian/Alaska Native",
    raceeth == 8 ~ "Other/Multiracial",
    TRUE ~ NA_character_
  )
)
