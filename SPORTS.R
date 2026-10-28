## Project:  Senior Thesis - SPORTS
# Located:   SPORTS folder in Google Drive
# File Name: SPORTS.R
# Date:      9/1/2026
# Who:       Jenna Needham supervised by Zachary Kline



############              Pre-Analysis: settings, packages, and data    ############

### Settings and Packages
# set WD for Needham
setwd("G:/.shortcut-targets-by-id/1BNQKSfuws_0ShC-VsiG83yK6DMI5EXrn/SPORTS/work")

# set WD for kline
setwd("G:/My Drive/EDU_SYNC/Research/Active/SPORTS/work")

# install.packages('dplyr', repos = 'https://cloud.r-project.org')
# install.packages("psych")
# install.packages("tidyverse")
# install.packages("patchwork")

library(dplyr)
library(psych)
library(tidyverse)
library(patchwork)


### Load Data
## 1997

yrbs1997 <- read.csv("yrbs1997.csv")

## 2007

yrbs2007 <- read.csv("yrbs2007.csv")

## 2017

yrbs2017 <- read.csv("yrbs2017.csv")

## 2023

yrbs2023 <- read.csv("yrbs2023.csv")


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



############              PHASE 2: CREATE MY DATASET                    ############


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



