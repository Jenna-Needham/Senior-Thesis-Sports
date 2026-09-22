## Project:  Senior Thesis - SPORTS
# Located:   SPORTS folder in Google Drive
# File Name: SPORTS.R
# Date:      9/1/2026
# Who:       Jenna Needham supervised by Zachary Kline


####################################################################################
############              Pre-Analysis: settings, packages, and data    ############
####################################################################################

### Settings and Packages
# set WD for Needham
setwd("G:/My Drive/Senior Thesis/SPORTS/work")

# set WD for kline
setwd("G:/My Drive/EDU_SYNC/Research/Active/SPORTS/work")

install.packages('dplyr', repos = 'https://cloud.r-project.org')
install.packages("psych")
install.packages("tidyverse")

library(dplyr)
library(psych)
library(tidyverse)


### Load Data for Needham
## 1997

yrbs1997 <- read.csv("yrbs1997.csv")

## 2007

yrbs2007 <- read.csv("yrbs2007.csv")

## 2017

yrbs2017 <- read.csv("yrbs2017.csv")

## 2023

yrbs2023 <- read.csv("yrbs2023.csv")


####################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############
####################################################################################

# follow three steps of cleaning data for each variable for each year
# step 1: examine the variable
# Step 2: Clean the variable (always create a new variable!)
# Step 3: Confirm cleaning was correct


############                     INDEPENDENT VARIABLE                     ############
############                       Sports Participation                   ############

## 1997

## AMOUNT OF TEAMS PLAYED FOR RUN BY SCHOOL

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q83)

# STEP 2: Create dummy variables for each category of sports participation

yrbs1997 <- mutate(yrbs1997, no_sport = ifelse(Q83 == 1, 1, 0))
yrbs1997 <- mutate(yrbs1997, one_sport = ifelse(Q83 == 2, 1, 0))
yrbs1997 <- mutate(yrbs1997, two_sport = ifelse(Q83 == 3, 1, 0))
yrbs1997 <- mutate(yrbs1997, three_or_more_sport = ifelse(Q83 == 4, 1, 0))

# STEP 3: Confirm creation at Q83 and each dummy variable

table(yrbs1997$no_sport)
table(yrbs1997$one_sport)
table(yrbs1997$two_sport)
table(yrbs1997$three_or_more_sport)

table(yrbs1997$Q83, yrbs1997$no_sport)
table(yrbs1997$Q83, yrbs1997$one_sport)
table(yrbs1997$Q83, yrbs1997$two_sport)
table(yrbs1997$Q83, yrbs1997$three_or_more_sport)

## AMOUNT OF TEAMS PLAYED FOR ORGS OUTSIDE OF SCHOOL

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q84)

# STEP 2: Create dummy variables for each category of sports participation

yrbs1997 <- mutate(yrbs1997, no_sport_org = ifelse(Q84 == 1, 1, 0))
yrbs1997 <- mutate(yrbs1997, one_sport_org = ifelse(Q84 == 2, 1, 0))
yrbs1997 <- mutate(yrbs1997, two_sport_org = ifelse(Q84 == 3, 1, 0))
yrbs1997 <- mutate(yrbs1997, three_or_more_sport_org = ifelse(Q84 == 4, 1, 0))

# STEP 3: Confirm creation at Q84 and each dummy variable

table(yrbs1997$no_sport_org)
table(yrbs1997$one_sport_org)
table(yrbs1997$two_sport_org)
table(yrbs1997$three_or_more_sport_org)

table(yrbs1997$Q84, yrbs1997$no_sport_org)
table(yrbs1997$Q84, yrbs1997$one_sport_org)
table(yrbs1997$Q84, yrbs1997$two_sport_org)
table(yrbs1997$Q84, yrbs1997$three_or_more_sport_org)

## 2007

# STEP 1: Examine variable and coding schema 

table(yrbs2007$q83)

yrbs2007 <- mutate(yrbs2007, no_sport = ifelse(q83 == 1, 1, 0))
yrbs2007 <- mutate(yrbs2007, one_sport = ifelse(q83 == 2, 1, 0))
yrbs2007 <- mutate(yrbs2007, two_sport = ifelse(q83 == 3, 1, 0))
yrbs2007 <- mutate(yrbs2007, three_or_more_sport = ifelse(q83 == 4, 1, 0))

table(yrbs2007$no_sport)
table(yrbs2007$one_sport)
table(yrbs2007$two_sport)
table(yrbs2007$three_or_more_sport)

table(yrbs2007$q83, yrbs2007$no_sport)
table(yrbs2007$q83, yrbs2007$one_sport)
table(yrbs2007$q83, yrbs2007$two_sport)
table(yrbs2007$q83, yrbs2007$three_or_more_sport)

## 2017 
# STEP 1: Examine variable and coding schema

table(yrbs2017$q83)


# STEP 2: Create dummy variables for each category of sports participation

yrbs2017 <- mutate(yrbs2017, no_sport = ifelse(q83 == 1, 1, 0))
yrbs2017 <- mutate(yrbs2017, one_sport = ifelse(q83 == 2, 1, 0))
yrbs2017 <- mutate(yrbs2017, two_sport = ifelse(q83 == 3, 1, 0))
yrbs2017 <- mutate(yrbs2017, three_or_more_sport = ifelse(q83 == 4, 1, 0))

# STEP 3: Confirm creation at q83 and each dummy variable

table(yrbs2017$no_sport)
table(yrbs2017$one_sport)
table(yrbs2017$two_sport)
table(yrbs2017$three_or_more_sport)

table(yrbs2017$q83, yrbs2017$no_sport)
table(yrbs2017$q83, yrbs2017$one_sport)
table(yrbs2017$q83, yrbs2017$two_sport)
table(yrbs2017$q83, yrbs2017$three_or_more_sport)


## 2023
# STEP 1: Examine variable and coding schema

table(yrbs2023$q78)

# STEP 2: Create dummy variables for each category of sports participation

yrbs2023 <- mutate(yrbs2023, no_sport = ifelse(q78 == 1, 1, 0))
yrbs2023 <- mutate(yrbs2023, one_sport = ifelse(q78 == 2, 1, 0))
yrbs2023 <- mutate(yrbs2023, two_sport = ifelse(q78 == 3, 1, 0))
yrbs2023 <- mutate(yrbs2023, three_or_more_sport = ifelse(q78 == 4, 1, 0))

# STEP 3: Confirm creation at q78 and each dummy variable

table(yrbs2023$no_sport)
table(yrbs2023$one_sport)
table(yrbs2023$two_sport)
table(yrbs2023$three_or_more_sport)

table(yrbs2023$q78, yrbs2023$no_sport)
table(yrbs2023$q78, yrbs2023$one_sport)
table(yrbs2023$q78, yrbs2023$two_sport)
table(yrbs2023$q78, yrbs2023$three_or_more_sport)


############                    DEMOGRAPHIC VARIABLES                   ############

### AGE ###

## 1997 ##

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q1)

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

table(yrbs1997$Q1, yrbs1997$age)
table(yrbs1997$age, useNA = "ifany")

## 2007 ##

# STEP 1: Examine variable and coding schema

table(yrbs2007$q1)

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

table(yrbs2007$q1, yrbs2007$age)
table(yrbs2007$age, useNA = "ifany")

## 2017 ##

# STEP 1: Examine variable and coding schema

table(yrbs2017$q1)

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

table(yrbs2017$q1, yrbs2017$age)
table(yrbs2017$age, useNA = "ifany")

## 2023 ##

# STEP 1: Examine variable and coding schema

table(yrbs2023$q1)

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

table(yrbs2023$q1, yrbs2023$age)
table(yrbs2023$age, useNA = "ifany")


### GENDER ###

## 1997

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q2, useNA = "ifany")

# STEP 2: Create female dummy variable
# Reference category = male

yrbs1997 <- mutate(yrbs1997, female = ifelse(Q2 == 1, 1, 0))

# STEP 3: Confirm creation

table(yrbs1997$Q2, yrbs1997$female)


## 2007

# STEP 1: Examine variable and coding schema

table(yrbs2007$q2, useNA = "ifany")

# STEP 2: Create female dummy variable
# Reference category = male

yrbs2007 <- mutate(yrbs2007, female = ifelse(q2 == 1, 1, 0))

# STEP 3: Confirm creation

table(yrbs2007$q2, yrbs2007$female)


## 2017

# STEP 1: Examine variable and coding schema

table(yrbs2017$q2, useNA = "ifany")

# STEP 2: Create female dummy variable
# Reference category = male

yrbs2017 <- mutate(yrbs2017, female = ifelse(q2 == 1, 1, 0))

# STEP 3: Confirm creation

table(yrbs2017$q2, yrbs2017$female)


## 2023

# STEP 1: Examine variable and coding schema

table(yrbs2023$q2, useNA = "ifany")

# STEP 2: Create female dummy variable
# Reference category = male

yrbs2023 <- mutate(yrbs2023, female = ifelse(q2 == 1, 1, 0))

# STEP 3: Confirm creation

table(yrbs2023$q2, yrbs2023$female)

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

table(yrbs1997$white)
table(yrbs1997$black)
table(yrbs1997$hispanic)
table(yrbs1997$asian)
table(yrbs1997$american_indian)
table(yrbs1997$other_race)

table(yrbs1997$Q4, yrbs1997$white)
table(yrbs1997$Q4, yrbs1997$black)
table(yrbs1997$Q4, yrbs1997$hispanic)
table(yrbs1997$Q4, yrbs1997$asian)
table(yrbs1997$Q4, yrbs1997$american_indian)
table(yrbs1997$Q4, yrbs1997$other_race)

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

table(yrbs2007$american_indian)
table(yrbs2007$asian)
table(yrbs2007$black)
table(yrbs2007$native_hawaiian)
table(yrbs2007$white)
table(yrbs2007$hispanic)
table(yrbs2007$multiple_hispanic)
table(yrbs2007$multiple_nonhispanic)

table(yrbs2007$raceeth, yrbs2007$american_indian)
table(yrbs2007$raceeth, yrbs2007$asian)
table(yrbs2007$raceeth, yrbs2007$black)
table(yrbs2007$raceeth, yrbs2007$native_hawaiian)
table(yrbs2007$raceeth, yrbs2007$white)
table(yrbs2007$raceeth, yrbs2007$hispanic)
table(yrbs2007$raceeth, yrbs2007$multiple_hispanic)
table(yrbs2007$raceeth, yrbs2007$multiple_nonhispanic)

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

table(yrbs2017$american_indian)
table(yrbs2017$asian)
table(yrbs2017$black)
table(yrbs2017$native_hawaiian)
table(yrbs2017$white)
table(yrbs2017$hispanic)
table(yrbs2017$multiple_hispanic)
table(yrbs2017$multiple_nonhispanic)

table(yrbs2017$raceeth, yrbs2017$american_indian)
table(yrbs2017$raceeth, yrbs2017$asian)
table(yrbs2017$raceeth, yrbs2017$black)
table(yrbs2017$raceeth, yrbs2017$native_hawaiian)
table(yrbs2017$raceeth, yrbs2017$white)
table(yrbs2017$raceeth, yrbs2017$hispanic)
table(yrbs2017$raceeth, yrbs2017$multiple_hispanic)
table(yrbs2017$raceeth, yrbs2017$multiple_nonhispanic)

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

table(yrbs2023$american_indian)
table(yrbs2023$asian)
table(yrbs2023$black)
table(yrbs2023$native_hawaiian)
table(yrbs2023$white)
table(yrbs2023$hispanic)
table(yrbs2023$multiple_hispanic)
table(yrbs2023$multiple_nonhispanic)

table(yrbs2023$raceeth, yrbs2023$american_indian)
table(yrbs2023$raceeth, yrbs2023$asian)
table(yrbs2023$raceeth, yrbs2023$black)
table(yrbs2023$raceeth, yrbs2023$native_hawaiian)
table(yrbs2023$raceeth, yrbs2023$white)
table(yrbs2023$raceeth, yrbs2023$hispanic)
table(yrbs2023$raceeth, yrbs2023$multiple_hispanic)
table(yrbs2023$raceeth, yrbs2023$multiple_nonhispanic)

############                    DEPENDENT VARIABLES                   ############
############                        FIGHTING                          ############

## 1997

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q18, useNA = "ifany")

# STEP 2: Create binary variable for fighting

yrbs1997 <- mutate(yrbs1997, fight = ifelse(Q18 >= 2 & Q18 <= 8, 1, 0))

# STEP 3: Confirm creation

table(yrbs1997$Q18, yrbs1997$fight)


## 2007

# STEP 1: Examine variable and coding schema

table(yrbs2007$q18, useNA = "ifany")

# STEP 2: Create binary variable for fighting

yrbs2007 <- mutate(yrbs2007, fight = ifelse(q18 >= 2 & q18 <= 8, 1, 0))

# STEP 3: Confirm creation

table(yrbs2007$q18, yrbs2007$fight)


## 2017

# STEP 1: Examine variable and coding schema

table(yrbs2017$q17, useNA = "ifany")

# STEP 2: Create binary variable for fighting

yrbs2017 <- mutate(yrbs2017, fight = ifelse(q17 >= 2 & q17 <= 8, 1, 0))

# STEP 3: Confirm creation

table(yrbs2017$q17, yrbs2017$fight)


## 2023

# STEP 1: Examine variable and coding schema

table(yrbs2023$q16, useNA = "ifany")

# STEP 2: Create binary variable for fighting

yrbs2023 <- mutate(yrbs2023, fight = ifelse(q16 >= 2 & q16 <= 8, 1, 0))

# STEP 3: Confirm creation

table(yrbs2023$q16, yrbs2023$fight)

############                    DEPENDENT VARIABLE                    ############
############                        ALCOHOL                           ############

## 1997

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q39, useNA = "ifany")

# STEP 2: Create binary variable for binge drinking

yrbs1997 <- mutate(yrbs1997, binge_drinking = ifelse(Q39 >= 2 & Q39 <= 7, 1, 0))

# STEP 3: Confirm creation

table(yrbs1997$Q39, yrbs1997$binge_drinking)


## 2007

# STEP 1: Examine variable and coding schema

table(yrbs2007$q42, useNA = "ifany")

# STEP 2: Create binary variable for binge drinking

yrbs2007 <- mutate(yrbs2007, binge_drinking = ifelse(q42 >= 2 & q42 <= 7, 1, 0))

# STEP 3: Confirm creation

table(yrbs2007$q42, yrbs2007$binge_drinking)


## 2017

# STEP 1: Examine variable and coding schema

table(yrbs2017$q44, useNA = "ifany")

# STEP 2: Create binary variable for binge drinking

yrbs2017 <- mutate(yrbs2017, binge_drinking = ifelse(q44 >= 2 & q44 <= 7, 1, 0))

# STEP 3: Confirm creation

table(yrbs2017$q44, yrbs2017$binge_drinking)


## 2023

# STEP 1: Examine variable and coding schema

table(yrbs2023$q43, useNA = "ifany")

# STEP 2: Create binary variable for binge drinking

yrbs2023 <- mutate(yrbs2023, binge_drinking = ifelse(q43 >= 2 & q43 <= 7, 1, 0))

# STEP 3: Confirm creation

table(yrbs2023$q43, yrbs2023$binge_drinking)

############                    DEPENDENT VARIABLE                    ############
############                        MARIJUANA                         ############

## 1997

# STEP 1: Examine variable and coding schema
table(yrbs1997$Q43)

# STEP 2: Create binary variable for marijuana use
yrbs1997 <- mutate(yrbs1997, marijuana_use = ifelse(Q43 >= 2 & Q43 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs1997$marijuana_use)
table(yrbs1997$Q43, yrbs1997$marijuana_use)


## 2007

# STEP 1: Examine variable and coding schema
table(yrbs2007$q47)

# STEP 2: Create binary variable for marijuana use
yrbs2007 <- mutate(yrbs2007, marijuana_use = ifelse(q47 >= 2 & q47 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs2007$marijuana_use)
table(yrbs2007$q47, yrbs2007$marijuana_use)


## 2017

# STEP 1: Examine variable and coding schema
table(yrbs2017$q48)

# STEP 2: Create binary variable for marijuana use
yrbs2017 <- mutate(yrbs2017, marijuana_use = ifelse(q48 >= 2 & q48 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs2017$marijuana_use)
table(yrbs2017$q48, yrbs2017$marijuana_use)


## 2023

# STEP 1: Examine variable and coding schema
table(yrbs2023$q48)

# STEP 2: Create binary variable for marijuana use
yrbs2023 <- mutate(yrbs2023, marijuana_use = ifelse(q48 >= 2 & q48 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs2023$marijuana_use)
table(yrbs2023$q48, yrbs2023$marijuana_use)


############              DEPENDENT VARIABLE                      ############
############              LIFETIME COCAINE USE                     ############

## 1997

# STEP 1: Examine variable and coding schema
table(yrbs1997$Q48)

# STEP 2: Create binary variable for lifetime cocaine use
yrbs1997 <- mutate(yrbs1997, cocaine_use = ifelse(Q48 >= 2 & Q48 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs1997$cocaine_use)
table(yrbs1997$Q48, yrbs1997$cocaine_use)


## 2007

# STEP 1: Examine variable and coding schema
table(yrbs2007$q49)

# STEP 2: Create binary variable for lifetime cocaine use
yrbs2007 <- mutate(yrbs2007, cocaine_use = ifelse(q49 >= 2 & q49 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs2007$cocaine_use)
table(yrbs2007$q49, yrbs2007$cocaine_use)


## 2017

# STEP 1: Examine variable and coding schema
table(yrbs2017$q49)

# STEP 2: Create binary variable for lifetime cocaine use
yrbs2017 <- mutate(yrbs2017, cocaine_use = ifelse(q49 >= 2 & q49 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs2017$cocaine_use)
table(yrbs2017$q49, yrbs2017$cocaine_use)


## 2023

# STEP 1: Examine variable and coding schema
table(yrbs2023$q50)

# STEP 2: Create binary variable for lifetime cocaine use
yrbs2023 <- mutate(yrbs2023, cocaine_use = ifelse(q50 >= 2 & q50 <= 6, 1, 0))

# STEP 3: Confirm creation
table(yrbs2023$cocaine_use)
table(yrbs2023$q50, yrbs2023$cocaine_use)

####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

### STEP 1: Create a list of variables to keep

my_varlist <- c("no_sport", "one_sport", "two_sport", "three_or_more_sport",
                "no_sport_org", "one_sport_org", "two_sport_org", "three_or_more_sport_org", 
                "age", "white", "black", "hispanic", "asian", "american_indian", "other_race", 
                "native_hawaiian", "multiple_hispanic", "multiple_nonhispanic", "fight", 
                "binge_drinking", "marijuana_use", "cocaine_use")


# Variables specific to 1997
my_varlist_1997 <- c(
  "no_sport_org",
  "one_sport_org",
  "two_sport_org",
  "three_or_more_sport_org",
  "other_race"
)

# Variables used in 2007, 2017, and 2023
my_varlist_2007_2023 <- c(
  "native_hawaiian",
  "multiple_hispanic",
  "multiple_nonhispanic"
)


### STEP 2: Create a new dataset for each year
### with only your variables and complete cases

yrbs_complete_case_1997 <- yrbs1997 %>%
  select(all_of(c(my_varlist, my_varlist_1997))) %>%
  filter(complete.cases(.))

yrbs_complete_case_2007 <- yrbs2007 %>%
  select(all_of(c(my_varlist, my_varlist_2007_2023))) %>%
  filter(complete.cases(.))

yrbs_complete_case_2017 <- yrbs2017 %>%
  select(all_of(c(my_varlist, my_varlist_2007_2023))) %>%
  filter(complete.cases(.))

yrbs_complete_case_2023 <- yrbs2023 %>%
  select(all_of(c(my_varlist, my_varlist_2007_2023))) %>%
  filter(complete.cases(.))


### STEP 3: Combine all four years into one dataset

my_dataset <- bind_rows(
  yrbs_complete_case_1997 %>% mutate(year = 1997),
  yrbs_complete_case_2007 %>% mutate(year = 2007),
  yrbs_complete_case_2017 %>% mutate(year = 2017),
  yrbs_complete_case_2023 %>% mutate(year = 2023)
)

### STEP 4: Gather summary statistics and confirm valid dataset construction

describe(my_dataset)
table(my_dataset$year)

####################################################################################
############              PHASE 3: Descriptive Statistics     ############
####################################################################################
# TABLE 1: Use describe command 

describe(my_dataset)
