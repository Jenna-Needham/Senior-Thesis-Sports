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

## 2007

# STEP 1: Examine variable and coding schema 

table(yrbs2007$q84, useNA = "ifany")

# STEP 2: Create binary sports participation variables 

yrbs2007 <- mutate(yrbs2007, sports_participation = ifelse(q84 == 1, 0, 1))

# STEP 3: Confirm creation

table(yrbs2007$sports_participation, useNA = "ifany")
table(yrbs2007$q84, yrbs2007$sports_participation, useNA = "ifany")

## 2017 
# STEP 1: Examine variable and coding schema

table(yrbs2017$q83, useNA = "ifany")

# STEP 2: Create binary sports participation variables 

yrbs2017 <- mutate(yrbs2017, sports_participation = ifelse(q83 == 1, 0, 1))

# STEP 3: Confirm creation

table(yrbs2017$sports_participation, useNA = "ifany")
table(yrbs2017$q83, yrbs2017$sports_participation, useNA = "ifany")


## 2023
# STEP 1: Examine variable and coding schema

table(yrbs2023$q78, useNA = "ifany")

# STEP 2: Create binary sports participation variables 

yrbs2023 <- mutate(yrbs2023, sports_participation = ifelse(q78 == 1, 0, 1))

# STEP 3: Confirm creation

table(yrbs2023$sports_participation, useNA = "ifany")
table(yrbs2023$q78, yrbs2023$sports_participation, useNA = "ifany")



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

############                    DEPENDENT VARIABLES                   ############
############                        FIGHTING                          ############

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

############                    DEPENDENT VARIABLE                    ############
############                        ALCOHOL USE                       ############

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

############                    DEPENDENT VARIABLE                    ############
############                      MARIJUANA USE                       ############

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


############              DEPENDENT VARIABLE                      ############
############              LIFETIME COCAINE USE                     ############

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

############              DEPENDENT VARIABLE                      ############
############                NICOTINE USE                         ############

## 1997

# STEP 1: Examine variable and coding schema

table(yrbs1997$Q28, useNA = "ifany")

# STEP 2: Create binary variable for nicotine use

yrbs1997 <- mutate(yrbs1997, nicotine_use = ifelse(Q28 >= 2 & Q28 <= 7, 1, 0))

# STEP 3: Confirm creation

table(yrbs1997$nicotine_use, useNA = "ifany")
table(yrbs1997$Q28, yrbs1997$nicotine_use, useNA = "ifany")


## 2007

# STEP 1: Examine variable and coding schema

table(yrbs2007$q30, useNA = "ifany")

# STEP 2: Create binary variable for nicotine use

yrbs2007 <- mutate(yrbs2007, nicotine_use = ifelse(q30 >= 2 & q30 <= 7, 1, 0))

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

yrbs2017 <- mutate(yrbs2017, nicotine_use = ifelse(
  (q32 >= 2 & q32 <= 7) | (q35 >= 2 & q35 <= 7),
  1,
  0
))

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

yrbs2023 <- mutate(yrbs2023, nicotine_use = ifelse(
  (q33 >= 2 & q33 <= 7) | (q36 >= 2 & q36 <= 7),
  1,
  0
))

# STEP 3: Confirm creation

table(yrbs2023$nicotine_use)
table(yrbs2023$q33, yrbs2023$nicotine_use, useNA = "ifany")
table(yrbs2023$q36, yrbs2023$nicotine_use, useNA = "ifany")


####################################################################################
############              PHASE 2: CREATE MY DATASET                    ############
####################################################################################

## STEP 1: Create a list of variables to keep across all four years

my_varlist <- c(
  "sports_participation", "age", "white", "black", "hispanic", "asian", 
  "american_indian", "fight", "binge_drinking", "marijuana_use", 
  "cocaine_use", "nicotine_use"
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


### STEP 2: Create a new dataset for each year
### with only your variables and complete cases

yrbs_complete_case_1997 <- yrbs1997 %>% 
  select(all_of(c(my_varlist, my_varlist_1997))) %>% 
  filter(complete.cases(.))


yrbs_complete_case_2007 <- yrbs2007 %>% 
  select(all_of(c(my_varlist, my_varlist_2007_2017_2023))) %>% 
  filter(complete.cases(.))

yrbs_complete_case_2017 <- yrbs2017 %>% 
  select(all_of(c(my_varlist, my_varlist_2007_2017_2023))) %>%
  filter(complete.cases(.))

yrbs_complete_case_2023 <- yrbs2023 %>% 
  select(all_of(c(my_varlist, my_varlist_2007_2017_2023))) %>%
  filter(complete.cases(.))

## STEP 3: Check the number of complete cases

nrow(yrbs_complete_case_1997) 
nrow(yrbs_complete_case_2007) 
nrow(yrbs_complete_case_2017) 
nrow(yrbs_complete_case_2023)

# proportion missing for each year
missing_97 <- 1 - (nrow(yrbs_complete_case_1997) / nrow(yrbs1997))
missing_07 <- 1 - (nrow(yrbs_complete_case_2007) / nrow(yrbs2007))
missing_17 <- 1 - (nrow(yrbs_complete_case_2017) / nrow(yrbs2017))
missing_23 <- 1 - (nrow(yrbs_complete_case_2023) / nrow(yrbs2023))
                   


### STEP 4: Combine all four years into one dataset

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

### TABLE 1: Descriptive statistics for 1997

describe(yrbs_complete_case_1997)


### TABLE 2: Descriptive statistics for 2007

describe(yrbs_complete_case_2007)


### TABLE 3: Descriptive statistics for 2017

describe(yrbs_complete_case_2017)


### TABLE 4: Descriptive statistics for 2023

describe(yrbs_complete_case_2023)


# figure showing important change trends


# set hypotheses; determine appropriate test; and bring model picture 
# DO CODE LAST! 

####################################################################################
############              PHASE 4: Figures and Trends                  ############
####################################################################################

## Deviant Behaviors

# STEP 1: Calculate the percentage of respondents reporting each deviant behavior (fight/binge_drinking/marijuana_use/cocaine_use) each year

deviance_trends <- my_dataset %>%
  group_by(year) %>%
  summarise(
    fight = mean(fight) * 100,
    binge_drinking = mean(binge_drinking) * 100,
    marijuana_use = mean(marijuana_use) * 100,
    cocaine_use = mean(cocaine_use) * 100
  )

# STEP 2: Reshape data from wide to long format in order to create a separate line for each behavior on the same graph

deviance_trends <- pivot_longer(
  deviance_trends,
  cols = c(fight, binge_drinking, marijuana_use, cocaine_use),
  names_to = "behavior",
  values_to = "percent"
)

# STEP 3: Create a line graph showing trends for all four deviant behaviors

p_deviance <- ggplot(
  deviance_trends,
  aes(
    x = year,
    y = percent,
    color = behavior,
    group = behavior
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Deviant Behaviors",
    x = "Year",
    y = "Percent Reporting Behavior",
    color = "Behavior"
  ) +
  scale_x_continuous(
    breaks = c(1997, 2007, 2017, 2023)
  ) +
  scale_color_brewer(
    palette = "Dark2"
  ) +
  theme_minimal()

## Sports

# STEP 1: Calculate the percentage of respondents participating in sports in each year

sports_trends <- my_dataset %>%
  group_by(year) %>%
  summarise(
    percent = mean(sports_participation) * 100
  )

# STEP 2: Create a line graph showing changes in sports participation across four survey years

p_sports <- ggplot(
  sports_trends,
  aes(
    x = year,
    y = percent
  )
) +
  geom_line(
    linewidth = 1,
    color = "steelblue"
  ) +
  geom_point(
    size = 2,
    color = "steelblue"
  ) +
  labs(
    title = "Sports Participation",
    x = "Year",
    y = "Percent Participating in Sports"
  ) +
  scale_x_continuous(
    breaks = c(1997, 2007, 2017, 2023)
  ) +
  theme_minimal()



## Nicotine

# STEP 1: Calculate the percentage of respondents reporting nicotine use in each year

nicotine_trends <- my_dataset %>%
  group_by(year) %>%
  summarise(
    percent = mean(nicotine_use) * 100
  )

# STEP 2: Create a line graph showing changes in nicotine use across the four years

p_nicotine <- ggplot(
  nicotine_trends,
  aes(
    x = year,
    y = percent
  )
) +
  geom_line(
    linewidth = 1,
    color = "firebrick"
  ) +
  geom_point(
    size = 2,
    color = "firebrick"
  ) +
  labs(
    title = "Nicotine Use",
    x = "Year",
    y = "Percent Reporting Nicotine Use"
  ) +
  scale_x_continuous(
    breaks = c(1997, 2007, 2017, 2023)
  ) +
  ylim(0, 100) +
  theme_minimal()


## Race

# STEP 1: Calculate the percentage of respondents in each race category within each survey year

race_trends <- my_dataset %>%
  group_by(year) %>%
  summarise(
    white = mean(white) * 100,
    black = mean(black) * 100,
    hispanic = mean(hispanic) * 100,
    asian = mean(asian) * 100,
    american_indian = mean(american_indian) * 100
  )

# STEP 2: Reshape the race data from wide to long format in order to create a separate stacked area for each race category

race_trends <- race_trends %>%
  pivot_longer(
    cols = -year,
    names_to = "race",
    values_to = "percent"
  )

# STEP 3: Create a stacked area chart showing changes in the racial composition of the sample across survey years

p_race <- ggplot(
  race_trends,
  aes(
    x = year,
    y = percent,
    fill = race
  )
) +
  geom_area(position = "stack") +
  labs(
    title = "Race Composition",
    x = "Year",
    y = "Percent",
    fill = "Race"
  ) +
  scale_x_continuous(
    breaks = c(1997, 2007, 2017, 2023)
  ) +
  scale_fill_brewer(
    palette = "Set2"
  ) +
  theme_minimal()


####################################################################################
############                   COMBINE ALL FOUR GRAPHS               ############
####################################################################################

# Use patchwork to arrange the four graphs into one 2 x 2 figure

combined_trends <- (
  p_deviance | p_sports
) / (
  p_nicotine | p_race
)

# Display the combined figure

combined_trends





####################################################################################
############              Add weights                     ############
####################################################################################
# adding weights will have to be in earlier portions of the code 
# this note is just a reminder


####################################################################################
############                   Compare our demographics with benchmarks               ############
####################################################################################

# to what extent is our sample different from the national population of high school students in the U.S.?
# 1) Look at weighted racial composition in the whole sample 
# and compare to the weighted racial composition of our selected sample 
# for example, given missingness

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

