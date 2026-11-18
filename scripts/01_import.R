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
