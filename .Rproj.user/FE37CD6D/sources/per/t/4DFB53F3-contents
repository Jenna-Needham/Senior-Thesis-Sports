####################################################################################
############       Liu & Sydnor (2022) 2015 Replication        ############
####################################################################################


### Settings + Packages
# Kline's command to set WD
setwd("G:/My Drive/EDU_SYNC/Research/Active/HDHP/work")

# Jenna's command to set up WD
setwd("G:/.shortcut-targets-by-id/14oLkrWtHW1NzX87aL0DxDGo_9Ysj-XBQ/HDHP/work")

#install.packages("dplyr")
#install.packages("haven")
#install.packages("tidyr")

library(dplyr)
library(tidyr)
library(haven)

# Step 1: Load the data

ls2015 <- read_sav('health benefits 2015.sav')

# Step 2: Give every firm a unique ID 
# each row in this dataset represents one firm

ls2015 <- ls2015 %>%
  mutate(
    firm = row_number()
  )

# Step 3: Identify which firms offer an HDHP 
# in the original data: 1 or 2 = firm offers HDHP 
# we turn that into 1 = offers HDHP and 0 = does not offer HDHP 

ls2015 <- ls2015 %>%
  mutate(
    hdhp3 = if_else(hdhp %in% c(1, 2), 1, 0)
  )

# Step 4: Identify what combination of plans each firm offers
# the variables hmo, ppo, and pos tell us whether the firm offers an HMO, PPO or POS plan
# pty = plan type combination

ls2015 <- ls2015 %>%
  mutate(
    pty = case_when(
      
      # Only HDHP
      hdhp3 == 1 & hmo == 0 & ppo == 0 & pos == 0 ~ 1,
      
      # Only HMO
      hdhp3 == 0 & hmo == 1 & ppo == 0 & pos == 0 ~ 2,
      
      # Only PPO
      hdhp3 == 0 & hmo == 0 & ppo == 1 & pos == 0 ~ 3,
      
      # Only POS
      hdhp3 == 0 & hmo == 0 & ppo == 0 & pos == 1 ~ 4,
      
      # HDHP + HMO
      hdhp3 == 1 & hmo == 1 & ppo == 0 & pos == 0 ~ 5,
      
      # HDHP + PPO
      hdhp3 == 1 & hmo == 0 & ppo == 1 & pos == 0 ~ 6,
      
      # HDHP + POS
      hdhp3 == 1 & hmo == 0 & ppo == 0 & pos == 1 ~ 7,
      
      # HMO + PPO
      hdhp3 == 0 & hmo == 1 & ppo == 1 & pos == 0 ~ 8,
      
      # HMO + POS
      hdhp3 == 0 & hmo == 1 & ppo == 0 & pos == 1 ~ 9,
      
      # PPO + POS
      hdhp3 == 0 & hmo == 0 & ppo == 1 & pos == 1 ~ 10,
      
      # HMO + PPO + POS
      hdhp3 == 0 & hmo == 1 & ppo == 1 & pos == 1 ~ 11,
      
      # HDHP + PPO + POS
      hdhp3 == 1 & hmo == 0 & ppo == 1 & pos == 1 ~ 12,
      
      # HDHP + HMO + POS
      hdhp3 == 1 & hmo == 1 & ppo == 0 & pos == 1 ~ 13,
      
      # HDHP + HMO + PPO
      hdhp3 == 1 & hmo == 1 & ppo == 1 & pos == 0 ~ 14,
      
      # HDHP + HMO + PPO + POS
      hdhp3 == 1 & hmo == 1 & ppo == 1 & pos == 1 ~ 15,
      
      # Anything that does not fit the combinations above
      TRUE ~ 0
    )
  )

# Step 5: Keep only firms offering HDHP and exactly ONE other plan
# We only want firms with 
# pty = 5 -> HDHP + HMO
# pty = 6 -> HDHP + PPO 
# pty = 7 -> HDHP + POS

ls2015 <- ls2015 %>%
  filter(pty %in% c(5, 6, 7))

# Check how many firms remain (the Liu and Sydnor sample should have 417 firms)

cat(
  "Firms offering HDHP + exactly one other plan:",
  nrow(ls2015),
  "\n"
)

# Step 6: Remove firms with missing HSA/HRA contribution information
# variables g43 and g41 contain information about employer contributions 
# if both are missing, we do not have the contribution information needed for analysis, so we remove firms where both are missing
# we keep firms where at least one is available 

ls2015 <- ls2015 %>%
  filter(
    !(is.na(g43) & is.na(g41))
  )

cat(
  "After contribution-information exclusion:",
  nrow(ls2015),
  "\n"
)

# Step 7: Remove firms with missing policy information
# e6, f6, and d6 contain policy information for the non-HDHP plan slots
# g6 contains the corresponding HDHP policy information 
# so we remove a firm when 
# all of d6, e6, and f6 are missing OR g6 is missing (so we need policy info for both the non HDHP and the HDHP)

ls2015 <- ls2015 %>%
  filter(
    !(
      (is.na(e6) & is.na(f6) & is.na(d6)) |
        is.na(g6)
    )
  )

cat(
  "After policy-information exclusion:",
  nrow(ls2015),
  "\n"
)

# Step 8: Remove firms with missing MOOP information 
# MOOP - maximum out-of-pocket limit
# g9 (HDHP MOOP information), e9, d9, and f9 are the possible non-HDHP MOOP variables
# so we remove a firm when
# g9 is missing OR ALL of e9, d9, and f9 are missing (so we need MOOP info for both plans)

ls2015 <- ls2015 %>%
  filter(
    !(
      is.na(g9) |
        (is.na(e9) & is.na(d9) & is.na(f9))
    )
  )

cat(
  "After missing-MOOP exclusion:",
  nrow(ls2015),
  "\n"
)

# Step 9: Remove firms with unknown or very high MOOP 
# Liu and Sydnor remove firms when the MOOP info is unknown or above $6,850 (limit was the relevant HDHP legal cap for 2015)
# which variable is checked depends on the firm's plan combination 
# pty = 5 -> use d9 / d9bx
# pty = 6 --> use e9 / e9bx
# pty = 7 --> use f9 / f9bx
# g9 / g9bx are used for the HDHP.

# HDHP + PPO
ls2015 <- ls2015 %>%
  filter(
    !(
      (e9 == 2 | e9bx > 6850) &
        pty == 6
    )
  )


# HDHP + HMO
ls2015 <- ls2015 %>%
  filter(
    !(
      (d9 == 2 | d9bx > 6850) &
        pty == 5
    )
  )


# HDHP + POS
ls2015 <- ls2015 %>%
  filter(
    !(
      (f9 == 2 | f9bx > 6850) &
        pty == 7
    )
  )


# HDHP itself
ls2015 <- ls2015 %>%
  filter(
    !(g9 == 2 | g9bx > 6850)
  )


# Step 10: Remove firms with too many plan-policy blocks filled in
# a firm in this sample should have info for 1 non HDHP plan and one HDHP plan
# d6, e6, and f6 are the possible slots for the non HDHP plan
# so we remove firms where MORE THAN ONE of those non HDHP slots is filled in 

ls2015 <- ls2015 %>%
  filter(
    !(
      !is.na(e6) &
        (!is.na(d6) | !is.na(f6))
    )
  )

ls2015 <- ls2015 %>%
  filter(
    !(
      !is.na(d6) &
        (!is.na(e6) | !is.na(f6))
    )
  )

ls2015 <- ls2015 %>%
  filter(
    !(
      !is.na(f6) &
        (!is.na(d6) | !is.na(e6))
    )
  )

# Step 11: Remove one known data-entry problem 
# the original stata code specifically removes firm 560 because the same two plans were entered twice

ls2015 <- ls2015 %>%
  filter(firm != 560)

# Step 12: Check sample size

cat(
  "Firms remaining after these exclusions:",
  nrow(ls2015),
  "\n"
)

####################################################################################
############       SECTION 2: Restructure the data from wide to long     ###############
####################################################################################


# Up to this point, each row represents one firm
# but each firm can have information for several plan types in different columns
# so we use reshape long so that each row represents one firm-plan instead of one firm

# Step 13: Create variables used in the reshape 
# the original Stata code creates g2 and g4 as empty variables before renaming plan-specific variables

if (!"g2" %in% names(ls2015)) {
  ls2015$g2 <- NA_real_
}

if (!"g4" %in% names(ls2015)) {
  ls2015$g4 <- NA_real_
}

# Step 14: Read the variable-renaming instructions from the STATA file (the .do file)
# readLines() lets us bring those commands into R as text (the .do file is a text file with all the commands)

do_file <- readLines(
  "clean_2015.do",
  warn = FALSE
)

# Find only the lines that have a rename command, which we need to translate into the plan-specific variable names

rename_lines <- do_file[
  grepl(
    "^\\s*rename\\s+",
    do_file
  )
]

# Convert rename commands into a data frame 
# "old" = the original KFF variable name 
# "new" = the name Liu and Sydnor gave the variable in Stata
# For example:
# d1   -> fr11
# e1   -> fr12
# f1   -> fr13
# g1   -> fr14
# The last digit tells us which plan type the variable belongs t

rename_map <- do.call(
  rbind,
  lapply(
    rename_lines,
    function(x) {
      
      parts <- strsplit(
        trimws(x),
        "\\s+"
      )[[1]]
      
      data.frame(
        old = parts[2],
        new = parts[3],
        stringsAsFactors = FALSE
      )
    }
  )
)

rownames(rename_map) <- NULL

# Look at the first few rename instructions to make sure the Stata file was read correctly 

head(rename_map)

# The original file contains a typo
# it first says "rename g40 ddct44", but g4 was created above and g40 is later renamed to cs404.
# The intended mapping is:
# g4 -> ddct44
# g40 -> cs404

rename_map <- rename_map %>%
  filter(
    !(old == "g40" & new == "ddct44")
  )

rename_map <- rbind(
  rename_map,
  data.frame(
    old = "g4",
    new = "ddct44",
    stringsAsFactors = FALSE
  )
)

# Check whether any original variable is being renamed more than once (this should return no rows) 

rename_map %>%
  count(old) %>%
  filter(n > 1)

# Step 15: Replace the numeric plan suffix with a readable plan name
# The authors use numbers at the end of their variable names:
# 1 = HMO
# 2 = PPO
# 3 = POS
# 4 = HDHP
# For our  dataset, we are going to replace those numbers with readable plan names instead
# Example:
# prmsa1 -> prmsa_hmo
# prmsa2 -> prmsa_ppo
# prmsa3 -> prmsa_pos
# prmsa4 -> prmsa_hdp

rename_map$plan <- case_when(
  grepl("1$", rename_map$new) ~ "hmo",
  grepl("2$", rename_map$new) ~ "ppo",
  grepl("3$", rename_map$new) ~ "pos",
  grepl("4$", rename_map$new) ~ "hdp"
)

# Remove the final plan number from the variable name
# Example:
# prmsa1 -> prmsa
# ddcts4 -> ddcts

rename_map$stem <- sub(
  "[1-4]$",
  "",
  rename_map$new
)

# Put the readable plan name back onto the variable
# Example:
# prmsa + hmo -> prmsa_hmo
# ddcts + hdp -> ddcts_hdp

rename_map$readable <- paste0(
  rename_map$stem,
  "_",
  rename_map$plan
)

# Check the first few readable names

head(rename_map, 20)

# Step 16: Rename the plan-specific variables
# setNames() creates a named vector telling rename() that readable_name = original_name
# This is the R equivalent of the many rename commands in the original Stata file.

ls2015 <- ls2015 %>%
  rename(all_of(setNames(rename_map$old, rename_map$readable)))

# Step 17: Reshape the data from one row per firm to one row per firm-plan

plan_columns <- rename_map$readable

ls2015_long <- ls2015 %>%
  pivot_longer(
    cols = all_of(plan_columns),
    names_to = c(".value", "plan_type"),
    names_pattern = "^(.*)_(hmo|ppo|pos|hdp)$"
  )

# Step 18: Label the plan types so it is easier to read
# At this point plan_type contains "hmo", "ppo", "pos", or "hdp"
# We turn these into labeled categories so the dataset is easier to interpret.

ls2015_long <- ls2015_long %>%
  mutate(
    plan_type = factor(
      plan_type,
      levels = c("hmo", "ppo", "pos", "hdp"),
      labels = c("HMO", "PPO", "POS", "HDP")
    )
  )

# Step 19: Remove plan rows that were not offered
# The reshape creates four possible rows for every firm: HMO, PPO, POS, and HDP.
# But each firm in our sample only offers two of these so the rows corresponding to plans the firm does not offerhave missing values for cs6.
# The original Stata code says: drop if cs6=

ls2015_long <- ls2015_long %>%
  filter(
    !is.na(cs6)
  )

####################################################################################
############       SECTION 3: Create problem flags (clean_2015.do)     ###############
####################################################################################

# These flags identify plan characteristics that require special treatment
# Each flag is a new variable: 
# 1 = the condition is true for the plan
# 0 = the condition is false for the plan
# This section should NOT remove any rows from the dataset

# Step 21: Preserve the original firm-level plan-combination code
# Original pty values: 
#   5 = HDHP + HMO
#   6 = HDHP + PPO
#   7 = HDHP + POS
#
# In our dataset, plan_type already identifies the individual plan: HMO, PPO, POS, or HDP 
# We rename pty to pty2 to distinguish the firm-level combination from the individual plan type 

ls2015_long <- ls2015_long %>%
  rename(
    pty2 = pty
  )

# Save the number of rows before creating the flags
# We will check that the row count is unchanged after

rows_before_flags <- nrow(ls2015_long)

# Step 22: Create flags for coinsurance structure (prob_1_xx)
# The codes 1, 2, and 3 identify different coinsurance structures in the corresponding source variables 
# The final digit identifies the service: 
#   1 = Primary office visits (cs6c)
#   2 = Specialty office visits (cs18c)
#   3 = Drug tier 1 (cs30ab1)
#   4 = Drug tier 2 (cs30bb1)
#   5 = Drug tier 3 (cs30cb1)
#   6 = Drug tier 4 (cs30db1)
#
# The middle digit identifies the structure:
#   1 = Code 1
#   2 = Code 2
#   3 = Code 3

ls2015_long <- ls2015_long %>%
  mutate(
    
    # Source-variable code 1
    prob_1_11 = as.integer(cs6c %in% 1),
    prob_1_12 = as.integer(cs18c %in% 1),
    prob_1_13 = as.integer(cs30ab1 %in% 1),
    prob_1_14 = as.integer(cs30bb1 %in% 1),
    prob_1_15 = as.integer(cs30cb1 %in% 1),
    prob_1_16 = as.integer(cs30db1 %in% 1),
    
    # Source-variable code 2
    prob_1_21 = as.integer(cs6c %in% 2),
    prob_1_22 = as.integer(cs18c %in% 2),
    prob_1_23 = as.integer(cs30ab1 %in% 2),
    prob_1_24 = as.integer(cs30bb1 %in% 2),
    prob_1_25 = as.integer(cs30cb1 %in% 2),
    prob_1_26 = as.integer(cs30db1 %in% 2),
    
    # Source-variable code 3
    prob_1_31 = as.integer(cs6c %in% 3),
    prob_1_32 = as.integer(cs18c %in% 3),
    prob_1_33 = as.integer(cs30ab1 %in% 3),
    prob_1_34 = as.integer(cs30bb1 %in% 3),
    prob_1_35 = as.integer(cs30cb1 %in% 3),
    prob_1_36 = as.integer(cs30db1 %in% 3)
  )

# Step 23: Create flags for copay and coinsurance structures (prob_2_xx)
# These flags identify special cost-sharing arrangements 
# We preserve the conditions used in the original Stata code

ls2015_long <- ls2015_long %>%
  mutate(
    
    # Both a copay and coinsurance
    prob_2_11 = as.integer(cs8x4 %in% 1), 
    prob_2_12 = as.integer(cs7x4 %in% 1),
    
    # Per-day copay AND coinsurance for inpatient care
    prob_2_13 = as.integer(
      cs8x5 %in% 1 & cs8x3 %in% 1
    ),
    
    # Per-day inpatient copay
    prob_2_14 = as.integer(cs8x5 %in% 1),
    
    # Two firms identified for special handling in the original code
    prob_2_15 = as.integer(firm %in% c(998, 3395)),
    
    # Either a copay or coinsurance, whichever is greater
    prob_2_21 = as.integer(cs8x4 %in% 2),
    prob_2_22 = as.integer(cs7x4 %in% 2),
    
    # "None of the above" responses
    prob_2_31 = as.integer(cs6 %in% 4),
    prob_2_32 = as.integer(cs18 %in% 4),
    prob_2_33 = as.integer(cs29 %in% 6),
    
    # Special outpatient-versus-inpatient coinsurance arrangement
    prob_2_42 = as.integer(
      cs7x3 %in% 2 &
        cs8x3 %in% 1 &
        cs7x2 %in% 1
    )
  )

# Step 24: Create flags for separate deductibles (prob_3_xx)
# plan_type identifies the individual plan, so we use it rather than pty 
# prob_3_4 applies to HMO, PPO, and POS plans
# prob_3_5 applies to HDHP plan
#
# We check for missing values deliberately because these flags identify missing deductible info 

ls2015_long <- ls2015_long %>%
  mutate(
    
    # Separate deductible for inpatient care
    prob_3_1 = as.integer(cs8x1 %in% 1),
    
    # Separate deductible for outpatient surgery
    prob_3_2 = as.integer(cs7x1 %in% 1),
    
    # Drugs have a separate deductible and a general deductible
    prob_3_3 = as.integer(
      cs27 %in% 1 &
        (ddct3c %in% 1 | is.na(ddct3c))
    ),
    
    # Missing deductible information for a non-HDP plan
    prob_3_4 = as.integer(
      (is.na(ddct3c) | is.na(ddct3b)) &
        plan_type %in% c("HMO", "PPO", "POS")
    ),
    
    # Missing deductible information for the HDP plan
    prob_3_5 = as.integer(
      (is.na(ddct3b) | is.na(ddct3c)) &
        plan_type %in% "HDP"
    )
  )

# Step 25: Check the problem flags

# Check that we still have 798 firm-plan observations

nrow(ls2015_long)

# Check that none of the problem flags have missing values
sum(is.na(ls2015_long$prob_1_31))

# Count how many observations are flagged for each condition

ls2015_long %>%
  summarise(
    across(starts_with("prob_"), sum)
  )

# generate descriptive statistics table and compare to descriptives presented by Liu and Sydnor (2022)

