
####################################################################################
############        MODEL EACH DEVIANT BEHAVIOR IN EACH YEAR             ############
####################################################################################


# STEP 2: Model Fighting ---------------------------------------------------

# 1997
glm_fight_1997 <- svyglm(fight ~
                           sports_participation +
                           age +
                           female +
                           race_group,
                         design = design_1997,
                         family = quasibinomial(link = "logit"))
summary(glm_fight_1997)

# 2007
glm_fight_2007 <- svyglm(fight ~
                           sports_participation +
                           age +
                           female +
                           race_group,
                         design = design_2007,
                         family = quasibinomial(link = "logit"))
summary(glm_fight_2007)

# 2017
glm_fight_2017 <- svyglm(fight ~
                           sports_participation +
                           age +
                           female +
                           race_group,
                         design = design_2017,
                         family = quasibinomial(link = "logit"))
summary(glm_fight_2017)

# 2023
glm_fight_2023 <- svyglm(fight ~
                           sports_participation +
                           age +
                           female +
                           race_group,
                         design = design_2023,
                         family = quasibinomial(link = "logit"))
summary(glm_fight_2023)

# Odds ratios and 95% CIs for one model (example)
exp(cbind(OR = coef(glm_fight_1997), confint(glm_fight_1997)))


# STEP 3: Model Binge Drinking ---------------------------------------------

# 1997
glm_binge_1997 <- svyglm(binge_drinking ~
                           sports_participation +
                           age +
                           female +
                           race_group,
                         design = design_1997,
                         family = quasibinomial(link = "logit"))
summary(glm_binge_1997)

# 2007
glm_binge_2007 <- svyglm(binge_drinking ~
                           sports_participation +
                           age +
                           female +
                           race_group,
                         design = design_2007,
                         family = quasibinomial(link = "logit"))
summary(glm_binge_2007)

# 2017
glm_binge_2017 <- svyglm(binge_drinking ~
                           sports_participation +
                           age +
                           female +
                           race_group,
                         design = design_2017,
                         family = quasibinomial(link = "logit"))
summary(glm_binge_2017)

# 2023
glm_binge_2023 <- svyglm(binge_drinking ~
                           sports_participation +
                           age +
                           female +
                           race_group,
                         design = design_2023,
                         family = quasibinomial(link = "logit"))
summary(glm_binge_2023)


# STEP 4: Model Marijuana Use ----------------------------------------------

# 1997
glm_marijuana_1997 <- svyglm(marijuana_use ~
                               sports_participation +
                               age +
                               female +
                               race_group,
                             design = design_1997,
                             family = quasibinomial(link = "logit"))
summary(glm_marijuana_1997)

# 2007
glm_marijuana_2007 <- svyglm(marijuana_use ~
                               sports_participation +
                               age +
                               female +
                               race_group,
                             design = design_2007,
                             family = quasibinomial(link = "logit"))
summary(glm_marijuana_2007)

# 2017
glm_marijuana_2017 <- svyglm(marijuana_use ~
                               sports_participation +
                               age +
                               female +
                               race_group,
                             design = design_2017,
                             family = quasibinomial(link = "logit"))
summary(glm_marijuana_2017)

# 2023
glm_marijuana_2023 <- svyglm(marijuana_use ~
                               sports_participation +
                               age +
                               female +
                               race_group,
                             design = design_2023,
                             family = quasibinomial(link = "logit"))
summary(glm_marijuana_2023)


# STEP 5: Model Cocaine Use ------------------------------------------------

# 1997
glm_cocaine_1997 <- svyglm(cocaine_use ~
                             sports_participation +
                             age +
                             female +
                             race_group,
                           design = design_1997,
                           family = quasibinomial(link = "logit"))
summary(glm_cocaine_1997)

# 2007
glm_cocaine_2007 <- svyglm(cocaine_use ~
                             sports_participation +
                             age +
                             female +
                             race_group,
                           design = design_2007,
                           family = quasibinomial(link = "logit"))
summary(glm_cocaine_2007)

# 2017
glm_cocaine_2017 <- svyglm(cocaine_use ~
                             sports_participation +
                             age +
                             female +
                             race_group,
                           design = design_2017,
                           family = quasibinomial(link = "logit"))
summary(glm_cocaine_2017)

# 2023
glm_cocaine_2023 <- svyglm(cocaine_use ~
                             sports_participation +
                             age +
                             female +
                             race_group,
                           design = design_2023,
                           family = quasibinomial(link = "logit"))
summary(glm_cocaine_2023)


# STEP 6: Model Nicotine Use -----------------------------------------------

# Note about measurement (same as Graph 3 in 05):
# 1997 and 2007 = cigarettes only; 2017 and 2023 = cigarettes OR vaping.
# Changes in the sports coefficient across years partly reflect this change in the measure.

# 1997
glm_nicotine_1997 <- svyglm(nicotine_use ~
                              sports_participation +
                              age +
                              female +
                              race_group,
                            design = design_1997,
                            family = quasibinomial(link = "logit"))
summary(glm_nicotine_1997)

# 2007
glm_nicotine_2007 <- svyglm(nicotine_use ~
                              sports_participation +
                              age +
                              female +
                              race_group,
                            design = design_2007,
                            family = quasibinomial(link = "logit"))
summary(glm_nicotine_2007)

# 2017
glm_nicotine_2017 <- svyglm(nicotine_use ~
                              sports_participation +
                              age +
                              female +
                              race_group,
                            design = design_2017,
                            family = quasibinomial(link = "logit"))
summary(glm_nicotine_2017)

# 2023
glm_nicotine_2023 <- svyglm(nicotine_use ~
                              sports_participation +
                              age +
                              female +
                              race_group,
                            design = design_2023,
                            family = quasibinomial(link = "logit"))
summary(glm_nicotine_2023)
