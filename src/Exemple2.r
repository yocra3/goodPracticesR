#################################################
###### PROJECT X OUTCOME Y ANALYSIS        ######
####### FINAL MODELS WITH IMPUTED DATASET! ######
#################################################

######################################
#### Author: X 			   ###########
#### Starting date: 30th Jan 2018 ####
#### RStudio version: 1.0.143 ########
#### Script version: 6.0 (clean) #####
######################################

#### UPLOAD DATABASE ####
#########################
load("G:/Mi unidad/phD/PROJECT/DB/Working databases/DB_final_imputed_date.RData")

#### LIBRARIES ####
###################
library(lme4)

#### MODEL 1 (FOR MEN) #### 
###########################

# OUTCOME 1 + EXPOSURE 1
##################
model1 <- lmer(sbp ~ PMresid + age + bp_med_ok + meanPMvillage + 
                 (1 | villageID/householdID), data = subset(imputed, sex== "male"))
summary(model1)$coefficients[2]
confint(model1, 'PMresid', level = 0.95)     

# OUTCOME 1 + EXPOSURE 2
##################
model1 <- lmer(sbp ~ BCresid_iqr + age + bp_med_ok + meanBCvillage +
                 (1 | villageID/householdID), data = subset(imputed, sex== "male"))
summary(model1)$coefficients[2]
confint(model1, 'BCresid_iqr', level = 0.95) 

# OUTCOME 2 + EXPOSURE 1
###################
model1 <- lmer(dbp ~ PMresid + age + bp_med_ok + meanPMvillage +
                 (1 | villageID/householdID), data = subset(imputed, sex== "male"))
summary(model1)$coefficients[2]
confint(model1, 'PMresid', level = 0.95)     

# OUTCOME 2 + EXPOSURE 2
##################
model1 <- lmer(dbp ~ BCresid_iqr + age + bp_med_ok + meanBCvillage +
                 (1 | villageID/householdID), data = subset(imputed, sex== "male"))
summary(model1)$coefficients[2]
confint(model1, 'BCresid_iqr', level = 0.95) 

#### MODEL 1 (FOR WOMEN) #### 
###########################
# OUTCOME 1 + EXPOSURE 1
##################
model1 <- lmer(sbp ~ PMresid + age + bp_med_ok + meanPMvillage + 
                 (1 | villageID/householdID), data = subset(imputed, sex== "female"))
summary(model1)$coefficients[2]
confint(model1, level = 0.95)     

# OUTCOME 1 + EXPOSURE 2
##################
model1 <- lmer(sbp ~ BCresid_iqr + age + bp_med_ok + meanBCvillage +
                 (1 | villageID/householdID), data = subset(imputed, sex== "female"))
summary(model1)$coefficients[2]
confint(model1, 'BCresid_iqr', level = 0.95) 

# OUTCOME 2 + EXPOSURE 1
###################
model1 <- lmer(dbp ~ PMresid + age + bp_med_ok + meanPMvillage +
                 (1 | villageID/householdID), data = subset(imputed, sex== "female"))
summary(model1)$coefficients[2]
confint(model1, level = 0.95)     

# OUTCOME 2 + EXPOSURE 2
##################
model1 <- lmer(dbp ~ BCresid_iqr + age + bp_med_ok + meanBCvillage +
                 (1 | villageID/householdID), data = subset(imputed, sex== "female"))
summary(model1)$coefficients[2]
confint(model1, 'BCresid_iqr', level = 0.95) 