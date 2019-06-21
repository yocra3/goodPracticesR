#'################################################
#'##### PROJECT X OUTCOME Y ANALYSIS        ######
#' Run final models using imputed dataset   ######
#'################################################

#'#####################################
#'### Author: X 			   ###########
#'### Starting date: 30th Jan 2018 ####
#'### RStudio version: 1.0.143 ########
#'### Script version: 6.0 (clean) #####
#'#####################################

#### UPLOAD DATABASE ####
#'########################

## Run in project folder
load("results/imputed/DB_final_imputed_date.RData")

#### LIBRARIES ####
#'##################
library(lme4)

#### DEFINE FUNCTION TO RUN THE MODELS ####
#'##########################################

#' Run a linear model in our imputed dataset. The code allows to stratify the analysis by gender.
#' 
#' @param dataset data.frame with the data to analyze. Dataset should contain a column
#' called sex to allow filtering individuals by sex. 
#' @param model Character with the linear model to run. It should start by ~ and 
#' include the exposure in the first position. Variables included in the model should
#' be present in the col.names of dataset.
#' @param gender Character with the gender of the individuals included in the analyses. 
#' To include all inviduals, pass a vector with all genders present in dataset. 
#' @return Named numeric vector with the results of the association between the
#' outcome and the exposure. This vector contains the estimate and the lower and 
#' upper 95% confidence interval values.
runModel <- function(dataset, model, gender){

	modelRes <- lmer(formula(model), data = subset(dataset, sex %in% gender))
  
	summary(modelRes)$coefficients[2, c("est", "lo 95", "hi 95")]
}
			
outcome <- c("sbp", "dbp")
exposures <- c("PMresid", "BCresid_iqr")

#### MODEL 1 #### 
#'################

## Define a character vector with the models
models1 <- paste(rep(outcome, each = 2),
                 "~",
                 rep(exposures, 2),
                 "+ age + bp_med_ok + meanPMvillage + (1 | villageID/householdID)")
names(models1) <- paste(rep(outcome, each = 2), rep(exposures, 2), sep = "_")            

## Run models in male and female
out1_models1_men <- lapply(models1, runModel, dataset = imputed, gender = "male")
out1_models1_women <- lapply(models1, runModel, dataset = imputed, gender = "female")

## Group models in a list
models1_res <- list(men = out1_models1_men, women = out1_models1_women)
save(models1_res, file = "results/models/model1.Rdata")