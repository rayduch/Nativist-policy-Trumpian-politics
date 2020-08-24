################################################################################
##                                                                            ##
##                      Populism: Magnet or Deterrent                         ##
##                      Generate models for main text                         ##
##                                                                            ##
##  Raymond Duch, Denise Laroze, Constantin Reinprecht, and Thomas Robinson   ##
##                              PSRM, 2020                                    ##
################################################################################

library(openxlsx)
library(xtable)
library(gridExtra)
library(coefplot)
library(dummies)
library(nnet)
library(stargazer)
library(tidyverse)
library(lubridate)
library(ggrepel)

##### Functions #####
# Include code to replace standard errors with robust S.Es
robustse.f <- function(model, cluster, df_correction) {
  ## Huber-White heteroskedasticity-robust standard error calculation and table generation code for lm and glm models in R.
  ##Written by Joshua Gubler ~  http://joshuagubler.com.  Note that the second half of this function is just a wrapper for the excellent "multiwaycov" package here: https://cran.r-project.org/web/packages/multiwayvcov/multiwayvcov.pdf .  Love the authors of that package...
  ##Last updated: 3 July 2017  
  
  #model = the model you estimated, now calculated with robust standard errors
  #cluster = the name of the variable on which you will cluster. Put a tilda in front of it (e.g. ~ state).  If you don't put in a cluster, you will simply get huber-white robust errors.
  #df_correction: If you do not want the number of levels in your cluster variable to count against your degrees of freedom (like the xt- options in Stata), then type "F".  Otherwise, type "T" and these levels will be counted against your degrees of freedom
  
  require(sandwich)
  require(lmtest)
  require(multiwayvcov)
  if(missing(cluster)) {
    name <- deparse(substitute(model))
    modelname <- paste(name,"rob",sep=".")
    model$se <- coeftest(model, vcov=vcovHC(model,"HC1"))[,2]
    model$vcovHC <- vcovHC(model,"HC1")
    assign(modelname,model,envir = .GlobalEnv)
    coeftest(model, vcov=vcovHC(model,"HC1"))
  } else {
    name <- deparse(substitute(model))
    vcovCL <- cluster.vcov(model, cluster, df_correction = df_correction)
    model$vcovCL <- vcovCL
    modelname <- paste(name,"clustrob",sep=".")
    model$se <- coeftest(model, vcovCL)[,2]
    assign(modelname,model,envir = .GlobalEnv)
    #coeftest(model, vcovCL)
  }
}

#### Data read-in and format ####

conjoint1 <- rbind(read.csv("data/conjoint1_uk.csv"),
                   read.csv("data/conjoint1_chile.csv"),
                   read.csv("data/conjoint1_china.csv"),
                   read.csv("data/conjoint1_india.csv"))
conjoint2 <- rbind(read.csv("data/conjoint2_uk.csv"),
                   read.csv("data/conjoint2_chile.csv"),
                   read.csv("data/conjoint2_china.csv"),
                   read.csv("data/conjoint2_india.csv"))
conjoint3 <- rbind(read.csv("data/conjoint3_uk.csv"),
                   read.csv("data/conjoint3_chile.csv"),
                   read.csv("data/conjoint3_china.csv"),
                   read.csv("data/conjoint3_india.csv"))

conjoint1 <- droplevels(conjoint1)
conjoint2 <- droplevels(conjoint2)
conjoint3 <- droplevels(conjoint3)

conjoint1 <- within(conjoint1, econ <- relevel(econ, ref = "Annual GDP Growth of 4%"))
conjoint1 <- within(conjoint1, service <- relevel(service, ref = "Average international ranking of service salaries: 70th Percentile"))
conjoint1 <- within(conjoint1, immigration <- relevel(immigration, ref = "Change in visa processing centres"))
conjoint1 <- within(conjoint1, education <- relevel(education, ref = "Average international ranking of universities: 60th Percentile"))

conjoint2 <- within(conjoint2, econ <- relevel(econ, ref = "Annual GDP Growth of 4%"))
conjoint2 <- within(conjoint2, service <- relevel(service, ref = "Average international ranking of service salaries: 70th Percentile"))
conjoint2 <- within(conjoint2, immigration <- relevel(immigration, ref = "Change in visa processing centres"))
conjoint2 <- within(conjoint2, education <- relevel(education, ref = "Average international ranking of universities: 60th Percentile"))

conjoint3 <- within(conjoint3, econ <- relevel(econ, ref = "Annual GDP Growth of 4%"))
conjoint3 <- within(conjoint3, service <- relevel(service, ref = "Average international ranking of service salaries: 70th Percentile"))
conjoint3 <- within(conjoint3, education <- relevel(education, ref = "Average international ranking of universities: 60th Percentile"))

######################################################################
#### Regression models ####
######################################################################
#### Country levels models ####
## Logit models UK
model1_uk <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$survey == "UK",])
model2_uk <- glm(destination ~ social + econ + service + immigration + education,family=binomial(link='logit'),data=conjoint2[conjoint2$survey == "UK",])
model3_uk <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint1$survey == "UK",])

# Cluster-robust standard errors
robustse.f(model1_uk, ~id, F)
robustse.f(model2_uk, ~id, F)
robustse.f(model3_uk, ~id, F)

## Logit models Chile
model1_chile <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$survey == "Chile",])
model2_chile <- glm(destination ~ social + econ + service + immigration + education,family=binomial(link='logit'),data=conjoint2[conjoint2$survey == "Chile",])
model3_chile <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$survey == "Chile",])

# Cluster-robust standard errors
robustse.f(model1_chile, ~id, F)
robustse.f(model2_chile, ~id, F)
robustse.f(model3_chile, ~id, F)

## Logit models China
model1_china <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$survey == "china",])
model2_china <- glm(destination ~ social + econ + service + immigration + education,family=binomial(link='logit'),data=conjoint2[conjoint2$survey == "china",])
model3_china <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$survey == "china",])

# Cluster-robust standard errors
robustse.f(model1_china, ~id, F)
robustse.f(model2_china, ~id, F)
robustse.f(model3_china, ~id, F)

## Logit models India
model1_india <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$survey == "India",])
model2_india <- glm(destination ~ social + econ + service + immigration + education,family=binomial(link='logit'),data=conjoint2[conjoint2$survey == "India",])
model3_india <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$survey == "India",])

# Cluster-robust standard errors
robustse.f(model1_india, ~id, F)
robustse.f(model2_india, ~id, F)
robustse.f(model3_india, ~id, F)

## Pooled results
conjoint1$pid <- paste0(conjoint1$survey,conjoint1$id)
conjoint2$pid <- paste0(conjoint2$survey,conjoint2$id)
conjoint3$pid <- paste0(conjoint3$survey,conjoint3$id)

model1 <- glm(destination ~ social + econ + service + immigration + education  + survey,family=binomial(link='logit'),data=conjoint1)
model2 <- glm(destination ~ social + econ + service + immigration + education + survey,family=binomial(link='logit'),data=conjoint2)
model3 <- glm(destination ~ social + econ + service + country + education  + survey,family=binomial(link='logit'),data=conjoint3)

# Cluster-robust standard errors
robustse.f(model1, ~pid, F)
robustse.f(model2, ~pid, F)
robustse.f(model3, ~pid, F)

#### Ideology models ####
model1_left_uk <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$ideology < 5 & conjoint1$survey == "UK",])
model1_left_chile <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$ideology < 5 & conjoint1$survey == "Chile",])
model1_left_china <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$ideology < 5 & conjoint1$survey == "china",])
model1_left_india <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$ideology < 5 & conjoint1$survey == "India",])

model1_right_uk <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$ideology > 5 & conjoint1$survey == "UK",])
model1_right_chile <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$ideology > 5 & conjoint1$survey == "Chile",])
model1_right_china <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$ideology > 5 & conjoint1$survey == "china",])
model1_right_india <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$ideology > 5 & conjoint1$survey == "India",])

model1_centre_uk <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$ideology == 5 & conjoint1$survey == "UK",])
model1_centre_chile <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$ideology == 5 & conjoint1$survey == "Chile",])
model1_centre_china <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$ideology == 5 & conjoint1$survey == "china",])
model1_centre_india <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint1[conjoint1$ideology == 5 & conjoint1$survey == "India",])

model2_left_uk <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint2[conjoint2$ideology < 5 & conjoint2$survey == "UK",])
model2_left_chile <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint2[conjoint2$ideology < 5 & conjoint2$survey == "Chile",])
model2_left_china <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint2[conjoint2$ideology < 5 & conjoint2$survey == "china",])
model2_left_india <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint2[conjoint2$ideology < 5 & conjoint2$survey == "India",])

model2_right_uk <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint2[conjoint2$ideology > 5 & conjoint2$survey == "UK",])
model2_right_chile <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint2[conjoint2$ideology > 5 & conjoint2$survey == "Chile",])
model2_right_china <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint2[conjoint2$ideology > 5 & conjoint2$survey == "china",])
model2_right_india <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint2[conjoint2$ideology > 5 & conjoint2$survey == "India",])

model2_centre_uk <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint2[conjoint2$ideology == 5 & conjoint2$survey == "UK",])
model2_centre_chile <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint2[conjoint2$ideology == 5 & conjoint2$survey == "Chile",])
model2_centre_china <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint2[conjoint2$ideology == 5 & conjoint2$survey == "china",])
model2_centre_india <- glm(destination ~ social + econ + service + immigration + education ,family=binomial(link='logit'),data=conjoint2[conjoint2$ideology == 5 & conjoint2$survey == "India",])

model3_left_uk <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$ideology < 5 & conjoint3$survey == "UK",])
model3_left_chile <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$ideology < 5 & conjoint3$survey == "Chile",])
model3_left_china <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$ideology < 5 & conjoint3$survey == "china",])
model3_left_india <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$ideology < 5 & conjoint3$survey == "India",])

model3_right_uk <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$ideology > 5 & conjoint3$survey == "UK",])
model3_right_chile <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$ideology > 5 & conjoint3$survey == "Chile",])
model3_right_china <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$ideology > 5 & conjoint3$survey == "china",])
model3_right_india <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$ideology > 5 & conjoint3$survey == "India",])

model3_centre_uk <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$ideology == 5 & conjoint3$survey == "UK",])
model3_centre_chile <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$ideology == 5 & conjoint3$survey == "Chile",])
model3_centre_china <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$ideology == 5 & conjoint3$survey == "china",])
model3_centre_india <- glm(destination ~ social + econ + service + country + education ,family=binomial(link='logit'),data=conjoint3[conjoint3$ideology == 5 & conjoint3$survey == "India",])

robustse.f(model1_left_uk, ~id, F)
robustse.f(model1_left_chile, ~id, F)
robustse.f(model1_left_china, ~id, F)
robustse.f(model1_left_india, ~id, F)
robustse.f(model2_left_uk, ~id, F)
robustse.f(model2_left_chile, ~id, F)
robustse.f(model2_left_china, ~id, F)
robustse.f(model2_left_india, ~id, F)
robustse.f(model3_left_uk, ~id, F)
robustse.f(model3_left_chile, ~id, F)
robustse.f(model3_left_china, ~id, F)
robustse.f(model3_left_india, ~id, F)
robustse.f(model1_right_uk, ~id, F)
robustse.f(model1_right_chile, ~id, F)
robustse.f(model1_right_china, ~id, F)
robustse.f(model1_right_india, ~id, F)
robustse.f(model2_right_uk, ~id, F)
robustse.f(model2_right_chile, ~id, F)
robustse.f(model2_right_china, ~id, F)
robustse.f(model2_right_india, ~id, F)
robustse.f(model3_right_uk, ~id, F)
robustse.f(model3_right_chile, ~id, F)
robustse.f(model3_right_china, ~id, F)
robustse.f(model3_right_india, ~id, F)
robustse.f(model1_centre_uk, ~id, F)
robustse.f(model1_centre_chile, ~id, F)
robustse.f(model1_centre_china, ~id, F)
robustse.f(model1_centre_india, ~id, F)
robustse.f(model2_centre_uk, ~id, F)
robustse.f(model2_centre_chile, ~id, F)
robustse.f(model2_centre_china, ~id, F)
robustse.f(model2_centre_india, ~id, F)
robustse.f(model3_centre_uk, ~id, F)
robustse.f(model3_centre_chile, ~id, F)
robustse.f(model3_centre_china, ~id, F)
robustse.f(model3_centre_india, ~id, F)

#### Age models ####

conjoint1$age_bin <- ifelse(conjoint1$age <= 25, "<=25",">25")
conjoint2$age_bin <- ifelse(conjoint2$age <= 25, "<=25",">25")
conjoint3$age_bin <- ifelse(conjoint3$age <= 25, "<=25",">25")

model1_age_young <- glm(destination ~ social + econ + service + immigration + education + survey,family=binomial(link='logit'),data=conjoint1[conjoint1$age_bin == "<=25",])
model1_age_old <- glm(destination ~ social + econ + service + immigration + education + survey,family=binomial(link='logit'),data=conjoint1[conjoint1$age_bin != "<=25",])

model2_age_young <- glm(destination ~ social + econ + service + immigration + education + survey,family=binomial(link='logit'),data=conjoint2[conjoint2$age_bin == "<=25",])
model2_age_old <- glm(destination ~ social + econ + service + immigration + education + survey,family=binomial(link='logit'),data=conjoint2[conjoint2$age_bin != "<=25",])

model3_age_young <- glm(destination ~ social + econ + service + country + education + survey,family=binomial(link='logit'),data=conjoint3[conjoint3$age_bin == "<=25",])
model3_age_old <- glm(destination ~ social + econ + service + country + education + survey,family=binomial(link='logit'),data=conjoint3[conjoint3$age_bin != "<=25",])

# Cluster-robust standard errors
robustse.f(model1_age_old, ~pid, F)
robustse.f(model1_age_young, ~pid, F)
robustse.f(model2_age_old, ~pid, F)
robustse.f(model2_age_young, ~pid, F)
robustse.f(model3_age_old, ~pid, F)
robustse.f(model3_age_young, ~pid, F)

#### Likelihood of migrating models ####

conjoint1$likely_bin <- ifelse(conjoint1$likely >= 4, 1, 0)
conjoint2$likely_bin <- ifelse(conjoint2$likely >= 4, 1, 0)
conjoint3$likely_bin <- ifelse(conjoint3$likely >= 4, 1, 0)

model1_likely_india <- glm(destination ~ social + econ + service + immigration + education,
                           family=binomial(link='logit'),
                           data=conjoint1[conjoint1$likely_bin == 1 & conjoint1$survey == "India",])

model1_likely_chile <- glm(destination ~ social + econ + service + immigration + education,
                           family=binomial(link='logit'),
                           data=conjoint1[conjoint1$likely_bin == 1 & conjoint1$survey == "Chile",])

model1_likely_uk <- glm(destination ~ social + econ + service + immigration + education,
                        family=binomial(link='logit'),
                        data=conjoint1[conjoint1$likely_bin == 1 & conjoint1$survey == "UK",])

model1_likely_china <- glm(destination ~ social + econ + service + immigration + education,
                           family=binomial(link='logit'),
                           data=conjoint1[conjoint1$likely_bin == 1 & conjoint1$survey == "china",])

model2_likely_india <- glm(destination ~ social + econ + service + immigration + education,
                           family=binomial(link='logit'),
                           data=conjoint2[conjoint2$likely_bin == 1 & conjoint2$survey == "India",])

model2_likely_chile <- glm(destination ~ social + econ + service + immigration + education,
                           family=binomial(link='logit'),
                           data=conjoint2[conjoint2$likely_bin == 1 & conjoint2$survey == "Chile",])

model2_likely_uk <- glm(destination ~ social + econ + service + immigration + education,
                        family=binomial(link='logit'),
                        data=conjoint2[conjoint2$likely_bin == 1 & conjoint2$survey == "UK",])

model2_likely_china <- glm(destination ~ social + econ + service + immigration + education,
                           family=binomial(link='logit'),
                           data=conjoint2[conjoint2$likely_bin == 1 & conjoint2$survey == "china",])

model3_likely_india <- glm(destination ~ social + econ + service + country + education,
                           family=binomial(link='logit'),
                           data=conjoint3[conjoint3$likely_bin == 1 & conjoint3$survey == "India",])

model3_likely_chile <- glm(destination ~ social + econ + service + country + education,
                           family=binomial(link='logit'),
                           data=conjoint3[conjoint3$likely_bin == 1 & conjoint3$survey == "Chile",])

model3_likely_uk <- glm(destination ~ social + econ + service + country + education,
                        family=binomial(link='logit'),
                        data=conjoint3[conjoint3$likely_bin == 1 & conjoint3$survey == "UK",])

model3_likely_china <- glm(destination ~ social + econ + service + country + education,
                           family=binomial(link='logit'),
                           data=conjoint3[conjoint3$likely_bin == 1 & conjoint3$survey == "china",])

robustse.f(model1_likely_india, ~pid, F)
robustse.f(model1_likely_chile, ~pid, F)
robustse.f(model1_likely_uk, ~pid, F)
robustse.f(model1_likely_china, ~pid, F)
robustse.f(model2_likely_india, ~pid, F)
robustse.f(model2_likely_chile, ~pid, F)
robustse.f(model2_likely_uk, ~pid, F)
robustse.f(model2_likely_china, ~pid, F)
robustse.f(model3_likely_india, ~pid, F)
robustse.f(model3_likely_chile, ~pid, F)
robustse.f(model3_likely_uk, ~pid, F)
robustse.f(model3_likely_china, ~pid, F)