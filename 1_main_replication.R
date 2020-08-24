################################################################################
##                                                                            ##
##                      Populism: Magnet or Deterrent                         ##
##                          Main Replication Code                             ##
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

theme_set(theme_bw())

source("0_main_models.R")

######################################################################
#### Figure Prep. ####
######################################################################

#### Full conjoint figure prep ####
## Model 1
plotdf1_uk <- data.frame(estimate = coeftest(model1_uk.clustrob, model1_uk.clustrob$vcovCL)[,1],
                         SE = coeftest(model1_uk.clustrob, model1_uk.clustrob$vcovCL)[,2])

# Add in empty spots for reference categories
plotdf1_uk <- plotdf1_uk[-1,]
plotdf1_uk <- rbind(plotdf1_uk[1,],
                    c(0,0),
                    plotdf1_uk[2:11,])
plotdf1_uk <- rbind(plotdf1_uk[1:4,],
                    c(0,0),
                    plotdf1_uk[5:12,])
plotdf1_uk <- rbind(plotdf1_uk[1:7,],
                    c(0,0),
                    plotdf1_uk[8:13,])
plotdf1_uk <- rbind(plotdf1_uk[1:10,],
                    c(0,0),
                    plotdf1_uk[11:14,])
plotdf1_uk <- rbind(plotdf1_uk[1:13,],
                    c(0,0),
                    plotdf1_uk[14,])
plotdf1_uk$coef <- c("Generous family allowance",
                     "Basic minimum wage",
                     "No minimum wage or income support",
                     "GDP 2 percent",
                     "GDP 4 percent",
                     "GDP 6 percent",
                     "Service salaries 50th pc",
                     "Service salaries 70th pc",
                     "Service salaries 90th pc",
                     "Point-system visa",
                     "Change in visa processing centres",
                     "Muslim Ban",
                     "University Ranking 40th pc",
                     "University Ranking 60th pc",
                     "University Ranking 90th pc")

plotdf1_chile <- data.frame(estimate = coeftest(model1_chile.clustrob, model1_chile.clustrob$vcovCL)[,1],SE = coeftest(model1_chile.clustrob, model1_chile.clustrob$vcovCL)[,2])
plotdf1_chile <- plotdf1_chile[-1,]
plotdf1_chile <- rbind(plotdf1_chile[1,],
                       c(0,0),
                       plotdf1_chile[2:11,])
plotdf1_chile <- rbind(plotdf1_chile[1:4,],
                       c(0,0),
                       plotdf1_chile[5:12,])
plotdf1_chile <- rbind(plotdf1_chile[1:7,],
                       c(0,0),
                       plotdf1_chile[8:13,])
plotdf1_chile <- rbind(plotdf1_chile[1:10,],
                       c(0,0),
                       plotdf1_chile[11:14,])
plotdf1_chile <- rbind(plotdf1_chile[1:13,],
                       c(0,0),
                       plotdf1_chile[14,])
plotdf1_chile$coef <- c("Generous family allowance",
                        "Basic minimum wage",
                        "No minimum wage or income support",
                        "GDP 2 percent",
                        "GDP 4 percent",
                        "GDP 6 percent",
                        "Service salaries 50th pc",
                        "Service salaries 70th pc",
                        "Service salaries 90th pc",
                        "Point-system visa",
                        "Change in visa processing centres",
                        "Muslim Ban",
                        "University Ranking 40th pc",
                        "University Ranking 60th pc",
                        "University Ranking 90th pc")

plotdf1_china <- data.frame(estimate = coeftest(model1_china.clustrob, model1_china.clustrob$vcovCL)[,1],SE = coeftest(model1_china.clustrob, model1_china.clustrob$vcovCL)[,2])
plotdf1_china <- plotdf1_china[-1,]
plotdf1_china <- rbind(plotdf1_china[1,],
                       c(0,0),
                       plotdf1_china[2:11,])
plotdf1_china <- rbind(plotdf1_china[1:4,],
                       c(0,0),
                       plotdf1_china[5:12,])
plotdf1_china <- rbind(plotdf1_china[1:7,],
                       c(0,0),
                       plotdf1_china[8:13,])
plotdf1_china <- rbind(plotdf1_china[1:10,],
                       c(0,0),
                       plotdf1_china[11:14,])
plotdf1_china <- rbind(plotdf1_china[1:13,],
                       c(0,0),
                       plotdf1_china[14,])
plotdf1_china$coef <- c("Generous family allowance",
                        "Basic minimum wage",
                        "No minimum wage or income support",
                        "GDP 2 percent",
                        "GDP 4 percent",
                        "GDP 6 percent",
                        "Service salaries 50th pc",
                        "Service salaries 70th pc",
                        "Service salaries 90th pc",
                        "Point-system visa",
                        "Change in visa processing centres",
                        "Muslim Ban",
                        "University Ranking 40th pc",
                        "University Ranking 60th pc",
                        "University Ranking 90th pc")

plotdf1_india <- data.frame(estimate = coeftest(model1_india.clustrob, model1_india.clustrob$vcovCL)[,1],SE = coeftest(model1_india.clustrob, model1_india.clustrob$vcovCL)[,2])
plotdf1_india <- plotdf1_india[-1,]
plotdf1_india <- rbind(plotdf1_india[1,],
                       c(0,0),
                       plotdf1_india[2:11,])
plotdf1_india <- rbind(plotdf1_india[1:4,],
                       c(0,0),
                       plotdf1_india[5:12,])
plotdf1_india <- rbind(plotdf1_india[1:7,],
                       c(0,0),
                       plotdf1_india[8:13,])
plotdf1_india <- rbind(plotdf1_india[1:10,],
                       c(0,0),
                       plotdf1_india[11:14,])
plotdf1_india <- rbind(plotdf1_india[1:13,],
                       c(0,0),
                       plotdf1_india[14,])
plotdf1_india$coef <- c("Generous family allowance",
                        "Basic minimum wage",
                        "No minimum wage or income support",
                        "GDP 2 percent",
                        "GDP 4 percent",
                        "GDP 6 percent",
                        "Service salaries 50th pc",
                        "Service salaries 70th pc",
                        "Service salaries 90th pc",
                        "Point-system visa",
                        "Change in visa processing centres",
                        "Muslim Ban",
                        "University Ranking 40th pc",
                        "University Ranking 60th pc",
                        "University Ranking 90th pc")

plotdf1_uk$survey <- "UK"
plotdf1_chile$survey <- "Chile"
plotdf1_china$survey <- "China"
plotdf1_india$survey <- "India"

plotdf1 <- rbind(plotdf1_uk,plotdf1_chile, plotdf1_china, plotdf1_india)

plotdf1$coef <- factor(plotdf1$coef, levels = c("University Ranking 90th pc",
                                                "University Ranking 60th pc",
                                                "University Ranking 40th pc",
                                                "Muslim Ban",
                                                "Change in visa processing centres",
                                                "Point-system visa",
                                                "Service salaries 90th pc",
                                                "Service salaries 70th pc",
                                                "Service salaries 50th pc",
                                                "GDP 6 percent",
                                                "GDP 4 percent",
                                                "GDP 2 percent",
                                                "No minimum wage or income support",
                                                "Basic minimum wage",
                                                "Generous family allowance",
                                                "Intercept"))

plotdf1$LCI <- plotdf1$estimate-1.96*plotdf1$SE
plotdf1$UCI <- plotdf1$estimate+1.96*plotdf1$SE

#Model 2
plotdf2_uk <- data.frame(estimate = coeftest(model2_uk.clustrob, model2_uk.clustrob$vcovCL)[,1],SE = coeftest(model2_uk.clustrob, model2_uk.clustrob$vcovCL)[,2])
plotdf2_uk <- plotdf2_uk[-1,]
plotdf2_uk <- rbind(plotdf2_uk[1,],
                    c(0,0),
                    plotdf2_uk[2:11,])
plotdf2_uk <- rbind(plotdf2_uk[1:4,],
                    c(0,0),
                    plotdf2_uk[5:12,])
plotdf2_uk <- rbind(plotdf2_uk[1:7,],
                    c(0,0),
                    plotdf2_uk[8:13,])
plotdf2_uk <- rbind(plotdf2_uk[1:10,],
                    c(0,0),
                    plotdf2_uk[11:14,])
plotdf2_uk <- rbind(plotdf2_uk[1:13,],
                    c(0,0),
                    plotdf2_uk[14,])
plotdf2_uk$coef <- c("Generous family allowance",
                     "Basic minimum wage",
                     "No minimum wage or income support",
                     "GDP 2 percent",
                     "GDP 4 percent",
                     "GDP 6 percent",
                     "Service salaries 50th pc",
                     "Service salaries 70th pc",
                     "Service salaries 90th pc",
                     "Deportation of all illegal immigrants",
                     "Change in visa processing centres",
                     "Point-system visa",
                     "University Ranking 40th pc",
                     "University Ranking 60th pc",
                     "University Ranking 90th pc")

plotdf2_chile <- data.frame(estimate = coeftest(model2_chile.clustrob, model2_chile.clustrob$vcovCL)[,1],SE = coeftest(model2_chile.clustrob, model2_chile.clustrob$vcovCL)[,2])
plotdf2_chile <- plotdf2_chile[-1,]
plotdf2_chile <- rbind(plotdf2_chile[1,],
                       c(0,0),
                       plotdf2_chile[2:11,])
plotdf2_chile <- rbind(plotdf2_chile[1:4,],
                       c(0,0),
                       plotdf2_chile[5:12,])
plotdf2_chile <- rbind(plotdf2_chile[1:7,],
                       c(0,0),
                       plotdf2_chile[8:13,])
plotdf2_chile <- rbind(plotdf2_chile[1:10,],
                       c(0,0),
                       plotdf2_chile[11:14,])
plotdf2_chile <- rbind(plotdf2_chile[1:13,],
                       c(0,0),
                       plotdf2_chile[14,])
plotdf2_chile$coef <- c("Generous family allowance",
                        "Basic minimum wage",
                        "No minimum wage or income support",
                        "GDP 2 percent",
                        "GDP 4 percent",
                        "GDP 6 percent",
                        "Service salaries 50th pc",
                        "Service salaries 70th pc",
                        "Service salaries 90th pc",
                        "Deportation of all illegal immigrants",
                        "Change in visa processing centres",
                        "Point-system visa",
                        "University Ranking 40th pc",
                        "University Ranking 60th pc",
                        "University Ranking 90th pc")

plotdf2_china <- data.frame(estimate = coeftest(model2_china.clustrob, model2_china.clustrob$vcovCL)[,1],SE = coeftest(model2_china.clustrob, model2_china.clustrob$vcovCL)[,2])
plotdf2_china <- plotdf2_china[-1,]
plotdf2_china <- rbind(plotdf2_china[1,],
                       c(0,0),
                       plotdf2_china[2:11,])
plotdf2_china <- rbind(plotdf2_china[1:4,],
                       c(0,0),
                       plotdf2_china[5:12,])
plotdf2_china <- rbind(plotdf2_china[1:7,],
                       c(0,0),
                       plotdf2_china[8:13,])
plotdf2_china <- rbind(plotdf2_china[1:10,],
                       c(0,0),
                       plotdf2_china[11:14,])
plotdf2_china <- rbind(plotdf2_china[1:13,],
                       c(0,0),
                       plotdf2_china[14,])
plotdf2_china$coef <- c("Generous family allowance",
                        "Basic minimum wage",
                        "No minimum wage or income support",
                        "GDP 2 percent",
                        "GDP 4 percent",
                        "GDP 6 percent",
                        "Service salaries 50th pc",
                        "Service salaries 70th pc",
                        "Service salaries 90th pc",
                        "Deportation of all illegal immigrants",
                        "Change in visa processing centres",
                        "Point-system visa",
                        "University Ranking 40th pc",
                        "University Ranking 60th pc",
                        "University Ranking 90th pc")

plotdf2_india <- data.frame(estimate = coeftest(model2_india.clustrob, model2_india.clustrob$vcovCL)[,1],SE = coeftest(model2_india.clustrob, model2_india.clustrob$vcovCL)[,2])
plotdf2_india <- plotdf2_india[-1,]
plotdf2_india <- rbind(plotdf2_india[1,],
                       c(0,0),
                       plotdf2_india[2:11,])
plotdf2_india <- rbind(plotdf2_india[1:4,],
                       c(0,0),
                       plotdf2_india[5:12,])
plotdf2_india <- rbind(plotdf2_india[1:7,],
                       c(0,0),
                       plotdf2_india[8:13,])
plotdf2_india <- rbind(plotdf2_india[1:10,],
                       c(0,0),
                       plotdf2_india[11:14,])
plotdf2_india <- rbind(plotdf2_india[1:13,],
                       c(0,0),
                       plotdf2_india[14,])
plotdf2_india$coef <- c("Generous family allowance",
                        "Basic minimum wage",
                        "No minimum wage or income support",
                        "GDP 2 percent",
                        "GDP 4 percent",
                        "GDP 6 percent",
                        "Service salaries 50th pc",
                        "Service salaries 70th pc",
                        "Service salaries 90th pc",
                        "Deportation of all illegal immigrants",
                        "Change in visa processing centres",
                        "Point-system visa",
                        "University Ranking 40th pc",
                        "University Ranking 60th pc",
                        "University Ranking 90th pc")

plotdf2_uk$survey <- "UK"
plotdf2_chile$survey <- "Chile"
plotdf2_china$survey <- "China"
plotdf2_india$survey <- "India"

plotdf2 <- rbind(plotdf2_uk,plotdf2_chile, plotdf2_china, plotdf2_india)

plotdf2$LCI <- plotdf2$estimate-1.96*plotdf2$SE
plotdf2$UCI <- plotdf2$estimate+1.96*plotdf2$SE

plotdf2$coef <- factor(plotdf2$coef, levels = c("University Ranking 90th pc",
                                                "University Ranking 60th pc",
                                                "University Ranking 40th pc",
                                                "Deportation of all illegal immigrants",
                                                "Change in visa processing centres",
                                                "Point-system visa",
                                                "Service salaries 90th pc",
                                                "Service salaries 70th pc",
                                                "Service salaries 50th pc",
                                                "GDP 6 percent",
                                                "GDP 4 percent",
                                                "GDP 2 percent",
                                                "No minimum wage or income support",
                                                "Basic minimum wage",
                                                "Generous family allowance",
                                                "Intercept"))
#Model 3
plotdf3_uk <- data.frame(estimate = coeftest(model3_uk.clustrob, model3_uk.clustrob$vcovCL)[,1],SE = coeftest(model3_uk.clustrob, model3_uk.clustrob$vcovCL)[,2])
plotdf3_uk <- plotdf3_uk[-1,]
plotdf3_uk <- rbind(plotdf3_uk[1,],
                    c(0,0),
                    plotdf3_uk[2:11,])
plotdf3_uk <- rbind(plotdf3_uk[1:4,],
                    c(0,0),
                    plotdf3_uk[5:12,])
plotdf3_uk <- rbind(plotdf3_uk[1:7,],
                    c(0,0),
                    plotdf3_uk[8:13,])
plotdf3_uk <- rbind(plotdf3_uk[1:10,],
                    c(0,0),
                    plotdf3_uk[11:14,])
plotdf3_uk <- rbind(plotdf3_uk[1:13,],
                    c(0,0),
                    plotdf3_uk[14,])
plotdf3_uk$coef <- c("Generous family allowance",
                     "Basic minimum wage",
                     "No minimum wage or income support",
                     "GDP 2 percent",
                     "GDP 4 percent",
                     "GDP 6 percent",
                     "Service salaries 50th pc",
                     "Service salaries 70th pc",
                     "Service salaries 90th pc",
                     "Canada",
                     "Australia",
                     "U.S.A.",
                     "University Ranking 40th pc",
                     "University Ranking 60th pc",
                     "University Ranking 90th pc")

plotdf3_chile <- data.frame(estimate = coeftest(model3_chile.clustrob, model3_chile.clustrob$vcovCL)[,1],SE = coeftest(model3_chile.clustrob, model3_chile.clustrob$vcovCL)[,2])
plotdf3_chile <- plotdf3_chile[-1,]
plotdf3_chile <- rbind(plotdf3_chile[1,],
                       c(0,0),
                       plotdf3_chile[2:11,])
plotdf3_chile <- rbind(plotdf3_chile[1:4,],
                       c(0,0),
                       plotdf3_chile[5:12,])
plotdf3_chile <- rbind(plotdf3_chile[1:7,],
                       c(0,0),
                       plotdf3_chile[8:13,])
plotdf3_chile <- rbind(plotdf3_chile[1:10,],
                       c(0,0),
                       plotdf3_chile[11:14,])
plotdf3_chile <- rbind(plotdf3_chile[1:13,],
                       c(0,0),
                       plotdf3_chile[14,])
plotdf3_chile$coef <- c("Generous family allowance",
                        "Basic minimum wage",
                        "No minimum wage or income support",
                        "GDP 2 percent",
                        "GDP 4 percent",
                        "GDP 6 percent",
                        "Service salaries 50th pc",
                        "Service salaries 70th pc",
                        "Service salaries 90th pc",
                        "U.S.A.",
                        "Australia",
                        "U.K.",
                        "University Ranking 40th pc",
                        "University Ranking 60th pc",
                        "University Ranking 90th pc")

plotdf3_china <- data.frame(estimate = coeftest(model3_china.clustrob, model3_china.clustrob$vcovCL)[,1],SE = coeftest(model3_china.clustrob, model3_china.clustrob$vcovCL)[,2])
plotdf3_china <- plotdf3_china[-1,]
plotdf3_china <- rbind(plotdf3_china[1,],
                       c(0,0),
                       plotdf3_china[2:11,])
plotdf3_china <- rbind(plotdf3_china[1:4,],
                       c(0,0),
                       plotdf3_china[5:12,])
plotdf3_china <- rbind(plotdf3_china[1:7,],
                       c(0,0),
                       plotdf3_china[8:13,])
plotdf3_china <- rbind(plotdf3_china[1:10,],
                       c(0,0),
                       plotdf3_china[11:14,])
plotdf3_china <- rbind(plotdf3_china[1:13,],
                       c(0,0),
                       plotdf3_china[14,])
plotdf3_china$coef <- c("Generous family allowance",
                        "Basic minimum wage",
                        "No minimum wage or income support",
                        "GDP 2 percent",
                        "GDP 4 percent",
                        "GDP 6 percent",
                        "Service salaries 50th pc",
                        "Service salaries 70th pc",
                        "Service salaries 90th pc",
                        "U.S.A.",
                        "Australia",
                        "U.K.",
                        "University Ranking 40th pc",
                        "University Ranking 60th pc",
                        "University Ranking 90th pc")

plotdf3_india <- data.frame(estimate = coeftest(model3_india.clustrob, model3_india.clustrob$vcovCL)[,1],SE = coeftest(model3_india.clustrob, model3_india.clustrob$vcovCL)[,2])
plotdf3_india <- plotdf3_india[-1,]
plotdf3_india <- rbind(plotdf3_india[1,],
                       c(0,0),
                       plotdf3_india[2:11,])
plotdf3_india <- rbind(plotdf3_india[1:4,],
                       c(0,0),
                       plotdf3_india[5:12,])
plotdf3_india <- rbind(plotdf3_india[1:7,],
                       c(0,0),
                       plotdf3_india[8:13,])
plotdf3_india <- rbind(plotdf3_india[1:10,],
                       c(0,0),
                       plotdf3_india[11:14,])
plotdf3_india <- rbind(plotdf3_india[1:13,],
                       c(0,0),
                       plotdf3_india[14,])
plotdf3_india$coef <- c("Generous family allowance",
                        "Basic minimum wage",
                        "No minimum wage or income support",
                        "GDP 2 percent",
                        "GDP 4 percent",
                        "GDP 6 percent",
                        "Service salaries 50th pc",
                        "Service salaries 70th pc",
                        "Service salaries 90th pc",
                        "U.S.A.",
                        "Australia",
                        "U.K.",
                        "University Ranking 40th pc",
                        "University Ranking 60th pc",
                        "University Ranking 90th pc")

plotdf3_uk$survey <- "UK"
plotdf3_chile$survey <- "Chile"
plotdf3_china$survey <- "China"
plotdf3_india$survey <- "India"

plotdf3 <- rbind(plotdf3_uk,plotdf3_chile, plotdf3_china, plotdf3_india)

plotdf3$LCI <- plotdf3$estimate-1.96*plotdf3$SE
plotdf3$UCI <- plotdf3$estimate+1.96*plotdf3$SE

plotdf3$coef <- factor(plotdf3$coef, levels = c("University Ranking 90th pc",
                                                "University Ranking 60th pc",
                                                "University Ranking 40th pc",
                                                "U.S.A.",
                                                "Australia",
                                                "Canada",
                                                "U.K.",
                                                "Service salaries 90th pc",
                                                "Service salaries 70th pc",
                                                "Service salaries 50th pc",
                                                "GDP 6 percent",
                                                "GDP 4 percent",
                                                "GDP 2 percent",
                                                "No minimum wage or income support",
                                                "Basic minimum wage",
                                                "Generous family allowance",
                                                "Intercept"))

# Combine into one plot

plotdf1 <- mutate(plotdf1,treatment = "Treatment 1")
plotdf2 <- mutate(plotdf2,treatment = "Treatment 2")
plotdf3 <- mutate(plotdf3,treatment = "Treatment 3")

plotdf <- rbind(plotdf1,plotdf2,plotdf3)
plotdf$coef <- factor(plotdf$coef, levels = c("University Ranking 90th pc",
                                              "University Ranking 60th pc",
                                              "University Ranking 40th pc",
                                              "U.S.A.",
                                              "Australia",
                                              "Canada",
                                              "U.K.",
                                              "Deportation of all illegal immigrants",
                                              "Muslim Ban",
                                              "Change in visa processing centres",
                                              "Point-system visa",
                                              "Service salaries 90th pc",
                                              "Service salaries 70th pc",
                                              "Service salaries 50th pc",
                                              "GDP 6 percent",
                                              "GDP 4 percent",
                                              "GDP 2 percent",
                                              "No minimum wage or income support",
                                              "Basic minimum wage",
                                              "Generous family allowance",
                                              "Intercept"))

#### Ideology figure prep ####

plotdf1_left_uk <- data.frame(estimate = coeftest(model1_left_uk.clustrob, model1_left_uk.clustrob$vcovCL)[,1],SE = coeftest(model1_left_uk.clustrob, model1_left_uk.clustrob$vcovCL)[,2],Ideology = "Left",Country = "UK",Treatment = "Treatment: 1")
plotdf1_centre_uk <- data.frame(estimate = coeftest(model1_centre_uk.clustrob, model1_centre_uk.clustrob$vcovCL)[,1],SE = coeftest(model1_centre_uk.clustrob, model1_centre_uk.clustrob$vcovCL)[,2],Ideology = "Centre",Country = "UK",Treatment = "Treatment: 1")
plotdf1_right_uk <- data.frame(estimate = coeftest(model1_right_uk.clustrob, model1_right_uk.clustrob$vcovCL)[,1],SE = coeftest(model1_right_uk.clustrob, model1_right_uk.clustrob$vcovCL)[,2],Ideology = "Right",Country = "UK",Treatment = "Treatment: 1")
plotdf1_left_chile <- data.frame(estimate = coeftest(model1_left_chile.clustrob, model1_left_chile.clustrob$vcovCL)[,1],SE = coeftest(model1_left_chile.clustrob, model1_left_chile.clustrob$vcovCL)[,2],Ideology = "Left",Country = "Chile",Treatment = "Treatment: 1")
plotdf1_centre_chile <- data.frame(estimate = coeftest(model1_centre_chile.clustrob, model1_centre_chile.clustrob$vcovCL)[,1],SE = coeftest(model1_centre_chile.clustrob, model1_centre_chile.clustrob$vcovCL)[,2],Ideology = "Centre",Country = "Chile",Treatment = "Treatment: 1")
plotdf1_right_chile <- data.frame(estimate = coeftest(model1_right_chile.clustrob, model1_right_chile.clustrob$vcovCL)[,1],SE = coeftest(model1_right_chile.clustrob, model1_right_chile.clustrob$vcovCL)[,2],Ideology = "Right",Country = "Chile",Treatment = "Treatment: 1")
plotdf1_left_china <- data.frame(estimate = coeftest(model1_left_china.clustrob, model1_left_china.clustrob$vcovCL)[,1],SE = coeftest(model1_left_china.clustrob, model1_left_china.clustrob$vcovCL)[,2],Ideology = "Left",Country = "China", Treatment = "Treatment: 1")
plotdf1_centre_china <- data.frame(estimate = coeftest(model1_centre_china.clustrob, model1_centre_china.clustrob$vcovCL)[,1],SE = coeftest(model1_centre_china.clustrob, model1_centre_china.clustrob$vcovCL)[,2],Ideology = "Centre",Country = "China", Treatment = "Treatment: 1")
plotdf1_right_china <- data.frame(estimate = coeftest(model1_right_china.clustrob, model1_right_china.clustrob$vcovCL)[,1],SE = coeftest(model1_right_china.clustrob, model1_right_china.clustrob$vcovCL)[,2],Ideology = "Right",Country = "China", Treatment = "Treatment: 1")
plotdf1_left_india <- data.frame(estimate = coeftest(model1_left_india.clustrob, model1_left_india.clustrob$vcovCL)[,1],SE = coeftest(model1_left_india.clustrob, model1_left_india.clustrob$vcovCL)[,2],Ideology = "Left",Country = "India",Treatment = "Treatment: 1")
plotdf1_centre_india <- data.frame(estimate = coeftest(model1_centre_india.clustrob, model1_centre_india.clustrob$vcovCL)[,1],SE = coeftest(model1_centre_india.clustrob, model1_centre_india.clustrob$vcovCL)[,2],Ideology = "Centre",Country = "India",Treatment = "Treatment: 1")
plotdf1_right_india <- data.frame(estimate = coeftest(model1_right_india.clustrob, model1_right_india.clustrob$vcovCL)[,1],SE = coeftest(model1_right_india.clustrob, model1_right_india.clustrob$vcovCL)[,2],Ideology = "Right",Country = "India",Treatment = "Treatment: 1")

t1_vars <- c("Intercept","Generous family allowance","No minimum wage",
          "GDP 2 percent","GDP 6 percent",
          "Service Salaries 50th pc","Service Salaries 90th pc",
          "Point-system visa","Muslim Ban",
          "University Ranking 40th pc","University Ranking 90th pc")

plotdf1_ideology <- rbind(plotdf1_left_uk,plotdf1_left_chile,plotdf1_left_china,plotdf1_left_india,
                          plotdf1_centre_uk,plotdf1_centre_chile,plotdf1_centre_china,plotdf1_centre_india,
                          plotdf1_right_uk,plotdf1_right_chile,plotdf1_right_china,plotdf1_right_india)
plotdf1_ideology <- cbind(plotdf1_ideology, vars = t1_vars)

plotdf2_left_uk <- data.frame(estimate = coeftest(model2_left_uk.clustrob, model2_left_uk.clustrob$vcovCL)[,1],SE = coeftest(model2_left_uk.clustrob, model2_left_uk.clustrob$vcovCL)[,2],Ideology = "Left",Country = "UK",Treatment = "Treatment: 2")
plotdf2_centre_uk <- data.frame(estimate = coeftest(model2_centre_uk.clustrob, model2_centre_uk.clustrob$vcovCL)[,1],SE = coeftest(model2_centre_uk.clustrob, model2_centre_uk.clustrob$vcovCL)[,2],Ideology = "Centre",Country = "UK",Treatment = "Treatment: 2")
plotdf2_right_uk <- data.frame(estimate = coeftest(model2_right_uk.clustrob, model2_right_uk.clustrob$vcovCL)[,1],SE = coeftest(model2_right_uk.clustrob, model2_right_uk.clustrob$vcovCL)[,2],Ideology = "Right",Country = "UK",Treatment = "Treatment: 2")
plotdf2_left_chile <- data.frame(estimate = coeftest(model2_left_chile.clustrob, model2_left_chile.clustrob$vcovCL)[,1],SE = coeftest(model2_left_chile.clustrob, model2_left_chile.clustrob$vcovCL)[,2],Ideology = "Left",Country = "Chile",Treatment = "Treatment: 2")
plotdf2_centre_chile <- data.frame(estimate = coeftest(model2_centre_chile.clustrob, model2_centre_chile.clustrob$vcovCL)[,1],SE = coeftest(model2_centre_chile.clustrob, model2_centre_chile.clustrob$vcovCL)[,2],Ideology = "Centre",Country = "Chile",Treatment = "Treatment: 2")
plotdf2_right_chile <- data.frame(estimate = coeftest(model2_right_chile.clustrob, model2_right_chile.clustrob$vcovCL)[,1],SE = coeftest(model2_right_chile.clustrob, model2_right_chile.clustrob$vcovCL)[,2],Ideology = "Right",Country = "Chile",Treatment = "Treatment: 2")
plotdf2_left_china <- data.frame(estimate = coeftest(model2_left_china.clustrob, model2_left_china.clustrob$vcovCL)[,1],SE = coeftest(model2_left_china.clustrob, model2_left_china.clustrob$vcovCL)[,2],Ideology = "Left",Country = "China", Treatment = "Treatment: 2")
plotdf2_centre_china <- data.frame(estimate = coeftest(model2_centre_china.clustrob, model2_centre_china.clustrob$vcovCL)[,1],SE = coeftest(model2_centre_china.clustrob, model2_centre_china.clustrob$vcovCL)[,2],Ideology = "Centre",Country = "China", Treatment = "Treatment: 2")
plotdf2_right_china <- data.frame(estimate = coeftest(model2_right_china.clustrob, model2_right_china.clustrob$vcovCL)[,1],SE = coeftest(model2_right_china.clustrob, model2_right_china.clustrob$vcovCL)[,2],Ideology = "Right",Country = "China", Treatment = "Treatment: 2")
plotdf2_left_india <- data.frame(estimate = coeftest(model2_left_india.clustrob, model2_left_india.clustrob$vcovCL)[,1],SE = coeftest(model2_left_india.clustrob, model2_left_india.clustrob$vcovCL)[,2],Ideology = "Left",Country = "India",Treatment = "Treatment: 2")
plotdf2_centre_india <- data.frame(estimate = coeftest(model2_centre_india.clustrob, model2_centre_india.clustrob$vcovCL)[,1],SE = coeftest(model2_centre_india.clustrob, model2_centre_india.clustrob$vcovCL)[,2],Ideology = "Centre",Country = "India",Treatment = "Treatment: 2")
plotdf2_right_india <- data.frame(estimate = coeftest(model2_right_india.clustrob, model2_right_india.clustrob$vcovCL)[,1],SE = coeftest(model2_right_india.clustrob, model2_right_india.clustrob$vcovCL)[,2],Ideology = "Right",Country = "India",Treatment = "Treatment: 2")

t2_vars <- c("Intercept","Generous family allowance","No minimum wage",
          "GDP 2 percent","GDP 6 percent",
          "Service Salaries 50th pc","Service Salaries 90th pc",
          "Deportation of all illegal immigrants","Point-system visa",
          "University Ranking 40th pc","University Ranking 90th pc")

plotdf2_ideology <- rbind(plotdf2_left_uk,plotdf2_left_chile,plotdf2_left_china,plotdf2_left_india,
                          plotdf2_centre_uk,plotdf2_centre_chile,plotdf2_centre_china,plotdf2_centre_india,
                          plotdf2_right_uk,plotdf2_right_chile,plotdf2_right_china,plotdf2_right_india)
plotdf2_ideology <- cbind(plotdf2_ideology, vars = t2_vars)

plotdf3_left_uk <- data.frame(estimate = coeftest(model3_left_uk.clustrob, model3_left_uk.clustrob$vcovCL)[,1],SE = coeftest(model3_left_uk.clustrob, model3_left_uk.clustrob$vcovCL)[,2],Ideology = "Left",Country = "UK",Treatment = "Treatment: 3")
plotdf3_centre_uk <- data.frame(estimate = coeftest(model3_centre_uk.clustrob, model3_centre_uk.clustrob$vcovCL)[,1],SE = coeftest(model3_centre_uk.clustrob, model3_centre_uk.clustrob$vcovCL)[,2],Ideology = "Centre",Country = "UK",Treatment = "Treatment: 3")
plotdf3_right_uk <- data.frame(estimate = coeftest(model3_right_uk.clustrob, model3_right_uk.clustrob$vcovCL)[,1],SE = coeftest(model3_right_uk.clustrob, model3_right_uk.clustrob$vcovCL)[,2],Ideology = "Right",Country = "UK",Treatment = "Treatment: 3")
plotdf3_left_chile <- data.frame(estimate = coeftest(model3_left_chile.clustrob, model3_left_chile.clustrob$vcovCL)[,1],SE = coeftest(model3_left_chile.clustrob, model3_left_chile.clustrob$vcovCL)[,2],Ideology = "Left",Country = "Chile",Treatment = "Treatment: 3")
plotdf3_centre_chile <- data.frame(estimate = coeftest(model3_centre_chile.clustrob, model3_centre_chile.clustrob$vcovCL)[,1],SE = coeftest(model3_centre_chile.clustrob, model3_centre_chile.clustrob$vcovCL)[,2],Ideology = "Centre",Country = "Chile",Treatment = "Treatment: 3")
plotdf3_right_chile <- data.frame(estimate = coeftest(model3_right_chile.clustrob, model3_right_chile.clustrob$vcovCL)[,1],SE = coeftest(model3_right_chile.clustrob, model3_right_chile.clustrob$vcovCL)[,2],Ideology = "Right",Country = "Chile",Treatment = "Treatment: 3")
plotdf3_left_china <- data.frame(estimate = coeftest(model3_left_china.clustrob, model3_left_china.clustrob$vcovCL)[,1],SE = coeftest(model3_left_china.clustrob, model3_left_china.clustrob$vcovCL)[,2],Ideology = "Left",Country = "China", Treatment = "Treatment: 3")
plotdf3_centre_china <- data.frame(estimate = coeftest(model3_centre_china.clustrob, model3_centre_china.clustrob$vcovCL)[,1],SE = coeftest(model3_centre_china.clustrob, model3_centre_china.clustrob$vcovCL)[,2],Ideology = "Centre",Country = "China", Treatment = "Treatment: 3")
plotdf3_right_china <- data.frame(estimate = coeftest(model3_right_china.clustrob, model3_right_china.clustrob$vcovCL)[,1],SE = coeftest(model3_right_china.clustrob, model3_right_china.clustrob$vcovCL)[,2],Ideology = "Right",Country = "China", Treatment = "Treatment: 3")
plotdf3_left_india <- data.frame(estimate = coeftest(model3_left_india.clustrob, model3_left_india.clustrob$vcovCL)[,1],SE = coeftest(model3_left_india.clustrob, model3_left_india.clustrob$vcovCL)[,2],Ideology = "Left",Country = "India",Treatment = "Treatment: 3")
plotdf3_centre_india <- data.frame(estimate = coeftest(model3_centre_india.clustrob, model3_centre_india.clustrob$vcovCL)[,1],SE = coeftest(model3_centre_india.clustrob, model3_centre_india.clustrob$vcovCL)[,2],Ideology = "Centre",Country = "India",Treatment = "Treatment: 3")
plotdf3_right_india <- data.frame(estimate = coeftest(model3_right_india.clustrob, model3_right_india.clustrob$vcovCL)[,1],SE = coeftest(model3_right_india.clustrob, model3_right_india.clustrob$vcovCL)[,2],Ideology = "Right",Country = "India",Treatment = "Treatment: 3")

t3_vars_uk <- c("Intercept","Generous family allowance","No minimum wage",
          "GDP 2 percent","GDP 6 percent",
          "Service Salaries 50th pc","Service Salaries 90th pc",
          "Canada","U.S.A.",
          "University Ranking 40th pc","University Ranking 90th pc")

plotdf3_ideology_uk <- rbind(plotdf3_left_uk,plotdf3_centre_uk,plotdf3_right_uk)
plotdf3_ideology_uk <- cbind(plotdf3_ideology_uk, vars = t3_vars_uk)

t3_vars <- c("Intercept","Generous family allowance","No minimum wage",
             "GDP 2 percent","GDP 6 percent",
             "Service Salaries 50th pc","Service Salaries 90th pc",
             "U.S.A.","U.K.",
             "University Ranking 40th pc","University Ranking 90th pc")

plotdf3_ideology_cci <- rbind(plotdf3_left_chile,plotdf3_left_china,plotdf3_left_india,
                              plotdf3_centre_chile,plotdf3_centre_china,plotdf3_centre_india,
                              plotdf3_right_chile,plotdf3_right_china,plotdf3_right_india)
plotdf3_ideology_cci <- cbind(plotdf3_ideology_cci, vars = t3_vars)

plotdf3_ideology <- rbind(plotdf3_ideology_uk,plotdf3_ideology_cci)

plotdf_ideology <- rbind(plotdf1_ideology,plotdf2_ideology,plotdf3_ideology)

#### Likely only conjoint figure prep ####

plotdf1_likely_uk <- data.frame(estimate = coeftest(model1_likely_uk.clustrob, model1_likely_uk.clustrob$vcovCL)[,1],
                                  SE = coeftest(model1_likely_uk.clustrob, model1_likely_uk.clustrob$vcovCL)[,2])

# Add in empty spots for reference categories
plotdf1_likely_uk <- plotdf1_likely_uk[-1,]
plotdf1_likely_uk <- rbind(plotdf1_likely_uk[1,],
                             c(0,0),
                             plotdf1_likely_uk[2:11,])
plotdf1_likely_uk <- rbind(plotdf1_likely_uk[1:4,],
                             c(0,0),
                             plotdf1_likely_uk[5:12,])
plotdf1_likely_uk <- rbind(plotdf1_likely_uk[1:7,],
                             c(0,0),
                             plotdf1_likely_uk[8:13,])
plotdf1_likely_uk <- rbind(plotdf1_likely_uk[1:10,],
                             c(0,0),
                             plotdf1_likely_uk[11:14,])
plotdf1_likely_uk <- rbind(plotdf1_likely_uk[1:13,],
                             c(0,0),
                             plotdf1_likely_uk[14,])
plotdf1_likely_uk$coef <- c("Generous family allowance",
                              "Basic minimum wage",
                              "No minimum wage or income support",
                              "GDP 2 percent",
                              "GDP 4 percent",
                              "GDP 6 percent",
                              "Service salaries 50th pc",
                              "Service salaries 70th pc",
                              "Service salaries 90th pc",
                              "Point-system visa",
                              "Change in visa processing centres",
                              "Muslim Ban",
                              "University Ranking 40th pc",
                              "University Ranking 60th pc",
                              "University Ranking 90th pc")

plotdf1_likely_chile <- data.frame(estimate = coeftest(model1_likely_chile.clustrob, model1_likely_chile.clustrob$vcovCL)[,1],SE = coeftest(model1_likely_chile.clustrob, model1_likely_chile.clustrob$vcovCL)[,2])
plotdf1_likely_chile <- plotdf1_likely_chile[-1,]
plotdf1_likely_chile <- rbind(plotdf1_likely_chile[1,],
                                c(0,0),
                                plotdf1_likely_chile[2:11,])
plotdf1_likely_chile <- rbind(plotdf1_likely_chile[1:4,],
                                c(0,0),
                                plotdf1_likely_chile[5:12,])
plotdf1_likely_chile <- rbind(plotdf1_likely_chile[1:7,],
                                c(0,0),
                                plotdf1_likely_chile[8:13,])
plotdf1_likely_chile <- rbind(plotdf1_likely_chile[1:10,],
                                c(0,0),
                                plotdf1_likely_chile[11:14,])
plotdf1_likely_chile <- rbind(plotdf1_likely_chile[1:13,],
                                c(0,0),
                                plotdf1_likely_chile[14,])
plotdf1_likely_chile$coef <- c("Generous family allowance",
                                 "Basic minimum wage",
                                 "No minimum wage or income support",
                                 "GDP 2 percent",
                                 "GDP 4 percent",
                                 "GDP 6 percent",
                                 "Service salaries 50th pc",
                                 "Service salaries 70th pc",
                                 "Service salaries 90th pc",
                                 "Point-system visa",
                                 "Change in visa processing centres",
                                 "Muslim Ban",
                                 "University Ranking 40th pc",
                                 "University Ranking 60th pc",
                                 "University Ranking 90th pc")

plotdf1_likely_china <- data.frame(estimate = coeftest(model1_likely_china.clustrob, model1_likely_china.clustrob$vcovCL)[,1],SE = coeftest(model1_likely_china.clustrob, model1_likely_china.clustrob$vcovCL)[,2])
plotdf1_likely_china <- plotdf1_likely_china[-1,]
plotdf1_likely_china <- rbind(plotdf1_likely_china[1,],
                                c(0,0),
                                plotdf1_likely_china[2:11,])
plotdf1_likely_china <- rbind(plotdf1_likely_china[1:4,],
                                c(0,0),
                                plotdf1_likely_china[5:12,])
plotdf1_likely_china <- rbind(plotdf1_likely_china[1:7,],
                                c(0,0),
                                plotdf1_likely_china[8:13,])
plotdf1_likely_china <- rbind(plotdf1_likely_china[1:10,],
                                c(0,0),
                                plotdf1_likely_china[11:14,])
plotdf1_likely_china <- rbind(plotdf1_likely_china[1:13,],
                                c(0,0),
                                plotdf1_likely_china[14,])
plotdf1_likely_china$coef <- c("Generous family allowance",
                                 "Basic minimum wage",
                                 "No minimum wage or income support",
                                 "GDP 2 percent",
                                 "GDP 4 percent",
                                 "GDP 6 percent",
                                 "Service salaries 50th pc",
                                 "Service salaries 70th pc",
                                 "Service salaries 90th pc",
                                 "Point-system visa",
                                 "Change in visa processing centres",
                                 "Muslim Ban",
                                 "University Ranking 40th pc",
                                 "University Ranking 60th pc",
                                 "University Ranking 90th pc")

plotdf1_likely_india <- data.frame(estimate = coeftest(model1_likely_india.clustrob, model1_likely_india.clustrob$vcovCL)[,1],SE = coeftest(model1_likely_india.clustrob, model1_likely_india.clustrob$vcovCL)[,2])
plotdf1_likely_india <- plotdf1_likely_india[-1,]
plotdf1_likely_india <- rbind(plotdf1_likely_india[1,],
                                c(0,0),
                                plotdf1_likely_india[2:11,])
plotdf1_likely_india <- rbind(plotdf1_likely_india[1:4,],
                                c(0,0),
                                plotdf1_likely_india[5:12,])
plotdf1_likely_india <- rbind(plotdf1_likely_india[1:7,],
                                c(0,0),
                                plotdf1_likely_india[8:13,])
plotdf1_likely_india <- rbind(plotdf1_likely_india[1:10,],
                                c(0,0),
                                plotdf1_likely_india[11:14,])
plotdf1_likely_india <- rbind(plotdf1_likely_india[1:13,],
                                c(0,0),
                                plotdf1_likely_india[14,])
plotdf1_likely_india$coef <- c("Generous family allowance",
                                 "Basic minimum wage",
                                 "No minimum wage or income support",
                                 "GDP 2 percent",
                                 "GDP 4 percent",
                                 "GDP 6 percent",
                                 "Service salaries 50th pc",
                                 "Service salaries 70th pc",
                                 "Service salaries 90th pc",
                                 "Point-system visa",
                                 "Change in visa processing centres",
                                 "Muslim Ban",
                                 "University Ranking 40th pc",
                                 "University Ranking 60th pc",
                                 "University Ranking 90th pc")

plotdf1_likely_uk$survey <- "UK"
plotdf1_likely_chile$survey <- "Chile"
plotdf1_likely_china$survey <- "China"
plotdf1_likely_india$survey <- "India"

plotdf1_likely <- rbind(plotdf1_likely_uk,plotdf1_likely_chile, plotdf1_likely_china, plotdf1_likely_india)

plotdf1_likely$coef <- factor(plotdf1_likely$coef, levels = c("University Ranking 90th pc",
                                                                  "University Ranking 60th pc",
                                                                  "University Ranking 40th pc",
                                                                  "Muslim Ban",
                                                                  "Change in visa processing centres",
                                                                  "Point-system visa",
                                                                  "Service salaries 90th pc",
                                                                  "Service salaries 70th pc",
                                                                  "Service salaries 50th pc",
                                                                  "GDP 6 percent",
                                                                  "GDP 4 percent",
                                                                  "GDP 2 percent",
                                                                  "No minimum wage or income support",
                                                                  "Basic minimum wage",
                                                                  "Generous family allowance",
                                                                  "Intercept"))

plotdf1_likely$LCI <- plotdf1_likely$estimate-1.96*plotdf1_likely$SE
plotdf1_likely$UCI <- plotdf1_likely$estimate+1.96*plotdf1_likely$SE

#Model 2
plotdf2_likely_uk <- data.frame(estimate = coeftest(model2_likely_uk.clustrob, model2_likely_uk.clustrob$vcovCL)[,1],SE = coeftest(model2_likely_uk.clustrob, model2_likely_uk.clustrob$vcovCL)[,2])
plotdf2_likely_uk <- plotdf2_likely_uk[-1,]
plotdf2_likely_uk <- rbind(plotdf2_likely_uk[1,],
                             c(0,0),
                             plotdf2_likely_uk[2:11,])
plotdf2_likely_uk <- rbind(plotdf2_likely_uk[1:4,],
                             c(0,0),
                             plotdf2_likely_uk[5:12,])
plotdf2_likely_uk <- rbind(plotdf2_likely_uk[1:7,],
                             c(0,0),
                             plotdf2_likely_uk[8:13,])
plotdf2_likely_uk <- rbind(plotdf2_likely_uk[1:10,],
                             c(0,0),
                             plotdf2_likely_uk[11:14,])
plotdf2_likely_uk <- rbind(plotdf2_likely_uk[1:13,],
                             c(0,0),
                             plotdf2_likely_uk[14,])
plotdf2_likely_uk$coef <- c("Generous family allowance",
                              "Basic minimum wage",
                              "No minimum wage or income support",
                              "GDP 2 percent",
                              "GDP 4 percent",
                              "GDP 6 percent",
                              "Service salaries 50th pc",
                              "Service salaries 70th pc",
                              "Service salaries 90th pc",
                              "Deportation of all illegal immigrants",
                              "Change in visa processing centres",
                              "Point-system visa",
                              "University Ranking 40th pc",
                              "University Ranking 60th pc",
                              "University Ranking 90th pc")

plotdf2_likely_chile <- data.frame(estimate = coeftest(model2_likely_chile.clustrob, model2_likely_chile.clustrob$vcovCL)[,1],SE = coeftest(model2_likely_chile.clustrob, model2_likely_chile.clustrob$vcovCL)[,2])
plotdf2_likely_chile <- plotdf2_likely_chile[-1,]
plotdf2_likely_chile <- rbind(plotdf2_likely_chile[1,],
                                c(0,0),
                                plotdf2_likely_chile[2:11,])
plotdf2_likely_chile <- rbind(plotdf2_likely_chile[1:4,],
                                c(0,0),
                                plotdf2_likely_chile[5:12,])
plotdf2_likely_chile <- rbind(plotdf2_likely_chile[1:7,],
                                c(0,0),
                                plotdf2_likely_chile[8:13,])
plotdf2_likely_chile <- rbind(plotdf2_likely_chile[1:10,],
                                c(0,0),
                                plotdf2_likely_chile[11:14,])
plotdf2_likely_chile <- rbind(plotdf2_likely_chile[1:13,],
                                c(0,0),
                                plotdf2_likely_chile[14,])
plotdf2_likely_chile$coef <- c("Generous family allowance",
                                 "Basic minimum wage",
                                 "No minimum wage or income support",
                                 "GDP 2 percent",
                                 "GDP 4 percent",
                                 "GDP 6 percent",
                                 "Service salaries 50th pc",
                                 "Service salaries 70th pc",
                                 "Service salaries 90th pc",
                                 "Deportation of all illegal immigrants",
                                 "Change in visa processing centres",
                                 "Point-system visa",
                                 "University Ranking 40th pc",
                                 "University Ranking 60th pc",
                                 "University Ranking 90th pc")

plotdf2_likely_china <- data.frame(estimate = coeftest(model2_likely_china.clustrob, model2_likely_china.clustrob$vcovCL)[,1],SE = coeftest(model2_likely_china.clustrob, model2_likely_china.clustrob$vcovCL)[,2])
plotdf2_likely_china <- plotdf2_likely_china[-1,]
plotdf2_likely_china <- rbind(plotdf2_likely_china[1,],
                                c(0,0),
                                plotdf2_likely_china[2:11,])
plotdf2_likely_china <- rbind(plotdf2_likely_china[1:4,],
                                c(0,0),
                                plotdf2_likely_china[5:12,])
plotdf2_likely_china <- rbind(plotdf2_likely_china[1:7,],
                                c(0,0),
                                plotdf2_likely_china[8:13,])
plotdf2_likely_china <- rbind(plotdf2_likely_china[1:10,],
                                c(0,0),
                                plotdf2_likely_china[11:14,])
plotdf2_likely_china <- rbind(plotdf2_likely_china[1:13,],
                                c(0,0),
                                plotdf2_likely_china[14,])
plotdf2_likely_china$coef <- c("Generous family allowance",
                                 "Basic minimum wage",
                                 "No minimum wage or income support",
                                 "GDP 2 percent",
                                 "GDP 4 percent",
                                 "GDP 6 percent",
                                 "Service salaries 50th pc",
                                 "Service salaries 70th pc",
                                 "Service salaries 90th pc",
                                 "Deportation of all illegal immigrants",
                                 "Change in visa processing centres",
                                 "Point-system visa",
                                 "University Ranking 40th pc",
                                 "University Ranking 60th pc",
                                 "University Ranking 90th pc")

plotdf2_likely_india <- data.frame(estimate = coeftest(model2_likely_india.clustrob, model2_likely_india.clustrob$vcovCL)[,1],SE = coeftest(model2_likely_india.clustrob, model2_likely_india.clustrob$vcovCL)[,2])
plotdf2_likely_india <- plotdf2_likely_india[-1,]
plotdf2_likely_india <- rbind(plotdf2_likely_india[1,],
                                c(0,0),
                                plotdf2_likely_india[2:11,])
plotdf2_likely_india <- rbind(plotdf2_likely_india[1:4,],
                                c(0,0),
                                plotdf2_likely_india[5:12,])
plotdf2_likely_india <- rbind(plotdf2_likely_india[1:7,],
                                c(0,0),
                                plotdf2_likely_india[8:13,])
plotdf2_likely_india <- rbind(plotdf2_likely_india[1:10,],
                                c(0,0),
                                plotdf2_likely_india[11:14,])
plotdf2_likely_india <- rbind(plotdf2_likely_india[1:13,],
                                c(0,0),
                                plotdf2_likely_india[14,])
plotdf2_likely_india$coef <- c("Generous family allowance",
                                 "Basic minimum wage",
                                 "No minimum wage or income support",
                                 "GDP 2 percent",
                                 "GDP 4 percent",
                                 "GDP 6 percent",
                                 "Service salaries 50th pc",
                                 "Service salaries 70th pc",
                                 "Service salaries 90th pc",
                                 "Deportation of all illegal immigrants",
                                 "Change in visa processing centres",
                                 "Point-system visa",
                                 "University Ranking 40th pc",
                                 "University Ranking 60th pc",
                                 "University Ranking 90th pc")

plotdf2_likely_uk$survey <- "UK"
plotdf2_likely_chile$survey <- "Chile"
plotdf2_likely_china$survey <- "China"
plotdf2_likely_india$survey <- "India"

plotdf2_likely<- rbind(plotdf2_likely_uk,plotdf2_likely_chile, plotdf2_likely_china, plotdf2_likely_india)

plotdf2_likely$LCI <- plotdf2_likely$estimate-1.96*plotdf2_likely$SE
plotdf2_likely$UCI <- plotdf2_likely$estimate+1.96*plotdf2_likely$SE

plotdf2_likely$coef <- factor(plotdf2_likely$coef, levels = c("University Ranking 90th pc",
                                                                  "University Ranking 60th pc",
                                                                  "University Ranking 40th pc",
                                                                  "Deportation of all illegal immigrants",
                                                                  "Change in visa processing centres",
                                                                  "Point-system visa",
                                                                  "Service salaries 90th pc",
                                                                  "Service salaries 70th pc",
                                                                  "Service salaries 50th pc",
                                                                  "GDP 6 percent",
                                                                  "GDP 4 percent",
                                                                  "GDP 2 percent",
                                                                  "No minimum wage or income support",
                                                                  "Basic minimum wage",
                                                                  "Generous family allowance",
                                                                  "Intercept"))
#Model 3
plotdf3_likely_uk <- data.frame(estimate = coeftest(model3_likely_uk.clustrob, model3_likely_uk.clustrob$vcovCL)[,1],SE = coeftest(model3_likely_uk.clustrob, model3_likely_uk.clustrob$vcovCL)[,2])
plotdf3_likely_uk <- plotdf3_likely_uk[-1,]
plotdf3_likely_uk <- rbind(plotdf3_likely_uk[1,],
                             c(0,0),
                             plotdf3_likely_uk[2:11,])
plotdf3_likely_uk <- rbind(plotdf3_likely_uk[1:4,],
                             c(0,0),
                             plotdf3_likely_uk[5:12,])
plotdf3_likely_uk <- rbind(plotdf3_likely_uk[1:7,],
                             c(0,0),
                             plotdf3_likely_uk[8:13,])
plotdf3_likely_uk <- rbind(plotdf3_likely_uk[1:10,],
                             c(0,0),
                             plotdf3_likely_uk[11:14,])
plotdf3_likely_uk <- rbind(plotdf3_likely_uk[1:13,],
                             c(0,0),
                             plotdf3_likely_uk[14,])
plotdf3_likely_uk$coef <- c("Generous family allowance",
                              "Basic minimum wage",
                              "No minimum wage or income support",
                              "GDP 2 percent",
                              "GDP 4 percent",
                              "GDP 6 percent",
                              "Service salaries 50th pc",
                              "Service salaries 70th pc",
                              "Service salaries 90th pc",
                              "Canada",
                              "Australia",
                              "U.S.A.",
                              "University Ranking 40th pc",
                              "University Ranking 60th pc",
                              "University Ranking 90th pc")

plotdf3_likely_chile <- data.frame(estimate = coeftest(model3_likely_chile.clustrob, model3_likely_chile.clustrob$vcovCL)[,1],SE = coeftest(model3_likely_chile.clustrob, model3_likely_chile.clustrob$vcovCL)[,2])
plotdf3_likely_chile <- plotdf3_likely_chile[-1,]
plotdf3_likely_chile <- rbind(plotdf3_likely_chile[1,],
                                c(0,0),
                                plotdf3_likely_chile[2:11,])
plotdf3_likely_chile <- rbind(plotdf3_likely_chile[1:4,],
                                c(0,0),
                                plotdf3_likely_chile[5:12,])
plotdf3_likely_chile <- rbind(plotdf3_likely_chile[1:7,],
                                c(0,0),
                                plotdf3_likely_chile[8:13,])
plotdf3_likely_chile <- rbind(plotdf3_likely_chile[1:10,],
                                c(0,0),
                                plotdf3_likely_chile[11:14,])
plotdf3_likely_chile <- rbind(plotdf3_likely_chile[1:13,],
                                c(0,0),
                                plotdf3_likely_chile[14,])
plotdf3_likely_chile$coef <- c("Generous family allowance",
                                 "Basic minimum wage",
                                 "No minimum wage or income support",
                                 "GDP 2 percent",
                                 "GDP 4 percent",
                                 "GDP 6 percent",
                                 "Service salaries 50th pc",
                                 "Service salaries 70th pc",
                                 "Service salaries 90th pc",
                                 "U.S.A.",
                                 "Australia",
                                 "U.K.",
                                 "University Ranking 40th pc",
                                 "University Ranking 60th pc",
                                 "University Ranking 90th pc")

plotdf3_likely_china <- data.frame(estimate = coeftest(model3_likely_china.clustrob, model3_likely_china.clustrob$vcovCL)[,1],SE = coeftest(model3_likely_china.clustrob, model3_likely_china.clustrob$vcovCL)[,2])
plotdf3_likely_china <- plotdf3_likely_china[-1,]
plotdf3_likely_china <- rbind(plotdf3_likely_china[1,],
                                c(0,0),
                                plotdf3_likely_china[2:11,])
plotdf3_likely_china <- rbind(plotdf3_likely_china[1:4,],
                                c(0,0),
                                plotdf3_likely_china[5:12,])
plotdf3_likely_china <- rbind(plotdf3_likely_china[1:7,],
                                c(0,0),
                                plotdf3_likely_china[8:13,])
plotdf3_likely_china <- rbind(plotdf3_likely_china[1:10,],
                                c(0,0),
                                plotdf3_likely_china[11:14,])
plotdf3_likely_china <- rbind(plotdf3_likely_china[1:13,],
                                c(0,0),
                                plotdf3_likely_china[14,])
plotdf3_likely_china$coef <- c("Generous family allowance",
                                 "Basic minimum wage",
                                 "No minimum wage or income support",
                                 "GDP 2 percent",
                                 "GDP 4 percent",
                                 "GDP 6 percent",
                                 "Service salaries 50th pc",
                                 "Service salaries 70th pc",
                                 "Service salaries 90th pc",
                                 "U.S.A.",
                                 "Australia",
                                 "U.K.",
                                 "University Ranking 40th pc",
                                 "University Ranking 60th pc",
                                 "University Ranking 90th pc")

plotdf3_likely_india <- data.frame(estimate = coeftest(model3_likely_india.clustrob, model3_likely_india.clustrob$vcovCL)[,1],SE = coeftest(model3_likely_india.clustrob, model3_likely_india.clustrob$vcovCL)[,2])
plotdf3_likely_india <- plotdf3_likely_india[-1,]
plotdf3_likely_india <- rbind(plotdf3_likely_india[1,],
                                c(0,0),
                                plotdf3_likely_india[2:11,])
plotdf3_likely_india <- rbind(plotdf3_likely_india[1:4,],
                                c(0,0),
                                plotdf3_likely_india[5:12,])
plotdf3_likely_india <- rbind(plotdf3_likely_india[1:7,],
                                c(0,0),
                                plotdf3_likely_india[8:13,])
plotdf3_likely_india <- rbind(plotdf3_likely_india[1:10,],
                                c(0,0),
                                plotdf3_likely_india[11:14,])
plotdf3_likely_india <- rbind(plotdf3_likely_india[1:13,],
                                c(0,0),
                                plotdf3_likely_india[14,])
plotdf3_likely_india$coef <- c("Generous family allowance",
                                 "Basic minimum wage",
                                 "No minimum wage or income support",
                                 "GDP 2 percent",
                                 "GDP 4 percent",
                                 "GDP 6 percent",
                                 "Service salaries 50th pc",
                                 "Service salaries 70th pc",
                                 "Service salaries 90th pc",
                                 "U.S.A.",
                                 "Australia",
                                 "U.K.",
                                 "University Ranking 40th pc",
                                 "University Ranking 60th pc",
                                 "University Ranking 90th pc")

plotdf3_likely_uk$survey <- "UK"
plotdf3_likely_chile$survey <- "Chile"
plotdf3_likely_china$survey <- "China"
plotdf3_likely_india$survey <- "India"

plotdf3_likely <- rbind(plotdf3_likely_uk,plotdf3_likely_chile, plotdf3_likely_china, plotdf3_likely_india)

plotdf3_likely$LCI <- plotdf3_likely$estimate-1.96*plotdf3_likely$SE
plotdf3_likely$UCI <- plotdf3_likely$estimate+1.96*plotdf3_likely$SE

plotdf3_likely$coef <- factor(plotdf3_likely$coef, levels = c("University Ranking 90th pc",
                                                                  "University Ranking 60th pc",
                                                                  "University Ranking 40th pc",
                                                                  "U.S.A.",
                                                                  "Australia",
                                                                  "Canada",
                                                                  "U.K.",
                                                                  "Service salaries 90th pc",
                                                                  "Service salaries 70th pc",
                                                                  "Service salaries 50th pc",
                                                                  "GDP 6 percent",
                                                                  "GDP 4 percent",
                                                                  "GDP 2 percent",
                                                                  "No minimum wage or income support",
                                                                  "Basic minimum wage",
                                                                  "Generous family allowance",
                                                                  "Intercept"))

# Combine into one plot

plotdf1_likely <- mutate(plotdf1_likely,treatment = "Treatment 1")
plotdf2_likely <- mutate(plotdf2_likely,treatment = "Treatment 2")
plotdf3_likely <- mutate(plotdf3_likely,treatment = "Treatment 3")

plotdf_likely <- rbind(plotdf1_likely,plotdf2_likely,plotdf3_likely)
plotdf_likely$coef <- factor(plotdf_likely$coef, 
                               levels = c("University Ranking 90th pc",
                                          "University Ranking 60th pc",
                                          "University Ranking 40th pc",
                                          "U.S.A.",
                                          "Australia",
                                          "Canada",
                                          "U.K.",
                                          "Deportation of all illegal immigrants",
                                          "Muslim Ban",
                                          "Change in visa processing centres",
                                          "Point-system visa",
                                          "Service salaries 90th pc",
                                          "Service salaries 70th pc",
                                          "Service salaries 50th pc",
                                          "GDP 6 percent",
                                          "GDP 4 percent",
                                          "GDP 2 percent",
                                          "No minimum wage or income support",
                                          "Basic minimum wage",
                                          "Generous family allowance",
                                          "Intercept"))

######################################################################
#### Figures ####
######################################################################
#### Figure 1 ####
tweets <- read_csv("data/trump-wall-tweets.csv") %>%
  rename(date = created_at) %>%
  mutate(date = substr(as.character(date),1,10),
         month = format(as.Date(date, "%m-%d-%Y"), "%Y-%m"),
         year = format(as.Date(date, "%m-%d-%Y"), "%Y"),
         date = as.Date(date, "%m-%d-%Y")) %>%
  filter(year > 2017)

ggplot(tweets, aes(x = date)) +
  stat_ecdf(geom="step") +
  geom_vline(xintercept = as.Date("06-20-2018","%m-%d-%Y"), linetype = "dashed") +
  annotate(geom="text", x=as.Date("06-12-2018","%m-%d-%Y"), y=0.65, label="Trump halts child separation policy",
           color="black", angle=90) +
  geom_vline(xintercept = as.Date("11-06-2018","%m-%d-%Y"), linetype = "dashed") +
  annotate(geom="text", x=as.Date("10-30-2018","%m-%d-%Y"), y=0.3, label="Midterm Election",
           color="black", angle=90) +
  geom_vline(xintercept = as.Date("12-22-2018","%m-%d-%Y"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("01-25-2019","%m-%d-%Y"), linetype = "dashed") +
  annotate(geom = "rect",xmin = as.Date("12-22-2018","%m-%d-%Y"),
           xmax = as.Date("01-25-2019","%m-%d-%Y"),
           ymin = 0, ymax = 1,
           fill = "grey", alpha = 0.5) +
  annotate(geom="text", x=as.Date("01-07-2019","%m-%d-%Y"), y=0.4, label="Government Shutdown",
           color="black", angle=90) +
  labs(x="", y = "Cumulative Border/Wall Tweets") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

ggsave(paste0("figures/figure_1.pdf"), width = 15, height = 12, units = c("cm"), dpi = 300)


#### Figure 2 ####

# Immigration subset plot
imm_plot <- plotdf %>% 
  filter(coef %in% c("Point-system visa",
                     "Change in visa processing centres",
                     "Muslim Ban",
                     "Deportation of all illegal immigrants",
                     "U.K.","Canada","Australia","U.S.A."))

ggplot(data = imm_plot, aes(x=coef, shape=survey, color = survey)) +
  facet_grid(.~treatment, scales = "free_y") +
  geom_point(aes(y=estimate), position=position_dodge(width = 0.9), size = 2) +
  geom_linerange(aes(max=UCI, min=LCI), position=position_dodge(width = 0.9)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(x="",y="Logit Coefficient", shape = "Subject Country", color = "Subject Country") +
  coord_flip() +
  theme(legend.position = "bottom")

ggsave(paste0("figures/figure_2.pdf"), width = 18, height = 10, units = c("cm"), dpi = 300)


                     
#### Figure 3 ####

plotdf_ideology$var_names <- factor(plotdf_ideology$vars, levels = c(
  "University Ranking 90th pc",
  "University Ranking 40th pc",
  "U.S.A.",
  "Canada",
  "U.K.",
  "Deportation of all illegal immigrants",
  "Muslim Ban",
  "Point-system visa",
  "Service Salaries 90th pc",
  "Service Salaries 50th pc",
  "GDP 6 percent",
  "GDP 2 percent",
  "No minimum wage",
  "Generous family allowance"))

plotdf_ideology$LCI <- plotdf_ideology$estimate-1.96*plotdf_ideology$SE
plotdf_ideology$UCI <- plotdf_ideology$estimate+1.96*plotdf_ideology$SE

ggplot(data = {plotdf_ideology %>% filter(var_names %in% c("U.S.A.",
                                                           "Canada",
                                                           "U.K.",
                                                           "Deportation of all illegal immigrants",
                                                           "Muslim Ban",
                                                           "Point-system visa"),
                                          Ideology != "Centre")},
       aes(x=vars, shape=Ideology, color = Ideology)) +
  facet_grid(Treatment~Country, scales="free_y", drop=T) +
  geom_point(aes(y=estimate), position=position_dodge(width = 0.7)) +
  geom_linerange(aes(max=UCI, min=LCI), position=position_dodge(width = 0.7)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(x="",y="Logit Coefficient") +
  theme(legend.position = "bottom") +
  coord_flip()

ggsave(paste0("figures/figure_3.pdf"), width = 20, height = 10, units = c("cm"), dpi = 300)


#### Figure 4 ####
likely_plot <- plotdf_likely %>% 
  filter(coef %in% c("Point-system visa",
                     "Change in visa processing centres",
                     "Muslim Ban",
                     "Deportation of all illegal immigrants",
                     "U.K.","Canada","Australia","U.S.A."))

ggplot(data = likely_plot, aes(x=coef, shape=survey, color = survey)) +
  facet_grid(.~treatment, scales = "free_y") +
  geom_point(aes(y=estimate), position=position_dodge(width = 0.9), size = 2) +
  geom_linerange(aes(max=UCI, min=LCI), position=position_dodge(width = 0.9)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(x="",y="Logit Coefficient", shape = "Subject Country", color = "Subject Country") +
  scale_alpha_manual(values = c(1,0.5)) +
  coord_flip() +
  theme(legend.position = "bottom")

ggsave(paste0("figures/figure_4.pdf"), width = 18, height = 10, units = c("cm"), dpi = 300)


######################################################################
##### Tables #####
######################################################################
#### Table 1 ####

# Non-analytic file -- generated manually

#### Table 2 ####

stargazer(model3_chile.clustrob, model3_china,model3_india.clustrob,model3_uk.clustrob,
          dep.var.caption = "Subject Country",
          model.numbers = FALSE,
          column.labels = c("Chile","China","India","UK"),
          dep.var.labels.include = FALSE,
          se = list(model3_chile.clustrob$se,model3_china.clustrob$se, model3_india.clustrob$se,model3_uk.clustrob$se),
          no.space = TRUE,
          title = "Treatment 3 (country labels) regression results by subjects' country",
          label = "tab:treat_3",
          omit.stat = "aic",
          notes = "Standard errors clustered by subject.",
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Canada",
                               "U.S.A",
                               "U.K.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc"),
          out = "tables/table_2.tex")

#### Table 3 ####

## See file `2_iet_analysis.R'

#### Table 4 ####

# Partial results for paper - NB final titles must be added manually due to stargazer limitations
stargazer(model1_age_young, model1_age_old, 
          model2_age_young, model2_age_old, 
          model3_age_young, model3_age_old,
          keep = c("immigration","country"),
          column.labels = c("Age $\\leq$ 25","Age > 25","Age $\\leq$ 25","Age > 25","Age $\\leq$ 25","Age > 25"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "",
          model.numbers = FALSE,
          se=list(model1_age_young.clustrob$se,model1_age_old.clustrob$se,
                  model2_age_young.clustrob$se,model2_age_old.clustrob$se,
                  model3_age_young.clustrob$se,model3_age_old.clustrob$se),
          no.space = TRUE,
          omit.stat = c("aic","ll"),
          covariate.labels = c("Deport illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "Canada",
                               "U.S.A",
                               "U.K."),
          add.lines = list(c("Fixed effects?", rep("Yes",6)),
                           c("Other attributes?", rep("Yes", 6))),
          title = "Partial regression results run on a pooled sample, by age cohort",
          label = "tab:imm_age",
          font.size  = "footnotesize",
          notes = "Standard errors clustered by subject.",
          digits = 3,
          out = "tables/table_4.tex")

