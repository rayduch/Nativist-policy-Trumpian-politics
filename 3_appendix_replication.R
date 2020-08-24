################################################################################
##                                                                            ##
##                      Populism: Magnet or Deterrent                         ##
##                        Appendix Replication Code                           ##
##                                                                            ##
##  Raymond Duch, Denise Laroze, Constantin Reinprecht, and Thomas Robinson   ##
##                                PSRM, 2020                                  ##
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
library(cregg)

theme_set(theme_bw())

source("0_main_models.R")

######################################################################
#### Appendix ####
######################################################################
#### Table A1 (Subject demographics summary) ####
desc <- data.frame(Country = as.character(),
                   Variable = as.character(),
                   Mean = as.double(),
                   SD = as.double(),
                   Min. = as.numeric(),
                   Max. = as.numeric(),
                   N = as.numeric(),
                   stringsAsFactors = FALSE)

for (i in c("Chile","china","India","UK")) {
  tmp <- conjoint1[conjoint1$survey == i,]
  tab1a <- list(tmp$age,tmp$ideology, tmp$aus,tmp$can,tmp$us, tmp$interest, tmp$likely)
  desc[nrow(desc)+1,] <- c(str_to_title(i),"","","","","","")
  for (j in tab1a) {
    row <- c("","",
             round(mean(j, na.rm=TRUE),2),
             round(sd(j, na.rm=TRUE),2),
             min(j, na.rm=TRUE),
             max(j, na.rm=TRUE),
             length(j[!is.na(j)])/6)
    desc[nrow(desc)+1,] <- row
  }
  # Add gender
  gender <- dummy(tmp$gender)
  desc[nrow(desc)+1,] <- c("","",round(mean(gender[,1]),2),round(sd(gender[,1]),2),0,1,length(gender[,1])/6)
}

desc$Variable <- c("","Age","Ideology","Favourability: Australia","Favourability: UK/Canada","Favourability: U.S.A.","Interest in emigrating","Likelihood of emigrating","Female")

stargazer(desc, summary = F, rownames = F,
          label = "tab:subjects",
          title = "Summary of Subject Demographics",
          digits = 1,
          out = "Tables/subjects.tex")

#### Figures A1-A5 (Subject demographics summary) ####
age_uk <- conjoint1[conjoint1$survey=="UK",]$age
gender_uk <- conjoint1[conjoint1$survey=="UK",]$gender
ideology_uk <- conjoint1[conjoint1$survey=="UK",]$ideology
interest_uk <- conjoint1[conjoint1$survey=="UK",]$interest
likely_uk <- conjoint1[conjoint1$survey=="UK",]$likely
uk <- cbind(age_uk,gender_uk,ideology_uk,interest_uk,likely_uk,"UK")[1:196,]


age_chile <- conjoint1[conjoint1$survey=="Chile",]$age
gender_chile <- conjoint1[conjoint1$survey=="Chile",]$gender
ideology_chile <- conjoint1[conjoint1$survey=="Chile",]$ideology
interest_chile <- conjoint1[conjoint1$survey=="Chile",]$interest
likely_chile <- conjoint1[conjoint1$survey=="Chile",]$likely
chile <- cbind(age_chile,gender_chile,ideology_chile,interest_chile,likely_chile,"Chile")[1:214,]


age_china <- conjoint1[conjoint1$survey=="china",]$age
gender_china <- conjoint1[conjoint1$survey=="china",]$gender
ideology_china <- conjoint1[conjoint1$survey=="china",]$ideology
interest_china <- conjoint1[conjoint1$survey=="china",]$interest
likely_china <- conjoint1[conjoint1$survey=="china",]$likely
china <- cbind(age_china,gender_china,ideology_china,interest_china,likely_china,"China")[1:303,]

age_india <- conjoint1[conjoint1$survey=="India",]$age
gender_india <- conjoint1[conjoint1$survey=="India",]$gender
ideology_india <- conjoint1[conjoint1$survey=="India",]$ideology
interest_india <- conjoint1[conjoint1$survey=="India",]$interest
likely_india <- conjoint1[conjoint1$survey=="India",]$likely
india <- cbind(age_india,gender_india,ideology_india,interest_india,likely_india,"India")[1:229,]

df_descriptives <- as.data.frame(rbind(uk, chile, china, india), stringsAsFactors = FALSE)
colnames(df_descriptives) <- c("age","gender","ideology","interest","likely","country")

df_descriptives$age <- as.numeric(df_descriptives$age)
df_descriptives$ideology <- as.integer(df_descriptives$ideology)
df_descriptives$gender <- as.factor(df_descriptives$gender)
df_descriptives$interest <- as.integer(df_descriptives$interest)
df_descriptives$likely <- as.integer(df_descriptives$likely)

#Age
ggplot(df_descriptives, aes(x = age)) +
  facet_wrap(~country, ncol=2) +
  geom_density() +
  labs(x="Age",y="Density") +
  ggsave("figures/age.png", width = 15, height = 10, units = c("cm"), dpi = 300)

#Gender
ggplot(df_descriptives, aes(x=country, fill=gender)) +
  geom_bar(position="fill") +
  scale_fill_discrete(name = "Gender", labels=c("Female","Male","Other")) +
  labs(x="Country",y="Proportion") +
  ggsave("figures/gender.png", width = 15, height = 10, units = c("cm"), dpi = 300)

#Ideology
ggplot(df_descriptives, aes(x=ideology)) +
  facet_wrap(~country, ncol=2) +
  geom_density() +
  labs(y="Density", x = "Ideology") +
  xlim(0,10) +
  ggsave("figures/ideology.png", width = 15, height = 10, units = c("cm"), dpi = 300)

#Interest
ggplot(df_descriptives, aes(x=interest)) +
  facet_wrap(~country, ncol=2) +
  geom_density() +
  labs(y="Density", x = "Interest in Emigrating") +
  xlim(0,10) +
  ggsave("figures/interest.png", width = 15, height = 10, units = c("cm"), dpi = 300)

#Likely
ggplot(df_descriptives, aes(x=likely)) +
  facet_wrap(~country, ncol=2) +
  geom_density() +
  labs(y="Density", x = "Likelihood of Emigrating") +
  xlim(0,10) + 
  ggsave("figures/likely.png", width = 15, height = 10, units = c("cm"), dpi = 300)

#### Table A2 (favourability t-tests) ####
ttests <- data.frame(pair = c("Australia - U.K. (Canada)","","Australia - U.S.A.","","U.S.A. - U.K. (Canada)",""))

for (i in c("Chile","china","India","UK")) {
  aus <- conjoint1[conjoint1$survey==i,]$aus
  can <- conjoint1[conjoint1$survey==i,]$can
  us <- conjoint1[conjoint1$survey==i,]$us
  
  a <- t.test(aus, can)
  b <- t.test(aus, us)
  c <- t.test(us, can)
  
  col <- c(round(a$estimate[1]-a$estimate[2],3),
           paste0("(p = ",round(a$p.value,3),")"),
           round(b$estimate[1]-b$estimate[2],3),
           paste0("(p = ",round(b$p.value,3),")"),
           round(c$estimate[1]-c$estimate[2],3),
           paste0("(p = ",round(c$p.value,3),")"))
  
  ttests <- cbind(ttests,col)
}

colnames(ttests) <- c("Pair","Chile","China","India","U.K.")

stargazer(ttests,summary = F, rownames = F,
          label = "tab:ttests",
          title = "T-tests of differences in favourability between countries",
          digits = 3,
          out = "tables/ttests.tex")

#### Table A3 (Randomisation) ####
tmp <- conjoint3
tmp$c.same <- NULL
colnames(tmp)[colnames(tmp)=="country"] <- "immigration"

cj <- rbind(droplevels(conjoint1),
            droplevels(conjoint2),
            droplevels(tmp))

cj <- cj[!is.na(cj$social),c(1:5)]

cj <- dummy.data.frame(cj)

dummyMeanscj <- round(as.double(colMeans(cj)),2)
dummyN <- as.numeric(colSums(cj))
dummySD <- round(as.double(apply(cj, 2, sd)),2)

var <- c("Basic hourly minimum wage",
         "Generous guaranteed monthly family allowance",
         "No state minimum wage or income support",
         "Annual GDP Growth of 2%",
         "Annual GDP Growth of 4%",
         "Annual GDP Growth of 6%",
         "Service salaries: 50th Percentile",
         "Service salaries: 70th Percentile",
         "Service salaries: 90th Percentile",
         "Change in visa processing centres", 
         "Implementation of point-system",
         "Restriction on Muslim immigration/tourist visas",
         "Deportation of all illegal immigrants",
         "Australia",
         "Canada",
         "U.S.A.",
         "U.K.",
         "Ranking of universities: 40th Percentile",
         "Ranking of universities: 60th Percentile",
         "Ranking of universities: 90th Percentile")
tab2 <- cbind(var,dummyMeanscj,dummySD,0,1,dummyN)

colnames(tab2) <- c("Conjoint attribute variable","Mean","SD","Min.","Max.","N")

stargazer(tab2, summary = F, rownames = F,
          label = "tab:randomisation",
          title = "Summary of Conjoint Attribute Randomisation",
          digits = 1,
          out = "tables/random.tex")

#### Tables A4-A8 (Country Models + Pooled) ####

## Chile results
stargazer(model1_chile.clustrob,model2_chile.clustrob, model3_chile.clustrob,
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "U.S.A",
                               "U.K.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc"),
          dep.var.caption = "Treatment",
          dep.var.labels.include = FALSE,
          se = list(model1_chile.clustrob$se,model2_chile.clustrob$se, model3_chile.clustrob$se),
          no.space = TRUE,
          title = "Chile only results",
          label = "tab:chile",
          notes = "Standard errors clustered by subject.",
          out = "tables/Chile.tex")

## China results
stargazer(model1_china.clustrob,model2_china.clustrob, model3_china.clustrob,
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "U.S.A",
                               "U.K.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc"),
          dep.var.caption = "Model",
          dep.var.labels.include = FALSE,
          se = list(model1_china.clustrob$se,model2_china.clustrob$se, model3_china.clustrob$se),
          no.space = TRUE,
          title = "China only results",
          label = "tab:china",
          notes = "Standard errors clustered by subject.",
          out = "tables/China.tex")

## India results
stargazer(model1_india.clustrob,model2_india.clustrob, model3_india.clustrob,
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "U.S.A",
                               "U.K.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc"),
          dep.var.caption = "Treatment",
          dep.var.labels.include = FALSE,
          se = list(model1_india.clustrob$se,model2_india.clustrob$se, model3_india.clustrob$se),
          no.space = TRUE,
          title = "India only results",
          label = "tab:india",
          notes = "Standard errors clustered by subject.",
          out = "tables/India.tex")

## UK results
stargazer(model1_uk.clustrob,model2_uk.clustrob, model3_uk.clustrob,
          dep.var.caption = "Treatment",
          dep.var.labels.include = FALSE,
          se = list(model1_uk.clustrob$se,model2_uk.clustrob$se, model3_uk.clustrob$se),
          no.space = TRUE,
          title = "UK only results",
          label = "tab:uk",
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "Canada",
                               "U.S.A.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc"),
          notes = "Standard errors clustered by subject.",
          out = "tables/UK.tex")

## Pooled table
stargazer(model1.clustrob,model2.clustrob, model3.clustrob,
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "Canada",
                               "U.S.A",
                               "U.K.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc",
                               "Chile",
                               "China",
                               "India"),
          dep.var.caption = "Model",
          dep.var.labels.include = FALSE,
          se = list(model1.clustrob$se,model2.clustrob$se, model3.clustrob$se),
          no.space = TRUE,
          title = "Pooled results",
          label = "tab:pooled",
          notes = "Standard errors clustered by subject.",
          out = "tables/Pooled.tex")

##### Additional models ####

## AMCE estimates of Figure 2
amce_cj1_uk <- amce(formula = destination ~ social + econ + service + immigration + education, id = id,
                    data=conjoint1[conjoint1$survey == "UK",]) %>% mutate(cj = "Treatment 1", country = "UK")
amce_cj2_uk <- amce(formula = destination ~ social + econ + service + immigration + education, id = id,
                    data=conjoint2[conjoint2$survey == "UK",]) %>% mutate(cj = "Treatment 2", country = "UK")
amce_cj3_uk <- amce(formula = destination ~ social + econ + service + country + education, id = id,
                    data=conjoint3[conjoint3$survey == "UK",]) %>% mutate(cj = "Treatment 3", country = "UK")

amce_cj1_chile <- amce(formula = destination ~ social + econ + service + immigration + education, id = id,
                       data=conjoint1[conjoint1$survey == "Chile",]) %>% mutate(cj = "Treatment 1", country = "Chile")
amce_cj2_chile <- amce(formula = destination ~ social + econ + service + immigration + education, id = id,
                       data=conjoint2[conjoint2$survey == "Chile",]) %>% mutate(cj = "Treatment 2", country = "Chile")
amce_cj3_chile <- amce(formula = destination ~ social + econ + service + country + education, id = id,
                       data=conjoint3[conjoint3$survey == "Chile",]) %>% mutate(cj = "Treatment 3", country = "Chile")

amce_cj1_china <- amce(formula = destination ~ social + econ + service + immigration + education, id = id,
                       data=conjoint1[conjoint1$survey == "china",]) %>% mutate(cj = "Treatment 1", country = "China")
amce_cj2_china <- amce(formula = destination ~ social + econ + service + immigration + education, id = id,
                       data=conjoint2[conjoint2$survey == "china",]) %>% mutate(cj = "Treatment 2", country = "China")
amce_cj3_china <- amce(formula = destination ~ social + econ + service + country + education, id = id,
                       data=conjoint3[conjoint3$survey == "china",]) %>% mutate(cj = "Treatment 3", country = "China")

amce_cj1_india <- amce(formula = destination ~ social + econ + service + immigration + education, id = id,
                       data=conjoint1[conjoint1$survey == "India",]) %>% mutate(cj = "Treatment 1", country = "India")
amce_cj2_india <- amce(formula = destination ~ social + econ + service + immigration + education, id = id,
                       data=conjoint2[conjoint2$survey == "India",]) %>% mutate(cj = "Treatment 2", country = "India")
amce_cj3_india <- amce(formula = destination ~ social + econ + service + country + education, id = id,
                       data=conjoint3[conjoint3$survey == "India",]) %>% mutate(cj = "Treatment 3", country = "India")

## Breakout models - T1
model1_uk_male <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint1[conjoint1$gender == "Male" & conjoint1$survey == "UK",])
model1_uk_female <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint1[conjoint1$gender == "Female" & conjoint1$survey == "UK",])
model1_chile_male <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint1[conjoint1$gender == "Male" & conjoint1$survey == "Chile",])
model1_chile_female <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint1[conjoint1$gender == "Female" & conjoint1$survey == "Chile",])
model1_china_male <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint1[conjoint1$gender == "Male" & conjoint1$survey == "china",])
model1_china_female <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint1[conjoint1$gender == "Female" & conjoint1$survey == "china",])
model1_india_male <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint1[conjoint1$gender == "Male" & conjoint1$survey == "India",])
model1_india_female <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint1[conjoint1$gender == "Female" & conjoint1$survey == "India",])

# Breakout models - T2
model2_uk_male <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint2[conjoint2$gender == "Male" & conjoint2$survey == "UK",])
model2_uk_female <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint2[conjoint2$gender == "Female" & conjoint2$survey == "UK",])
model2_chile_male <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint2[conjoint2$gender == "Male" & conjoint2$survey == "Chile",])
model2_chile_female <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint2[conjoint2$gender == "Female" & conjoint2$survey == "Chile",])
model2_china_male <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint2[conjoint2$gender == "Male" & conjoint2$survey == "china",])
model2_china_female <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint2[conjoint2$gender == "Female" & conjoint2$survey == "china",])
model2_india_male <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint2[conjoint2$gender == "Male" & conjoint2$survey == "India",])
model2_india_female <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=conjoint2[conjoint2$gender == "Female" & conjoint2$survey == "India",])

# Breakout models - T3
model3_uk_male <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=conjoint3[conjoint3$gender == "Male" & conjoint3$survey == "UK",])
model3_uk_female <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=conjoint3[conjoint3$gender == "Female" & conjoint3$survey == "UK",])
model3_chile_male <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=conjoint3[conjoint3$gender == "Male" & conjoint3$survey == "Chile",])
model3_chile_female <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=conjoint3[conjoint3$gender == "Female" & conjoint3$survey == "Chile",])
model3_china_male <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=conjoint3[conjoint3$gender == "Male" & conjoint3$survey == "china",])
model3_china_female <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=conjoint3[conjoint3$gender == "Female" & conjoint3$survey == "china",])
model3_india_male <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=conjoint3[conjoint3$gender == "Male" & conjoint3$survey == "India",])
model3_india_female <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=conjoint3[conjoint3$gender == "Female" & conjoint3$survey == "India",])

# Cluster-robust standard errors
robustse.f(model1_uk_male, ~id, F)
robustse.f(model1_uk_female, ~id, F)
robustse.f(model1_chile_male, ~id, F)
robustse.f(model1_chile_female, ~id, F)
robustse.f(model1_china_male, ~id, F)
robustse.f(model1_china_female, ~id, F)
robustse.f(model1_india_male, ~id, F)
robustse.f(model1_india_female, ~id, F)
robustse.f(model2_uk_male, ~id, F)
robustse.f(model2_uk_female, ~id, F)
robustse.f(model2_chile_male, ~id, F)
robustse.f(model2_chile_female, ~id, F)
robustse.f(model2_china_male, ~id, F)
robustse.f(model2_china_female, ~id, F)
robustse.f(model2_india_male, ~id, F)
robustse.f(model2_india_female, ~id, F)
robustse.f(model3_uk_male, ~id, F)
robustse.f(model3_uk_female, ~id, F)
robustse.f(model3_chile_male, ~id, F)
robustse.f(model3_chile_female, ~id, F)
robustse.f(model3_china_male, ~id, F)
robustse.f(model3_china_female, ~id, F)
robustse.f(model3_india_male, ~id, F)
robustse.f(model3_india_female, ~id, F)

## RET Breakout

ret1 <- conjoint1
ret2 <- conjoint2
ret3 <- conjoint3

ret1_uk <- ret1[ret1$survey=="UK",]
ret1_uk$ability <- ifelse(ret1_uk$dice > mean(ret1_uk$dice),1,0)
ret2_uk <- ret2[ret2$survey=="UK",]
ret2_uk$ability <- ifelse(ret2_uk$dice > mean(ret2_uk$dice),1,0)
ret3_uk <- ret3[ret3$survey=="UK",]
ret3_uk$ability <- ifelse(ret3_uk$dice > mean(ret3_uk$dice),1,0)

ret1_chile <- ret1[ret1$survey=="Chile",]
ret1_chile$ability <- ifelse(ret1_chile$dice > mean(ret1_chile$dice),1,0)
ret2_chile <- ret2[ret2$survey=="Chile",]
ret2_chile$ability <- ifelse(ret2_chile$dice > mean(ret2_chile$dice),1,0)
ret3_chile <- ret3[ret3$survey=="Chile",]
ret3_chile$ability <- ifelse(ret3_chile$dice > mean(ret3_chile$dice),1,0)

ret1_china <- ret1[ret1$survey=="china",]
ret1_china$ability <- ifelse(ret1_china$dice > mean(ret1_china$dice),1,0)
ret2_china <- ret2[ret2$survey=="china",]
ret2_china$ability <- ifelse(ret2_china$dice > mean(ret2_china$dice),1,0)
ret3_china <- ret3[ret3$survey=="china",]
ret3_china$ability <- ifelse(ret3_china$dice > mean(ret3_china$dice),1,0)

ret1_india <- ret1[ret1$survey=="India",]
ret1_india$ability <- ifelse(ret1_india$dice > mean(ret1_india$dice),1,0)
ret2_india <- ret2[ret2$survey=="India",]
ret2_india$ability <- ifelse(ret2_india$dice > mean(ret2_india$dice),1,0)
ret3_india <- ret3[ret3$survey=="India",]
ret3_india$ability <- ifelse(ret3_india$dice > mean(ret3_india$dice),1,0)

model1_uk_high <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret1_uk[ret1_uk$ability == 1,])
model1_uk_low <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret1_uk[ret1_uk$ability == 0,])
model1_chile_high <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret1_chile[ret1_chile$ability == 1,])
model1_chile_low <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret1_chile[ret1_chile$ability == 0,])
model1_china_high <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret1_china[ret1_china$ability == 1,])
model1_china_low <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret1_china[ret1_china$ability == 0,])
model1_india_high <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret1_india[ret1_india$ability == 1,])
model1_india_low <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret1_india[ret1_india$ability == 0,])

model2_uk_high <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret2_uk[ret2_uk$ability == 1,])
model2_uk_low <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret2_uk[ret2_uk$ability == 0,])
model2_chile_high <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret2_chile[ret2_chile$ability == 1,])
model2_chile_low <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret2_chile[ret2_chile$ability == 0,])
model2_china_high <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret2_china[ret2_china$ability == 1,])
model2_china_low <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret2_china[ret2_china$ability == 0,])
model2_india_high <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret2_india[ret2_india$ability == 1,])
model2_india_low <- glm(destination ~ social + econ + service + immigration + education + likely,family=binomial(link='logit'),data=ret2_india[ret2_india$ability == 0,])

model3_uk_high <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=ret3_uk[ret3_uk$ability == 1,])
model3_uk_low <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=ret3_uk[ret3_uk$ability == 0,])
model3_chile_high <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=ret3_chile[ret3_chile$ability == 1,])
model3_chile_low <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=ret3_chile[ret3_chile$ability == 0,])
model3_china_high <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=ret3_china[ret3_china$ability == 1,])
model3_china_low <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=ret3_china[ret3_china$ability == 0,])
model3_india_high <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=ret3_india[ret3_india$ability == 1,])
model3_india_low <- glm(destination ~ social + econ + service + country + education + likely,family=binomial(link='logit'),data=ret3_india[ret3_india$ability == 0,])

robustse.f(model1_uk_high, ~id, F)
robustse.f(model1_uk_low, ~id, F)
robustse.f(model1_chile_high, ~id, F)
robustse.f(model1_chile_low, ~id, F)
robustse.f(model1_china_high, ~id, F)
robustse.f(model1_china_low, ~id, F)
robustse.f(model1_india_high, ~id, F)
robustse.f(model1_india_low, ~id, F)
robustse.f(model2_uk_high, ~id, F)
robustse.f(model2_uk_low, ~id, F)
robustse.f(model2_chile_high, ~id, F)
robustse.f(model2_chile_low, ~id, F)
robustse.f(model2_china_high, ~id, F)
robustse.f(model2_china_low, ~id, F)
robustse.f(model2_india_high, ~id, F)
robustse.f(model2_india_low, ~id, F)
robustse.f(model3_uk_high, ~id, F)
robustse.f(model3_uk_low, ~id, F)
robustse.f(model3_chile_high, ~id, F)
robustse.f(model3_chile_low, ~id, F)
robustse.f(model3_china_high, ~id, F)
robustse.f(model3_china_low, ~id, F)
robustse.f(model3_india_high, ~id, F)
robustse.f(model3_india_low, ~id, F)

#### Figure A9 ####

amce_df <- rbind(amce_cj1_chile, amce_cj2_chile, amce_cj3_chile,
                 amce_cj1_china, amce_cj2_china, amce_cj3_china,
                 amce_cj1_india, amce_cj2_india, amce_cj3_india,
                 amce_cj1_uk, amce_cj2_uk, amce_cj3_uk)

amce_df %>% 
  filter(feature %in% c("immigration","country")) %>% 
  mutate(level = gsub("Restriction on Muslim immigration/tourist visas","Muslim Ban",level)) %>% 
  mutate(level = factor(level, levels = rev(c("Implementation of point-system",
                                              "Change in visa processing centres",
                                              "Muslim Ban",
                                              "Deportation of all illegal immigrants",
                                              "U.K.",
                                              "Canada",
                                              "Australia",
                                              "U.S.A.")))) %>% 
  ggplot(aes(x=level, shape=country, color = country)) +
  facet_grid(.~cj, scales = "free_y") +
  geom_point(aes(y=estimate), position=position_dodge(width = 0.9), size = 2) +
  geom_linerange(aes(max=upper, min=lower), position=position_dodge(width = 0.9)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(x="",y="AMCE Estimate", shape = "Subject Country", color = "Subject Country") +
  coord_flip() +
  theme(legend.position = "bottom") +
  ggsave("figures/amce_main_figure.pdf", width = 18, height = 10, units = c("cm"), dpi = 300)

##### Tables A9-A12 (RET) #####

# Chile
stargazer(model1_chile_high,model1_chile_low,model2_chile_high,model2_chile_low,model3_chile_high,model3_chile_low,
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "U.S.A.",
                               "U.K.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc",
                               "Likelihood of emigrating"),
          dep.var.caption = "Model",
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          column.labels = c("High","Low","High","Low","High","Low"),
          se=list(model1_chile_high.clustrob$se,model1_chile_low.clustrob$se,model2_chile_high.clustrob$se,model2_chile_low.clustrob$se,model3_chile_high.clustrob$se,model3_chile_low.clustrob$se),
          no.space = TRUE,
          float.env = "sidewaystable",
          title = "Chile Model Breakout by Ability Level (RET > Mean)",
          label = "tab:chile_ability",
          notes = "Standard errors clustered by subject.",
          out = "Tables/Chile_ability.tex")

# China
stargazer(model1_china_high,model1_china_low,model2_china_high,model2_china_low,model3_china_high,model3_china_low,
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "U.S.A.",
                               "U.K.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc",
                               "Likelihood of emigrating"),
          dep.var.caption = "Model",
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          column.labels = c("High","Low","High","Low","High","Low"),
          se=list(model1_china_high.clustrob$se,model1_china_low.clustrob$se,model2_china_high.clustrob$se,model2_china_low.clustrob$se,model3_china_high.clustrob$se,model3_china_low.clustrob$se),
          no.space = TRUE,
          float.env = "sidewaystable",
          title = "China Model Breakout by Ability Level (RET > Mean)",
          label = "tab:china_ability",
          notes = "Standard errors clustered by subject.",
          out = "Tables/China_ability.tex")

# India
stargazer(model1_india_high,model1_india_low,model2_india_high,model2_india_low,model3_india_high,model3_india_low,
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "U.S.A.",
                               "U.K.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc",
                               "Likelihood of emigrating"),
          dep.var.caption = "Model",
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          column.labels = c("High","Low","High","Low","High","Low"),
          se=list(model1_india_high.clustrob$se,model1_india_low.clustrob$se,model2_india_high.clustrob$se,model2_india_low.clustrob$se,model3_india_high.clustrob$se,model3_india_low.clustrob$se),
          no.space = TRUE,
          float.env = "sidewaystable",
          title = "India Model Breakout by Ability Level (RET > Mean)",
          label = "tab:india_ability",
          notes = "Standard errors clustered by subject.",
          out = "Tables/India_ability.tex")

# UK
stargazer(model1_uk_high,model1_uk_low,model2_uk_high,model2_uk_low,model3_uk_high,model3_uk_low,
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "Canada",
                               "U.S.A.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc",
                               "Likelihood of emigrating"),
          dep.var.caption = "Model",
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          column.labels = c("High","Low","High","Low","High","Low"),
          se=list(model1_uk_high.clustrob$se,model1_uk_low.clustrob$se,model2_uk_high.clustrob$se,model2_uk_low.clustrob$se,model3_uk_high.clustrob$se,model3_uk_low.clustrob$se),
          no.space = TRUE,
          float.env = "sidewaystable",
          title = "UK Model Breakout by Ability Level (RET > Mean)",
          label = "tab:uk_ability",
          notes = "Standard errors clustered by subject.",
          out = "Tables/UK_ability.tex")

#### Figure A9 (RET) ####
plotdf1_uk_high <- data.frame(estimate = coeftest(model1_uk_high.clustrob, model1_uk_high.clustrob$vcovCL)[,1],SE = coeftest(model1_uk_high, model1_uk_high.clustrob$vcovCL)[,2], Country = "UK",Treatment = "Treatment: 1", Ability = "High")
plotdf1_uk_low <- data.frame(estimate = coeftest(model1_uk_low.clustrob, model1_uk_low.clustrob$vcovCL)[,1],SE = coeftest(model1_uk_low, model1_uk_low.clustrob$vcovCL)[,2], Country = "UK",Treatment = "Treatment: 1", Ability = "Low")
plotdf1_chile_high <- data.frame(estimate = coeftest(model1_chile_high.clustrob, model1_chile_high.clustrob$vcovCL)[,1],SE = coeftest(model1_chile_high, model1_chile_high.clustrob$vcovCL)[,2], Country = "Chile",Treatment = "Treatment: 1", Ability = "High")
plotdf1_chile_low <- data.frame(estimate = coeftest(model1_chile_low.clustrob, model1_chile_low.clustrob$vcovCL)[,1],SE = coeftest(model1_chile_low, model1_chile_low.clustrob$vcovCL)[,2], Country = "Chile",Treatment = "Treatment: 1", Ability = "Low")
plotdf1_china_high <- data.frame(estimate = coeftest(model1_china_high.clustrob, model1_china_high.clustrob$vcovCL)[,1],SE = coeftest(model1_china_high, model1_china_high.clustrob$vcovCL)[,2], Country = "China",Treatment = "Treatment: 1", Ability = "High")
plotdf1_china_low <- data.frame(estimate = coeftest(model1_china_low.clustrob, model1_china_low.clustrob$vcovCL)[,1],SE = coeftest(model1_china_low, model1_china_low.clustrob$vcovCL)[,2], Country = "China",Treatment = "Treatment: 1", Ability = "Low")
plotdf1_india_high <- data.frame(estimate = coeftest(model1_india_high.clustrob, model1_india_high.clustrob$vcovCL)[,1],SE = coeftest(model1_india_high, model1_india_high.clustrob$vcovCL)[,2], Country = "India",Treatment = "Treatment: 1", Ability = "High")
plotdf1_india_low <- data.frame(estimate = coeftest(model1_india_low.clustrob, model1_india_low.clustrob$vcovCL)[,1],SE = coeftest(model1_india_low, model1_india_low.clustrob$vcovCL)[,2], Country = "India",Treatment = "Treatment: 1", Ability = "Low")

vars <- c("Intercept","Generous family allowance","No minimum wage","GDP 4 percent","GDP 6 percent","Service Salaries 70th pc",
          "Service Salaries 90th pc","Point-system visa","Muslim Ban","University Ranking 60th pc","University Ranking 90th pc","Likelihood of emigrating")

plotdf1_ability <- rbind(plotdf1_uk_high,plotdf1_uk_low,plotdf1_chile_high,plotdf1_chile_low,
                         plotdf1_china_high,plotdf1_china_low,plotdf1_india_high,plotdf1_india_low)

plotdf1_ability <- cbind(plotdf1_ability,vars)


plotdf2_uk_high <- data.frame(estimate = coeftest(model2_uk_high.clustrob, model2_uk_high.clustrob$vcovCL)[,1],SE = coeftest(model2_uk_high, model2_uk_high.clustrob$vcovCL)[,2], Country = "UK",Treatment = "Treatment: 2", Ability = "High")
plotdf2_uk_low <- data.frame(estimate = coeftest(model2_uk_low.clustrob, model2_uk_low.clustrob$vcovCL)[,1],SE = coeftest(model2_uk_low, model2_uk_low.clustrob$vcovCL)[,2], Country = "UK",Treatment = "Treatment: 2", Ability = "Low")
plotdf2_chile_high <- data.frame(estimate = coeftest(model2_chile_high.clustrob, model2_chile_high.clustrob$vcovCL)[,1],SE = coeftest(model2_chile_high, model2_chile_high.clustrob$vcovCL)[,2], Country = "Chile",Treatment = "Treatment: 2", Ability = "High")
plotdf2_chile_low <- data.frame(estimate = coeftest(model2_chile_low.clustrob, model2_chile_low.clustrob$vcovCL)[,1],SE = coeftest(model2_chile_low, model2_chile_low.clustrob$vcovCL)[,2], Country = "Chile",Treatment = "Treatment: 2", Ability = "Low")
plotdf2_china_high <- data.frame(estimate = coeftest(model2_china_high.clustrob, model2_china_high.clustrob$vcovCL)[,1],SE = coeftest(model2_china_high, model2_china_high.clustrob$vcovCL)[,2], Country = "China",Treatment = "Treatment: 2", Ability = "High")
plotdf2_china_low <- data.frame(estimate = coeftest(model2_china_low.clustrob, model2_china_low.clustrob$vcovCL)[,1],SE = coeftest(model2_china_low, model2_china_low.clustrob$vcovCL)[,2], Country = "China",Treatment = "Treatment: 2", Ability = "Low")
plotdf2_india_high <- data.frame(estimate = coeftest(model2_india_high.clustrob, model2_india_high.clustrob$vcovCL)[,1],SE = coeftest(model2_india_high, model2_india_high.clustrob$vcovCL)[,2], Country = "India",Treatment = "Treatment: 2", Ability = "High")
plotdf2_india_low <- data.frame(estimate = coeftest(model2_india_low.clustrob, model2_india_low.clustrob$vcovCL)[,1],SE = coeftest(model2_india_low, model2_india_low.clustrob$vcovCL)[,2], Country = "India",Treatment = "Treatment: 2", Ability = "Low")

vars <- c("Intercept","Generous family allowance","No minimum wage","GDP 4 percent","GDP 6 percent","Service Salaries 70th pc",
          "Service Salaries 90th pc","Deportation of all illegal immigrants","Point-system visa","University Ranking 60th pc","University Ranking 90th pc","Likelihood of emigrating")

plotdf2_ability <- rbind(plotdf2_uk_high,plotdf2_uk_low,plotdf2_chile_high,plotdf2_chile_low,
                         plotdf2_china_high,plotdf2_china_low,plotdf2_india_high,plotdf2_india_low)

plotdf2_ability <- cbind(plotdf2_ability,vars)

plotdf3_uk_high <- data.frame(estimate = coeftest(model3_uk_high.clustrob, model3_uk_high.clustrob$vcovCL)[,1],SE = coeftest(model3_uk_high, model3_uk_high.clustrob$vcovCL)[,2], Country = "UK",Treatment = "Treatment: 3", Ability = "High")
plotdf3_uk_low <- data.frame(estimate = coeftest(model3_uk_low.clustrob, model3_uk_low.clustrob$vcovCL)[,1],SE = coeftest(model3_uk_low, model3_uk_low.clustrob$vcovCL)[,2], Country = "UK",Treatment = "Treatment: 3", Ability = "Low")
plotdf3_chile_high <- data.frame(estimate = coeftest(model3_chile_high.clustrob, model3_chile_high.clustrob$vcovCL)[,1],SE = coeftest(model3_chile_high, model3_chile_high.clustrob$vcovCL)[,2], Country = "Chile",Treatment = "Treatment: 3", Ability = "High")
plotdf3_chile_low <- data.frame(estimate = coeftest(model3_chile_low.clustrob, model3_chile_low.clustrob$vcovCL)[,1],SE = coeftest(model3_chile_low, model3_chile_low.clustrob$vcovCL)[,2], Country = "Chile",Treatment = "Treatment: 3", Ability = "Low")
plotdf3_china_high <- data.frame(estimate = coeftest(model3_china_high.clustrob, model3_china_high.clustrob$vcovCL)[,1],SE = coeftest(model3_china_high, model3_china_high.clustrob$vcovCL)[,2], Country = "China",Treatment = "Treatment: 3", Ability = "High")
plotdf3_china_low <- data.frame(estimate = coeftest(model3_china_low.clustrob, model3_china_low.clustrob$vcovCL)[,1],SE = coeftest(model3_china_low, model3_china_low.clustrob$vcovCL)[,2], Country = "China",Treatment = "Treatment: 3", Ability = "Low")
plotdf3_india_high <- data.frame(estimate = coeftest(model3_india_high.clustrob, model3_india_high.clustrob$vcovCL)[,1],SE = coeftest(model3_india_high, model3_india_high.clustrob$vcovCL)[,2], Country = "India",Treatment = "Treatment: 3", Ability = "High")
plotdf3_india_low <- data.frame(estimate = coeftest(model3_india_low.clustrob, model3_india_low.clustrob$vcovCL)[,1],SE = coeftest(model3_india_low, model3_india_low.clustrob$vcovCL)[,2], Country = "India",Treatment = "Treatment: 3", Ability = "Low")

vars <- c("Intercept","Generous family allowance","No minimum wage","GDP 4 percent","GDP 6 percent","Service Salaries 70th pc",
          "Service Salaries 90th pc","Canada","U.S.A.","University Ranking 60th pc","University Ranking 90th pc","Likelihood of emigrating")

plotdf3_ability_uk <- rbind(plotdf3_uk_high,plotdf3_uk_low)
plotdf3_ability_uk <- cbind(plotdf3_ability_uk,vars)

vars <- c("Intercept","Generous family allowance","No minimum wage","GDP 4 percent","GDP 6 percent","Service Salaries 70th pc",
          "Service Salaries 90th pc","U.S.A.","U.K.","University Ranking 60th pc","University Ranking 90th pc","Likelihood of emigrating")

plotdf3_ability_cci <- rbind(plotdf3_chile_high,plotdf3_chile_low,
                             plotdf3_china_high,plotdf3_china_low,
                             plotdf3_india_high,plotdf3_india_low)
plotdf3_ability_cci <- cbind(plotdf3_ability_cci,vars)

plotdf3_ability <- rbind(plotdf3_ability_uk,plotdf3_ability_cci)

### Combined plot
plotdf_ability <- rbind(plotdf1_ability,plotdf2_ability,plotdf3_ability)
plotdf_ability <- plotdf_ability[!(plotdf_ability$vars %in% c("Intercept","Likelihood of emigrating")),]

plotdf_ability$vars <- factor(plotdf_ability$vars, levels = c(
  "University Ranking 90th pc",
  "University Ranking 60th pc",
  "U.S.A.",
  "Canada",
  "U.K.",
  "Deportation of all illegal immigrants",
  "Muslim Ban",
  "Point-system visa",
  "Service Salaries 90th pc",
  "Service Salaries 70th pc",
  "GDP 6 percent",
  "GDP 4 percent",
  "No minimum wage",
  "Generous family allowance"))

plotdf_ability$LCI <- plotdf_ability$estimate-1.96*plotdf_ability$SE
plotdf_ability$UCI <- plotdf_ability$estimate+1.96*plotdf_ability$SE

ggplot(data = plotdf_ability, aes(x=vars, shape=Ability, color = Ability)) +
  facet_grid(Treatment~Country, scales="free_y", drop=T) +
  geom_point(aes(y=estimate), position=position_dodge(width = 0.7)) +
  geom_linerange(aes(max=UCI, min=LCI), position=position_dodge(width = 0.7)) +
  geom_hline(yintercept=0, linetype = "dashed") +
  labs(x="",y="") +
  theme(legend.position = "bottom") +
  coord_flip() +
  ggsave(paste0("figures/conjoint_combined_ability.png"), width = 25, height = 30, units = c("cm"), dpi = 300)


#### Tables A13-A16 (gender) ####

# Chile
stargazer(model1_chile_male.clustrob,model1_chile_female.clustrob, model2_chile_male.clustrob,model2_chile_female.clustrob,model3_chile_male.clustrob,model3_chile_female.clustrob,
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "U.S.A.",
                               "U.K.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc",
                               "Likelihood of emigrating"),
          dep.var.caption = "Treatment",
          dep.var.labels.include = FALSE,
          se = list(model1_chile_male.clustrob$se,model1_chile_female.clustrob$se, model2_chile_male.clustrob$se,model2_chile_female.clustrob$se,model3_chile_male.clustrob$se,model3_chile_female.clustrob$se),
          model.numbers = FALSE,
          column.labels = c("Male","Female","Male","Female","Male","Female"),
          no.space = TRUE,
          float.env = "sidewaystable",
          title = "Chile only results, separate models per gender",
          label = "tab:chile_gender",
          notes = "Standard errors clustered by subject.",
          out = "Tables/Chile_gender.tex")

# China
stargazer(model1_china_male.clustrob,model1_china_female.clustrob, model2_china_male.clustrob,model2_china_female.clustrob,model3_china_male.clustrob,model3_china_female.clustrob,
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "U.S.A.",
                               "U.K.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc",
                               "Likelihood of emigrating"),
          dep.var.caption = "Treatment",
          dep.var.labels.include = FALSE,
          se = list(model1_china_male.clustrob$se,model1_china_female.clustrob$se, model2_china_male.clustrob$se,model2_china_female.clustrob$se,model3_china_male.clustrob$se,model3_china_female.clustrob$se),
          model.numbers = FALSE,
          column.labels = c("Male","Female","Male","Female","Male","Female"),
          no.space = TRUE,
          float.env = "sidewaystable",
          title = "China only results, separate models per gender",
          label = "tab:china_gender",
          notes = "Standard errors clustered by subject.",
          out = "Tables/China_gender.tex")

# India
stargazer(model1_india_male.clustrob,model1_india_female.clustrob, model2_india_male.clustrob,model2_india_female.clustrob,model3_india_male.clustrob,model3_india_female.clustrob,
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "U.S.A.",
                               "U.K.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc",
                               "Likelihood of emigrating"),
          dep.var.caption = "Treatment",
          dep.var.labels.include = FALSE,
          se = list(model1_india_male.clustrob$se,model1_india_female.clustrob$se, model2_india_male.clustrob$se,model2_india_female.clustrob$se,model3_india_male.clustrob$se,model3_india_female.clustrob$se),
          model.numbers = FALSE,
          column.labels = c("Male","Female","Male","Female","Male","Female"),
          no.space = TRUE,
          float.env = "sidewaystable",
          title = "India only results, separate models per gender",
          label = "tab:india_gender",
          notes = "Standard errors clustered by subject.",
          out = "Tables/India_gender.tex")

# UK
stargazer(model1_uk_male.clustrob,model1_uk_female.clustrob, model2_uk_male.clustrob,model2_uk_female.clustrob,model3_uk_male.clustrob,model3_uk_female.clustrob,
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "Muslim Ban",
                               "Canada",
                               "U.S.A.",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc",
                               "Likelihood of emigrating"),
          dep.var.caption = "Treatment",
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          column.labels = c("Male","Female","Male","Female","Male","Female"),
          se = list(model1_uk_male.clustrob$se,model1_uk_female.clustrob$se, model2_uk_male.clustrob$se,model2_uk_female.clustrob$se,model3_uk_male.clustrob$se,model3_uk_female.clustrob$se),
          no.space = TRUE,
          float.env = "sidewaystable",
          title = "UK only results, separate models per gender",
          label = "tab:uk_gender",
          notes = "Standard errors clustered by subject.",
          out = "Tables/UK_gender.tex")

#### Tables A17-A19 (age full results) ####

# Treatment 1
stargazer(model1_age_young, model1_age_old, 
          label = "tab:age_full_1",
          title = "Full logistic regression results on pooled sample, by age cohort (immigration treatment 1)",
          dep.var.labels.include = FALSE,
          dep.var.caption = "Immigration Treatment 1",
          column.labels = rep(c("Younger","Older"),3),
          model.numbers = FALSE,
          se=list(model1_age_young.clustrob$se,model1_age_old.clustrob$se),
          no.space = TRUE,
          font.size  = "footnotesize",
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Point-system visa",
                               "Muslim Ban",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc",
                               "Chile",
                               "China",
                               "India"),
          notes = "Standard errors clustered by subject.",
          out = "tables/robust_age_full_1.tex")

# Treatment 2

stargazer(model2_age_young, model2_age_old, 
          label = "tab:age_full_2",
          title = "Full logistic regression results on pooled sample, by age cohort (immigration treatment 2)",
          dep.var.caption = "Immigration Treatment 2",
          dep.var.labels.include = FALSE,
          column.labels = rep(c("Younger","Older"),3),
          model.numbers = FALSE,
          se=list(model2_age_young.clustrob$se,model2_age_old.clustrob$se),
          no.space = TRUE,
          font.size  = "footnotesize",
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc",
                               "Chile",
                               "China",
                               "India"),
          notes = "Standard errors clustered by subject.",
          out = "tables/robust_age_full_2.tex")

# Treatment 3
stargazer(model3_age_young, model3_age_old,
          label = "tab:age_full_3",
          title = "Full logistic regression results on pooled sample, by age cohort (immigration treatment 3)",
          dep.var.caption = "Immigration Treatment 3",
          dep.var.labels.include = FALSE,
          column.labels = rep(c("Younger","Older"),3),
          model.numbers = FALSE,
          se=list(model3_age_young.clustrob$se,model3_age_old.clustrob$se),
          no.space = TRUE,
          font.size  = "footnotesize",
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
                               "University Ranking 90th pc",
                               "Chile",
                               "China",
                               "India"),
          notes = "Standard errors clustered by subject.",
          out = "tables/robust_age_full_3.tex")

#### Tables A20-22 (likely to emigrate results) ####
stargazer(model1_likely_chile, model1_likely_china,model1_likely_india, model1_likely_uk,
          se = list(model1_likely_chile.clustrob$se,
                    model1_likely_china.clustrob$se,
                    model1_likely_india.clustrob$se,
                    model1_likely_uk.clustrob$se),
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Point-system visa",
                               "Muslim Ban",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc"),
          title = "Full logistic regression results, on only those subjects likely to emigrate (immigration treatment 1)",
          dep.var.caption = "Immigration Treatment 1",
          column.labels = c("Chile","China","India","UK"),
          model.numbers = FALSE,
          dep.var.labels.include = FALSE,
          notes = "Standard errors clustered by subject.",
          out = "tables/likely_full_1.tex")

stargazer(model2_likely_chile, model2_likely_china,model2_likely_india, model2_likely_uk,
          se = list(model2_likely_chile.clustrob$se,
                    model2_likely_china.clustrob$se,
                    model2_likely_india.clustrob$se,
                    model2_likely_uk.clustrob$se),
          covariate.labels = c("Generous family allowance",
                               "No minimum wage or income support",
                               "GDP 2 percent",
                               "GDP 6 percent",
                               "Service salaries 50th pc",
                               "Service salaries 90th pc",
                               "Deportation of all illegal immigrants",
                               "Point-system visa",
                               "University Ranking 40th pc",
                               "University Ranking 90th pc"),
          title = "Full logistic regression results, on only those subjects likely to emigrate (immigration treatment 2)",
          dep.var.caption = "Immigration Treatment 2",
          column.labels = c("Chile","China","India","UK"),
          model.numbers = FALSE,
          dep.var.labels.include = FALSE,
          notes = "Standard errors clustered by subject.",
          out = "tables/likely_full_2.tex")

stargazer(model3_likely_chile, model3_likely_china,model3_likely_india, model3_likely_uk,
          se = list(model3_likely_chile.clustrob$se,
                    model3_likely_china.clustrob$se,
                    model3_likely_india.clustrob$se,
                    model3_likely_uk.clustrob$se),
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
          title = "Full logistic regression results, on only those subjects likely to emigrate (immigration treatment 3)",
          dep.var.caption = "Immigration Treatment 3",
          dep.var.labels.include = FALSE,
          column.labels = c("Chile","China","India","UK"),
          model.numbers = FALSE,
          notes = "Standard errors clustered by subject.",
          out = "tables/likely_full_3.tex")


######################################################################
####  Additional Analysis ####
######################################################################
#### Balance tests [not displayed in text or appendix] #####

## Not run:
# tmp <- conjoint3; tmp$c.same <- NULL; colnames(tmp)[colnames(tmp)=="country"] <- "immigration"
# conjoint <- rbind(conjoint1,conjoint2,tmp)
# rm(tmp)
# 
# countrylist <- c("UK","Chile","china","India")
# 
# for (i in countrylist) {
#   df <- conjoint[conjoint$survey == i,]
#   
#   vars <- c("Age","Gender: Male","Gender: Other","Likelihood of emigrating",
#             "Ideology")
#   
#   bal_social <- multinom(social ~ age + gender + likely + ideology, data = df)
#   bal_econ <- multinom(econ ~ age + gender + likely + ideology, data = df)
#   bal_service <- multinom(service ~ age + gender + likely + ideology, data = df)
#   bal_education <- multinom(education ~ age + gender + likely + ideology, data = df)
#   bal_immigration <- multinom(immigration ~ age + gender + likely + ideology, data = df)
#   
#   stargazer(bal_social,
#             covariate.labels = vars,
#             title = paste0(i," Balance test: Social"),
#             label = paste0("tab:bal_social_",tolower(i)),
#             out = paste0("Tables/balance_social_",tolower(i),".tex"),
#             float.env = "sidewaystable",
#             no.space = T)
#   
#   stargazer(bal_econ,
#             covariate.labels = vars,
#             title = paste0(i," Balance test: Economy"),
#             label = paste0("tab:bal_econ_",tolower(i)),
#             out = paste0("Tables/balance_econ_",tolower(i),".tex"),
#             float.env = "sidewaystable",
#             no.space = T)
#   stargazer(bal_service,
#             covariate.labels = vars,
#             title = paste0(i," Balance test: Service"),
#             label = paste0("tab:bal_service_",tolower(i)),
#             out = paste0("Tables/balance_service_",tolower(i),".tex"),
#             float.env = "sidewaystable",
#             no.space = T)
#   stargazer(bal_education,
#             covariate.labels = vars,
#             title = paste0(i," Balance test: Education"),
#             label = paste0("tab:bal_education_",tolower(i)),
#             out = paste0("Tables/balance_education_",tolower(i),".tex"),
#             float.env = "sidewaystable",
#             no.space = T)
#   stargazer(bal_immigration,
#             covariate.labels = vars,
#             title = paste0(i," Balance test: Immigration"),
#             label = paste0("tab:bal_immigration_",tolower(i)),
#             out = paste0("Tables/balance_immigration_",tolower(i),".tex"),
#             float.env = "sidewaystable",
#             no.space = T)
# }
# 
# 
# #### Ideology full results ####
# summary(model1_left_uk)
# summary(model1_left_chile)
# summary(model1_left_china)
# summary(model1_left_india)
# summary(model2_left_uk)
# summary(model2_left_chile)
# summary(model2_left_china)
# summary(model2_left_india)
# summary(model3_left_uk)
# summary(model3_left_chile)
# summary(model3_left_china)
# summary(model3_left_india)
# summary(model1_right_uk)
# summary(model1_right_chile)
# summary(model1_right_china)
# summary(model1_right_india)
# summary(model2_right_uk)
# summary(model2_right_chile)
# summary(model2_right_china)
# summary(model2_right_india)
# summary(model3_right_uk)
# summary(model3_right_chile)
# summary(model3_right_china)
# summary(model3_right_india)
# summary(model1_centre_uk)
# summary(model1_centre_chile)
# summary(model1_centre_china)
# summary(model1_centre_india)
# summary(model2_centre_uk)
# summary(model2_centre_chile)
# summary(model2_centre_china)
# summary(model2_centre_india)
# summary(model3_centre_uk)
# summary(model3_centre_chile)
# summary(model3_centre_china)
# summary(model3_centre_india)
# 
