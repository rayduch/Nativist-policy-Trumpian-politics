################################################################################
##                                                                            ##
##                             Conjoint Analysis                              ##
##                              Data Processing                               ##
##                                                                            ##
##  Raymond Duch, Denise Laroze, Constantin Reinprecht, and Thomas Robinson   ##
##                                    2018                                    ##
################################################################################

library(openxlsx)
library(xtable)
library(gridExtra)
library(coefplot)
library(dummies)
library(nnet)
library(stargazer)
theme_set(theme_bw())


######################################################################
######## Conjoint long format data processing                 ########
######################################################################

######################################################################
#### UK DATA ####
## Read in data, tidy and create necessary vectors
master <- read.csv("data/UK_FINAL.csv",na.strings=c(""))
master<- master[master$Q74 == "Yes, I have read the above statement and understood it" & master$Finished == "TRUE" & !is.na(master$Q74),]

gender <- as.factor(as.character(master$Q30))
age <- 2018-as.numeric(as.character(master$Q28)) - ifelse(!(master$Q29 %in% c("January","February")),1,0)
ideology <- as.numeric(as.character(master$Q72))
student <- as.character(master$Q38)

master$Q98_1 <- ifelse(master$Q98_1 == "Highly Favourable7",7,ifelse(master$Q98_1 == "Highly Unfavourable1",1,master$Q98_1))
master$Q98_2 <- ifelse(master$Q98_2 == "Highly Favourable7",7,ifelse(master$Q98_2 == "Highly Unfavourable1",1,master$Q98_2))
master$Q98_3 <- ifelse(master$Q98_3 == "Highly Favourable7",7,ifelse(master$Q98_3 == "Highly Unfavourable1",1,master$Q98_3))
master$Q8 <- ifelse(master$Q8 == "Very Interested7",7,ifelse(master$Q8 == "Not at all Interested1",1,master$Q8))
master$Q10 <- ifelse(master$Q10 == "Very Likely7",7,ifelse(master$Q10 == "Not at all Likely1",1,master$Q10))

aus <-as.numeric(master$Q98_1)
can <-as.numeric(master$Q98_2)
us <- as.numeric(master$Q98_3)
interest <- as.numeric(master$Q8)
likely <- as.numeric(master$Q10)

AvgTime_UK <- median(as.numeric(as.character(master$Duration..in.seconds.)))/60
AvgPay_UK <- mean(as.numeric(as.character(master$payTotal)))

dice <- as.integer(as.character(master$numberCorrect))

######################################################################
#### TREATMENT 1 ####
##CJ1a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.1.1 == attribute,as.character(master$F.1.1.1),
                         ifelse(master$F.1.2 == attribute,as.character(master$F.1.1.2),
                                ifelse(master$F.1.3 == attribute,as.character(master$F.1.1.3),
                                       ifelse(master$F.1.4 == attribute,as.character(master$F.1.1.4),
                                              as.character(master$F.1.1.5))))))
  age <- 2018-as.numeric(as.character(master$Q28)) - ifelse(!(master$Q29 %in% c("January","February")),1,0)
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.1.1 == attribute,as.character(master$F.1.2.1),
                         ifelse(master$F.1.2 == attribute,as.character(master$F.1.2.2),
                                ifelse(master$F.1.3 == attribute,as.character(master$F.1.2.3),
                                       ifelse(master$F.1.4 == attribute,as.character(master$F.1.2.4),
                                              as.character(master$F.1.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X1_Q5 == "Employment Destination 1" & candidate == 1) | (master$X1_Q5 == "Employment Destination 2" & candidate == 2),1,0)

conjoint1a <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ1b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.2.1 == attribute,as.character(master$F.2.1.1),
                         ifelse(master$F.2.2 == attribute,as.character(master$F.2.1.2),
                                ifelse(master$F.2.3 == attribute,as.character(master$F.2.1.3),
                                       ifelse(master$F.2.4 == attribute,as.character(master$F.2.1.4),
                                              as.character(master$F.2.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.2.1 == attribute,as.character(master$F.2.2.1),
                         ifelse(master$F.2.2 == attribute,as.character(master$F.2.2.2),
                                ifelse(master$F.2.3 == attribute,as.character(master$F.2.2.3),
                                       ifelse(master$F.2.4 == attribute,as.character(master$F.2.2.4),
                                              as.character(master$F.2.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X2_Q5 == "Employment Destination 1" & candidate == 1) | (master$X2_Q5 == "Employment Destination 2" & candidate == 2),1,0)

conjoint1b <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ1c
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.3.1 == attribute,as.character(master$F.3.1.1),
                         ifelse(master$F.3.2 == attribute,as.character(master$F.3.1.2),
                                ifelse(master$F.3.3 == attribute,as.character(master$F.3.1.3),
                                       ifelse(master$F.3.4 == attribute,as.character(master$F.3.1.4),
                                              as.character(master$F.3.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.3.1 == attribute,as.character(master$F.3.2.1),
                         ifelse(master$F.3.2 == attribute,as.character(master$F.3.2.2),
                                ifelse(master$F.3.3 == attribute,as.character(master$F.3.2.3),
                                       ifelse(master$F.3.4 == attribute,as.character(master$F.3.2.4),
                                              as.character(master$F.3.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X3_Q5 == "Employment Destination 1" & candidate == 1) | (master$X3_Q5 == "Employment Destination 2" & candidate == 2),1,0)

conjoint1c <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

conjoint1 <- rbind(conjoint1a,conjoint1b,conjoint1c)
conjoint1 <- cbind(conjoint1, age, gender, ideology, likely)

######################.
#########################
#### TREATMENT 2 ####
#######################

##CJ2a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())
#Candidate 1
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.1.1 == attribute,as.character(master$G.1.1.1),
                         ifelse(master$G.1.2 == attribute,as.character(master$G.1.1.2),
                                ifelse(master$G.1.3 == attribute,as.character(master$G.1.1.3),
                                       ifelse(master$G.1.4 == attribute,as.character(master$G.1.1.4),
                                              as.character(master$G.1.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}
#Candidate 2
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.1.1 == attribute,as.character(master$G.1.2.1),
                         ifelse(master$G.1.2 == attribute,as.character(master$G.1.2.2),
                                ifelse(master$G.1.3 == attribute,as.character(master$G.1.2.3),
                                       ifelse(master$G.1.4 == attribute,as.character(master$G.1.2.4),
                                              as.character(master$G.1.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

#Gen. columns
social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X1_Q100 == "Employment Destination 1" & candidate == 1) | (master$X1_Q100 == "Employment Destination 2" & candidate == 2),1,0)

conjoint2a <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ2b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.2.1 == attribute,as.character(master$G.2.1.1),
                         ifelse(master$G.2.2 == attribute,as.character(master$G.2.1.2),
                                ifelse(master$G.2.3 == attribute,as.character(master$G.2.1.3),
                                       ifelse(master$G.2.4 == attribute,as.character(master$G.2.1.4),
                                              as.character(master$G.2.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.2.1 == attribute,as.character(master$G.2.2.1),
                         ifelse(master$G.2.2 == attribute,as.character(master$G.2.2.2),
                                ifelse(master$G.2.3 == attribute,as.character(master$G.2.2.3),
                                       ifelse(master$G.2.4 == attribute,as.character(master$G.2.2.4),
                                              as.character(master$G.2.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X2_Q100 == "Employment Destination 1" & candidate == 1) | (master$X2_Q100 == "Employment Destination 2" & candidate == 2),1,0)

conjoint2b <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ2c
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.3.1 == attribute,as.character(master$G.3.1.1),
                         ifelse(master$G.3.2 == attribute,as.character(master$G.3.1.2),
                                ifelse(master$G.3.3 == attribute,as.character(master$G.3.1.3),
                                       ifelse(master$G.3.4 == attribute,as.character(master$G.3.1.4),
                                              as.character(master$G.3.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.3.1 == attribute,as.character(master$G.3.2.1),
                         ifelse(master$G.3.2 == attribute,as.character(master$G.3.2.2),
                                ifelse(master$G.3.3 == attribute,as.character(master$G.3.2.3),
                                       ifelse(master$G.3.4 == attribute,as.character(master$G.3.2.4),
                                              as.character(master$G.3.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X3_Q100 == "Employment Destination 1" & candidate == 1) | (master$X3_Q100 == "Employment Destination 2" & candidate == 2),1,0)

conjoint2c <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

conjoint2 <- rbind(conjoint2a,conjoint2b,conjoint2c)
conjoint2 <- cbind(conjoint2, age, gender, ideology, likely)


#######################
#### TREATMENT 3 ####
#######################
##CJ3a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())
#Candidate 1
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.1.1 == attribute,as.character(master$H.1.1.1),
                         ifelse(master$H.1.2 == attribute,as.character(master$H.1.1.2),
                                ifelse(master$H.1.3 == attribute,as.character(master$H.1.1.3),
                                       ifelse(master$H.1.4 == attribute,as.character(master$H.1.1.4),
                                              as.character(master$H.1.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

#Candidate 2
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.1.1 == attribute,as.character(master$H.1.2.1),
                         ifelse(master$H.1.2 == attribute,as.character(master$H.1.2.2),
                                ifelse(master$H.1.3 == attribute,as.character(master$H.1.2.3),
                                       ifelse(master$H.1.4 == attribute,as.character(master$H.1.2.4),
                                              as.character(master$H.1.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

# Check if countries the same
c1 <- ifelse(master$H.1.1 == "Country",as.character(master$H.1.1.1),
             ifelse(master$H.1.2 == "Country",as.character(master$H.1.1.2),
                    ifelse(master$H.1.3 == "Country",as.character(master$H.1.1.3),
                           ifelse(master$H.1.4 == "Country",as.character(master$H.1.1.4),
                                  as.character(master$H.1.1.5)))))
c2 <- ifelse(master$H.1.1 == "Country",as.character(master$H.1.2.1),
             ifelse(master$H.1.2 == "Country",as.character(master$H.1.2.2),
                    ifelse(master$H.1.3 == "Country",as.character(master$H.1.2.3),
                           ifelse(master$H.1.4 == "Country",as.character(master$H.1.2.4),
                                  as.character(master$H.1.2.5)))))
#If countries are the same, 1 else 0
c.same <- ifelse(c1 == c2,1,0)

#Gen. columns
social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
country <- df$Level[df$Attribute == "Country"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X1_Q107 == "Employment Destination 1" & candidate == 1) | (master$X1_Q107 == "Employment Destination 2" & candidate == 2),1,0)

conjoint3a <- data.frame(social, econ, service, country,education,destination,candidate,c.same, id = (1:nrow(master)))

##CJ3b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.2.1 == attribute,as.character(master$H.2.1.1),
                         ifelse(master$H.2.2 == attribute,as.character(master$H.2.1.2),
                                ifelse(master$H.2.3 == attribute,as.character(master$H.2.1.3),
                                       ifelse(master$H.2.4 == attribute,as.character(master$H.2.1.4),
                                              as.character(master$H.2.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.2.1 == attribute,as.character(master$H.2.2.1),
                         ifelse(master$H.2.2 == attribute,as.character(master$H.2.2.2),
                                ifelse(master$H.2.3 == attribute,as.character(master$H.2.2.3),
                                       ifelse(master$H.2.4 == attribute,as.character(master$H.2.2.4),
                                              as.character(master$H.2.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

c1 <- ifelse(master$H.2.1 == "Country",as.character(master$H.2.1.1),
             ifelse(master$H.2.2 == "Country",as.character(master$H.2.1.2),
                    ifelse(master$H.2.3 == "Country",as.character(master$H.2.1.3),
                           ifelse(master$H.2.4 == "Country",as.character(master$H.2.1.4),
                                  as.character(master$H.2.1.5)))))

c2 <- ifelse(master$H.2.1 == "Country",as.character(master$H.2.2.1),
             ifelse(master$H.2.2 == "Country",as.character(master$H.2.2.2),
                    ifelse(master$H.2.3 == "Country",as.character(master$H.2.2.3),
                           ifelse(master$H.2.4 == "Country",as.character(master$H.2.2.4),
                                  as.character(master$H.2.2.5)))))

#If countries are the same, 1 else 0
c.same <- ifelse(c1 == c2,1,0)

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
country <- df$Level[df$Attribute == "Country"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X2_Q107 == "Employment Destination 1" & candidate == 1) | (master$X2_Q107 == "Employment Destination 2" & candidate == 2),1,0)

conjoint3b <- data.frame(social, econ, service, country,education,destination,candidate,c.same, id = (1:nrow(master)))

##CJ3c
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.3.1 == attribute,as.character(master$H.3.1.1),
                         ifelse(master$H.3.2 == attribute,as.character(master$H.3.1.2),
                                ifelse(master$H.3.3 == attribute,as.character(master$H.3.1.3),
                                       ifelse(master$H.3.4 == attribute,as.character(master$H.3.1.4),
                                              as.character(master$H.3.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.3.1 == attribute,as.character(master$H.3.2.1),
                         ifelse(master$H.3.2 == attribute,as.character(master$H.3.2.2),
                                ifelse(master$H.3.3 == attribute,as.character(master$H.3.2.3),
                                       ifelse(master$H.3.4 == attribute,as.character(master$H.3.2.4),
                                              as.character(master$H.3.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

c1 <- ifelse(master$H.3.1 == "Country",as.character(master$H.3.1.1),
             ifelse(master$H.3.2 == "Country",as.character(master$H.3.1.2),
                    ifelse(master$H.3.3 == "Country",as.character(master$H.3.1.3),
                           ifelse(master$H.3.4 == "Country",as.character(master$H.3.1.4),
                                  as.character(master$H.3.1.5)))))

c2 <- ifelse(master$H.3.1 == "Country",as.character(master$H.3.2.1),
             ifelse(master$H.3.2 == "Country",as.character(master$H.3.2.2),
                    ifelse(master$H.3.3 == "Country",as.character(master$H.3.2.3),
                           ifelse(master$H.3.4 == "Country",as.character(master$H.3.2.4),
                                  as.character(master$H.3.2.5)))))

#If countries are the same, 1 else 0
c.same <- ifelse(c1 == c2,1,0)

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
country <- df$Level[df$Attribute == "Country"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X3_Q107 == "Employment Destination 1" & candidate == 1) | (master$X3_Q107 == "Employment Destination 2" & candidate == 2),1,0)

conjoint3c <- data.frame(social, econ, service, country,education,destination,candidate,c.same, id = (1:nrow(master)))

conjoint3 <- rbind(conjoint3a,conjoint3b,conjoint3c)
conjoint3 <- cbind(conjoint3, age, gender, ideology, likely)

conjoint1 <- cbind(conjoint1,interest,aus,can,us,survey="UK",dice)
conjoint2 <- cbind(conjoint2,interest,aus,can,us,survey="UK",dice)
conjoint3 <- cbind(conjoint3,interest,aus,can,us,survey="UK",dice)

write.csv(conjoint1,"Processed/conjoint1_uk.csv",row.names=FALSE)
write.csv(conjoint2,"Processed/conjoint2_uk.csv",row.names=FALSE)
write.csv(conjoint3,"Processed/conjoint3_uk.csv",row.names=FALSE)

######################################################################
#### CHILE DATA ####

transDF <- read.xlsx("data/Translations.xlsx","Spanish")
chile <- read.csv("data/CHILE_final.csv")

# Attributes & levels
chile <- data.frame(lapply(chile, function(x) {
  gsub("Promedio internacional en ranking salarial: Percentil 90 ", "Promedio internacional en ranking salarial: Percentil 90", x)
}))


for (i in transDF$Spanish[!is.na(transDF$Spanish)]) {
  chile <- data.frame(lapply(chile, function(x) {
    gsub(i, transDF$English[transDF$Spanish==i][1], x)
  }))
}

# Employment destinations
chile <- data.frame(lapply(chile, function(x) {
  gsub("Destino laboral 1", "Employment Destination 1", x)
}))

chile <- data.frame(lapply(chile, function(x) {
  gsub("Destino laboral 2", "Employment Destination 2", x)
}))

chile <- data.frame(lapply(chile, function(x) {
  gsub("Destino Laboral 1", "Employment Destination 1", x)
}))

chile <- data.frame(lapply(chile, function(x) {
  gsub("Destino Laboral 2", "Employment Destination 2", x)
}))

# Gender
chile$Q30 <- ifelse(chile$Q30 == "Femenino", "Female", ifelse(chile$Q30 == "Masculino", "Male", chile$Q30))

## Read in data, tidy and create necessary vectors
master <- chile
master<- master[master$Q74 == "Sí, he leído y comprendido" & master$Finished == "TRUE" & !is.na(master$Q74),]

gender <- as.factor(as.character(master$Q30))
age <- 2018-as.numeric(as.character(master$Q28)) - ifelse(!(master$Q29 %in% c("Enero","Febrero")),1,0)

master$Q72 <- as.character(master$Q72)
master$Q72 <- ifelse(master$Q72 == "Extrema Izquierda0",0,ifelse(master$Q72 == "Extrema Derecha10",10,master$Q72))
ideology <- as.integer(as.character(master$Q72))
student <- as.character(master$Q38)

master$Q98_1 <- ifelse(master$Q92_1 == "Muy Favorable7",7,ifelse(master$Q92_1 == "Muy Desfavorable1",1,master$Q92_1))
master$Q98_2 <- ifelse(master$Q92_2 == "Muy Favorable7",7,ifelse(master$Q92_2 == "Muy Desfavorable1",1,master$Q92_2))
master$Q98_3 <- ifelse(master$Q92_3 == "Muy Favorable7",7,ifelse(master$Q92_3 == "Muy Desfavorable1",1,master$Q92_3))

master$Q8 <- ifelse(master$Q8 == "Muy interesado 7",7,ifelse(master$Q8 == "Nada interesado1",1,as.integer(as.character(master$Q8))))
master$Q10 <- ifelse(master$Q10 == "Muy posible 7",7,ifelse(master$Q10 == "Nada posible 1",1,master$Q10))

aus <-as.numeric(master$Q92_1)
can <-as.numeric(master$Q92_2)
us <- as.numeric(master$Q92_3)
interest <- as.numeric(master$Q8)
likely <- as.numeric(master$Q10)

AvgTime_Chile <- median(as.numeric(as.character(master$Duration..in.seconds.)))/60
AvgPay_Chile <- mean(as.numeric(as.character(master$pay))) + 1000

dice <- as.integer(as.character(master$numberCorrect))

######################################################################
#### TREATMENT 1 ####
##CJ1a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.1.1 == attribute,as.character(master$F.1.1.1),
                         ifelse(master$F.1.2 == attribute,as.character(master$F.1.1.2),
                                ifelse(master$F.1.3 == attribute,as.character(master$F.1.1.3),
                                       ifelse(master$F.1.4 == attribute,as.character(master$F.1.1.4),
                                              as.character(master$F.1.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.1.1 == attribute,as.character(master$F.1.2.1),
                         ifelse(master$F.1.2 == attribute,as.character(master$F.1.2.2),
                                ifelse(master$F.1.3 == attribute,as.character(master$F.1.2.3),
                                       ifelse(master$F.1.4 == attribute,as.character(master$F.1.2.4),
                                              as.character(master$F.1.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X1_Q5 == "Employment Destination 1" & candidate == 1) | (master$X1_Q5 == "Employment Destination 2" & candidate == 2),1,0)

conjoint1a <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ1b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.2.1 == attribute,as.character(master$F.2.1.1),
                         ifelse(master$F.2.2 == attribute,as.character(master$F.2.1.2),
                                ifelse(master$F.2.3 == attribute,as.character(master$F.2.1.3),
                                       ifelse(master$F.2.4 == attribute,as.character(master$F.2.1.4),
                                              as.character(master$F.2.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.2.1 == attribute,as.character(master$F.2.2.1),
                         ifelse(master$F.2.2 == attribute,as.character(master$F.2.2.2),
                                ifelse(master$F.2.3 == attribute,as.character(master$F.2.2.3),
                                       ifelse(master$F.2.4 == attribute,as.character(master$F.2.2.4),
                                              as.character(master$F.2.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X2_Q5 == "Employment Destination 1" & candidate == 1) | (master$X2_Q5 == "Employment Destination 2" & candidate == 2),1,0)

conjoint1b <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ1c
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.3.1 == attribute,as.character(master$F.3.1.1),
                         ifelse(master$F.3.2 == attribute,as.character(master$F.3.1.2),
                                ifelse(master$F.3.3 == attribute,as.character(master$F.3.1.3),
                                       ifelse(master$F.3.4 == attribute,as.character(master$F.3.1.4),
                                              as.character(master$F.3.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.3.1 == attribute,as.character(master$F.3.2.1),
                         ifelse(master$F.3.2 == attribute,as.character(master$F.3.2.2),
                                ifelse(master$F.3.3 == attribute,as.character(master$F.3.2.3),
                                       ifelse(master$F.3.4 == attribute,as.character(master$F.3.2.4),
                                              as.character(master$F.3.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X3_Q5 == "Employment Destination 1" & candidate == 1) | (master$X3_Q5 == "Employment Destination 2" & candidate == 2),1,0)

conjoint1c <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

conjoint1 <- rbind(conjoint1a,conjoint1b,conjoint1c)
conjoint1 <- cbind(conjoint1, age, gender, ideology, likely)

######################.
#########################
#### TREATMENT 2 ####
#######################

##CJ2a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())
#Candidate 1
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.1.1 == attribute,as.character(master$G.1.1.1),
                         ifelse(master$G.1.2 == attribute,as.character(master$G.1.1.2),
                                ifelse(master$G.1.3 == attribute,as.character(master$G.1.1.3),
                                       ifelse(master$G.1.4 == attribute,as.character(master$G.1.1.4),
                                              as.character(master$G.1.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}
#Candidate 2
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.1.1 == attribute,as.character(master$G.1.2.1),
                         ifelse(master$G.1.2 == attribute,as.character(master$G.1.2.2),
                                ifelse(master$G.1.3 == attribute,as.character(master$G.1.2.3),
                                       ifelse(master$G.1.4 == attribute,as.character(master$G.1.2.4),
                                              as.character(master$G.1.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

#Gen. columns
social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X1_Q100 == "Employment Destination 1" & candidate == 1) | (master$X1_Q100 == "Employment Destination 2" & candidate == 2),1,0)

conjoint2a <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ2b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.2.1 == attribute,as.character(master$G.2.1.1),
                         ifelse(master$G.2.2 == attribute,as.character(master$G.2.1.2),
                                ifelse(master$G.2.3 == attribute,as.character(master$G.2.1.3),
                                       ifelse(master$G.2.4 == attribute,as.character(master$G.2.1.4),
                                              as.character(master$G.2.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.2.1 == attribute,as.character(master$G.2.2.1),
                         ifelse(master$G.2.2 == attribute,as.character(master$G.2.2.2),
                                ifelse(master$G.2.3 == attribute,as.character(master$G.2.2.3),
                                       ifelse(master$G.2.4 == attribute,as.character(master$G.2.2.4),
                                              as.character(master$G.2.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X2_Q100 == "Employment Destination 1" & candidate == 1) | (master$X2_Q100 == "Employment Destination 2" & candidate == 2),1,0)

conjoint2b <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ2c
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.3.1 == attribute,as.character(master$G.3.1.1),
                         ifelse(master$G.3.2 == attribute,as.character(master$G.3.1.2),
                                ifelse(master$G.3.3 == attribute,as.character(master$G.3.1.3),
                                       ifelse(master$G.3.4 == attribute,as.character(master$G.3.1.4),
                                              as.character(master$G.3.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.3.1 == attribute,as.character(master$G.3.2.1),
                         ifelse(master$G.3.2 == attribute,as.character(master$G.3.2.2),
                                ifelse(master$G.3.3 == attribute,as.character(master$G.3.2.3),
                                       ifelse(master$G.3.4 == attribute,as.character(master$G.3.2.4),
                                              as.character(master$G.3.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X3_Q100 == "Employment Destination 1" & candidate == 1) | (master$X3_Q100 == "Employment Destination 2" & candidate == 2),1,0)

conjoint2c <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

conjoint2 <- rbind(conjoint2a,conjoint2b,conjoint2c)
conjoint2 <- cbind(conjoint2, age, gender, ideology, likely)


#######################
#### TREATMENT 3 ####
#######################
##CJ3a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())
#Candidate 1
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.1.1 == attribute,as.character(master$H.1.1.1),
                         ifelse(master$H.1.2 == attribute,as.character(master$H.1.1.2),
                                ifelse(master$H.1.3 == attribute,as.character(master$H.1.1.3),
                                       ifelse(master$H.1.4 == attribute,as.character(master$H.1.1.4),
                                              as.character(master$H.1.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

#Candidate 2
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.1.1 == attribute,as.character(master$H.1.2.1),
                         ifelse(master$H.1.2 == attribute,as.character(master$H.1.2.2),
                                ifelse(master$H.1.3 == attribute,as.character(master$H.1.2.3),
                                       ifelse(master$H.1.4 == attribute,as.character(master$H.1.2.4),
                                              as.character(master$H.1.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

# Check if countries the same
c1 <- ifelse(master$H.1.1 == "Country",as.character(master$H.1.1.1),
             ifelse(master$H.1.2 == "Country",as.character(master$H.1.1.2),
                    ifelse(master$H.1.3 == "Country",as.character(master$H.1.1.3),
                           ifelse(master$H.1.4 == "Country",as.character(master$H.1.1.4),
                                  as.character(master$H.1.1.5)))))
c2 <- ifelse(master$H.1.1 == "Country",as.character(master$H.1.2.1),
             ifelse(master$H.1.2 == "Country",as.character(master$H.1.2.2),
                    ifelse(master$H.1.3 == "Country",as.character(master$H.1.2.3),
                           ifelse(master$H.1.4 == "Country",as.character(master$H.1.2.4),
                                  as.character(master$H.1.2.5)))))
#If countries are the same, 1 else 0
c.same <- ifelse(c1 == c2,1,0)

#Gen. columns
social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
country <- df$Level[df$Attribute == "Country"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X1_Q107 == "Employment Destination 1" & candidate == 1) | (master$X1_Q107 == "Employment Destination 2" & candidate == 2),1,0)

conjoint3a <- data.frame(social, econ, service, country,education,destination,candidate,c.same, id = (1:nrow(master)))

##CJ3b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.2.1 == attribute,as.character(master$H.2.1.1),
                         ifelse(master$H.2.2 == attribute,as.character(master$H.2.1.2),
                                ifelse(master$H.2.3 == attribute,as.character(master$H.2.1.3),
                                       ifelse(master$H.2.4 == attribute,as.character(master$H.2.1.4),
                                              as.character(master$H.2.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.2.1 == attribute,as.character(master$H.2.2.1),
                         ifelse(master$H.2.2 == attribute,as.character(master$H.2.2.2),
                                ifelse(master$H.2.3 == attribute,as.character(master$H.2.2.3),
                                       ifelse(master$H.2.4 == attribute,as.character(master$H.2.2.4),
                                              as.character(master$H.2.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

c1 <- ifelse(master$H.2.1 == "Country",as.character(master$H.2.1.1),
             ifelse(master$H.2.2 == "Country",as.character(master$H.2.1.2),
                    ifelse(master$H.2.3 == "Country",as.character(master$H.2.1.3),
                           ifelse(master$H.2.4 == "Country",as.character(master$H.2.1.4),
                                  as.character(master$H.2.1.5)))))

c2 <- ifelse(master$H.2.1 == "Country",as.character(master$H.2.2.1),
             ifelse(master$H.2.2 == "Country",as.character(master$H.2.2.2),
                    ifelse(master$H.2.3 == "Country",as.character(master$H.2.2.3),
                           ifelse(master$H.2.4 == "Country",as.character(master$H.2.2.4),
                                  as.character(master$H.2.2.5)))))

#If countries are the same, 1 else 0
c.same <- ifelse(c1 == c2,1,0)

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
country <- df$Level[df$Attribute == "Country"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X2_Q107 == "Employment Destination 1" & candidate == 1) | (master$X2_Q107 == "Employment Destination 2" & candidate == 2),1,0)

conjoint3b <- data.frame(social, econ, service, country,education,destination,candidate,c.same, id = (1:nrow(master)))

##CJ3c
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.3.1 == attribute,as.character(master$H.3.1.1),
                         ifelse(master$H.3.2 == attribute,as.character(master$H.3.1.2),
                                ifelse(master$H.3.3 == attribute,as.character(master$H.3.1.3),
                                       ifelse(master$H.3.4 == attribute,as.character(master$H.3.1.4),
                                              as.character(master$H.3.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.3.1 == attribute,as.character(master$H.3.2.1),
                         ifelse(master$H.3.2 == attribute,as.character(master$H.3.2.2),
                                ifelse(master$H.3.3 == attribute,as.character(master$H.3.2.3),
                                       ifelse(master$H.3.4 == attribute,as.character(master$H.3.2.4),
                                              as.character(master$H.3.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

c1 <- ifelse(master$H.3.1 == "Country",as.character(master$H.3.1.1),
             ifelse(master$H.3.2 == "Country",as.character(master$H.3.1.2),
                    ifelse(master$H.3.3 == "Country",as.character(master$H.3.1.3),
                           ifelse(master$H.3.4 == "Country",as.character(master$H.3.1.4),
                                  as.character(master$H.3.1.5)))))

c2 <- ifelse(master$H.3.1 == "Country",as.character(master$H.3.2.1),
             ifelse(master$H.3.2 == "Country",as.character(master$H.3.2.2),
                    ifelse(master$H.3.3 == "Country",as.character(master$H.3.2.3),
                           ifelse(master$H.3.4 == "Country",as.character(master$H.3.2.4),
                                  as.character(master$H.3.2.5)))))

#If countries are the same, 1 else 0
c.same <- ifelse(c1 == c2,1,0)

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
country <- df$Level[df$Attribute == "Country"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X3_Q107 == "Employment Destination 1" & candidate == 1) | (master$X3_Q107 == "Employment Destination 2" & candidate == 2),1,0)

conjoint3c <- data.frame(social, econ, service, country,education,destination,candidate,c.same, id = (1:nrow(master)))

conjoint3 <- rbind(conjoint3a,conjoint3b,conjoint3c)
conjoint3 <- cbind(conjoint3, age, gender, ideology, likely)

conjoint1 <- cbind(conjoint1,interest,aus,can,us,survey="Chile",dice)
conjoint2 <- cbind(conjoint2,interest,aus,can,us,survey="Chile",dice)
conjoint3 <- cbind(conjoint3,interest,aus,can,us,survey="Chile",dice)

write.csv(conjoint1,"Processed/conjoint1_chile.csv",row.names=FALSE)
write.csv(conjoint2,"Processed/conjoint2_chile.csv",row.names=FALSE)
write.csv(conjoint3,"Processed/conjoint3_chile.csv",row.names=FALSE)







######################################################################
#### CHINA DATA ####
######################################################################
transDF <- read.xlsx("data/Translations.xlsx","Levels")
china <- read.csv("data/China_FINAL.csv")

# Attributes & levels
# china <- data.frame(lapply(china, function(x) {
#   gsub("Promedio internacional en ranking salarial: Percentil 90 ", "Promedio internacional en ranking salarial: Percentil 90", x)
# }))


for (i in transDF$New.Chinese[!is.na(transDF$New.Chinese)]) {
  china <- data.frame(lapply(china, function(x) {
    gsub(i, transDF$English[transDF$New.Chinese==i][1], x)
  }))
}

unique(china$H.1.1)
unique(china$H.1.1.1)

# Employment destinations
china <- data.frame(lapply(china, function(x) {
  gsub("就业目的地 1", "Employment Destination 1", x)
}))

china <- data.frame(lapply(china, function(x) {
  gsub("就业目的地 2", "Employment Destination 2", x)
}))


# Gender
china$Q30 <- ifelse(china$Q30 == "女", "Female", ifelse(china$Q30 == "男", "Male", china$Q30))


## Excludes responses where age is in 90's because not-potential emigrant, most probably rogue data
master <- china
master<- master[master$Q74 == "是，我已阅读上述同意书并充分理解" & master$Finished == "TRUE" 
                & !is.na(master$Q74) & master$ID != "TEST"
                & as.integer(as.character(master$Q28)) > 1928,]

gender <- as.factor(as.character(master$Q30))
age <- 2018-as.integer(as.character(master$Q28)) - ifelse(!(master$Q29 %in% c("一月","二月")),1,0)

master$Q72 <- as.character(master$Q72)
master$Q72 <- ifelse(master$Q72 == "极左 0",0,ifelse(master$Q72 == "极右  10",10,master$Q72))
ideology <- as.numeric(as.character(master$Q72))
student <- as.character(master$Q38)

master$Q98_1 <- ifelse(master$Q92_1 == "强烈支持7",7,ifelse(master$Q92_1 == "强烈反对1",1,master$Q92_1))
master$Q98_2 <- ifelse(master$Q92_2 == "强烈支持7",7,ifelse(master$Q92_2 == "强烈反对1",1,master$Q92_2))
master$Q98_3 <- ifelse(master$Q92_3 == "强烈支持7",7,ifelse(master$Q92_3 == "强烈反对1",1,master$Q92_3))
master$Q8 <- ifelse(master$Q8 == "非常感兴趣7",7,ifelse(master$Q8 == "根本不感兴趣1",1,master$Q8))
master$Q10 <- ifelse(master$Q10 == "非常可能7",7,ifelse(master$Q10 == "完全不可能1",1,master$Q10))

aus <-as.numeric(master$Q92_1)
can <-as.numeric(master$Q92_2)
us <- as.numeric(master$Q92_3)
interest <- as.numeric(master$Q8)
likely <- as.numeric(master$Q10)

AvgTime_China <- median(as.numeric(as.character(master$Duration..in.seconds.)))/60
AvgPay_China <- mean(as.numeric(as.character(master$payTotal)))

dice <- as.integer(as.character(master$numberCorrect))

#### TREATMENT 1 ####
##CJ1a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.1.1 == attribute,as.character(master$F.1.1.1),
                         ifelse(master$F.1.2 == attribute,as.character(master$F.1.1.2),
                                ifelse(master$F.1.3 == attribute,as.character(master$F.1.1.3),
                                       ifelse(master$F.1.4 == attribute,as.character(master$F.1.1.4),
                                              as.character(master$F.1.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.1.1 == attribute,as.character(master$F.1.2.1),
                         ifelse(master$F.1.2 == attribute,as.character(master$F.1.2.2),
                                ifelse(master$F.1.3 == attribute,as.character(master$F.1.2.3),
                                       ifelse(master$F.1.4 == attribute,as.character(master$F.1.2.4),
                                              as.character(master$F.1.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X1_Q5 == "Employment Destination 1" & candidate == 1) | (master$X1_Q5 == "Employment Destination 2" & candidate == 2),1,0)

conjoint1a <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ1b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.2.1 == attribute,as.character(master$F.2.1.1),
                         ifelse(master$F.2.2 == attribute,as.character(master$F.2.1.2),
                                ifelse(master$F.2.3 == attribute,as.character(master$F.2.1.3),
                                       ifelse(master$F.2.4 == attribute,as.character(master$F.2.1.4),
                                              as.character(master$F.2.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.2.1 == attribute,as.character(master$F.2.2.1),
                         ifelse(master$F.2.2 == attribute,as.character(master$F.2.2.2),
                                ifelse(master$F.2.3 == attribute,as.character(master$F.2.2.3),
                                       ifelse(master$F.2.4 == attribute,as.character(master$F.2.2.4),
                                              as.character(master$F.2.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X2_Q5 == "Employment Destination 1" & candidate == 1) | (master$X2_Q5 == "Employment Destination 2" & candidate == 2),1,0)

conjoint1b <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ1c
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.3.1 == attribute,as.character(master$F.3.1.1),
                         ifelse(master$F.3.2 == attribute,as.character(master$F.3.1.2),
                                ifelse(master$F.3.3 == attribute,as.character(master$F.3.1.3),
                                       ifelse(master$F.3.4 == attribute,as.character(master$F.3.1.4),
                                              as.character(master$F.3.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.3.1 == attribute,as.character(master$F.3.2.1),
                         ifelse(master$F.3.2 == attribute,as.character(master$F.3.2.2),
                                ifelse(master$F.3.3 == attribute,as.character(master$F.3.2.3),
                                       ifelse(master$F.3.4 == attribute,as.character(master$F.3.2.4),
                                              as.character(master$F.3.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X3_Q5 == "Employment Destination 1" & candidate == 1) | (master$X3_Q5 == "Employment Destination 2" & candidate == 2),1,0)

conjoint1c <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

conjoint1 <- rbind(conjoint1a,conjoint1b,conjoint1c)
conjoint1 <- cbind(conjoint1, age, gender, ideology, likely)

######################.
#########################
#### TREATMENT 2 ####
#######################

##CJ2a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())
#Candidate 1
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.1.1 == attribute,as.character(master$G.1.1.1),
                         ifelse(master$G.1.2 == attribute,as.character(master$G.1.1.2),
                                ifelse(master$G.1.3 == attribute,as.character(master$G.1.1.3),
                                       ifelse(master$G.1.4 == attribute,as.character(master$G.1.1.4),
                                              as.character(master$G.1.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}
#Candidate 2
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.1.1 == attribute,as.character(master$G.1.2.1),
                         ifelse(master$G.1.2 == attribute,as.character(master$G.1.2.2),
                                ifelse(master$G.1.3 == attribute,as.character(master$G.1.2.3),
                                       ifelse(master$G.1.4 == attribute,as.character(master$G.1.2.4),
                                              as.character(master$G.1.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

#Gen. columns
social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X1_Q100 == "Employment Destination 1" & candidate == 1) | (master$X1_Q100 == "Employment Destination 2" & candidate == 2),1,0)

conjoint2a <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ2b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.2.1 == attribute,as.character(master$G.2.1.1),
                         ifelse(master$G.2.2 == attribute,as.character(master$G.2.1.2),
                                ifelse(master$G.2.3 == attribute,as.character(master$G.2.1.3),
                                       ifelse(master$G.2.4 == attribute,as.character(master$G.2.1.4),
                                              as.character(master$G.2.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.2.1 == attribute,as.character(master$G.2.2.1),
                         ifelse(master$G.2.2 == attribute,as.character(master$G.2.2.2),
                                ifelse(master$G.2.3 == attribute,as.character(master$G.2.2.3),
                                       ifelse(master$G.2.4 == attribute,as.character(master$G.2.2.4),
                                              as.character(master$G.2.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X2_Q100 == "Employment Destination 1" & candidate == 1) | (master$X2_Q100 == "Employment Destination 2" & candidate == 2),1,0)

conjoint2b <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ2c
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.3.1 == attribute,as.character(master$G.3.1.1),
                         ifelse(master$G.3.2 == attribute,as.character(master$G.3.1.2),
                                ifelse(master$G.3.3 == attribute,as.character(master$G.3.1.3),
                                       ifelse(master$G.3.4 == attribute,as.character(master$G.3.1.4),
                                              as.character(master$G.3.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.3.1 == attribute,as.character(master$G.3.2.1),
                         ifelse(master$G.3.2 == attribute,as.character(master$G.3.2.2),
                                ifelse(master$G.3.3 == attribute,as.character(master$G.3.2.3),
                                       ifelse(master$G.3.4 == attribute,as.character(master$G.3.2.4),
                                              as.character(master$G.3.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X3_Q100 == "Employment Destination 1" & candidate == 1) | (master$X3_Q100 == "Employment Destination 2" & candidate == 2),1,0)

conjoint2c <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

conjoint2 <- rbind(conjoint2a,conjoint2b,conjoint2c)
conjoint2 <- cbind(conjoint2, age, gender, ideology, likely)


#######################
#### TREATMENT 3 ####
#######################
##CJ3a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())
#Candidate 1
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.1.1 == attribute,as.character(master$H.1.1.1),
                         ifelse(master$H.1.2 == attribute,as.character(master$H.1.1.2),
                                ifelse(master$H.1.3 == attribute,as.character(master$H.1.1.3),
                                       ifelse(master$H.1.4 == attribute,as.character(master$H.1.1.4),
                                              as.character(master$H.1.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

#Candidate 2
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.1.1 == attribute,as.character(master$H.1.2.1),
                         ifelse(master$H.1.2 == attribute,as.character(master$H.1.2.2),
                                ifelse(master$H.1.3 == attribute,as.character(master$H.1.2.3),
                                       ifelse(master$H.1.4 == attribute,as.character(master$H.1.2.4),
                                              as.character(master$H.1.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

# Check if countries the same
c1 <- ifelse(master$H.1.1 == "Country",as.character(master$H.1.1.1),
             ifelse(master$H.1.2 == "Country",as.character(master$H.1.1.2),
                    ifelse(master$H.1.3 == "Country",as.character(master$H.1.1.3),
                           ifelse(master$H.1.4 == "Country",as.character(master$H.1.1.4),
                                  as.character(master$H.1.1.5)))))
c2 <- ifelse(master$H.1.1 == "Country",as.character(master$H.1.2.1),
             ifelse(master$H.1.2 == "Country",as.character(master$H.1.2.2),
                    ifelse(master$H.1.3 == "Country",as.character(master$H.1.2.3),
                           ifelse(master$H.1.4 == "Country",as.character(master$H.1.2.4),
                                  as.character(master$H.1.2.5)))))
#If countries are the same, 1 else 0
c.same <- ifelse(c1 == c2,1,0)

#Gen. columns
social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
country <- df$Level[df$Attribute == "Country"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X1_Q107 == "Employment Destination 1" & candidate == 1) | (master$X1_Q107 == "Employment Destination 2" & candidate == 2),1,0)

conjoint3a <- data.frame(social, econ, service, country,education,destination,candidate,c.same, id = (1:nrow(master)))

##CJ3b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.2.1 == attribute,as.character(master$H.2.1.1),
                         ifelse(master$H.2.2 == attribute,as.character(master$H.2.1.2),
                                ifelse(master$H.2.3 == attribute,as.character(master$H.2.1.3),
                                       ifelse(master$H.2.4 == attribute,as.character(master$H.2.1.4),
                                              as.character(master$H.2.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.2.1 == attribute,as.character(master$H.2.2.1),
                         ifelse(master$H.2.2 == attribute,as.character(master$H.2.2.2),
                                ifelse(master$H.2.3 == attribute,as.character(master$H.2.2.3),
                                       ifelse(master$H.2.4 == attribute,as.character(master$H.2.2.4),
                                              as.character(master$H.2.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

c1 <- ifelse(master$H.2.1 == "Country",as.character(master$H.2.1.1),
             ifelse(master$H.2.2 == "Country",as.character(master$H.2.1.2),
                    ifelse(master$H.2.3 == "Country",as.character(master$H.2.1.3),
                           ifelse(master$H.2.4 == "Country",as.character(master$H.2.1.4),
                                  as.character(master$H.2.1.5)))))

c2 <- ifelse(master$H.2.1 == "Country",as.character(master$H.2.2.1),
             ifelse(master$H.2.2 == "Country",as.character(master$H.2.2.2),
                    ifelse(master$H.2.3 == "Country",as.character(master$H.2.2.3),
                           ifelse(master$H.2.4 == "Country",as.character(master$H.2.2.4),
                                  as.character(master$H.2.2.5)))))

#If countries are the same, 1 else 0
c.same <- ifelse(c1 == c2,1,0)

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
country <- df$Level[df$Attribute == "Country"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X2_Q107 == "Employment Destination 1" & candidate == 1) | (master$X2_Q107 == "Employment Destination 2" & candidate == 2),1,0)

conjoint3b <- data.frame(social, econ, service, country,education,destination,candidate,c.same, id = (1:nrow(master)))

##CJ3c
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.3.1 == attribute,as.character(master$H.3.1.1),
                         ifelse(master$H.3.2 == attribute,as.character(master$H.3.1.2),
                                ifelse(master$H.3.3 == attribute,as.character(master$H.3.1.3),
                                       ifelse(master$H.3.4 == attribute,as.character(master$H.3.1.4),
                                              as.character(master$H.3.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.3.1 == attribute,as.character(master$H.3.2.1),
                         ifelse(master$H.3.2 == attribute,as.character(master$H.3.2.2),
                                ifelse(master$H.3.3 == attribute,as.character(master$H.3.2.3),
                                       ifelse(master$H.3.4 == attribute,as.character(master$H.3.2.4),
                                              as.character(master$H.3.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

c1 <- ifelse(master$H.3.1 == "Country",as.character(master$H.3.1.1),
             ifelse(master$H.3.2 == "Country",as.character(master$H.3.1.2),
                    ifelse(master$H.3.3 == "Country",as.character(master$H.3.1.3),
                           ifelse(master$H.3.4 == "Country",as.character(master$H.3.1.4),
                                  as.character(master$H.3.1.5)))))

c2 <- ifelse(master$H.3.1 == "Country",as.character(master$H.3.2.1),
             ifelse(master$H.3.2 == "Country",as.character(master$H.3.2.2),
                    ifelse(master$H.3.3 == "Country",as.character(master$H.3.2.3),
                           ifelse(master$H.3.4 == "Country",as.character(master$H.3.2.4),
                                  as.character(master$H.3.2.5)))))

#If countries are the same, 1 else 0
c.same <- ifelse(c1 == c2,1,0)

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
country <- df$Level[df$Attribute == "Country"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X3_Q107 == "Employment Destination 1" & candidate == 1) | (master$X3_Q107 == "Employment Destination 2" & candidate == 2),1,0)

conjoint3c <- data.frame(social, econ, service, country,education,destination,candidate,c.same, id = (1:nrow(master)))

conjoint3 <- rbind(conjoint3a,conjoint3b,conjoint3c)
conjoint3 <- cbind(conjoint3, age, gender, ideology, likely)

conjoint1 <- cbind(conjoint1,interest,aus,can,us,survey="china",dice)
conjoint2 <- cbind(conjoint2,interest,aus,can,us,survey="china",dice)
conjoint3 <- cbind(conjoint3,interest,aus,can,us,survey="china",dice)

write.csv(conjoint1,"Processed/conjoint1_china.csv",row.names=FALSE)
write.csv(conjoint2,"Processed/conjoint2_china.csv",row.names=FALSE)
write.csv(conjoint3,"Processed/conjoint3_china.csv",row.names=FALSE)

######################################################################
#### INDIA DATA ####
######################################################################

master <- read.csv("data/India_FINAL.csv",na.strings=c(""))
master<- master[master$Q74 == "Yes, I have read the above statement and understood it" & master$Finished == "TRUE" & !is.na(master$Q74),]

gender <- as.factor(as.character(master$Q30))
age <- 2018-as.integer(as.character(master$Q28)) - ifelse(!(master$Q29 %in% c("January","February","March","April","May")),1,0)

master$Q72 <- as.character(master$Q72)
master$Q72 <- ifelse(master$Q72 == "Extreme Right  10",10,ifelse(master$Q72 == "Extreme Left\n0",0,master$Q72))
ideology <- as.numeric(as.character(master$Q72))
student <- as.character(master$Q38)

master$Q92_1 <- ifelse(master$Q92_1 == "Highly Favourable7",7,ifelse(master$Q92_1 == "Highly Unfavourable1",1,master$Q92_1))
master$Q92_2 <- ifelse(master$Q92_2 == "Highly Favourable7",7,ifelse(master$Q92_2 == "Highly Unfavourable1",1,master$Q92_2))
master$Q92_3 <- ifelse(master$Q92_3 == "Highly Favourable7",7,ifelse(master$Q92_3 == "Highly Unfavourable1",1,master$Q92_3))
master$Q8 <- ifelse(master$Q8 == "Very Interested7",7,ifelse(master$Q8 == "Not at all Interested1",1,master$Q8))
master$Q10 <- ifelse(master$Q10 == "Very Likely7",7,ifelse(master$Q10 == "Not at all Likely1",1,master$Q10))

aus <-as.numeric(master$Q92_1)
can <-as.numeric(master$Q92_2)
us <- as.numeric(master$Q92_3)
interest <- as.numeric(master$Q8)
likely <- as.numeric(master$Q10)

AvgTime_India <- median(as.numeric(as.character(master$Duration..in.seconds.)))/60
AvgPay_India <- mean(as.numeric(as.character(master$payTotal)))

dice <- as.integer(as.character(master$numberCorrect))

#### TREATMENT 1 ####
##CJ1a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.1.1 == attribute,as.character(master$F.1.1.1),
                         ifelse(master$F.1.2 == attribute,as.character(master$F.1.1.2),
                                ifelse(master$F.1.3 == attribute,as.character(master$F.1.1.3),
                                       ifelse(master$F.1.4 == attribute,as.character(master$F.1.1.4),
                                              as.character(master$F.1.1.5))))))
  age <- 2018-as.numeric(as.character(master$Q28)) - ifelse(!(master$Q29 %in% c("January","February")),1,0)
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.1.1 == attribute,as.character(master$F.1.2.1),
                         ifelse(master$F.1.2 == attribute,as.character(master$F.1.2.2),
                                ifelse(master$F.1.3 == attribute,as.character(master$F.1.2.3),
                                       ifelse(master$F.1.4 == attribute,as.character(master$F.1.2.4),
                                              as.character(master$F.1.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X1_Q5 == "Employment Destination 1" & candidate == 1) | (master$X1_Q5 == "Employment Destination 2" & candidate == 2),1,0)

conjoint1a <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ1b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.2.1 == attribute,as.character(master$F.2.1.1),
                         ifelse(master$F.2.2 == attribute,as.character(master$F.2.1.2),
                                ifelse(master$F.2.3 == attribute,as.character(master$F.2.1.3),
                                       ifelse(master$F.2.4 == attribute,as.character(master$F.2.1.4),
                                              as.character(master$F.2.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.2.1 == attribute,as.character(master$F.2.2.1),
                         ifelse(master$F.2.2 == attribute,as.character(master$F.2.2.2),
                                ifelse(master$F.2.3 == attribute,as.character(master$F.2.2.3),
                                       ifelse(master$F.2.4 == attribute,as.character(master$F.2.2.4),
                                              as.character(master$F.2.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X2_Q5 == "Employment Destination 1" & candidate == 1) | (master$X2_Q5 == "Employment Destination 2" & candidate == 2),1,0)

conjoint1b <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ1c
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.3.1 == attribute,as.character(master$F.3.1.1),
                         ifelse(master$F.3.2 == attribute,as.character(master$F.3.1.2),
                                ifelse(master$F.3.3 == attribute,as.character(master$F.3.1.3),
                                       ifelse(master$F.3.4 == attribute,as.character(master$F.3.1.4),
                                              as.character(master$F.3.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$F.3.1 == attribute,as.character(master$F.3.2.1),
                         ifelse(master$F.3.2 == attribute,as.character(master$F.3.2.2),
                                ifelse(master$F.3.3 == attribute,as.character(master$F.3.2.3),
                                       ifelse(master$F.3.4 == attribute,as.character(master$F.3.2.4),
                                              as.character(master$F.3.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X3_Q5 == "Employment Destination 1" & candidate == 1) | (master$X3_Q5 == "Employment Destination 2" & candidate == 2),1,0)

conjoint1c <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

conjoint1 <- rbind(conjoint1a,conjoint1b,conjoint1c)
conjoint1 <- cbind(conjoint1, age, gender, ideology, likely)

######################.
#########################
#### TREATMENT 2 ####
#######################

##CJ2a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())
#Candidate 1
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.1.1 == attribute,as.character(master$G.1.1.1),
                         ifelse(master$G.1.2 == attribute,as.character(master$G.1.1.2),
                                ifelse(master$G.1.3 == attribute,as.character(master$G.1.1.3),
                                       ifelse(master$G.1.4 == attribute,as.character(master$G.1.1.4),
                                              as.character(master$G.1.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}
#Candidate 2
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.1.1 == attribute,as.character(master$G.1.2.1),
                         ifelse(master$G.1.2 == attribute,as.character(master$G.1.2.2),
                                ifelse(master$G.1.3 == attribute,as.character(master$G.1.2.3),
                                       ifelse(master$G.1.4 == attribute,as.character(master$G.1.2.4),
                                              as.character(master$G.1.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

#Gen. columns
social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X1_Q100 == "Employment Destination 1" & candidate == 1) | (master$X1_Q100 == "Employment Destination 2" & candidate == 2),1,0)

conjoint2a <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ2b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.2.1 == attribute,as.character(master$G.2.1.1),
                         ifelse(master$G.2.2 == attribute,as.character(master$G.2.1.2),
                                ifelse(master$G.2.3 == attribute,as.character(master$G.2.1.3),
                                       ifelse(master$G.2.4 == attribute,as.character(master$G.2.1.4),
                                              as.character(master$G.2.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.2.1 == attribute,as.character(master$G.2.2.1),
                         ifelse(master$G.2.2 == attribute,as.character(master$G.2.2.2),
                                ifelse(master$G.2.3 == attribute,as.character(master$G.2.2.3),
                                       ifelse(master$G.2.4 == attribute,as.character(master$G.2.2.4),
                                              as.character(master$G.2.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X2_Q100 == "Employment Destination 1" & candidate == 1) | (master$X2_Q100 == "Employment Destination 2" & candidate == 2),1,0)

conjoint2b <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

##CJ2c
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.3.1 == attribute,as.character(master$G.3.1.1),
                         ifelse(master$G.3.2 == attribute,as.character(master$G.3.1.2),
                                ifelse(master$G.3.3 == attribute,as.character(master$G.3.1.3),
                                       ifelse(master$G.3.4 == attribute,as.character(master$G.3.1.4),
                                              as.character(master$G.3.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","New Immigration Policies", "Education Opportunities")) {
  level <- factor(ifelse(master$G.3.1 == attribute,as.character(master$G.3.2.1),
                         ifelse(master$G.3.2 == attribute,as.character(master$G.3.2.2),
                                ifelse(master$G.3.3 == attribute,as.character(master$G.3.2.3),
                                       ifelse(master$G.3.4 == attribute,as.character(master$G.3.2.4),
                                              as.character(master$G.3.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
immigration <- df$Level[df$Attribute == "New Immigration Policies"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X3_Q100 == "Employment Destination 1" & candidate == 1) | (master$X3_Q100 == "Employment Destination 2" & candidate == 2),1,0)

conjoint2c <- data.frame(social, econ, service, immigration,education,destination,candidate, id = (1:nrow(master)))

conjoint2 <- rbind(conjoint2a,conjoint2b,conjoint2c)
conjoint2 <- cbind(conjoint2, age, gender, ideology, likely)


#######################
#### TREATMENT 3 ####
#######################
##CJ3a
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())
#Candidate 1
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.1.1 == attribute,as.character(master$H.1.1.1),
                         ifelse(master$H.1.2 == attribute,as.character(master$H.1.1.2),
                                ifelse(master$H.1.3 == attribute,as.character(master$H.1.1.3),
                                       ifelse(master$H.1.4 == attribute,as.character(master$H.1.1.4),
                                              as.character(master$H.1.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

#Candidate 2
for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.1.1 == attribute,as.character(master$H.1.2.1),
                         ifelse(master$H.1.2 == attribute,as.character(master$H.1.2.2),
                                ifelse(master$H.1.3 == attribute,as.character(master$H.1.2.3),
                                       ifelse(master$H.1.4 == attribute,as.character(master$H.1.2.4),
                                              as.character(master$H.1.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

# Check if countries the same
c1 <- ifelse(master$H.1.1 == "Country",as.character(master$H.1.1.1),
             ifelse(master$H.1.2 == "Country",as.character(master$H.1.1.2),
                    ifelse(master$H.1.3 == "Country",as.character(master$H.1.1.3),
                           ifelse(master$H.1.4 == "Country",as.character(master$H.1.1.4),
                                  as.character(master$H.1.1.5)))))
c2 <- ifelse(master$H.1.1 == "Country",as.character(master$H.1.2.1),
             ifelse(master$H.1.2 == "Country",as.character(master$H.1.2.2),
                    ifelse(master$H.1.3 == "Country",as.character(master$H.1.2.3),
                           ifelse(master$H.1.4 == "Country",as.character(master$H.1.2.4),
                                  as.character(master$H.1.2.5)))))
#If countries are the same, 1 else 0
c.same <- ifelse(c1 == c2,1,0)

#Gen. columns
social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
country <- df$Level[df$Attribute == "Country"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X1_Q107 == "Employment Destination 1" & candidate == 1) | (master$X1_Q107 == "Employment Destination 2" & candidate == 2),1,0)

conjoint3a <- data.frame(social, econ, service, country,education,destination,candidate,c.same, id = (1:nrow(master)))

##CJ3b
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.2.1 == attribute,as.character(master$H.2.1.1),
                         ifelse(master$H.2.2 == attribute,as.character(master$H.2.1.2),
                                ifelse(master$H.2.3 == attribute,as.character(master$H.2.1.3),
                                       ifelse(master$H.2.4 == attribute,as.character(master$H.2.1.4),
                                              as.character(master$H.2.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.2.1 == attribute,as.character(master$H.2.2.1),
                         ifelse(master$H.2.2 == attribute,as.character(master$H.2.2.2),
                                ifelse(master$H.2.3 == attribute,as.character(master$H.2.2.3),
                                       ifelse(master$H.2.4 == attribute,as.character(master$H.2.2.4),
                                              as.character(master$H.2.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

c1 <- ifelse(master$H.2.1 == "Country",as.character(master$H.2.1.1),
             ifelse(master$H.2.2 == "Country",as.character(master$H.2.1.2),
                    ifelse(master$H.2.3 == "Country",as.character(master$H.2.1.3),
                           ifelse(master$H.2.4 == "Country",as.character(master$H.2.1.4),
                                  as.character(master$H.2.1.5)))))

c2 <- ifelse(master$H.2.1 == "Country",as.character(master$H.2.2.1),
             ifelse(master$H.2.2 == "Country",as.character(master$H.2.2.2),
                    ifelse(master$H.2.3 == "Country",as.character(master$H.2.2.3),
                           ifelse(master$H.2.4 == "Country",as.character(master$H.2.2.4),
                                  as.character(master$H.2.2.5)))))

#If countries are the same, 1 else 0
c.same <- ifelse(c1 == c2,1,0)

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
country <- df$Level[df$Attribute == "Country"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X2_Q107 == "Employment Destination 1" & candidate == 1) | (master$X2_Q107 == "Employment Destination 2" & candidate == 2),1,0)

conjoint3b <- data.frame(social, econ, service, country,education,destination,candidate,c.same, id = (1:nrow(master)))

##CJ3c
df <- data.frame(Attribute=as.character(),Level=as.character(),Candidate=as.character())

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.3.1 == attribute,as.character(master$H.3.1.1),
                         ifelse(master$H.3.2 == attribute,as.character(master$H.3.1.2),
                                ifelse(master$H.3.3 == attribute,as.character(master$H.3.1.3),
                                       ifelse(master$H.3.4 == attribute,as.character(master$H.3.1.4),
                                              as.character(master$H.3.1.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 1))
}

for (attribute in c("Social Benefits", "Economic Performance", "Service Sector Salaries","Country", "Education Opportunities")) {
  level <- factor(ifelse(master$H.3.1 == attribute,as.character(master$H.3.2.1),
                         ifelse(master$H.3.2 == attribute,as.character(master$H.3.2.2),
                                ifelse(master$H.3.3 == attribute,as.character(master$H.3.2.3),
                                       ifelse(master$H.3.4 == attribute,as.character(master$H.3.2.4),
                                              as.character(master$H.3.2.5))))))
  df <- rbind(df,data.frame(Attribute = attribute, Level = level, Candidate = 2))
}

c1 <- ifelse(master$H.3.1 == "Country",as.character(master$H.3.1.1),
             ifelse(master$H.3.2 == "Country",as.character(master$H.3.1.2),
                    ifelse(master$H.3.3 == "Country",as.character(master$H.3.1.3),
                           ifelse(master$H.3.4 == "Country",as.character(master$H.3.1.4),
                                  as.character(master$H.3.1.5)))))

c2 <- ifelse(master$H.3.1 == "Country",as.character(master$H.3.2.1),
             ifelse(master$H.3.2 == "Country",as.character(master$H.3.2.2),
                    ifelse(master$H.3.3 == "Country",as.character(master$H.3.2.3),
                           ifelse(master$H.3.4 == "Country",as.character(master$H.3.2.4),
                                  as.character(master$H.3.2.5)))))

#If countries are the same, 1 else 0
c.same <- ifelse(c1 == c2,1,0)

social <- df$Level[df$Attribute == "Social Benefits"]
econ <- df$Level[df$Attribute == "Economic Performance"]
service <- df$Level[df$Attribute == "Service Sector Salaries"]
country <- df$Level[df$Attribute == "Country"]
education <- df$Level[df$Attribute == "Education Opportunities"]
candidate <- df$Candidate[df$Attribute == "Education Opportunities"]
destination <- ifelse((master$X3_Q107 == "Employment Destination 1" & candidate == 1) | (master$X3_Q107 == "Employment Destination 2" & candidate == 2),1,0)

conjoint3c <- data.frame(social, econ, service, country,education,destination,candidate,c.same, id = (1:nrow(master)))

conjoint3 <- rbind(conjoint3a,conjoint3b,conjoint3c)
conjoint3 <- cbind(conjoint3, age, gender, ideology, likely)

conjoint1 <- cbind(conjoint1,interest,aus,can,us,survey="India",dice)
conjoint2 <- cbind(conjoint2,interest,aus,can,us,survey="India",dice)
conjoint3 <- cbind(conjoint3,interest,aus,can,us,survey="India",dice)

write.csv(conjoint1,"Processed/conjoint1_india.csv",row.names=FALSE)
write.csv(conjoint2,"Processed/conjoint2_india.csv",row.names=FALSE)
write.csv(conjoint3,"Processed/conjoint3_india.csv",row.names=FALSE)

