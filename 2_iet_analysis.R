#########################################
##                                     ##
## Analyse information extraction task ##
##                                     ##
#########################################

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

#### Data read in, translation, tidy and format ####

##  UK
master <- read.csv("data/UK_FINAL.csv",na.strings=c(""))
master<- master[master$Q74 == "Yes, I have read the above statement and understood it" & master$Finished == "TRUE" & !is.na(master$Q74),]

master$Q98_1 <- ifelse(master$Q98_1 == "Highly Favourable7",7,ifelse(master$Q98_1 == "Highly Unfavourable1",1,master$Q98_1))
master$Q98_2 <- ifelse(master$Q98_2 == "Highly Favourable7",7,ifelse(master$Q98_2 == "Highly Unfavourable1",1,master$Q98_2))
master$Q98_3 <- ifelse(master$Q98_3 == "Highly Favourable7",7,ifelse(master$Q98_3 == "Highly Unfavourable1",1,master$Q98_3))
master$Q8 <- ifelse(master$Q8 == "Very Interested7",7,ifelse(master$Q8 == "Not at all Interested1",1,master$Q8))
master$Q10 <- ifelse(master$Q10 == "Very Likely7",7,ifelse(master$Q10 == "Not at all Likely1",1,master$Q10))

master_uk <- master

## Chile

master <- NULL

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

chile <- data.frame(lapply(chile, function(x) {
  gsub("Estados Unidos", "United States", x)
}))

# Gender
chile$Q30 <- ifelse(chile$Q30 == "Femenino", "Female", ifelse(chile$Q30 == "Masculino", "Male", chile$Q30))

master <- chile
master<- master[master$Q74 == "Sí, he leído y comprendido" & master$Finished == "TRUE" & !is.na(master$Q74),]

master$Q72 <- as.character(master$Q72)
master$Q72 <- ifelse(master$Q72 == "Extrema Izquierda0",0,ifelse(master$Q72 == "Extrema Derecha10",10,master$Q72))

master$Q98_1 <- ifelse(master$Q92_1 == "Muy Favorable7",7,ifelse(master$Q92_1 == "Muy Desfavorable1",1,master$Q92_1))
master$Q98_2 <- ifelse(master$Q92_2 == "Muy Favorable7",7,ifelse(master$Q92_2 == "Muy Desfavorable1",1,master$Q92_2))
master$Q98_3 <- ifelse(master$Q92_3 == "Muy Favorable7",7,ifelse(master$Q92_3 == "Muy Desfavorable1",1,master$Q92_3))

master$Q8 <- ifelse(master$Q8 == "Muy interesado 7",7,ifelse(master$Q8 == "Nada interesado1",1,as.integer(as.character(master$Q8))))
master$Q10 <- ifelse(master$Q10 == "Muy posible 7",7,ifelse(master$Q10 == "Nada posible 1",1,master$Q10))

master_chile <- master

## China

master <- NULL

china <- read.csv("data/China_FINAL.csv")
transDF <- read.xlsx("data/Translations.xlsx","Levels")

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

master$Q72 <- as.character(master$Q72)
master$Q72 <- ifelse(master$Q72 == "极左 0",0,ifelse(master$Q72 == "极右  10",10,master$Q72))

master$Q98_1 <- ifelse(master$Q92_1 == "强烈支持7",7,ifelse(master$Q92_1 == "强烈反对1",1,master$Q92_1))
master$Q98_2 <- ifelse(master$Q92_2 == "强烈支持7",7,ifelse(master$Q92_2 == "强烈反对1",1,master$Q92_2))
master$Q98_3 <- ifelse(master$Q92_3 == "强烈支持7",7,ifelse(master$Q92_3 == "强烈反对1",1,master$Q92_3))
master$Q8 <- ifelse(master$Q8 == "非常感兴趣7",7,ifelse(master$Q8 == "根本不感兴趣1",1,master$Q8))
master$Q10 <- ifelse(master$Q10 == "非常可能7",7,ifelse(master$Q10 == "完全不可能1",1,master$Q10))

master$Q11 <- ifelse(master$Q11 == "U.S.A.","United States",as.character(master$Q11))

master_china <- master

## India
master <- NULL

master <- read.csv("data/India_FINAL.csv",na.strings=c(""))
master<- master[master$Q74 == "Yes, I have read the above statement and understood it" & master$Finished == "TRUE" & !is.na(master$Q74),]

master$Q72 <- as.character(master$Q72)
master$Q72 <- ifelse(master$Q72 == "Extreme Right  10",10,ifelse(master$Q72 == "Extreme Left\n0",0,master$Q72))

master$Q92_1 <- ifelse(master$Q92_1 == "Highly Favourable7",7,ifelse(master$Q92_1 == "Highly Unfavourable1",1,master$Q92_1))
master$Q92_2 <- ifelse(master$Q92_2 == "Highly Favourable7",7,ifelse(master$Q92_2 == "Highly Unfavourable1",1,master$Q92_2))
master$Q92_3 <- ifelse(master$Q92_3 == "Highly Favourable7",7,ifelse(master$Q92_3 == "Highly Unfavourable1",1,master$Q92_3))
master$Q8 <- ifelse(master$Q8 == "Very Interested7",7,ifelse(master$Q8 == "Not at all Interested1",1,master$Q8))
master$Q10 <- ifelse(master$Q10 == "Very Likely7",7,ifelse(master$Q10 == "Not at all Likely1",1,master$Q10))

master$Q11 <- ifelse(master$Q11 == "United Kingdom","U.K.",as.character(master$Q11))

master_india <- master

rm(chile, china, master)

#### Table 3 ####

master_uk$Q97 <- as.factor(as.character(master_uk$Q97))
master_chile$Q11 <- as.factor(as.character(master_chile$Q11))
master_china$Q11 <- as.factor(as.character(master_china$Q11))
master_india$Q11 <- as.factor(as.character(master_india$Q11))

pops <- c("U.K.","Chile","China","India")
choices <- c("Australia","United States","U.K.","Canada")
choice_proportions <- matrix(nrow = 4, ncol = 4, dimnames = list(pops,choices))

for (j in choices) {
    choice_proportions["U.K.", j] <- summary(master_uk$Q97)[j]/sum(!is.na(master_uk$Q97))
    choice_proportions["Chile", j] <- summary(master_chile$Q11)[j]/sum(!is.na(master_chile$Q11))
    choice_proportions["China", j] <- summary(master_china$Q11)[j]/sum(!is.na(master_china$Q11))
    choice_proportions["India", j] <- summary(master_india$Q11)[j]/sum(!is.na(master_india$Q11))
}

print(xtable(choice_proportions,
             caption = "Proportion of times each country was chosen for the information extraction task (by subject pool)",
             label = "tab:iet"),
      add.to.row = list(pos = list(-1),
                        command = "\\hline & \\multicolumn{4}{c}{\\textbf{Choice}} \\\\ \\cline{2-5} \\textbf{Subject Pool}"),
      hline.after = c(0,nrow(choice_proportions)),
      caption.placement = "top",
      file = "tables/table_3.tex")
