library(tidyverse)
library(dplyr)

#Imports IPUMS dataset

data <- read.csv("usa_00011.csv.gz")

#Columns of interest(COI)
COI <- c("YEAR","SERIAL","STATEFIP","DENSITY","RENTGRS","INCTOT",
         "PERNUM","SEX","AGE","MARST","RACE","BPL","CITIZEN",
         "LANGUAGE","TRIBE","EDUC","EMPSTAT")


data_COI <- data %>% dplyr::select(all_of(COI)) #selects COI from "data" 

#Checks for/removes missing data

nrow(data_COI[is.na(data_COI)]) #checks for NA values in entire data_COI data set
nrow(data_COI[data_COI$STATEFIP == 99,]) #Number of missing state entries: 0
nrow(data_COI[data_COI$SEX == 9,]) #Number of missing sex entries: 0
nrow(data_COI[data_COI$AGE == 999,]) #Number of missing age entries: 0
nrow(data_COI[data_COI$MARST == 9,]) #Number of missing marital status entries: 0
nrow(data_COI[data_COI$BPL == 997 | data_COI$BPL == 999 ,]) #Number of missing birthplace entries: 0
nrow(data_COI[data_COI$LANGUAGE == 0,]) #Number of missing language entries: 589032
nrow(data_COI[data_COI$INCTOT == 9999999|data_COI$INCTOT == 9999998, ]) #Number of missing income entries: 1966979

#Imputation for language variables

###

###Data Partitions
renters <- data_COI[data_COI$RENTGRS != 0 & data_COI$RENTGRS >= 200 &
                    data_COI$AGE >= 18,] #Adults who are renters paying $200 or more in rent
renters_2022 <- renters[renters$YEAR == 2022,] #2022 renters data
renters_pre2022 <- renters[renters$YEAR != 2022,] #2019 - 2021 renters data
####

###Feature Engineering/Type Conversions

#Creates a list of the number of people in each household
hhmbs_list <- list(); HHMEMBS_list <- list()
hhmbs_list <- append(hhmbs_list,renters_2022 %>% count(SERIAL))
hhmbs_table <- data.frame(hhmbs_list)

for (i in 1:nrow(renters_2022)){
  index <- match(renters_2022$SERIAL[i],hhmbs_table$SERIAL)
  HHMEMBS_list <- append(HHMEMBS_list,hhmbs_table$n[index])
}

#Creates new column in "renters_2022" from HHMEMBS_list

renters_2022$HHMEMBS <- HHMEMBS_list

renters_2022$STATEFIP <- as.factor(renters_2022$STATEFIP) #Converts STATEFIP type to factors
renters_2022$SEX <- as.factor(renters_2022$SEX) #Converts SEX type to factors
renters_2022$MARST <- as.factor(renters_2022$MARST) #Converts MARST type to factors
renters_2022$BPL <- as.factor(renters_2022$STATEFIP) #Converts BPL type to factors
renters_2022$CITIZEN <- as.factor(renters_2022$SEX) #Converts CITIZEN type to factors
renters_2022$RACE <- as.factor(renters_2022$MARST) #Converts RACE type to factors
renters_2022$LANGUAGE <- as.factor(renters_2022$STATEFIP) #Converts LANGUAGE type to factors
renters_2022$TRIBE <- as.factor(renters_2022$SEX) #Converts TRIBE type to factors
renters_2022$EDUC <- as.factor(renters_2022$MARST) #Converts EDUC type to factors
renters_2022$EMPSTAT <- as.factor(renters_2022$MARST) #Converts EMPSTAT type to factors

#Cross-validation partitioning
set.seed(1234)

spec = c(train = .6, test = .2, validation = .2)

partitions = sample(cut(seq(nrow(renters_2022)), nrow(renters_2022)*cumsum(c(0,spec)),labels = names(spec)))

res = split(renters_2022, partitions)

###File Exporting
renters_2022 <- apply(renters_2022,2,as.character)
res$train <- apply(res$train,2,as.character)
res$test <- apply(res$test,2,as.character)
res$validation <- apply(res$validation,2,as.character)

write.csv(renters_2022,'renters_2022.csv')
write.csv(res$train,'train.csv')
write.csv(res$test,'test.csv')
write.csv(res$validation,'validation.csv')












###Models

#model <- lm(RENTGRS ~ STATEFIP + SEX + AGE + MARST + BPL + LANGUAGE + DENSITY + 
#              INCTOT + RACE + CITIZEN + EDUC + EMPSTAT, data = renters_2022)
#summary(model)




