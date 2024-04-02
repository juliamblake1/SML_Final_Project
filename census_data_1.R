# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

#setwd("C:/IPUMS")
#install.packages("ipumsr")
#library(ipumsr)

ddi <- read_ipums_ddi("usa_00002.xml")
data <- read_ipums_micro(ddi)

data_df <- as.data.frame(data)
head(data_df)


summary(data_df)
str(data_df)
head(data_df)

############################################################################################################################

library(ggplot2)  # For data visualization
library(caret)    # For machine learning functions
