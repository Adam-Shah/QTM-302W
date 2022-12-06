library(ggplot2)
library(dplyr)
library(plotly)

HD_NeurEx_data <- read.csv("HD_NeurEx_data.csv")
NeurEx <- read.csv("NeurEx_data.csv")
fullNeurEx <- full_join(HD_NeurEx_data, NeurEx, by= c("patientID", "New_ID","NeurEx_Panel_02_L", 
                                                      "NeurEx_Panel_02_R","NeurEx_Panel_03_L",
                                                      "NeurEx_Panel_03_R","NeurEx_Panel_04_L",
                                                      "NeurEx_Panel_04_R", "NeurEx_total"))

summary(fullNeurEx)
fullNeurEx
head(fullNeurEx)
tail(fullNeurEx)

ggplot(fullNeurEx, aes(x= Age, y=NeurEx_total)) + geom_point()

