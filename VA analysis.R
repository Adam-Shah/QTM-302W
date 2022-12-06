library(ggplot2)
library(plotly)
library(dplyr)

full_data <- read.csv("full_vision_data.csv")
NeurEx <- read.csv("NeurEx_data.csv")
combined <- full_join(full_data, NeurEx, by="New_ID")

VA_data <- subset(combined, select=c("New_ID", "gender", "diagnosis", "Age", "Date", "NeurEx_total",
                                      "VA_1.25_contrast_OU","VA_2.5_contrast_OU", "VA_5_contrast_OU", "VA_10_contrast_OU", "VA_100_contrast_OU",
                                      "VA_1.25_contrast_OD","VA_2.5_contrast_OD", "VA_5_contrast_OD", "VA_10_contrast_OD", "VA_100_contrast_OD",
                                      "VA_1.25_contrast_OS","VA_2.5_contrast_OS", "VA_5_contrast_OS", "VA_10_contrast_OS", "VA_100_contrast_OS"))
ggplotly(ggplot(VA_data, aes(x=VA_1.25_contrast_OU, y= VA_100_contrast_OU)) +geom_point(aes(col=diagnosis)))

ggplot(VA_data, aes(x=VA_1.25_contrast_OU, y=NeurEx_total)) +geom_point(aes(col=diagnosis)) +geom_smooth(method="lm")
model <- lm(NeurEx_total ~ VA_1.25_contrast_OU + VA_100_contrast_OU, data=VA_data)
summary(model)

