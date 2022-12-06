library(ggplot2)
library(plotly)
library(dplyr)

NeurEx <- read.csv("NeurEx_data.csv")
full_data <- read.csv("full_vision_data.csv")

summary(NeurEx)
summary(full_data)

?full_join
combined <- full_join(full_data, NeurEx, by="New_ID")

summary(combined)
p <- ggplot(combined, aes(x=Age, y=NeurEx_total), rm.na=T) + 
  geom_point(aes(col=diagnosis)) #+
#  scale_fill_manual(
#    values= combined$diagnosis,
#    limits = c('CIS','Healthy Donor', 'NIND', 'OIND', 'PP-MS','RIS','RR-MS','SP-MS')
# )
ggplotly(p)

#NeurEx total vs. avg. RNFL thickness
ggplotly(ggplot(combined, aes(x=Mean_RNFL, y=NeurEx_total)) + 
  geom_point(aes(col=diagnosis, alpha = Age)))

#avg. RNFL thickness vs. age
r <- ggplotly(ggplot(combined, aes(x=Age, y=Mean_RNFL)) + 
  geom_point(aes(col=diagnosis)))
htmlwidgets::saveWidget(as_widget(r), "Mean_RNFL_vs_Age.html")

r <- ggplotly(ggplot(combined, aes(x=Age, y=NeurEx_total)) + 
                geom_point(aes(col=diagnosis))) 
r



summary(combined)
f <- ggplot(combined, aes(x=VA_1.25_contrast_OU, y=VA_100_contrast_OU)) + 
  geom_point()
ggplotly(f)



#PCA with VA data
VA_OU_PCAdata <- subset(combined, select=c("VA_1.25_contrast_OU","VA_2.5_contrast_OU", "VA_5_contrast_OU", "VA_10_contrast_OU", "VA_100_contrast_OU"))
VA_OU_pca <- prcomp(na.omit(VA_OU_PCAdata), scale=T)
plot(VA_OU_pca$x[,1], VA_OU_pca$x[,2])

pca.var <- VA_OU_pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main= "Scree Plot", xlab = "Principal Component", ylab= "Percent of Variation")

pca_plotdata <- data.frame(sample=rownames(VA_OU_pca$x), x=VA_OU_pca$x[,1], y=VA_OU_pca$x[,2])
summary(pca_plotdata)

ggplotly(ggplot(data=pca_plotdata, aes(x=x, y=y, label=sample)) + geom_point() + 
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) + 
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw()
)


p <- ggplot(combined, aes(x=Age, y=Height), rm.na=T) + 
  geom_point(aes(col=gender)) 
ggplotly(p)

nonMScohort <- subset(combined,diagnosis == "CIS" | 
                        diagnosis == "Healthy Donor"|diagnosis ==  "NIND"|
                        diagnosis ==  "OIND"|diagnosis ==  "RIS")

#effect of age on RNFL and NeurEx with correlation statistics
ggplot(nonMScohort, aes(x=Age, y=Mean_RNFL)) + 
  geom_point(aes(col=diagnosis)) + stat_cor(method="pearson")

ggplot(nonMScohort, aes(x=Age, y=NeurEx_total)) + 
  geom_point(aes(col=diagnosis)) + stat_cor(method="pearson")
cor.test(nonMScohort$Age, nonMScohort$NeurEx_total)
summary(lm(NeurEx_total~Age, data=nonMScohort))
                                                                   
                                                                   