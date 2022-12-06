gss<-read.csv("gss.csv")

summary(gss$year)
is.factor(gss$year)
gss$year2<-as.factor(gss$year)

summary(gss$year2)
levels(gss$year2)
is.factor(gss$year2)
summary(gss$year2)
#Now shows the quantity in each level of the factor as opposed to considering it as a numeric

#2006-> 4510

#question 4
gss_female<-subset(gss, subset=sex=="FEMALE")
mean(gss$age[gss$year=="2006" & gss$sex=="FEMALE"],na.rm=TRUE)
str(gss_female$age)
#mean is 47.65586

?df
?data.frame

#6
gss$income2<-factor(gss$income)
is.factor(gss$income2)
levels(gss$income2)
table(gss$income2)
levels(gss$income2)<- c("2000", "12500", "17500", "22500", "25000", "3500", "4500", "5500", "6500", "7500", "9000", "DK", "1000", "REFUSED")
levels(gss$income2)
table(gss$income2)
sum(is.na(gss$income2))

#7
gss$income2[gss$income=="DK"] <- NA
gss$income2[gss$income=="REFUSED"] <- NA
sum(is.na(gss$income2))
summary(gss$income2)

#8
gss$income2<-as.numeric(as.character(gss$income2))
is.numeric(gss$income2)
mean(gss$income2[gss$sex=="MALE"],na.rm=TRUE)
mean(gss$income2[gss$sex=="FEMALE"], na.rm=TRUE)
#male- 19540.32, female-18534.34