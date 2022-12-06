gss<-read.csv("gss.csv")
dim(gss)
summary(gss)
gss[1:10,"marital"]
is.na()
str(gss)
sum(is.na(gss$marital))

table(gss$sex)
female_percent<-(30022/(30022+23452))*100
male_percent<-(23452/(30022+23452))*100


table(gss$age)
hist(gss$age)
sum(gss$age, na.rm = TRUE)
gss[substr("age",30,40)]
age_vector<-c(gss$age, na.rm=TRUE)
gss[substr(age_vector,30,40)]
str(subset(age_vector,subset<30))
str(subset(age_vector,subset=age>40))
((53474-(29179+11400))/53474)*100

str(subset(gss, subset = age<30, na.rm=TRUE))
str(subset(gss, subset=age>40, na.rm=TRUE))
x<- subset(gss, subset=year==2008)
str(x)
y<- subset(gss, subset=year==2010)
str(y)
a<-mean(x[,"age"], na.rm = TRUE)
b<-mean(y[,"age"], na.rm=TRUE)
a*(2023/(2023+2044))+b*(2044/(2023+2044))
