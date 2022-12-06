#PROBLEM 0
LBO <- read.csv("LifetimeBoxOffice.csv")
str(LBO)
#200 obs. or 4 variables
#PROBLEM 1
LBO[1:10,2:3]
#PROBLEM 2
subset(LBO,subset=LifetimeGross>1e+09)
#PROBLEM 3
subset(LBO,subset=Year==2019)
#PROBLEM 4
year_df<-subset(LBO,subset=Year==2019)
gross_vec<-year_df[,3]
mean(gross_vec)
sd(gross_vec)
#the mean is $1,183,262,439, and the standard deviation is $558,485,335
#PROBLEM 5
plot(LBO[,4],LBO[,3],main= "Lifetime Gross by Year", xlab = "Year", ylab = "Lifetime Gross (USD)")
#On average, Lifetime Gross increases as time progresses, and movies produced 
#in later years are more likely to appear on the top 200 list. 
#PROBLEM 6
y <- c(1,2,1,3,4,1,1,4,2,1,3,4,3,2,1,3,4,1,2,3,1,1,2)
fac_y<-factor(y, ordered=TRUE)
levels(fac_y)<-c("Freshman","Sophomore","Junior","Senior")
fac_y
