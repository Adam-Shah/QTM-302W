install.packages("glmnet")
install.packages("caret")
install.packages("mlbench")
install.packages("psych")

library(glmnet)
library(psych)
library(caret)

set.seed(42)
n<- 1000
p<- 5000

real_p <- 15

#creating data set
x <- matrix(rnorm(n*p), nrow=n, ncol=p)
y <- apply(x[,1:real_p],1,sum) +rnorm(n)

train_rows <- sample(1:n, 0.66*n)
x.train <- x[train_rows,]
x.test <- x[-train_rows,]
y.train <- y[train_rows]
y.test <- y[-train_rows]

#ridge regression (alpha = 0)
ridge.fit <- cv.glmnet(x.train, y.train, type.measure="mse", alpha=0, family="gaussian")

