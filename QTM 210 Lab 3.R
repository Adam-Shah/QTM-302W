dicedoubles<- function(n) {
  doubles=0

  for (i in 1:n){
    die1 <- sample(1:6, size=n, replace=T)
    die2 <- sample(1:6, size=n. replace=T)
    
    if (die1==die2) {
      doubles=doubles +1
    } else {
      doubles = doubles
    }
  }
  pdoubles <- doubles/n
}


die1<- sample(1:6, size=10, replace = T)
die2<- sample(1:6, size=10, replace = T)
die1[4]==die2[4]

doubles=0
  
for (i in 1:100){ 
  die1 <- sample(1:6, size=1, replace=T)
  die2 <- sample(1:6, size=1, replace=T)
    
  if (die1==die2) {
    doubles=doubles +1
  } else {
    doubles = doubles
  }
}

pdoubles <- doubles/100
pdoubles

dicedoubles<- function(n) {
  doubles=0
  
  for (i in 1:n){ 
    die1 <- sample(1:6, size=1, replace=T)
    die2 <- sample(1:6, size=1, replace=T)
    
    if (die1==die2) {
      doubles=doubles +1
    } else {
      doubles = doubles
    }
  }
pdoubles<- doubles/n
pdoubles
}
dicedoubles(100)
pdoubles <- doubles/n
dicedoubles(100)
doubles

N<-c(10,100,200,300,400,500)
dicedoubles(N)

plot(10,dicedoubles(10), geom='point')

#test for different values of n
N <- 1:10 * 50
#create a vector to save results
result<- matrix(NA, nrow=10, ncol=1)
for (j in 1:10){
  result[j,] <- dicedoubles(N[j])
}

#final step, plot our result
plot(N, result[,1], type= 'b', ylab='p of doubles', ylim=c(0,1))
abline(h=(1/6),col='green')
     