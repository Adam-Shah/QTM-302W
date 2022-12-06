#simulate Monty Hall problem
noswitchwins = 0
switchwins=0

for (i in 1:1000){
  car <- sample(1:3, size=1)
  guess <- sample(1:3, size=1)

  if (car==guess) {
    noswitchwins=noswitchwins +1
  } else {
    switchwins = switchwins +1
  }
}
pnoswitchwins=noswitchwins/1000
pswitchwins=switchwins/1000

#store as a function
MontyHall <- function(n) {
  noswitchwins = 0
  switchwins=0

  for (i in 1:n){
    car <- sample(1:3, size=1)
    guess <- sample(1:3, size=1)
    
    if (car==guess) {
      noswitchwins=noswitchwins +1
    } else {
      switchwins = switchwins +1
    }
  }
  pnoswitchwins=noswitchwins/n
  pswitchwins=switchwins/n
  
  result<-c(pnoswitchwins,pswitchwins)
}

#test for different values of n
N <- 1:20 * 1000
#create a vector to save results
result<- matrix(NA, nrow=20, ncol=2)
for (j in 1:20){
  result[j,] <- MontyHall(N[j])
}

#final step, plot our result
plot(N, result[,1], type= 'b', ylab='p of wins', ylim=c(0,1))
plot(N, result[,2], type= 'b', ylab='p of wins', ylim=c(0,1), col='blue')
