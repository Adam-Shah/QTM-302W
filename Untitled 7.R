m = function(w,x) { w*x^2 + (1-w)*sin(pi*x/2) }
p1 = function(x) {x^3}
simulate.data = function(n) {
  X = runif(n)
  W = as.integer(rbinom(n, 1, prob=p1(X)))
  U = rnorm(n)*(1+W)/4
  Y = m(W,X) + U
  data.frame(W=W, X=X, Y=Y)
}
set.seed(0)
sim.data = simulate.data(2000)
xrange = seq(0,1,by=.01)
ggplot() +
  geom_point(aes(x=X, y=Y, color=factor(W)), size=.1, data=sim.data) +
  geom_line(aes(x=c(xrange, xrange),
                y=c(m(0,xrange), m(1,xrange)),
                color=rep(factor(c(0,1)), each=length(xrange))))


target = function(m) {
  integrate(function(x){m(1,x)}, lower = 0, upper = 1)
}
target(m)
