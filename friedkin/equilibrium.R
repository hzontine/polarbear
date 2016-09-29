
# Equilibrium mode, p.193

# number of agents
n <- 10

# number of exogenous variables
k <- 5

# how long to run simulation
num.iter <- 100


# matrix of coefficient of effects of agents on each other
W <- matrix(runif(n*n,min=-1,max=1),nrow=n)

# matrix of exogenous scores
X <- matrix(runif(n*k,min=-1,max=1),nrow=n)

# matrix of exogenous coefficients
B <- runif(k*1,min=-1,max=1)

# residual matrix
U <- runif(n*1,min=-1,max=1)

# initial condition
Y.init <- runif(n*1,min=-1,max=1)

Y <- matrix(0,nrow=n,ncol=num.iter)
Y[,1] <- Y.init


for (i in 2:num.iter) {
    Y[,i] <- W %*% Y[,i-1] + X %*% B + U
}

plot(1:num.iter,Y[1,],type="n",ylim=c(min(Y),max(Y)))
for (i in 1:n) {
    lines(1:num.iter,Y[i,],type="l")
}
