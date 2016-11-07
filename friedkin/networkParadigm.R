
# Network paradigm, p.194
# Super flexible: all coefficients and scaling factors differ per time period.

use.scaling <- FALSE

# number of agents
n <- 6

# number of exogenous variables
k <- 3

# how long to run simulation
num.iter <- 100


# time-varying matrix of coefficient of effects of agents on each other
W <- array(runif(n*n*num.iter,min=-1,max=1),dim=c(n,n,num.iter))

# time-varying matrix of exogenous scores
X <- array(runif(n*k*num.iter,min=-1,max=1),dim=c(n,k,num.iter))

# time-varying matrix of exogenous coefficients
B <- matrix(runif(k*1*num.iter,min=-1,max=1),nrow=k)

# scaling factors (variable over time)
if (use.scaling) {
    alpha <- runif(num.iter)
    beta <- runif(num.iter)
} else {
    alpha <- rep(.5,num.iter)
    beta <- rep(.5,num.iter)
}

# initial condition
Y.init <- X[,,1] %*% B[,1]

Y <- matrix(0,nrow=n,ncol=num.iter)
Y[,1] <- Y.init


for (i in 2:num.iter) {
    Y[,i] <- alpha[i] * W[,,i] %*% Y[,i-1] + beta[i] * X[,,i] %*% B[,i]
}

colors <- c("red","blue","green","orange","purple","yellow")

plot(1:num.iter,Y[1,],type="n",ylim=c(min(Y),max(Y)))
for (i in 1:n) {
    lines(1:num.iter,Y[i,],type="l",col=colors[i])
}
