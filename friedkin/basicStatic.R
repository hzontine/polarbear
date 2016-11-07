
# "Basic model" for static conditions, p.196-7
# All coefficients and scaling factors fixed over entire simulation.

# number of agents
n <- 6

# number of exogenous variables
k <- 3

# how long to run simulation
num.iter <- 100


# constant matrix of coefficient of effects of agents on each other
#W <- matrix(runif(n*n,min=-1,max=1),nrow=n)


# Let's make each column sum to 1 for "proportionate impact of influential
# opinions on individuals' opinions."
#for (i in 1:n) {
#    W[,i] <- W[,i] / sum(W[,i])
#}

# Make W symmetric for real eigenvals
#W[lower.tri(W)] <- t(W)[lower.tri(W)]

# constant matrix of exogenous scores
X <- matrix(runif(n*k,min=-1,max=1),nrow=n)

# constant matrix of exogenous coefficients
B <- runif(k*1,min=-1,max=1)

cat("Dave's norm is: ", norm(X %*% B,"I"),"\n")
# scaling factors (constant over time)
alpha <- runif(1)
beta <- 1 - alpha

par(mfrow=c(2,3))
for (sim in 1:6) {

    # initial condition
    Y.init <- X %*% B
    Y.init <- rep(0,n)
    Y.init <- runif(n,min=-1,max=1)

    Y <- matrix(0,nrow=n,ncol=num.iter)
    Y[,1] <- Y.init


    for (i in 2:num.iter) {
        Y[,i] <- alpha * W %*% Y[,i-1] + X %*% B
    }

    colors <- c("red","blue","green","orange","purple","yellow")
    #plot(1:num.iter,Y[1,],type="n",ylim=c(min(Y),max(Y)))
    plot(1:num.iter,Y[1,],type="n",ylim=c(-2,2))
    for (i in 1:n) {
        lines(1:num.iter,Y[i,],type="l",col=colors[i])
    }
}


# Verifying p.197 (6)  ???
Y.inf <- solve(diag(n) - alpha * W) %*% (beta * Y.init)
Y.inf2 <- solve(diag(n) - alpha * W) %*% (beta * X %*% B)
# compare to Y[,num.iter]. But totally different.
