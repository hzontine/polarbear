
require(ggplot2)
load("peerPressureParamSweep.RData")

the.data <- data.frame(prob=list(),poll.bias=list())
for (prob.num in 1:length(clean.data)) {
    caveman <- vector()
    for (seed in 1:ncol(clean.data[[prob.num]])) {
        the.data <- rbind(the.data,
            data.frame(prob=(.1*(prob.num-1)),
                poll.bias=clean.data[[prob.num]]
                    [nrow(clean.data[[prob.num]]),seed]))
    }
}

g <- ggplot(the.data, aes(x=prob, y=poll.bias)) + geom_point(alpha=.1) +
    ylim(-1,1) + geom_smooth(method="loess")
print(g)
