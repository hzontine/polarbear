
require(ggplot2)
load("hteParamSweep.RData")

clean.data <- update

the.data <- data.frame(prob=list(),poll.bias=list())
for (prob.num in 1:length(clean.data)) {
    caveman <- vector()
    for (seed in 1:ncol(clean.data[[prob.num]]$biasVector)) {
        the.data <- rbind(the.data,
            data.frame(prob=(.05*(prob.num-1)),
                poll.bias=clean.data[[prob.num]]$biasVector
                    [max(clean.data[[prob.num]]$biasVector),seed]))
    }
}

g <- ggplot(the.data, aes(x=prob, y=poll.bias)) + geom_point(alpha=.1) +
    ylim(-1,1) + geom_smooth(method="loess")
g <- g + ggtitle("Update Probability") + xlab("Probability") + ylab("Maximum Poll Bias")

ggsave(filename="updatePlot.pdf", plot=g)
