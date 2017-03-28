
require(ggplot2)
load("hteParamSweep.RData")

get.max.bias.Plot <- function(){
    clean.data <- update
    the.data <- data.frame(prob=list(),poll.bias=list())
    for (prob.num in 1:length(clean.data)) {
        caveman <- vector()
        for (seed in 1:ncol(clean.data[[prob.num]]$biasVector)) {
            the.data <- rbind(the.data,
                data.frame(prob=(.05*(prob.num-1)),
                    poll.bias=max(clean.data[[prob.num]]$biasVector[, seed])))
        }
    }
    g <- ggplot(the.data, aes(x=prob, y=poll.bias)) + geom_point(alpha=.1) +
        ylim(-1,1) + geom_smooth(method="loess")
    g <- g + ggtitle("Update Probability") + xlab("Probability") + ylab("Maximum Poll Bias")
    ggsave(filename="updatePlotMax.pdf", plot=g)
}

get.tenth.bias <- function(){
    tenths <- 1:10
    total.enc <- 64*200
    data <- update
    seeds <- c(10,500,10)
    the.data <- data.frame(prob=list(), bias=list())

    for (i in 1:length(tenths)){
        cur.time <- (total.enc/10)* tenths[i]
        for (prob in 1:length(data)){
            values <- c()
            for (seed in 1:ncol(data[[prob]]$biasVector)){ 
                poll.bias <- data[[prob]]$biasVector[,seed]
                current.val <- poll.bias[cur.time]
                values[seed] <- current.val
            }
            the.data <- rbind(the.data, data.frame(prob=data[[prob]]$probability, bias=values))
        }
        a <- ggplot(the.data, aes(x=prob, y=bias)) + geom_point(alpha=.1) + ylim(-1,1) + geom_smooth(method="loess")
        a <- a + ggtitle(paste0("Update Probability:\n",cur.time," encounters")) + xlab("Probability") + ylab("Poll Bias")
        
        switch(i, "1"=ggsave(filename="oneTenth.pdf",plot=a), "2"=ggsave(filename="twoTenth.pdf", plot=a),
            "3"=ggsave("threeTenth.pdf", plot=a), "4"=ggsave("fourTenth.pdf", plot=a), "5"=ggsave("fiveTenth.pdf", plot=a),
            "6"=ggsave("sixTenth.pdf", plot=a), "7"=ggsave("sevenTenth.pdf", plot=a), "8"=ggsave("eightTenth.pdf", plot=a),
            "9"=ggsave("nineTenth.pdf", plot=a), "10"=ggsave("tenTenth.pdf", plot=a), ggsave("error.pdf", plot=a))
   }

}
