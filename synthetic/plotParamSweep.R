
require(ggplot2)
load("hteParamSweep.RData")
load("internProbData.RData", verbose=TRUE)

get.max.bias.Plot <- function(){
    clean.data <- data
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
    total.enc <- 64*50
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
        a <- ggplot(the.data, aes(x=prob, y=bias)) + geom_point(alpha=.1) + ylim(-1,1) 
        a <- a + ggtitle(paste0("Internalize Probability:\n",cur.time," encounters  (",tenths[i]*10,"%)")) + xlab("Probability") + ylab("Poll Bias")
        
        switch(tenths[i], "1"=ggsave(filename="1INT.pdf",plot=a), "2"=ggsave(filename="2INT.pdf", plot=a),
            "3"=ggsave("3INT.pdf", plot=a), "4"=ggsave("4INT.pdf", plot=a), "5"=ggsave("5INT.pdf", plot=a),
            "6"=ggsave("6INT.pdf", plot=a), "7"=ggsave("7INT.pdf", plot=a), "8"=ggsave("8INT.pdf", plot=a),
            "9"=ggsave("9INT.pdf", plot=a), "10"=ggsave("10INT.pdf", plot=a), ggsave("error.pdf", plot=a))
   }

}
