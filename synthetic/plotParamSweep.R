
require(ggplot2)
#load("hteParamSweep.RData")
#load("connectedProbData.RData", verbose=TRUE)

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


clean.data <- function(file){
    load(file, verbose=TRUE)
    clean.data <- list(list(probability=data[1]$probability, biasVector=data[2]$biasVector), list(probability=data[3]$probability, biasVector=data[4]$biasVector), list(probability=data[5]$probability, biasVector=data[6]$biasVector), list(probability=data[7]$probability, biasVector=data[8]$biasVector), list(probability=data[9]$probability, biasVector=data[10]$biasVector), list(probability=data[11]$probability, biasVector=data[12]$biasVector), list(probability=data[13]$probability, biasVector=data[14]$biasVector), list(probability=data[15]$probability, biasVector=data[16]$biasVector), list(probability=data[17]$probability, biasVector=data[18]$biasVector), list(probability=data[19]$probability, biasVector=data[20]$biasVector), list(probability=data[21]$probability, biasVector=data[22]$biasVector), list(probability=data[23]$probability, biasVector=data[24]$biasVector), list(probability=data[25]$probability, biasVector=data[26]$biasVector), list(probability=data[27]$probability, biasVector=data[28]$biasVector), list(probability=data[29]$probability, biasVector=data[30]$biasVector), list(probability=data[31]$probability, biasVector=data[32]$biasVector), list(probability=data[33]$probability, biasVector=data[34]$biasVector), list(probability=data[35]$probability, biasVector=data[36]$biasVector), list(probability=data[37]$probability, biasVector=data[38]$biasVector), list(probability=data[39]$probability, biasVector=data[40]$biasVector), list(probability=data[41]$probability, biasVector=data[42]$biasVector))
    save(data, clean.data, file=file)
}

get.tenth.bias <- function(file){
    load(file, verbose=TRUE)
    data <- clean.data
    tenths <- 1:10
    total.enc <- 64*50
    seeds <- c(10,500,10)
    for (i in 1:length(tenths)){
        the.data <- data.frame(prob=list(), bias=list())
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
        a <- ggplot(the.data, aes(x=prob, y=bias)) + geom_smooth(method="loess") + geom_point(alpha=.1) + ylim(-1,1) 
        a <- a + ggtitle(paste0("Probability of Connection between Agents:\n",cur.time," encounters  (",tenths[i]*10,"%)")) + xlab("Probability") + ylab("Poll Bias")
        
        switch(tenths[i], "1"=ggsave(filename="1CON.pdf",plot=a), "2"=ggsave(filename="2CON.pdf", plot=a),
            "3"=ggsave("3CON.pdf", plot=a), "4"=ggsave("4CON.pdf", plot=a), "5"=ggsave("5CON.pdf", plot=a),
            "6"=ggsave("6CON.pdf", plot=a), "7"=ggsave("7CON.pdf", plot=a), "8"=ggsave("8CON.pdf", plot=a),
            "9"=ggsave("9CON.pdf", plot=a), "10"=ggsave("10CON.pdf", plot=a), ggsave("error.pdf", plot=a))
   }

}

get.cumsum.of.bias <- function(file, prob){
    load(file, verbose=TRUE)
    data <- clean.data
    seeds <- c(10,500,10)
    probs <- seq(0,1,0.05)
    poll.bias <- data[[prob]]$biasVector[,1]
    sum <- cumsum(poll.bias)              
    values <- data.frame(bias=sum)
    for (seed in 2:ncol(data[[prob]]$biasVector)){ 
        poll.bias <- data[[prob]]$biasVector[,seed]
        sum <- cumsum(poll.bias)
        sum <- sum / (seq(1,length(sum)))            
        values <- cbind(values, data.frame(bias=sum))
    }
    png(paste0("PP",prob,".png"))
    plot(values[,1], type="l", main=paste0("Probability of being Peer Pressured: ",probs[prob]), xlab="Encounters", ylab="Average Cumulative Sum")
    for(i in 2:ncol(values)){
        lines(values[,i])
    } 
    dev.off()
}
