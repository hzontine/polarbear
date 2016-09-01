
require(ggplot2)

source("munge.R")


p <- ggplot(data, aes(x=selection,y=num.iter,fill=selection)) + 
    facet_wrap(~direction) +
    geom_boxplot(notch=TRUE)
print(p)

p2 <- ggplot(subset(data,direction=="neighbor"), 
        aes(x=selection,y=num.iter,fill=selection)) + 
    scale_fill_discrete(name="Selection method",
                labels=c("with replacement","without replacement")) +
    ylab("# of iterations to convergence") +
    geom_boxplot(notch=TRUE)
print(p2)

