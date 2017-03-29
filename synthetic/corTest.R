

load("hteParamSweep.RData", verbose=TRUE)



run.cor.test <- function(probability="internalize"){
    switch(probability, "internalize"=data<-internalize,
        "peer pressure"=data<-peer.pressure, "update"=data<-update,
        "connected"=data<-connected)
    cat(paste0(probability, " probability\n"))
    x <- c()
    y <- c()
    for(i in 1:length(data)){
        x <- c(x, rep(data[[i]]$probability, length(data[[i]]$biasVector)))
        y <- c(y, data[[i]]$biasVector)
    }
    cor.test(x,y)
}


