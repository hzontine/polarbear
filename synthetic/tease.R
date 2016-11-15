
require(igraph)

load("variables.RData")

data.var.names <- grep("data\\.",ls(),value=TRUE)

for (data.var.name in data.var.names) {
    data.var <- get(data.var.name)
    num.iters.to.converge <- sapply(data.var, function(sim.run.results) {
        sim.run.results[[1]] })
    cat("There were ", length(which(is.infinite(num.iters.to.converge))), 
    " infinites.\n",sep="")
    hidden.course.reversals <- sapply(data.var, function(sim.run.results) {
        sim.run.results[[2]][1] })
    expressed.course.reversals <- sapply(data.var, function(sim.run.results) {
        sim.run.results[[2]][2] })

    hist(num.iters.to.converge,breaks=50)
    print(summary(num.iters.to.converge))
    readline("Press enter.")

    hist(hidden.course.reversals,breaks=50)
    print(summary(hidden.course.reversals))
    readline("Press enter.")

    hist(expressed.course.reversals,breaks=50)
    print(summary(expressed.course.reversals))
    readline("Press enter.")
}
