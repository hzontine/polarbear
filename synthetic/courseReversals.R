
library(igraph)
source("reproduceLitModels.R")


# Sweep of # of course reversals for n trials of either
# opinion OR hidden and expressed.e
parameter.sweep <- function(n=200, attribute1="Opinion", attribute2="NULL"){
	library(doParallel)	
	registerDoParallel(50)
	
	if(attribute2 == "NULL"){
		result <- matrix(nrow=n, ncol=1)
		colnames(result) <- c("opinion")
		result <<- foreach(trial = 1:n, .combine=rbind) %dopar% {
			graph <- binary.voter(plot=FALSE, num=20, prob=0.3)		
			course <- detect.course.reversal(graph)	
			cat("Trial: ", trial, "  -  ", course, "\n")
			return(course)
		}
	} else{
		result <- matrix(nrow=n, ncol=2)
		colnames(result) <- c("hidden", "expressed")
		result <<- foreach(trial = 1:n, .combine=rbind) %dopar% {
			graph <- hannahModel(num=100, prob=0.3)
			rev <- detect.course.reversal(graph)
			cat("Trial: ", trial, "  -  ", rev[1],"  ", rev[2],"\n")
			return(rev)
		}
	}
	return(result)
}


# For one simluation
detect.course.reversal <- function(graphs){
    num <- vcount(graphs[[1]])

    # do we have two attributes: hidden and expressed?
    if ("hidden" %in% list.vertex.attributes(graphs[[1]]) &&
        "expressed" %in% list.vertex.attributes(graphs[[1]])) {
        
        # find the last value for hidden
        hidden <- sapply(1:num, function(x) get.vertex.attribute(graphs[[length(graphs)]], 
            "hidden", V(graphs[[length(graphs)]])[x]))

        # find the last value for expressed
        expressed <- sapply(1:num, function(y) get.vertex.attribute(graphs[[length(graphs)]],
            "expressed", V(graphs[[length(graphs)]])[y]))


# HIDDEN ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # if the number of agents whose HIDDEN opinion == 0 is 
        # greater than half of the total number of agents
        if(length(which(hidden == 0)) > (num / 2)){ 
            # 0 is the max and 1 is the min
            # what is the maximum percentage 1 ever got to?
            # if any instances of 1 greater than 50%
            max <- 50.0
            for(g in 1:length(graphs)){
                h <- sapply(1:num, function(x) get.vertex.attribute(graphs[[g]], 
                    "hidden", V(graphs[[g]])[x]))
                percentage <- length(which(h == 1)) / num
                if(percentage > max){
                    max <- percentage
                }
            }
            hidden.result <- max
        } else{ # if hidden is split in half evenly
            # should literally never happen bec we terminate after unanimity
            if(length(which(hidden == 0)) == (num / 2)){ 
                hidden.result <- 0
            } else { # if the number of agents whose hidden opinion is 0 is less than
                     # half of the total number of agents (AKA more 1s than 0s)
                     # 1 is the max and 0 is the min
                
                #what is the highest percentage that 0 ever got to?
                max <- 50.0
                for( g in 1:length(graphs)){
                    h <- sapply(1:num, function(x) get.vertex.attribute(graphs[[g]], 
                    "hidden", V(graphs[[g]])[x]))
                    percentage <- length(which(h == 0)) / num
                    if(percentage > max){
                        max <- percentage
                    }
                }
                hidden.result <- max
            }
        }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPRESSED ~~~~~~~~~~~~~~~~~~~~~~~~~

        # if the number of agents whose expressed == 0 is 
        # greater than half of the total number of agents
        if(length(which(expressed == 0)) > (num / 2)){ 
            # 0 is the max and 1 is the min
            max <- 50.0
            # what is the highest percentage that 1 ever got to?
            for(g in 1:length(graphs)){
                ex <- sapply(1:num, function(x) get.vertex.attribute(graphs[[g]], 
                "expressed", V(graphs[[g]])[x]))
                percentage <- length(which(ex == 1)) / num
                if(percentage > max){
                    max <- percentage
                }
            }
            expressed.result <- max
        } else{ # if opinion is split in half evenly
            if(length(which(expressed == 0)) == (num / 2)){ 
                # should literally never happen
                expressed.result <- 0
            } else { # if the number of agents whose opinion is 0 is less than
                     # half of the total number of agents (AKA more 1s than 0s)
                     # 1 is the max and 0 is the min
                # was 0 ever the max?
                max <- 50.0
                # what is the highest percentage that 0 ever got to?
                for (g in 1:length(graphs)){
                    ex <- sapply(1:num, function(x) get.vertex.attribute(graphs[[g]], 
                    "expressed", V(graphs[[g]])[x]))
                    percentage <- length(which(ex == 0)) /num
                    if(percentage > max){
                        max <- percentage
                    }
                }
		        expressed.result <- max
            }
        }
 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        return(c(hidden.result, expressed.result))

    }else{
	op1 <- sapply(1:num, function(x) get.vertex.attribute(graphs[[1]], "opinion",
		V(graphs[[1]])[x]))
	cat("Starting table in CR: \n",table(op1), "\n")


        # find vector of opinion values that represent the last graph
        opinion <- sapply(1:num, function(x) get.vertex.attribute(graphs[[length(graphs)]], 
            "opinion", V(graphs[[length(graphs)]])[x]))
        
        # if the number of agents whose opinion is 0 is greater than
        # half of the total number of agents
        if(length(which(opinion == 0)) > (num / 2)){ 
            # 0 is the max and 1 is the min
            # so we need to find the maximum percentage that 1 ever had?
            max <- 0
            # for each graph in graphs, what is the highest 1 ever got to?
            for(g in 1:length(graphs)){
                op <- sapply(1:num, function(x) get.vertex.attribute(graphs[[g]], 
                "opinion", V(graphs[[g]])[x]))

                percentage <- length(which(op == 1)) / num 
                if(percentage > max){
                    	max <- percentage
                }
            }
            return(max)
        } else { # if opinion is split in half evenly
            if(length(which(opinion == 0)) == (length(opinion) / 2)){ 
                # should never happen
                return (0) 
            } else { # if the number of agents whose opinion is 0 is less than
                     # half of the total number of agents (AKA more 1s than 0s)
                     # 1 is the max and 0 is the min

                    max <- 0
                    # what is the maximum percentage that 0 ever reached?
                    for(g in 1:length(graphs)){
                        op <- sapply(1:num, function(x) get.vertex.attribute(graphs[[g]], 
                            "opinion", V(graphs[[g]])[x]))
                        percentage <- length(which(op == 0)) / num
                        if(percentage > max){
                            	max <- percentage
                        }
                    }
                    return(max)
            }
        }
    }

}






