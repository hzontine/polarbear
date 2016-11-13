
library(igraph)
source("reproduceLitModels.R")
source("hannahModel.R")



# Calculate percent genuineness over time where expressed = hidden
# Returns a list of confusion matrices representing the state of
# the graph at one instance of time. The rows represent an agent's
# expressed opinion. The columns represent an agent's hidden opinion.
#	  	(hidden)
#	  blue=0	red=1
# blue=0 			(expressed)
# red=1

calculate.genuineness <- function(time.stamps, graphs){
	list.of.confusion.matrices <- list()
	num <- vcount(graphs[[1]])
	for(i in 1:length(time.stamps)){
		cat("Starting time.stamps[",i,"]\n") 
        list.of.confusion.matrices[[i]] <- 
            compute.confusion.matrix(graphs[[time.stamps[i]]])
	}
	return(list.of.confusion.matrices)
}

# Sweep of # of course reversals for n trials of either
# opinion OR hidden and expressed.e
parameter.sweep <- function(n=200, attribute1="Opinion", attribute2="NULL"){
	library(doParallel)	
	registerDoParallel(60)
	
	if(attribute2 == "NULL"){
		result <- matrix(nrow=n, ncol=1)
		colnames(result) <- c("opinion")
		result <<- foreach(trial = 1:n, .combine=rbind) %dopar% {
			num.nodes <- 50
			graphs <- binary.voter(plot=FALSE, num=num.nodes, prob=0.35, num.enc=num.nodes*2000)		
			course <- detect.course.reversal(graphs)	
			cat("Trial: ", trial, "  -  ", course, "\n")
			cat("#",trial, "took this long: ", length(graphs), "\n")
			return(course)
		}
	} else{
		result <- foreach(trial = 1:n, .combine=c) %dopar% {
			num.nodes <- 20
			graphs <- hannahModel(num=num.nodes, prob=0.3, num.enc=num.nodes*1000)
			course.reversal <- detect.course.reversal(graphs)
			#cat("Trial: ", trial, "  -  ", course.reversal[1],"  ",
				#course.reversal[2],"\n")
			#cat("#",trial, "took this long: ", length(graphs), "\n")
			confusion.matrices <- calculate.genuineness(
				time.stamps=c(length(graphs)#/3),(2*(length(graph)/3)),
				#length(graph)), graph)
				), graphs=graphs)
			return(list(list(course.reversal, confusion.matrices)))
		}
		#save(result, all.the.matrices, file="variables.RData")
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
	cat("starting hidden....\n")
	# If it never converges
	if(length(unique(hidden)) > 1){
		hidden.result <- Inf
		cat("Unique: ", unique(hidden), "\n")
	}else{
	        # if the number of agents whose HIDDEN opinion == 0 is 
        	# greater than half of the total number of agents
        	if(length(which(hidden == 0)) > (num / 2)){ 
		    # 0 is the max and 1 is the min
		    # what is the maximum percentage 1 ever got to?
		    # if any instances of 1 greater than 50%
		    max <- 0
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
			max <- 0
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
	}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	cat("starting expressed....\n")
# EXPRESSED ~~~~~~~~~~~~~~~~~~~~~~~~~
	if(length(unique(expressed)) > 1){
		expressed.result <- Inf
		cat("Unique: ", unique(expressed), "\n")
	} else{
		# if the number of agents whose expressed == 0 is 
		# greater than half of the total number of agents
		if(length(which(expressed == 0)) > (num / 2)){ 
		    # 0 is the max and 1 is the min
		    max <- 0
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
			max <- 0
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
	}
	 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        return(c(hidden.result, expressed.result))

    }else{

        # find vector of opinion values that represent the last graph
        opinion <- sapply(1:num, function(x) get.vertex.attribute(graphs[[length(graphs)]], 
            "opinion", V(graphs[[length(graphs)]])[x]))

	if(length(unique(opinion)) > 1){
		cat("Unique: ", unique(opinion), "\n")
		return(Inf)
	}else{        
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
}






