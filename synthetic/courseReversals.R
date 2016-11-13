
library(igraph)
source("reproduceLitModels.R")
source("hannahModel.R")



# Calculate percent genuineness over time where expressed = hidden
# Returns a list of confusion matricies representing the state of
# the graph at one instance of time. The rows represent an agent's
# expressed opinion. The columns represent an agent's hidden opinion.
#	  	(hidden)
#	  blue=0	red=1
# blue=0 			(expressed)
# red=1

calculate.genuineness <- function(time.stamps, graph){
	list.of.confusion.matricies <- list()
	for(i in 1:length(time.stamps)){
		a <- matrix(nrow=2, ncol=2)
		rownames(a) <- c("E- blue", "E- red")
		colnames(a) <- c("H- blue", "H- red")
		list.of.confusion.matricies[[i]] <- a		
	}
	num <- vcount(graph[[1]])
	for(i in 1:length(time.stamps)){
		cat("Starting time.stamps[",i,"]\n") 
		blue.blue <- 0
		blue.red <- 0
		red.blue <- 0
		red.red <- 0
		hidden.results <- sapply(1:num, function(x) 
			get.vertex.attribute(graph[[time.stamps[i]]],
			"hidden", V(graph[[time.stamps[i]]])[x]))
		expressed.results <- sapply(1:num, function(x) 
			get.vertex.attribute(graph[[time.stamps[i]]],
			"expressed", V(graph[[time.stamps[i]]])[x]))
		for(j in 1:num){
			if(hidden.results[j] == 0 && expressed.results[j] == 0){
				blue.blue <- blue.blue+1
			}else{
				if(hidden.results[j] == 1 && expressed.results[j] == 1){
					red.red <- red.red+1
				} else {
					if(hidden.results[j] == 0){
						red.blue <- red.blue+1
					} else {
						if(hidden.results[j] == 1){
							blue.red <- blue.red+1
						} else {
							cat("we should never get here.....\n")	
						}
					}
				}
			}
		}
		all <- blue.blue+blue.red+red.blue+red.red 
		if(all == num){
			list.of.confusion.matricies[[i]]["E- blue","H- blue"] <- blue.blue
			list.of.confusion.matricies[[i]]["E- blue","H- red"] <- blue.red
			list.of.confusion.matricies[[i]]["E- red","H- blue"] <- red.blue
			list.of.confusion.matricies[[i]]["E- red","H- red"] <- red.red
		} else {
			cat("Only did ", all/num,"% of the nodes. I smell a bug....\n")
		}
	}
	return(list.of.confusion.matricies)
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
			graph <- binary.voter(plot=FALSE, num=num.nodes, prob=0.35, num.enc=num.nodes*2000)		
			course <- detect.course.reversal(graph)	
			cat("Trial: ", trial, "  -  ", course, "\n")
			cat("#",trial, "took this long: ", length(graph), "\n")
			return(course)
		}
	} else{
		result <- foreach(trial = 1:n, .combine=c) %dopar% {
			num.nodes <- 20
			graph <- hannahModel(num=num.nodes, prob=0.3, num.enc=num.nodes*1000)
			course.reversal <- detect.course.reversal(graph)
			cat("Trial: ", trial, "  -  ", course.reversal[1],"  ",
				course.reversal[2],"\n")
			cat("#",trial, "took this long: ", length(graph), "\n")
			confusion.matricies <- calculate.genuineness(
				time.stamps=c((length(graph)/3),(2*(length(graph)/3)),
				length(graph)), graph)
				#), graph=graph)
			return(list(list(length(graph), course.reversal, confusion.matricies)))
		}
		#save(result, all.the.matricies, file="variables.RData")
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






