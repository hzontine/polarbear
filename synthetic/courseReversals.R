
library(igraph)
source("reproduceLitModels.R")




detect.course.reversals <- function(graphs){
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
            # was 1 ever the max?
            g <- 1
            hidden.result <- FALSE
            # for each graph in graphs, what is the highest 1 ever got to?
            while (g <= length(graphs)){
                hh <- sapply(1:num, function(x) get.vertex.attribute(graphs[[g]], 
                "hidden", V(graphs[[g]])[x]))
                if(length(which(hh == 1)) > (num / 2)){
                    # then there used to be more 1s than 0s at graphs[[g]] 

                    # so it counts as a course reversal if at SOME point
                    # there were more 1s than 0s but it ended up that there 
                    # were actually more 0s than 1s.
                    hidden.result <- TRUE
                    break
                    # g represents the first instance 
                }
                g <- g + 1
            }
            # if no iteration of the graph ever had more 1s than 0s
            # so it's not a course reversal
            # then hidden.result will not change so it remains false
        } else{ # if hidden is split in half evenly

            if(length(which(hidden == 0)) == (num / 2)){ 
                # definitely not a course reversal               
                hidden.result <- FALSE 
            } else { # if the number of agents whose hidden opinion is 0 is less than
                     # half of the total number of agents (AKA more 1s than 0s)
                     # 1 is the max and 0 is the min
                
                # was 0 ever the max?
                # for each graph in graphs, what is the highest 0 ever got to?
                hidden.result <- FALSE
                g <- 1
                while (g <= length(graphs)){
                    hh <- sapply(1:num, function(x) get.vertex.attribute(graphs[[g]], 
                    "hidden", V(graphs[[g]])[x]))
                    if(length(which(hh == 0)) > (num / 2)){
                        # then there used to be more 0s than 1s at graphs[[g]] 

                        # so it counts as a course reversal if at SOME point
                        # there were more 1s than 1s but it ended up that there 
                        # were actually more 1s than 0s.
                        hidden.result <- TRUE
                        break
                    }
                    g <- g + 1
                }
                # if no iteration of the graph ever had more 0s than 1s
                # so it's not a course reversal
            }
        }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# EXPRESSED ~~~~~~~~~~~~~~~~~~~~~~~~~

        # if the number of agents whose expressed == 0 is 
        # greater than half of the total number of agents
        if(length(which(expressed == 0)) > (num / 2)){ 
            # 0 is the max and 1 is the min
            # was 1 ever the max?
            expressed.result <- FALSE
            g <- 1
            # for each graph in graphs, what is the highest 1 ever got to?
            while (g <= length(graphs)){
                ex <- sapply(1:num, function(x) get.vertex.attribute(graphs[[g]], 
                "expressed", V(graphs[[g]])[x]))
                if(length(which(ex == 1)) > (num / 2)){
                    # then there used to be more 1s than 0s at graphs[[g]] 

                    # so it counts as a course reversal if at SOME point
                    # there were more 1s than 0s but it ended up that there 
                    # were actually more 0s than 1s.
                    expressed.result <- TRUE
                    break
                    # g represents the first instance 
                }
                g <- g + 1
            }
            # if no iteration of the graph ever had more 1s than 0s
            # so it's not a course reversal

        } else{ # if opinion is split in half evenly
            if(length(which(expressed == 0)) == (num / 2)){ 
                # definitely not a course reversal               
                expressed.result <- FALSE
            } else { # if the number of agents whose opinion is 0 is less than
                     # half of the total number of agents (AKA more 1s than 0s)
                     # 1 is the max and 0 is the min
                # was 0 ever the max?
                g <- 1
                expressed.result <- FALSE
                # for each graph in graphs, what is the highest 0 ever got to?
                while (g <= length(graphs)){
                    ex <- sapply(1:num, function(x) get.vertex.attribute(graphs[[g]], 
                    "expressed", V(graphs[[g]])[x]))
                    if(length(which(ex == 0)) > (num / 2)){
                        # then there used to be more 0s than 1s at graphs[[g]] 
                        # so it counts as a course reversal if at SOME point
                        # there were more 1s than 1s but it ended up that there 
                        # were actually more 1s than 0s.
                        expressed.result <- TRUE 
                        break
                    }
                    g <- g + 1
                }
                # if no iteration of the graph ever had more 0s than 1s
                # so it's not a course reversal
            }
        }
 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        return(c(hidden.result, expressed.result))

    }else{

        # find vector of opinion values that represent the last graph
        opinion <- sapply(1:num, function(x) get.vertex.attribute(graphs[[length(graphs)]], 
            "opinion", V(graphs[[length(graphs)]])[x]))
        
        # if the number of agents whose opinion is 0 is greater than
        # half of the total number of agents
        if(length(which(opinion == 0)) > (num / 2)){ 
            # 0 is the max and 1 is the min
            # was 1 ever the max?

            # for each graph in graphs, what is the highest 1 ever got to?
            for(g in 1:length(graphs)){
                op <- sapply(1:num, function(x) get.vertex.attribute(graphs[[g]], 
                "opinion", V(graphs[[g]])[x]))
                if(length(which(op == 1)) > (num / 2)){
                    # then there used to be more 1s than 0s at graphs[[g]] 
        
                    # so it counts as a course reversal if at SOME point
                    # there were more 1s than 0s but it ended up that there 
                    # were actually more 0s than 1s.
                    return(TRUE)
                    # g represents the first instance 
                }
            }
            # if no iteration of the graph ever had more 1s than 0s
            # so it's not a course reversal
            return(FALSE)

        } else{ # if opinion is split in half evenly
            if(length(which(opinion == 0)) == (length(opinion) / 2)){ 
                # definitely not a course reversal               
                return (FALSE) 
            } else { # if the number of agents whose opinion is 0 is less than
                     # half of the total number of agents (AKA more 1s than 0s)
                     # 1 is the max and 0 is the min
                
                # was 0 ever the max?
                # for each graph in graphs, what is the highest 0 ever got to?
                for(g in 1:length(graphs)){
                    op <- sapply(1:num, function(x) get.vertex.attribute(graphs[[g]], 
                    "opinion", V(graphs[[g]])[x]))
                    if(length(which(op == 0)) > (num / 2)){
                        # then there used to be more 0s than 1s at graphs[[g]] 

                        # so it counts as a course reversal if at SOME point
                        # there were more 1s than 1s but it ended up that there 
                        # were actually more 1s than 0s.
                        return(TRUE) 
                    }
                }
                # if no iteration of the graph ever had more 0s than 1s
                # so it's not a course reversal
                return(FALSE)
            }
        }
    }
}








