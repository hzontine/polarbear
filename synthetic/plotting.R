
# Common plotting functions.
# Stephen and Hannah

library(igraph)
library(RColorBrewer)
library(stringr)

#source("fatCircle.R")


plot.hidden.effective.encounters <- function(graphs){
 
  all <- sapply(graphs, function(g) get.graph.attribute(g, "num.current.hidden.encounters"))
  blue <- sapply(graphs, function(g) get.graph.attribute(g, "blue.hidden.effective.encounters"))
  red <- sapply(graphs, function(g) get.graph.attribute(g, "red.hidden.effective.encounters"))
  
  #browser()
  
  # denominator: # current online encounters
  # encounter results in conversion
      # red and blue
  
  
  percentage.blue <- (blue / all) * 100
  percentage.red <- (red / all) * 100
  time.pts <- sapply(graphs, function(g) get.graph.attribute(g, "num.encounters"))
  
  plot(time.pts, percentage.blue, type="l", lwd=2, col="blue", main="Hidden Effective Encounters", xlab="time (iteration)", ylab = "% of hidden effective encounters out of current hidden encounters", ylim=c(0,100))
  lines(time.pts, percentage.red, type="l", lwd=2, col="red")
}

plot.exp.effective.encounters <- function(graphs){
  
  all <- sapply(graphs, function(g) get.graph.attribute(g, "num.current.exp.encounters"))
  blue <- sapply(graphs, function(g) get.graph.attribute(g, "blue.exp.effective.encounters"))
  red <- sapply(graphs, function(g) get.graph.attribute(g, "red.exp.effective.encounters"))
  
  
  # denominator: # current face-to-face encounters
  # effective face-to-face resulting in conversion of exp ---- dotted line
      # red and blue 
  # face-to-face encounter that results in internalization of opinion ---- solid line
      # red and blue 
  
  percentage.blue <- (blue / all) * 100
  percentage.red <- (red / all) * 100
  time.pts <- sapply(graphs, function(g) get.graph.attribute(g, "num.encounters"))
  
  plot(time.pts, percentage.blue, type="l", lwd=2, col="blue", main="Expressed Effective Encounters", xlab="time (iteration)", ylab = "% of expressed effective encounters out of current expressed encounters", ylim=c(0,100))
  lines(time.pts, percentage.red, type="l", lwd=2, col="red")
}


plot.hidden <- function(graphs){
  blue <- sapply(graphs, function(graph) {
    all.hidden <- sapply(1:length(V(graph)), function(v){
      get.vertex.attribute(graph, "hidden", index=v)
    })
    sum.blue <- length(which(all.hidden == 0))
    if(sum.blue == 0){
      NA
    } else{
      x <- sapply(1:length(V(graph)), function(v){
        hidden <- get.vertex.attribute(graph, "hidden", index=v)
        if(hidden == 0){
          exp <- get.vertex.attribute(graph, "expressed", index=v)
          if(exp == 0){
            1
          } else{
            0
          }
        } else{
          0
        }
      })
      trunc(((length(which(x == 1))) / sum.blue) * 100)
    }
  })

  red <- sapply(graphs, function(graph) {
    all.hidden <- sapply(1:length(V(graph)), function(v){
      get.vertex.attribute(graph, "hidden", index=v)
    })
    sum.red <- length(which(all.hidden == 1))
    if(sum.red == 0){
      NA
    } else{
      x <- sapply(1:length(V(graph)), function(v){
        hidden <- get.vertex.attribute(graph, "hidden", index=v)
        if(hidden == 1){
          exp <- get.vertex.attribute(graph, "expressed", index=v)
          if(exp == 1){
            1
          } else{
            0
          }
        } else{
          0
        }
      })  
      trunc(((length(which(x == 1))) / sum.red) * 100)
    }
  })
  
  time.pts <- sapply(graphs, function(g) get.graph.attribute(g, "num.encounters"))
  plot(time.pts, blue, type="l", lwd=2, col="blue", main="(hidden) True Believers", xlab="time (iteration)", ylab = "% of hidden who are also expressed", ylim=c(0,100))
  lines(time.pts, red, type="l", lwd=2, col="red")
  
}

plot.expressed <- function(graphs){

  blue <- sapply(graphs, function(graph) {
    all.exp <- sapply(1:length(V(graph)), function(v){
      get.vertex.attribute(graph, "expressed", index=v)
    })
    sum.blue <- length(which(all.exp == 0))
    if(sum.blue == 0){
      NA
    } else{
      x <- sapply(1:length(V(graph)), function(v){
        exp <- get.vertex.attribute(graph, "expressed", index=v)
        if(exp == 0){
          hidden <- get.vertex.attribute(graph, "hidden", index=v)
          if(hidden == 0){
            1
          } else{
            0
          }
        } else{
          0
        }
      })
      trunc(((length(which(x == 1))) / sum.blue) * 100)
    }
  })
  
  red <- sapply(graphs, function(graph) {
    all.exp <- sapply(1:length(V(graph)), function(v){
      get.vertex.attribute(graph, "expressed", index=v)
    })
    sum.red <- length(which(all.exp == 1))
    if(sum.red == 0){
      NA
    } else{
      x <- sapply(1:length(V(graph)), function(v){
        exp <- get.vertex.attribute(graph, "expressed", index=v)
        if(exp == 1){
          hidden <- get.vertex.attribute(graph, "hidden", index=v)
          if(hidden == 1){
            1
          } else{
            0
          }
        } else{
          0
        }
      })  
      trunc(((length(which(x == 1))) / sum.red) * 100)
      
    }
  })

  time.pts <- sapply(graphs, function(g) get.graph.attribute(g, "num.encounters"))
  plot(time.pts, blue, type="l", lwd=2, col="blue", main="(expressed) True Believers", xlab="time (iteration)", ylab = "% of expressed who are also hidden", ylim=c(0,100))
  lines(time.pts, red, type="l", lwd=2, col="red")
}

plot.polar.graph <- function(graph, legend=c("L","C"), 
    legend.fill=c("blue","red"), 
    vertex.coords=NULL,
    vertex.frame.color="black", main.title="", subtitle="") {

    plot(graph,
        layout=vertex.coords,
        edge.arrow.size=1.5,
        vertex.shape="fcircle",
        vertex.frame.color=vertex.frame.color,
        vertex.frame.width=8,
        vertex.size=15,
        main=main.title, 
        sub=subtitle)

        legend("bottomleft",legend=legend, fill=legend.fill)
}


plot.bias <- function(graphs){
 
  # To keep the plots comparable between runs, impose a consistent, but
  # "soft," scale: make the y scale always end at SOFT.MAX percentage points, 
  # unless the bias actually goes over that, in which case draw a bold red
  # line to make the degree of overflow clear.
  SOFT.MAX <- 35

  # what % of expressed say blue - what % of hidden say blue * 100
  bias <- sapply(graphs, function(graph){
    exp <- sapply(1:length(V(graph)), function(v){
      get.vertex.attribute(graph, "expressed", index=v)
    })
    hidden <- sapply(1:length(V(graph)), function(v){
      get.vertex.attribute(graph, "hidden", index=v)
    })
    percent.exp <- length(which(exp == 0)) / length(exp)
    percent.hidden <- length(which(hidden == 0)) / length(hidden)
    return((percent.exp - percent.hidden)*100)
  })
  time.pts <- sapply(graphs, function(g) get.graph.attribute(g, "num.encounters"))
  plot(time.pts, bias, type="l", lwd=2, main="Poll Bias Over Time", xlab="time (iteration)", ylab = "% expressed blue - % hidden blue", 
    ylim=c(min(bias),max(c(bias,SOFT.MAX))))
  
  if(any(bias < 0)){
    lines(time.pts, rep(0, length(time.pts)), type="l", col="red", lty=2)
  }
  if(any(bias> SOFT.MAX)){
    lines(time.pts, rep(SOFT.MAX, length(time.pts)), type="l", col="red", lwd=2)
  }
  
}


plot.genuine <- function(graphs){
  
  expressed <- sapply(graphs, function(graph) {
    y <- sapply(1:length(V(graph)), function(v){
      get.vertex.attribute(graph, "expressed", index=v)
    })
    sum(y == 0)
  })
  hidden <- sapply(graphs, function(graph) {
    x <- sapply(1:length(V(graph)), function(v){
      get.vertex.attribute(graph, "hidden", index=v)
    })
    sum(x == 0)
  })
  totals <- expressed - hidden
  time.pts <- sapply(graphs, function(g) get.graph.attribute(g, "num.encounters"))
  
  plot(time.pts, totals, type="l", lwd=2, main="Poll % Genuinenss Over Time", xlab="time (iteration)", ylab = "Difference between expressed and hidden")

  if(any(totals < 0)){
    lines(time.pts, rep(0, length(time.pts)), type="l", col="red", lty=2)
  }
}



# Given a list of graphs, plot them, (possibly) ensuring that vertices are
# plotted in the same location from graph to graph.
#
# graphs -- a list of igraph objects as produced by run.polar(),
# hannahModel(), or other functions.
#
# attribute.name -- the name of a vertex attribute whose values should be
# mapped to vertex color.
#
# try.to.keep.vertex.positions -- if TRUE, try to plot each vertex in a
# similar x,y position from frame to frame. If FALSE, layout each frame of the
# animation anew, with no relation to the old.
#
# interactive -- if TRUE, display the animation within R. If FALSE, create an
# animation file in the current directory with the name specified.
#
# delay.between.frames -- the delay, in seconds, between graph displays.
# If NA, will wait for an enter keystroke between frames.
#
# animation.filename -- only relevant if interactive is FALSE.
#
# overwrite.animation.file -- controls whether to overwrite or error out if
# file already exists. Only relevant if interactive is FALSE.
#
# subtitle -- either a character string giving a subtitle for the plot (could
# be empty string for no subtitle), or else the value TRANSCRIPT to use the
# transcript for the upcoming frame, or the value SUMMARY.STATS to use
# "current frame's graph statistics".
#
TRANSCRIPT <- 1
SUMMARY.STATS <- 2
plot.animation <- function(graphs, attribute.name="ideology",
    second.attribute = "none",
    try.to.keep.vertex.positions=TRUE, delay.between.frames=.5, 
    interactive=TRUE, animation.filename="polar.gif",
    overwrite.animation.file=FALSE, subtitle="") {

    if (!interactive && !overwrite.animation.file) {
        if (file.exists(animation.filename)) {
            stop(paste0("File ",animation.filename, " already exists!"))
        }
    }

    if (!interactive) {
        base.filename <- tempfile(pattern="polar")
    }
    if (second.attribute != "none"){
        two.attr = TRUE
    }else{
        two.attr = FALSE
    }

    vertex.shape = "fcircle"

    # Detect discrete graphs so we can plot colors differently.
    first.values <- get.vertex.attribute(graphs[[1]], attribute.name)
    if(two.attr){
        if (all(first.values == floor(first.values))) {
            attr.one.is.discrete <- TRUE
        } else {
            attr.one.is.discrete <- FALSE
        }
        second.values <- get.vertex.attribute(graphs[[1]], second.attribute)
        if (all(second.values == floor(second.values))) {
            attr.two.is.discrete <- TRUE
        } else {
            attr.two.is.discrete <- FALSE
        }
    }else{
        if (all(first.values == floor(first.values))) {
            the.only.attr.is.discrete <- TRUE
        } else {
            the.only.attr.is.discrete <- FALSE
        }
    }


    vertex.coords <- layout_with_kk(graphs[[1]])
    for (i in 1:length(graphs)) {

        if (try.to.keep.vertex.positions) {
            vertex.coords <- layout_with_kk(graphs[[i]],coords=vertex.coords)
        } else {
            vertex.coords <- layout_with_kk(graphs[[i]],coords=NULL)
        }

        # Set the fill color (and label color, for readability).
        if(two.attr && attr.one.is.discrete ||
            !two.attr && the.only.attr.is.discrete) {

            num.distinct.attr.one.vals <- 
                max(get.vertex.attribute(graphs[[1]],attribute.name)) + 1

            if (num.distinct.attr.one.vals == 2) {

                V(graphs[[i]])$color <- ifelse(
                    get.vertex.attribute(graphs[[i]],attribute.name) == 0,
                    "blue","red")
                V(graphs[[i]])$label.color <- ifelse(
                    get.vertex.attribute(graphs[[i]],attribute.name) == 0,
                    "white","black")
                fill <- c("blue","red")
                legend <- c("Liberal","Conservative")

            } else {    # first (discrete) attribute has more than two values

                    if(num.distinct.attr.one.vals > 9){
                        colors <- brewer.pal(num.distinct.attr.one.vals, "Set3")
                    } else {
                        colors <- brewer.pal(num.distinct.attr.one.vals, "Pastel1")
                    }
                    for (node in 1:vcount(graphs[[i]])){ 
                        ideology <- vertex_attr(graphs[[i]],attribute.name,node)
                        for (num in 0:num.distinct.attr.one.vals){
                            if(ideology == num) {
                                V(graphs[[i]])[node]$color <- colors[num+1]
                            }
                        }
                    }
                fill <- ""
                legend <- ""
            }
        }

        # Set the frame color. If only one attribute, just make it black.
        # Otherwise, make it correspond to the second attribute.
        if (!two.attr) {
            vertex.frame.color <- "black"
        }
        if(two.attr && attr.two.is.discrete) {
            if (max(get.vertex.attribute(graphs[[1]],second.attribute)) == 1){
                 # if there is a second attrbute that has exactly two values
                 vertex.frame.color <- ifelse(
                     get.vertex.attribute(graphs[[i]],second.attribute) == 0,
                           "blue","red")
                fill <- c("blue","red")
                legend <- c("Liberal","Conservative")
            } else {
                 stop("second discrete attribute with > 2 vals not supported yet")
            }
        }
        if(two.attr && !attr.two.is.discrete) {
             stop("second continuous attribute not supported yet")
        }


        if (!interactive) {
            png(paste0(base.filename,"plot",
                paste0(rep(0,3-floor(log10(i)+1)),collapse=""), i,".png"))
            cat("Building frame",i,"of",length(graphs),"...\n")
        }


        if (is.character(subtitle)) {
            # The caller is explicitly giving us a subtitle to use. Use it.
            subtitle.to.plot <- subtitle
        } else if (is.numeric(subtitle) && subtitle == TRANSCRIPT) {
            if (i+1<=length(graphs)) {
                message.for.next.frame <- 
                    get.graph.attribute(graphs[[i+1]],"message")
                if (is.null(message.for.next.frame)) {
                    subtitle.to.plot <- "(no changes about to happen)"
                } else {
                    subtitle.to.plot <- paste0("this is about to happen:\n",
                        message.for.next.frame)
                }
            } else {
                subtitle.to.plot <- "last one!"
            }
        } else if (is.numeric(subtitle) && subtitle == SUMMARY.STATS) {
            mat <- compute.confusion.matrix(graphs[[i]])
            total <- sum(mat)
            subtitle.to.plot <- paste0(
                "B(B): ",round(mat[1,1]/total*100,1),"%  ",
                "B(R): ",round(mat[1,2]/total*100,1),"%  ",
                "R(B): ",round(mat[2,1]/total*100,1),"%  ",
                "R(R): ",round(mat[2,2]/total*100,1),"%\n",
                "genuine: ",
                    round((mat[1,1]+mat[2,2])/total*100,1),"%\n",
                "true blue: ",
                    round((mat[1,1]+mat[2,1])/total*100,1),"%  ",
                "true red: ",
                    round((mat[1,2]+mat[2,2])/total*100,1),"%\n")
        } else {
            stop(paste0("Illegal subtitle value ", subtitle, ".\n"))
        }
    
        plot.polar.graph(graphs[[i]],
            legend=legend,
            legend.fill=fill,
            vertex.coords=vertex.coords,
            vertex.frame.color=vertex.frame.color,
            main.title=paste("Iteration",i,"of",length(graphs)),
            subtitle=subtitle.to.plot)

        if (interactive) {
            if (is.na(delay.between.frames)) {
                cat(get.graph.attribute(graphs[[i]],"message"),"\n")
                if (readline("Press ENTER (q to quit) ") == "q") return()
            } else {
                Sys.sleep(delay.between.frames)
            }
        } else {
            dev.off()
        }
    }
    if (!interactive) {
        if (is.na(delay.between.frames)) {
            stop("Can't have NA delay for non-interactive mode.")
        }
        cat("Assembling animation...\n")
        system(paste0("convert -loop 1 -delay ",delay.between.frames*100," ",base.filename,"plot*.png ", animation.filename))
        system(paste0("rm ",base.filename,"plot*.png"))
        cat("Animation in file ",animation.filename,".\n",sep="")
    }
}


# Plot the polarization vs. time for the list of graphs passed.
#
# graphs -- a list of igraph objects, presumably created from run.polar().
#
# attribute.name -- the name of a vertex attribute whose assortativity is to
# be computed and plotted.
#
plot.polarization <- function(graphs, attribute1="hidden", attribute2="expressed") {
    assortativities1 <- sapply(graphs, function(graph) {
        assortativity(graph,
            types1=get.vertex.attribute(graph,attribute1))
    })
    assortativities2 <- sapply(graphs, function(graph) {
      assortativity(graph,
                    types1=get.vertex.attribute(graph,attribute2))
    })
    
    time.pts <- sapply(graphs, function(g) get.graph.attribute(g, "num.encounters"))

    plot(time.pts,assortativities1, type="l",ylim=c(-1,1),
        main="Polarization over time", xlab="time (iteration)",
        ylab=paste("Assortativity of opinion"), lwd=2)
    lines(time.pts,assortativities2, lty="dashed", lwd=2)
    legend("bottomleft",legend=c(attribute1,attribute2),
           lty=c("solid","dashed"))
}



# Given a list of igraph objects representing an evolving graph, plot the 
# fraction of vertices with opinion 1 over time.
plot.binary.opinions <- function(graphs, attribute1="opinion", attribute2="none") {


    if(attribute2 != "none"){
        attr1 <- function(graph, op){
            sum(get.vertex.attribute(graph, attribute1) == op) / gorder(graph)
        }
        attr2 <- function(graph, op){
            sum(get.vertex.attribute(graph, attribute2) == op) / gorder(graph)
        }
        frac.0s <- sapply(graphs, attr1, op=0)
        frac2.0s <- sapply(graphs, attr2, op=0)
        frac.1s <- sapply(graphs, attr1, op=1)
        frac2.1s <- sapply(graphs, attr2, op=1)

        time.pts <- sapply(graphs, function(g) get.graph.attribute(g, "num.encounters"))

        plot(time.pts,frac.0s,ylim=c(0,1),type="l",col="blue",
            xlab="time (encounters)", ylab="% of Agents",
            lwd=2, main="Opinion Change over time")
        lines(time.pts, frac2.1s, col="red", lty="dotted", lwd=2)
        lines(time.pts, frac.1s, col="red", lwd=2)
        lines(time.pts, frac2.0s, col="blue", lty="dotted", lwd=2)

        legend("topleft",legend=c("Liberal","Conservative"),
            fill=c("blue","red"))
        legend("bottomleft",legend=c(attribute1,attribute2),
            lty=c("solid","dotted"))

    } else {
        attr <- function(graph, op){
            sum(get.vertex.attribute(graph, attribute1) == op) / gorder(graph)
        }
        frac.1s <- sapply(graphs, attr, op=1)
        frac.0s <- sapply(graphs, attr, op=0)
        time.pts <- sapply(graphs, function(g) get.graph.attribute(g, "num.encounters"))

        plot(time.pts,frac.0s,ylim=c(0,1),type="l", lwd=2, col="blue",
            xlab="time (encounters)")
        lines(time.pts, frac.1s, col="red")
    }
}

