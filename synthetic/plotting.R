
# Common plotting functions.
# Stephen and Hannah

library(igraph)
library(RColorBrewer)
library(stringr)

source("fatCircle.R")

plot.polar.graph <- function(graph, legend, legend.fill, vertex.coords,
    vertex.frame.color, main.title="", subtitle="") {

    plot(graph,
        layout=vertex.coords,
        edge.arrow.size=1.5,
        vertex.shape="fcircle",
        vertex.frame.color=vertex.frame.color,
        vertex.frame.width=8,
        vertex.size=15,
        main=main.title, 
        sub=subtitle)

        legend("bottomright",legend=legend, fill=legend.fill)
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
# subtitle -- an optional subtitle for the plot.
#
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


        message.for.next.frame <- 
            get.graph.attribute(graphs[[i+1]],"message")

        plot.polar.graph(graphs[[i]],
            legend=legend,
            legend.fill=fill,
            vertex.coords=vertex.coords,
            vertex.frame.color=vertex.frame.color,
            main.title=paste("Iteration",i,"of",length(graphs)),
            subtitle=ifelse(nchar(subtitle)==0 && i<length(graphs),
                ifelse(is.null(message.for.next.frame),
                    "(no changes about to happen)", 
                    paste0("this is about to happen:\n",
                        message.for.next.frame)),
                subtitle))

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
        system(paste0("convert -delay ",delay.between.frames*100," ",base.filename,"plot*.png ", animation.filename))
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
plot.polarization <- function(graphs, attribute.name="ideology") {
    assortativities <- sapply(graphs, function(graph) {
        assortativity(graph,
            types1=get.vertex.attribute(graph,attribute.name))
    })
    time.pts <- sapply(graphs, function(g) get.graph.attribute(g, "num.encounters"))

    plot(time.pts,assortativities, type="l",ylim=c(-1,1),
        main="Polarization over time", xlab="time (iteration)",
        ylab=paste("Assortativity of",attribute.name))
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
            xlab="time (encounters)", lwd=3)
        lines(time.pts, frac2.1s, col="red", lty="dashed", lwd=3)
        lines(time.pts, frac.1s, col="red", lwd=3)
        lines(time.pts, frac2.0s, col="blue", lty="dashed", lwd=3)

        legend("topleft",legend=c("Liberal","Conservative"),
            fill=c("blue","red"))
        legend("bottomleft",legend=c(attribute1,attribute2),
            lty=c("solid","dashed"))

    } else {
        attr <- function(graph, op){
            sum(get.vertex.attribute(graph, attribute1) == op) / gorder(graph)
        }
        frac.1s <- sapply(graphs, attr, op=1)
        frac.0s <- sapply(graphs, attr, op=0)
        time.pts <- sapply(graphs, function(g) get.graph.attribute(g, "num.encounters"))

        plot(time.pts,frac.0s,ylim=c(0,1),type="l",col="blue",
            xlab="time (encounters)")
        lines(time.pts, frac.1s, col="red")
    }
}

