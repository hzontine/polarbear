
require(ggplot2)

source("munge.R")

labels <- list("neighbor"="neighbor influences node",
               "node"="node influences neighbor")

facet.labeler <- function(variable, value) {
    return(labels[value])
}

p <- ggplot(data, aes(x=selection,y=num.iter,fill=selection)) + 
    facet_wrap(~direction, labeller=facet.labeler) +
    scale_fill_manual(name="Selection method",
                values=c("#841F27","darkgreen")) +
    scale_x_discrete(
        labels=c("with replacement","without replacement")) +
    guides(fill=FALSE) +
#    coord_fixed(ratio=1) +
    ylab("# encounters before convergence") +
    xlab("Selection method / Influence direction") +
    ylim(0,60000) +
    geom_boxplot(notch=TRUE)
ggsave("../CSSSA/interactionBoxplot.pdf",p,width=16,height=9,dpi=120)

p2 <- ggplot(subset(data,direction=="neighbor"), 
        aes(x=selection,y=num.iter,fill=selection)) + 
    scale_fill_manual(name="Selection method",
        values=c("#841F27","darkgreen")) +
    scale_x_discrete(
        labels=c("with replacement","without replacement")) +
    guides(fill=FALSE) +
    ylab("# encounters before convergence") +
    xlab("Selection method") +
    ylim(0,60000) +
    geom_boxplot(notch=TRUE)
pdf("../CSSSA/selectionMethodBoxplot.pdf")
print(p2)
dev.off()

p3 <- ggplot(subset(data,selection=="without"), 
        aes(x=direction,y=num.iter)) +
    scale_x_discrete(labels=
            c("neighbor influences node","node influences neighbor")) +
    ylab("# encounters before convergence") +
    xlab("Influence direction") +
    ylim(0,60000) +
    geom_boxplot(notch=TRUE, fill="darkgreen")
pdf("../CSSSA/influenceDirectionBoxplot.pdf")
print(p3)
dev.off()
