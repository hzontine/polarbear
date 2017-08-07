#!/usr/bin/env Rscript
# Just plot info about the initial graphs (before any BVM process begins).

# Run as:
# plotInit.R --args csv_filename

require(ggplot2)
require(dplyr)

if (length(commandArgs(TRUE)) == 0) {
    filename <- '/tmp/final_output.csv'
} else {
    filename <- commandArgs(TRUE)[2]
}

# Get rid of all lines that don't have iteration 0, and put them in a file
# with ".zero" extension.
system(paste0("head -1 ",filename," > ",filename,".zero"))
system(paste0("egrep '^[0-9]+,0,' ",filename," >> ",filename,".zero"))

results <- tbl_df(read.csv(paste0(filename,".zero"),header=TRUE,
    stringsAsFactors=FALSE))

# (Should be unnecessary based on egrep, above.)
results <- results %>% dplyr::filter(iteration == 0)

# Compute the number of sims per parameter value tuple, for display.
first.accessibility <- as.numeric(results[1,'accessibility'])
first.homophily <- as.numeric(results[1,'homophily'])
n <- nrow(results %>% dplyr::filter(accessibility==first.accessibility,
                               homophily==first.homophily)) / 
    (max(results$iteration) + 1)

results %>% 
    group_by(seed) -> sum.each.run.results

g <- ggplot(sum.each.run.results, 
    aes(x=accessibility, group=accessibility, fill=accessibility, 
        y=assortativity)) + 
    facet_wrap(~homophily, labeller="label_both") +
    ggtitle(paste0('(n=',n,' sims for each param value)')) +
    xlab('Accessibility') +
    ylab(expression('Assortativity of' ~ bold(initial) ~ 'graph')) +
    ylim(c(-.2,1)) +
    geom_boxplot(show.legend=FALSE) +
    scale_x_continuous(breaks=unique(results$accessibility)) +
    theme(axis.text.x=element_text(size=9))
image_name <- str_replace(paste0(filename,".zero"), '.csv.zero', '.zero.png')
ggsave(image_name, g)
system(paste("eog",image_name))
