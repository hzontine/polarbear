#!/usr/bin/env Rscript

# Plot the assortativity over time for a suite of simulations.
# Run as:
# plotSuite.R --args csv_filename

require(ggplot2)

filename <- commandArgs(TRUE)[2]
results <- read.csv(filename,header=TRUE,stringsAsFactors=FALSE)

g <- ggplot(results, aes(col=seed, x=iteration, y=assortativity)) + 
    geom_line(show.legend=FALSE) + 
    ylim(-.7,.7) +
    geom_hline(yintercept=0, color="darkgrey")
image_name <- str_replace(filename, '.csv', '.png')
ggsave(image_name, g)
system(paste("eog",image_name))
