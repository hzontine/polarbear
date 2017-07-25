#!/usr/bin/env Rscript

# Plot the assortativity over time for a single simulation run.
# Run as:
# plotSingle.R --args csv_filename

require(ggplot2)

filename <- commandArgs(TRUE)[2]
results <- read.csv(filename,header=TRUE)

g <- ggplot(results, aes(x=iteration, y=assortativity)) + 
    geom_point() + 
    geom_smooth(method="loess")
image_name <- str_replace(filename, '.csv', '.png')
ggsave(image_name, g)
system(paste("eog",image_name))
