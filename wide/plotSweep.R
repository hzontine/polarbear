#!/usr/bin/env Rscript

# Run as:
# plotSweep.R --args csv_filename

require(ggplot2)
require(dplyr)

capitalize <- function(s) {
    return(paste0(toupper(substring(s,1,1)),substring(s,2)))
}

ITER.CUTOFF <- 50   # Iteration at which to start counting assortativity
PLOT.RELATIVE <- TRUE   # Plot assortativity relative to initial (t=0) value

if (length(commandArgs(TRUE)) == 0) {
    filename <- '/tmp/final_output.csv'
} else {
    filename <- commandArgs(TRUE)[2]
}
results <- tbl_df(read.csv(filename,header=TRUE,stringsAsFactors=FALSE))

# Compute the number of sims per parameter value tuple, for display.
first.accessibility <- as.numeric(results[1,'accessibility'])
first.homophily <- as.numeric(results[1,'homophily'])
n <- nrow(results %>% dplyr::filter(accessibility==first.accessibility,
                               homophily==first.homophily)) / 
    (max(results$iteration) + 1)

n.eo <- length(unique(results$accessibility))
n.h <- length(unique(results$homophily))
if (n.eo > 1 && n.h > 1) {
    stop("Run plot2dSweep.R instead.")
}
if (n.eo > 1) {
    indep.var.name='accessibility'
} else {
    indep.var.name='homophily'
}


results %>% 
    group_by(seed) %>%
    arrange(iteration) %>%
    slice(1) %>% rename(orig.a=assortativity) %>% ungroup -> orig.results

results %>% 
    dplyr::filter(iteration >= ITER.CUTOFF) -> trunc.results

final.results <- inner_join(trunc.results, orig.results, by=c("seed"))

final.results %>%
    select(seed, iteration=iteration.x, accessibility=accessibility.x,
        homophily=homophily.x, assortativity, orig.a) %>%
    group_by(seed) %>%
    # hack -- "min" for homophily and accessibility is just "the one value
    # they all share" since these are all the same value for a given seed
    summarize(homophily=min(homophily), accessibility=min(accessibility),
        mean.a=mean(assortativity),
        orig.a=min(orig.a)) %>% ungroup %>%
    mutate(normalized.mean.a=mean.a-orig.a) -> sum.each.run.results


plots <- list(
    list(dep.var='mean.a',
         dep.label=paste('Mean assortativity',
            ifelse(ITER.CUTOFF > 0,paste('\n(past',ITER.CUTOFF,'iterations)'),
                '')),
         prefix=''),
    list(dep.var='normalized.mean.a',
         dep.label=paste('Normalized mean assortativity',
            ifelse(ITER.CUTOFF > 0,paste('\n(past',ITER.CUTOFF,'iterations)'),
                '')),
        prefix='norm_'),
    list(dep.var='orig.a',dep.label='Assortativity of initial graph',
        prefix='orig_')
)

eog.cmd <- "eog "
for (plot in plots) {
    g <- ggplot(sum.each.run.results, 
        aes_string(x=indep.var.name, group=indep.var.name, fill=indep.var.name,
        y=plot$dep.var)) +
        scale_x_continuous(breaks=unique(results[[indep.var.name]])) +
        ggtitle(paste0('(n=',n,' sims for each param value)')) +
        ylab(plot$dep.label) +
        xlab(capitalize(indep.var.name)) +
        ylim(c(-.4,1)) +
        geom_boxplot(show.legend=FALSE) +
        theme(axis.text.x=element_text(size=8))

    image_name <- str_replace(filename, '.csv', '.png')
    image_name <- file.path(dirname(image_name),
        paste0(plot$prefix,basename(image_name)))
    ggsave(image_name, g)
    eog.cmd <- paste(eog.cmd,image_name)
}

system(eog.cmd)
