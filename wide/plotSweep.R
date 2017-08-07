#!/usr/bin/env Rscript

# Run as:
# plotSweep.R --args csv_filename

require(ggplot2)
require(dplyr)

ITER.CUTOFF <- 50   # Iteration at which to start counting assortativity
PLOT.RELATIVE <- TRUE   # Plot assortativity relative to initial (t=0) value

if (length(commandArgs(TRUE)) == 0) {
    filename <- '/tmp/final_output.csv'
} else {
    filename <- commandArgs(TRUE)[2]
}
results <- tbl_df(read.csv(filename,header=TRUE,stringsAsFactors=FALSE))

# Compute the number of sims per parameter value tuple, for display.
first.env_openness <- as.numeric(results[1,'env_openness'])
first.homophily <- as.numeric(results[1,'homophily'])
n <- nrow(results %>% dplyr::filter(env_openness==first.env_openness,
                               homophily==first.homophily)) / 
    (max(results$iteration) + 1)

n.eo <- length(unique(results$env_openness))
n.h <- length(unique(results$homophily))
if (n.eo > 1 && n.h > 1) {
    stop("Run plot2dSweep.R instead.")
}
if (n.eo > 1) {
    indep.var.name='env_openness'
    indep.var.label='Environmental openness'
} else {
    indep.var.name='homophily'
    indep.var.label='Homophily'
}


results %>% 
    group_by(seed) %>%
    arrange(iteration) %>%
    slice(1) %>% rename(orig.a=assortativity) %>% ungroup -> orig.results

results %>% 
    dplyr::filter(iteration >= ITER.CUTOFF) -> trunc.results

final.results <- inner_join(trunc.results, orig.results, by=c("seed"))

final.results %>%
    select(seed, iteration=iteration.x, env_openness=env_openness.x,
        homophily=homophily.x, assortativity, orig.a) %>%
    group_by(seed) %>%
    # hack -- "min" for homophily and env_openness is just "the one value
    # they all share" since these are all the same value for a given seed
    summarize(homophily=min(homophily), env_openness=min(env_openness),
        mean.a=mean(assortativity),
        orig.a=min(orig.a)) %>% ungroup %>%
    mutate(normalized.mean.a=mean.a-orig.a) -> sum.each.run.results


plots <- list(
    list(dep.var='mean.a',dep.label='Mean assortativity',prefix=''),
    list(dep.var='normalized.mean.a',dep.label='Normalized mean assortativity',
        prefix='norm_'),
    list(dep.var='orig.a',dep.label='Original assortativity',prefix='orig_')
)

eog.cmd <- "eog "
for (plot in plots) {
    g <- ggplot(sum.each.run.results, 
        aes_string(x=indep.var.name, group=indep.var.name, fill=indep.var.name,
        y=plot$dep.var)) +
        scale_x_continuous(breaks=unique(results[[indep.var.name]])) +
        ggtitle(paste0('(n=',n,' sims for each param value)')) +
        ylab(paste(plot$dep.label,
            ifelse(ITER.CUTOFF > 0,paste('\n(past',ITER.CUTOFF,'iterations)'),
                ''))) +
        xlab(indep.var.label) +
        ylim(c(-.4,1)) +
        geom_boxplot(show.legend=FALSE) +
        theme(axis.text.x=element_text(size=9))

    image_name <- str_replace(filename, '.csv', '.png')
    image_name <- file.path(dirname(image_name),
        paste0(plot$prefix,basename(image_name)))
    ggsave(image_name, g)
    eog.cmd <- paste(eog.cmd,image_name)
}

system(eog.cmd)
