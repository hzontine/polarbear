#!/usr/bin/env Rscript
# Careful -- results are not regenerated if they already exist. (Remove
# sum.each.run.results variable from current environment to regenerate.)

# Run as:
# plotRebalancingSweep.R --args csv_filename

require(ggplot2)
require(dplyr)

capitalize <- function(s) {
    return(paste0(toupper(substring(s,1,1)),substring(s,2)))
}


if (!exists('sum.each.run.results')) {

    ITER.CUTOFF <- 50   # Iteration at which to start counting assortativity

    if (length(commandArgs(TRUE)) == 0) {
        filename <- 'final_output.csv'
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

    num.dim <- 2

    results %>% 
        group_by(seed,rebalancing) %>%
        arrange(iteration) %>%
        slice(1) %>% rename(orig.a=assortativity) %>% ungroup -> orig.results

    results %>% 
        dplyr::filter(iteration >= ITER.CUTOFF) -> trunc.results

    final.results <- inner_join(trunc.results, orig.results, 
        by=c("seed","rebalancing"))

    final.results %>%
        select(seed, rebalancing, iteration=iteration.x, 
            accessibility=accessibility.x, homophily=homophily.x, assortativity, 
            orig.a) %>%
        group_by(seed,rebalancing) %>%
        summarize(homophily=min(homophily), accessibility=min(accessibility),
            mean.a=mean(assortativity),
            orig.a=min(orig.a)) %>% ungroup %>%
        mutate(normalized.mean.a=mean.a-orig.a) -> sum.each.run.results

    sum.each.run.results$accessibility <- 
        as.factor(sum.each.run.results$accessibility)
    sum.each.run.results$rebalancing <- 
        as.factor(sum.each.run.results$rebalancing)

}

g <- ggplot(sum.each.run.results, 
    aes_string(x='accessibility', fill='rebalancing',
        y='normalized.mean.a')) +
    geom_boxplot(show.legend=TRUE, outlier.shape=NA) +
    facet_wrap(~homophily, labeller="label_both") +
    scale_x_discrete(breaks=unique(results[['accessibility']])) +
    scale_fill_manual(name='DynamicRebalancing',values=c("on"="green","off"="royalblue")) +
    ylab("Normalized mean polarization (>= 50 iterations)") +
    xlab(capitalize('accessibility')) +
    ylim(c(-.4,1)) +
    geom_hline(yintercept=0, color="darkgrey") +
    theme(axis.text.x=element_text(size=8),legend.position="bottom") 


image_name <- str_replace(filename, '.csv', '.png')
ggsave(image_name, width=8, height=6, dpi=600, g)

system(paste("eog",image_name))
