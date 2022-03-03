# ----------------------------------------------------------------------
# Information
# ----------------------------------------------------------------------

# simulating and illustrating two alternative case, using the example
# of two textbooks


# ----------------------------------------------------------------------
# Load libraries
# ----------------------------------------------------------------------

# set the directory if using the script interactively, 
# below you see example of my path to the folder
# setwd("/home/hstojic/research/project/gap_popularity/econ_paper/")

# house keeping
rm(list = ls())

# load libraries and functions
source(file.path(".", "libraries.R"))
source(file.path(".", "utils.R"))

# directories
dirOut <- file.path(".")


# ----------------------------------------------------------------------
# Simulation functions
# ----------------------------------------------------------------------


popularityWalk = function(
    num_steps = 10000, q1_1, q2_1, q_tie
) {
    x1 <- x2 <- rep(0, num_steps)

    for (i in 2:num_steps) {
        r <- runif(1)
        x1t <- x1[i-1]
        x2t <- x2[i-1]
        if (((x1t==x2t) & (r<q_tie)) | ((x1t>x2t) & (r<q1_1)) | ((x1t<x2t) & (r<q2_1))) {
            x1[i] <- x1t + 1
            x2[i] <- x2t
        } else {
            x1[i] <- x1t
            x2[i] <- x2t + 1
        }
    }
    diff <- x1 - x2
    return(diff)
}


simulatePopularity <- function(
    num_sim, num_steps, q1_1, q2_1, q_tie, seed = 1
) {
    set.seed(seed)
    results <- vector("list", num_sim)
    for (i in 1:num_sim) {
        diffs <- popularityWalk(num_steps, q1_1, q2_1, q_tie)
        results[[i]] <- data.frame(sim=i, diff=diffs)
    }
    out <- bind_rows(results)
    return(out)
}


# ----------------------------------------------------------------------
# Simulations
# ----------------------------------------------------------------------

q_tie <- 0.55
q1_1 <- 0.7
q2_1 <- 0.4  # Probability of choosing 1 when 2 is ahead
q1_2 <- 1-q1_1  # Probability of choosing 2 when 1 is ahead
q2_2 <- 1-q2_1

num_steps <- 150
num_sim <- 100
seed <- 1234

results <- simulatePopularity(
    num_sim, num_steps, q1_1, q2_1, q_tie, seed
) 


# ----------------------------------------------------------------------
# Figure
# ----------------------------------------------------------------------

# proportion gets to theoretical 68% for about 10000 simulations
# but for 100 simulations used here we get 66%
prop <- results %>% 
    group_by(sim) %>% 
    mutate(step = 1:n()) %>%
    filter(step == n()) %>%
    mutate(bin = ifelse(diff < 0, 0, 1)) %>%
    ungroup() %>%
    summarise(bin_mean = mean(bin))

# zoomed in figure with types of walks singled out
figure <- 
    ggplot(
        results %>% 
            group_by(sim) %>% 
            mutate(step = 1:n()), 
        aes(x=step, y=diff, group=sim)) +
    geom_hline(
        aes(yintercept=0),
        colour="black", size=lineSize, linetype=3
    ) + 
    geom_line(size=lineSize, alpha=0.1, colour="grey") +
    geom_line(
        data=results %>% 
            group_by(sim) %>% 
            mutate(step = 1:n()) %>% 
            filter(sim %in% c(1,12,20,48)),
        aes(x=step, y=diff, group=sim, linetype=factor(sim), color=factor(sim)),
        size=lineSize*1.5
    ) +
    scale_color_manual("",
        values = cbPalette[c(4,8,8,4)]
    ) + 
    scale_linetype_manual("",
        values = c(2,1,2,1)
    ) + 
    scale_x_continuous(
        "Agent number (n)", limits=c(0,30), expand=c(0,1)
    ) + 
    scale_y_continuous(
        expression(Z[n](a)-Z[n](b)*", Choice difference"),
        limits=c(-15,20)
    ) + 
    mytheme +
    theme(
        legend.position = "none"
    )
figure_name <- paste("two_alt_popularity_walk_30")
saveFig(figure, dirOut, figure_name, fd_1c_1x1)


# zoomed out figures, showing two clear groups, one bigger and one smaller
figure <- 
    ggplot(results %>% 
        group_by(sim) %>% mutate(step=1:n()), 
        aes(x=step, y=diff, group=sim)) +
    geom_hline(
        aes(yintercept=0),
        colour="black", size=lineSize, linetype=3
    ) + 
    geom_abline(aes(intercept=0, slope=0.4), colour="black", size=lineSize) + 
    geom_abline(aes(intercept=0, slope=-0.2), colour="black", size=lineSize) + 
    geom_line(size=lineSize, alpha=0.2, color="grey40") +
    geom_line(
        data=results %>% 
            group_by(sim) %>% 
            mutate(step = 1:n()) %>% 
            filter(sim %in% c(1,12,20,48)),
        aes(x=step, y=diff, group=sim, linetype=factor(sim), color=factor(sim)),
        size=lineSize*1.5
    ) +
    scale_color_manual("",
        values = cbPalette[c(4,8,8,4)]
    ) + 
    scale_linetype_manual("",
        values = c(2,1,2,1)
    ) + 
    scale_x_continuous(
        "Agent number (n)", limits=c(0,150), expand=c(0,1)
    ) + 
    scale_y_continuous(
        expression(Z[n](a)-Z[n](b)*", Choice difference"),
        limits=c(-60,80)
    ) + 
    annotate(
        "text", x = 35, y = 65, label = "italic(P) == 0.68", parse=TRUE,
        size=fontSize, family=fontSetup
    ) +
    annotate(
        "text", x = 35, y = -50, label = "italic(P) == 0.32", parse=TRUE,
        size=fontSize, family=fontSetup
    ) +
    mytheme +
    theme(
        legend.position = "none"
    )
figure_name <- paste("two_alt_popularity_walk_150")
saveFig(figure, dirOut, figure_name, fd_1c_1x1)


# zoomed out figures, showing two clear groups, one bigger and one smaller
figure <- 
    ggplot(results %>% 
        group_by(sim) %>% 
        mutate(
            step=1:n(),
            temp=ifelse(step==n() & diff>0, 1, 0),
            positive=max(temp)
        ), 
        aes(x=step, y=diff, group=sim)) +
    geom_hline(
        aes(yintercept=0),
        colour="black", size=lineSize, linetype=3
    ) + 
    geom_line(
        aes(colour=factor(positive)),
        size=lineSize, alpha=0.1,
    ) +
    scale_color_manual("",
        values = c("0"=cbPalette[8], "1"=cbPalette[4])
    ) + 
    geom_abline(
        aes(intercept=0, slope=0.4), colour=cbPalette[4], size=lineSize
    ) + 
    geom_abline(
        aes(intercept=0, slope=-0.2), colour=cbPalette[8], size=lineSize
    ) + 
    scale_x_continuous(
        "Agent number (n)", limits=c(0,150), expand=c(0,1)
    ) + 
    scale_y_continuous(
        expression(Z[n](a)-Z[n](b)*", Choice difference"),
        limits=c(-60,80)
    ) + 
    annotate(
        "text", x = 35, y = 65, label = "italic(P) == 0.68", parse=TRUE,
        size=fontSize, family=fontSetup
    ) +
    annotate(
        "text", x = 35, y = -50, label = "italic(P) == 0.32", parse=TRUE,
        size=fontSize, family=fontSetup
    ) +
    mytheme +
    theme(
        legend.position = "none"
    )
figure_name <- paste("two_alt_popularity_walk_150_coloured")
saveFig(figure, dirOut, figure_name, fd_1c_1x1)
