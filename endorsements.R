#A script to create a data frame and plot to track out-of-state endorsements
#in the 2020 Democratic presidential primary.
#
#My goal is try to get at the point at which there begins to be a national
#groundswell around a candidate, given that a large percentage of early
#endorsements come from within a candidate's state.
#
#Created by Joseph Scheidt

library(tidyverse)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(scales)


endorsements <- read_csv("endorsements-2020.csv")

#Set endorser category factor levels based on points
endorsements <- endorsements %>%
    arrange(points) %>%
    mutate(category = factor(category, levels = unique(category)))

candidates <- read_csv("candidates.csv")

endorse <- left_join(endorsements, candidates, by = c("endorsee" = "candidate"))

out_of_state <- endorse %>%
    rename(position = position.x, endorsee_position = position.y,
           state = state.x, endorsee_state = state.y) %>%
    select(date, state, endorser, endorsee, 
           endorsee_state, category, points) %>%
    filter(!is.na(endorsee) & state != endorsee_state) %>%
    group_by(endorsee) %>%
    mutate(total_points = sum(points)) %>%
    ungroup() %>%
    arrange(total_points) %>%
    mutate(endorsee = factor(endorsee, levels = unique(endorsee)))

png(filename = "outofstate.png", width = 600, height = 335)

ggplot(out_of_state, aes(x = endorsee, y = points, 
                         fill = category)) +
    geom_col() +
    scale_fill_brewer(palette = "Blues") +
    theme_classic() +
    theme(axis.title.y=element_blank()) +
    coord_flip() +
    scale_y_continuous(breaks = pretty_breaks()) +
    theme(legend.position = c(0.8, 0.35)) +
    ggtitle("Out-of-State Endorsements",
            subtitle = "2020 Democratic Primary")
    
dev.off()
