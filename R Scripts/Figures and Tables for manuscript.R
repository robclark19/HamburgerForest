# Import data and models for table & figure generation only

# Libraries ####
library("tidyverse")
library("readr")
library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("ggpubr")
library("plotrix") # for std.error function

# Fig 1a: Bagged branch biomass ####





# Fig 1b: Bag effect ######
# Import model estimates and table
mod2_lsm <- read.csv("./Data/Models/model2.csv")

Fig_1b <- ggplot(data=mod2_lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = tree)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Average arthropod biomass (g)") +
  xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ tree, nrow=2) 
Fig_1b

# export Fig 1b #




# Fig 2abcd #####
# Insect abundances #####