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
library("ggsignif")

# Fig 1a: Bagged branch biomass ####
biomass_summary <- read.csv("./Data/Models/model1.csv")

# Arrange in order of native vs exotic
biomass_order <- c("Beech","Musclewood","Shadbush","Striped Maple", "Sweet Birch", "Witch-hazel", "Autumn Olive", "Barberry", "Burning Bush", "Honeysuckle")
biomass_summary <- arrange(transform(biomass_summary, tree=factor(tree,levels=biomass_order)), tree) 



biomass_plot <- ggplot(data=biomass_summary, aes(x = tree, y = response, shape=exo)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Average arthropod biomass on bagged branches (g)") +
  xlab("Plant species") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom") +
  geom_signif(comparisons = list(c("Shadbush", "Barberry")), 
              y_position = 0.12,
              tip_length = 0.21,
              annotation = c("***")) +
  geom_signif(y_position = c(0.11), 
              xmin = c(0.9, 6.9), 
              xmax = c(6.1, 10),
    annotation = c(" ", " "), tip_length = 0.01)
biomass_plot 




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