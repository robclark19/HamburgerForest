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

# Plant arrangment rule ######
# Arrange in order of native vs exotic
biomass_order <- c("Beech","Musclewood","Shadbush","Striped Maple", "Sweet Birch", "Witch-hazel", "Autumn Olive", "Barberry", "Burning Bush", "Honeysuckle")



# Fig 1a: Bagged biomass ####
biomass_summary <- read.csv("./Data/Models/model1.csv")

biomass_summary <- arrange(transform(biomass_summary, tree=factor(tree,levels=biomass_order)), tree) 

biomass_summary

biomass_plot <- ggplot(data=biomass_summary, aes(x = tree, y = response, shape=exo)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Arthropod biomass on bagged branches (g)") +
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

# Fig 1b: Biomass posthoc ####







# Fig 2 ver 1: Bag effect ######
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

# export Fig 2 ver 1 #








# Fig 2a ver 2: Bag lRR #####
mod2a_lsm <- read.csv("./Data/Models/model2a.csv")

mod2a_lsm <- arrange(transform(mod2a_lsm, tree=factor(tree,levels=biomass_order)), tree) 

mod2a_lsm

lrr_plot <- ggplot(data=mod2a_lsm, aes(x = tree, y = LRR_mean, shape=exo)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=LRR_mean-(LRR_sem), ymax=LRR_mean+(LRR_sem), width=0)) +
  ylab("Bird effect on biomass (LRR)") +
  xlab("Plant species") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = c(0.075,0.3)) +
  geom_signif(comparisons = list(c("Shadbush", "Barberry")), 
              y_position = 1.35,
              tip_length = 0.05,
              annotation = c("See Fig 2b")) +
  geom_signif(y_position = c(1.35), 
              xmin = c(0.9, 6.9), 
              xmax = c(6.1, 10),
              annotation = c(" ", " "), tip_length = 0.01) +
  geom_hline(yintercept=0.0,linetype=2)  +
  ylim(-0.25,1.5)
lrr_plot

# Fig 2b #####
model_2a_posthoc <- read.csv("./Data/Models/model2a_posthoc.csv")

# import p-value #
nugget_1 <- "P = 0.364" 

# this needs to use the same mean and SEM as the other figure, it currently does NOT

lrr_posthoc <- ggplot(data=model_2a_posthoc, aes(x = exo, y = emmean,shape=exo)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=emmean -(SE), ymax=emmean +(SE), width=0)) +
  ylab("Bird effect on biomass (LRR)") +
  xlab("Plant group") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept=0.0,linetype=2) +
  ylim(-0.25,1.5) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(1), 
              xmin = c(1), 
              xmax = c(2),
              annotation = c(nugget_1), tip_length = 0.01)
lrr_posthoc

# merge 2a and 2b
Fig_2ab <- ggarrange(lrr_plot, lrr_posthoc , labels = c("2A", "2B"), nrow = 1,
                     common.legend = FALSE, widths = c(1.75, 0.5))

ggsave(filename = "./Figures/Fig_2ab.svg", plot = Fig_2ab , device = "svg",
              width = 15, height = 5, units = "in")



# Fig 3abcd #####
# Insect abundances #####










# Fig 4 #####
# 4a: Herbivore N% ######
# write csv for summary table to use in figure generation
mod7_summary <- read.csv("./Data/Models/model7.csv")
mod7_summary  <- arrange(transform(mod7_summary , tree=factor(tree,levels=biomass_order)), tree) 

HN_plot <- ggplot(data=mod7_summary , aes(x = tree, y = mean_nitrogen, shape=exo)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=mean_nitrogen-(sem), ymax=mean_nitrogen+(sem), width=0)) +
  ylab("Nitrogen content of insect herbivores (% mass)") +
  xlab("Plant species") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="bottom") +
  geom_signif(comparisons = list(c("Shadbush", "Barberry")), 
              y_position = 11.5,
              tip_length = 0.235,
              annotation = c("P = 0.002")) +
  geom_signif(y_position = c(11.2), 
              xmin = c(0.9, 6.9), 
              xmax = c(6.1, 10),
              annotation = c(" ", " "), tip_length = 0.01)
HN_plot

# 4b: Herbivore posthoc ####

# 4c: Spider N% ####

# 4d: Spider posthoc ####
