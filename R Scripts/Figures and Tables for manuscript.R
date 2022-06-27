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
  ylab("Biomass on bagged branches (g)") +
  xlab("Plant species") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none") +
  geom_signif(comparisons = list(c("Shadbush", "Barberry")), 
              y_position = 0.155,
              tip_length = 0.05,
              annotation = c("See Fig 1b")) +
  geom_signif(y_position = c(0.155), 
              xmin = c(0.9, 6.9), 
              xmax = c(6.1, 10.1),
    annotation = c(" ", " "), tip_length = 0.01)
biomass_plot 

# Fig 1b: Biomass posthoc ####

mod1_posthoc <- read.csv("./Data/Models/model1_posthoc.csv")


# import p-value #
nugget_1 <- "P = 0.089" 

# this needs to use the same mean and SEM as the other figure, it currently does NOT

biomass_posthoc_plot <- ggplot(data=mod1_posthoc, aes(x = Exo, y = response,shape=Exo)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=response -(SE), ymax=response +(SE), width=0)) +
  ylab("Arthropod biomass on bagged branches") +
  xlab("Plant group") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept=0.0,linetype=2) +
  ylim(0.06,0.16) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(0.12), 
              xmin = c(1), 
              xmax = c(2),
              annotation = c(nugget_1), tip_length = 0.01)
biomass_posthoc_plot

# merge 1a and 1b
Fig_1ab <- ggarrange(biomass_plot, biomass_posthoc_plot, labels = c("1A", "1B"), nrow = 1,
                     common.legend = FALSE, widths = c(1.75, 0.5))

ggsave(filename = "./Figures/Fig_1ab.svg", plot = Fig_1ab , device = "svg",
       width = 15, height = 5, units = "in")













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
              xmax = c(6.1, 10.1),
              annotation = c(" ", " "), tip_length = 0.01) +
  geom_hline(yintercept=0.0,linetype=2)  +
  ylim(-0.25,1.5)
lrr_plot

# Fig 2b #####
model_2a_posthoc <- read.csv("./Data/Models/model2_posthoc.csv")

# import p-value #
nugget_1 <- "P = 0.364" 

# this needs to use the same mean and SEM as the other figure, it currently does NOT

lrr_posthoc <- ggplot(data=model_2a_posthoc, aes(x = Exo, y = emmean,shape=Exo)) +
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
# Insect abundances #
# Araneae plot
spider_lsm <- read.csv("./Data/Models/spider_model.csv")

# ! manually set tukey letters ####
spider_lsm$.group <- c("b","a","b","a")

Fig_3a <- ggplot(data=spider_lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = exo)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Araneae count per branch") +
  xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
Fig_3a

# Hemiptera plot

hemiptera_lsm <- read.csv("./Data/Models/hemiptera_model.csv")

#manually set tukey letters
hemiptera_lsm$.group <- c("a","a","a","a")

Fig_3b <- ggplot(data=hemiptera_lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = exo)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Hemiptera count per branch") +
  xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
Fig_3b

# Lepidopetera plot
lepidoptera_lsm <- read.csv("./Data/Models/lepidoptera_model.csv")

lepidoptera_lsm

#manually set tukey letters
lepidoptera_lsm$.group <- c("c","bc","ab","a")

Fig_3c <- ggplot(data=lepidoptera_lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = exo)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Lepidoptera count per branch") +
  xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
Fig_3c

# Orthoptera plot
orthoptera_lsm <- read.csv("./Data/Models/orthoptera_model.csv")

orthoptera_lsm 

#manually set tukey letters
orthoptera_lsm$.group <- c("b","a","b","a")

Fig_3d <- ggplot(data=orthoptera_lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = exo)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Orthoptera count per branch") +
  xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
Fig_3d


# Fig 3 all #####
# arrange figure 2
Fig_3abcd <- ggarrange(Fig_3a, Fig_3b, Fig_3c, Fig_3d,
                       labels = c("A","B","C","D"), 
                       nrow = 2, ncol = 2)
Fig_3abcd 

# write figure 3 to folder
ggsave(filename = "./Figures/Fig3abcd.png", plot = Fig_3abcd, device = "png",
       width = 10, height = 8, units = "in")





# Fig 4 #####
# 4a: Herbivore N % ######
mod7_summary <- read.csv("./Data/Models/model7.csv")
mod7_summary  <- arrange(transform(mod7_summary , tree=factor(tree,levels=biomass_order)), tree) 

HN_plot <- ggplot(data=mod7_summary , aes(x = tree, y = mean_nitrogen, shape=exo)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=mean_nitrogen-(sem), ymax=mean_nitrogen+(sem), width=0)) +
  ylab("% N content of insect herbivores") +
  xlab("Plant species") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none") +
  geom_signif(comparisons = list(c("Shadbush", "Barberry")), 
              y_position = 11.5,
              tip_length = 0.235,
              annotation = c("See Fig. 4b")) +
  geom_signif(y_position = c(11.2), 
              xmin = c(0.9, 6.9), 
              xmax = c(6.1, 10),
              annotation = c(" ", " "), tip_length = 0.01)
HN_plot

# 4b: Herbivore posthoc ####
model_4b_posthoc <- read.csv("./Data/Models/mod7_posthoc.csv")

# import p-value #
nugget_2 <- "P = 0.001" 

# this needs to use the same mean and SEM as the other figure, it currently does NOT

model_4b_plot <- ggplot(data=model_4b_posthoc, aes(x = Exo, y = emmean,shape=Exo)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=emmean -(SE), ymax=emmean +(SE), width=0)) +
  ylab("") +
  xlab("Plant group") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept=0.0,linetype=2) +
  ylim(9.5,11.5) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(10.5), 
              xmin = c(1), 
              xmax = c(2),
              annotation = c(nugget_2), tip_length = 0.01)
model_4b_plot




# Merge 4a4b ####
Fig_4ab <- ggarrange(HN_plot, model_4b_plot, labels = c("4A", "4B"), nrow = 1,
                     common.legend = FALSE, widths = c(1.75, 0.5))

ggsave(filename = "./Figures/Fig_4ab.svg", plot = Fig_4ab , device = "svg",
       width = 15, height = 5, units = "in")






# 4c: Spider N% ####
mod9_summary <- read.csv("./Data/Models/model9.csv")
mod9_summary  <- arrange(transform(mod9_summary , tree=factor(tree,levels=biomass_order)), tree) 

SN_plot <- ggplot(data=mod9_summary , aes(x = tree, y = mean_nitrogen, shape=exo)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=mean_nitrogen-(sem), ymax=mean_nitrogen+(sem), width=0)) +
  ylab("% N content of spiders") +
  xlab("Plant species") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none") +
  geom_signif(comparisons = list(c("Shadbush", "Barberry")), 
              y_position = 11.9,
              tip_length = 0.195,
              annotation = c("See Fig. 4d")) +
  geom_signif(y_position = c(11.7), 
              xmin = c(0.9, 6.9), 
              xmax = c(6.1, 10),
              annotation = c(" ", " "), tip_length = 0.01)
SN_plot





# 4d: Spider posthoc ####
model_4d_posthoc <- read.csv("./Data/Models/mod9_posthoc.csv")

# import p-value #
nugget_3 <- "P = 0.002" 

# now uses model emmeans
model_4d_plot <- ggplot(data=model_4d_posthoc, aes(x = Exo, y = emmean,shape=Exo)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=emmean -(SE), ymax=emmean +(SE), width=0)) +
  ylab("") +
  xlab("Plant group") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_hline(yintercept=0.0,linetype=2) +
  ylim(10.5,12) +
  theme(axis.title.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(11.5), 
              xmin = c(1), 
              xmax = c(2),
              annotation = c(nugget_3), tip_length = 0.01)
model_4d_plot

Fig_4cd <- ggarrange(SN_plot, model_4d_plot, labels = c("4C", "4D"), nrow = 1,
common.legend = FALSE, widths = c(1.75, 0.5))

ggsave(filename = "./Figures/Fig_4cd.svg", plot = Fig_4cd, device = "svg",
       width = 15, height = 5, units = "in")

