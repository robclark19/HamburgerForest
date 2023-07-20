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
library("patchwork")

# problem solving links
# https://stackoverflow.com/questions/20041136/avoid-ggplot-sorting-the-x-axis-while-plotting-geom-bar
# https://stackoverflow.com/questions/11889625/annotating-text-on-individual-facet-in-ggplot2

# Plant arrangement rule ######
# Arrange in order of native vs exotic
biomass_order <- c("Beech","Musclewood","Shadbush","Striped Maple", "Sweet Birch", "Witch-hazel", "Autumn Olive", "Barberry", "Burning Bush", "Honeysuckle")

# plant letter codes #####
# do we need to move sweet birch and shadbush to keep things in alphabetical order?
plant_short = c("Beech" = "BE",
                "Musclewood" = "MW",
                "Shadbush" = "SH",
                "Striped Maple" = "SM",
                "Sweet Birch" = "SB",
                "Witch-hazel" = "WH",
                "Autumn Olive" = "AO",
                "Barberry" = "BA",
                "Burning Bush" = "BU",
                "Honeysuckle" = "HS")

# Fig 1a: Bagged biomass ####
biomass_summary <- read.csv("./Data/Models/model1.csv")

biomass_summary <- arrange(transform(biomass_summary, tree=factor(tree,levels=biomass_order)), tree) 

biomass_summary

# great, now this uses actual mean and SEM, not parameter estimates

biomass_plot <- ggplot(data=biomass_summary, aes(x = tree, y = biomass_mean, shape=exo)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=biomass_mean-(sem), ymax=biomass_mean+(sem), width=0)) +
  ylab("Arthropod biomass (g)") +
  xlab("Plant species") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none") +
  # geom_signif(comparisons = list(c("Shadbush", "Barberry")), 
  #             y_position = 0.167,
  #             tip_length = 0.05,
  #             annotation = c("Planned contrast groups")) +
  # geom_signif(y_position = c(0.167), 
  #             xmin = c(0.9, 6.9), 
  #             xmax = c(6.1, 10.1),
  # annotation = c(" ", " "), tip_length = 0.01) +
  scale_x_discrete(labels=plant_short) +
  ylim(0.06,0.175)
biomass_plot 

# Fig 1b: Biomass posthoc ####

mod1_posthoc <- read.csv("./Data/Models/model1_posthoc.csv")

b1 = ggplot(data=mod1_posthoc %>% filter(exo %in% c("Autumn Olive","Native")), aes(x = exo, y = biomass_mean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=biomass_mean -(sem), ymax=biomass_mean +(sem), width=0)) +
  labs(subtitle="A.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylim(0.06,0.175) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(0.13),
  xmin = c(1),
  xmax = c(2),
  annotation = c("P = 0.27"), tip_length = 0.02)
b1

b2 = ggplot(data=mod1_posthoc %>% filter(exo %in% c("Barberry","Native")), aes(x = exo, y = biomass_mean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=biomass_mean -(sem), ymax=biomass_mean +(sem), width=0)) +
  labs(subtitle="B.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylim(0.06,0.175) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(0.13),
  xmin = c(1),
  xmax = c(2),
  annotation = c("P = 0.21"), tip_length = 0.02)
b2

b3 = ggplot(data=mod1_posthoc %>% filter(exo %in% c("Burning Bush","Native")), aes(x = exo, y = biomass_mean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=biomass_mean -(sem), ymax=biomass_mean +(sem), width=0)) +
  labs(subtitle="C.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylim(0.06,0.175) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(0.13),
  xmin = c(1),
  xmax = c(2),
  annotation = c("P = 0.24"), tip_length = 0.02)
b3

b4 = ggplot(data=mod1_posthoc %>% filter(exo %in% c("Honeysuckle","Native")), aes(x = exo, y = biomass_mean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=biomass_mean -(sem), ymax=biomass_mean +(sem), width=0)) +
  labs(subtitle="D.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ylim(0.06,0.175) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(0.16),
  xmin = c(1),
  xmax = c(2),
  annotation = c("P = 0.56"), tip_length = 0.02)
b4

b_ag = (b1|b2)/(b3|b4)

b_fin = wrap_elements(panel=b_ag) +
labs(tag = "Arthropod biomass on bagged branches") +
theme(
plot.tag = element_text(size = rel(1.5), angle = 90),
plot.tag.position = "left"
)

ggsave(filename = "./Figures/Fig_1_4p.png", plot = b_fin , device = "png",
width = 10, height = 10, units = "in", scale = 0.9)


# merge 1a and 1b
ggsave(filename = "./Figures/Fig_S4.png", plot = biomass_plot , device = "png",
       width = 8, height = 4, units = "in", scale = 0.9)













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
  theme(legend.position="none") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # geom_signif(comparisons = list(c("Shadbush", "Barberry")), 
  #             y_position = 1.35,
  #             tip_length = 0.05,
  #             annotation = c("Planned contrast groups")) +
  # geom_signif(y_position = c(1.35), 
  #             xmin = c(0.9, 6.9), 
  #             xmax = c(6.1, 10.1),
  #             annotation = c(" ", " "), tip_length = 0.01) +
  geom_hline(yintercept=0.0,linetype=2)  +
  scale_x_discrete(labels=plant_short) +
  ylim(-0.25,1.5)
lrr_plot

# Fig 2b #####
model_2a_posthoc <- read.csv("./Data/Models/model2_posthoc.csv")


b1 = ggplot(data=model_2a_posthoc %>% filter(Exo %in% c("Autumn Olive","Native")), aes(x = Exo, y = emmean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=emmean -(SE), ymax=emmean +(SE), width=0)) +
  labs(subtitle="A.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # geom_hline(yintercept=0.0,linetype=2) +
  ylim(-0.25,1.5) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(1),
              xmin = c(1),
              xmax = c(2),
              annotation = c("P = 0.99"), tip_length = 0.01)
b1

b2 =ggplot(data=model_2a_posthoc %>% filter(Exo %in% c("Barberry","Native")), aes(x = Exo, y = emmean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=emmean -(SE), ymax=emmean +(SE), width=0)) +
  labs(subtitle="B.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # geom_hline(yintercept=0.0,linetype=2) +
  ylim(-0.25,1.5) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(1),
              xmin = c(1),
              xmax = c(2),
              annotation = c("P = 0.38"), tip_length = 0.01)
b2

b3 = ggplot(data=model_2a_posthoc %>% filter(Exo %in% c("Burning Bush","Native")), aes(x = Exo, y = emmean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=emmean -(SE), ymax=emmean +(SE), width=0)) +
  labs(subtitle="C.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # geom_hline(yintercept=0.0,linetype=2) +
  ylim(-0.25,1.5) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(1),
              xmin = c(1),
              xmax = c(2),
              annotation = c("P = 0.94"), tip_length = 0.01)
b3

b4 = ggplot(data=model_2a_posthoc %>% filter(Exo %in% c("Honeysuckle","Native")), aes(x = Exo, y = emmean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=emmean -(SE), ymax=emmean +(SE), width=0)) +
  labs(subtitle="D.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # geom_hline(yintercept=0.0,linetype=2) +
  ylim(-0.25,1.5) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(1),
              xmin = c(1),
              xmax = c(2),
              annotation = c("P = 0.99"), tip_length = 0.01)
b4

b_ag = (b1|b2)/(b3|b4)

b_fin = wrap_elements(panel=b_ag) +
  labs(tag = "Bird effect on biomass (LRR)") +
  theme(
    plot.tag = element_text(size = rel(1.5), angle = 90),
    plot.tag.position = "left"
  )

ggsave(filename = "./Figures/Fig_2_4p.png", plot = b_fin , device = "png",
       width = 10, height = 10, units = "in", scale = 0.9)



# merge 2a and 2b
# Fig_2ab <- ggarrange(lrr_plot, lrr_posthoc , labels = c("", ""), nrow = 1,
#                      common.legend = FALSE, widths = c(1.75, 0.5))

ggsave(filename = "./Figures/Fig_2.png", plot = lrr_plot , device = "png",
       width = 8, height = 4, units = "in", scale = 0.9)




# Fig S3 #####
# Now supplemental figure not in manuscript

# Insect abundances #
# Araneae plot
spider_lsm <- read.csv("./Data/Models/spider_model.csv") %>%
  mutate(exo = str_replace(exo,'Non-native','Invasive'))

# ! manually set tukey letters ####
# this is pairwise within treatment only
spider_lsm$.group2 <- c("b","a","b","a")

Fig_3a <- ggplot(data=spider_lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = exo)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Araneae # per branch") +
  theme(axis.title.x=element_blank()) +
  #xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group2, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
Fig_3a

# Hemiptera plot

hemiptera_lsm <- read.csv("./Data/Models/hemiptera_model.csv") %>%
  mutate(exo = str_replace(exo,'Non-native','Invasive'))

#manually set tukey letters
hemiptera_lsm$.group2 <- c("a","a","a","a")

Fig_3b <- ggplot(data=hemiptera_lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = exo)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Hemiptera # per branch") +
  theme(axis.title.x=element_blank()) +
  #xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group2, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
Fig_3b

# Lepidopetera plot
lepidoptera_lsm <- read.csv("./Data/Models/lepidoptera_model.csv") %>%
  mutate(exo = str_replace(exo,'Non-native','Invasive'))

lepidoptera_lsm

#manually set tukey letters
lepidoptera_lsm$.group2 <- c("b","a","b","a")

Fig_3c <- ggplot(data=lepidoptera_lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = exo)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Lepidoptera # per branch") +
  theme(axis.title.x=element_blank()) +
  #xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group2, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
Fig_3c

# Orthoptera plot
orthoptera_lsm <- read.csv("./Data/Models/orthoptera_model.csv") %>%
  mutate(exo = str_replace(exo,'Non-native','Invasive'))

orthoptera_lsm 

#manually set tukey letters
orthoptera_lsm$.group2 <- c("b","a","b","a")

Fig_3d <- ggplot(data=orthoptera_lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = exo)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Orthoptera # per branch") +
  theme(axis.title.x=element_blank()) +
  #xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group2, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
Fig_3d


# Fig S3 all #####
Fig_3abcd <- ggarrange(Fig_3a, Fig_3b, Fig_3c, Fig_3d,
                       labels = c("A","B","C","D"), 
                       nrow = 2, ncol = 2)
Fig_3abcd 

# write figure 3 to folder
ggsave(filename = "./Figures/FigS3.png", plot = Fig_3abcd, device = "png",
       width = 6, height = 5, units = "in")





# Fig 3 #####
# 4a: Herbivore N % ######
mod7_summary <- read.csv("./Data/Models/model7.csv")
mod7_summary  <- arrange(transform(mod7_summary , tree=factor(tree,levels=biomass_order)), tree) 

HN_plot <- ggplot(data=mod7_summary , aes(x = tree, y = mean_nitrogen, shape=exo)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=mean_nitrogen-(sem), ymax=mean_nitrogen+(sem), width=0)) +
  ylab("% N content of herbivores") +
  xlab("Plant species") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none") +
  # geom_signif(comparisons = list(c("Shadbush", "Barberry")), 
  #             y_position = 11.5,
  #             tip_length = 0.235,
  #             annotation = c("Planned contrast groups")) +
  # geom_signif(y_position = c(11.2), 
  #             xmin = c(0.9, 6.9), 
  #             xmax = c(6.1, 10),
  #             annotation = c(" ", " "), tip_length = 0.01) +
  scale_x_discrete(labels=plant_short) +
  ylim(9.25,11.75)
HN_plot

# 3b: Herbivore posthoc ####
model_4b_posthoc <- read.csv("./Data/Models/mod7_posthoc.csv")

b1 = ggplot(data=model_4b_posthoc %>% filter(Exo %in% c("Autumn Olive","Native")), aes(x = Exo, y = emmean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, width=0)) +
  labs(subtitle="A.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # geom_hline(yintercept=0.0,linetype=2) +
  ylim(9,12) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(11),
              xmin = c(1),
              xmax = c(2),
              annotation = c("P = 0.19"), tip_length = 0.01)
b1

b2 =ggplot(data=model_4b_posthoc %>% filter(Exo %in% c("Barberry","Native")), aes(x = Exo, y = emmean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, width=0)) +
  labs(subtitle="B.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # geom_hline(yintercept=0.0,linetype=2) +
  ylim(9,12) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(11),
              xmin = c(1),
              xmax = c(2),
              annotation = c("P = 0.99"), tip_length = 0.01)
b2

b3 = ggplot(data=model_4b_posthoc %>% filter(Exo %in% c("Burning Bush","Native")), aes(x = Exo, y = emmean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, width=0)) +
  labs(subtitle="C.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # geom_hline(yintercept=0.0,linetype=2) +
  ylim(9,12) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(11),
              xmin = c(1),
              xmax = c(2),
              annotation = c("P = 0.88"), tip_length = 0.01)
b3

b4 = ggplot(data=model_4b_posthoc %>% filter(Exo %in% c("Honeysuckle","Native")), aes(x = Exo, y = emmean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, width=0)) +
  labs(subtitle="D.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # geom_hline(yintercept=0.0,linetype=2) +
  ylim(9,12) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(11.5),
              xmin = c(1),
              xmax = c(2),
              annotation = c("P < 0.0001"), tip_length = 0.01)
b4

b_ag = (b1|b2)/(b3|b4)

b_fin = wrap_elements(panel=b_ag) +
  labs(tag = "% N content of herbivores") +
  theme(
    plot.tag = element_text(size = rel(1.5), angle = 90),
    plot.tag.position = "left"
  )

ggsave(filename = "./Figures/Fig_3_4p.png", plot = b_fin , device = "png",
       width = 10, height = 10, units = "in", scale = 0.9)




# Merge 4a4b ####
# Fig_4ab <- ggarrange(HN_plot, model_4b_plot, labels = c("4A", ""), nrow = 1,
#                      common.legend = FALSE, widths = c(1.75, 0.5))

# ggsave(filename = "./Figures/Fig_3.png", plot = HN_plot , device = "png",
#        width = 8, height = 4, units = "in", scale = 0.9)






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
  # geom_signif(comparisons = list(c("Shadbush", "Barberry")), 
  #             y_position = 11.9,
  #             tip_length = 0.195,
  #             annotation = c("Planned contrast groups")) +
  # geom_signif(y_position = c(11.7), 
  #             xmin = c(0.9, 6.9), 
  #             xmax = c(6.1, 10),
  #             annotation = c(" ", " "), tip_length = 0.01) +
  scale_x_discrete(labels=plant_short)  +
  ylim(10.25,12.25)
SN_plot





# 4d: Spider posthoc ####
model_4d_posthoc <- read.csv("./Data/Models/mod9_posthoc.csv")

b1 = ggplot(data=model_4d_posthoc %>% filter(Exo %in% c("Autumn Olive","Native")), aes(x = Exo, y = emmean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, width=0)) +
  labs(subtitle="A.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # geom_hline(yintercept=0.0,linetype=2) +
  ylim(10.25,12.25) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(11.5), 
              xmin = c(1),
              xmax = c(2),
              annotation = c("P = 0.08"), tip_length = 0.01)
b1

b2 =ggplot(data=model_4d_posthoc %>% filter(Exo %in% c("Barberry","Native")), aes(x = Exo, y = emmean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, width=0)) +
  labs(subtitle="B.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # geom_hline(yintercept=0.0,linetype=2) +
  ylim(10.25,12.25) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(11.5), 
              xmin = c(1),
              xmax = c(2),
              annotation = c("P < 0.0001"), tip_length = 0.01)
b2

b3 = ggplot(data=model_4d_posthoc %>% filter(Exo %in% c("Burning Bush","Native")), aes(x = Exo, y = emmean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, width=0)) +
  labs(subtitle="C.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # geom_hline(yintercept=0.0,linetype=2) +
  ylim(10.25,12.25) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(11.5), 
              xmin = c(1),
              xmax = c(2),
              annotation = c("P = 0.96"), tip_length = 0.01)
b3

b4 = ggplot(data=model_4d_posthoc %>% filter(Exo %in% c("Honeysuckle","Native")), aes(x = Exo, y = emmean)) +
  theme_bw(base_size=16) +
  geom_point(size=4.5) +
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL, width=0)) +
  labs(subtitle="D.",x="",y="") +
  guides(shape=guide_legend(title="", title.position = "left")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  # geom_hline(yintercept=0.0,linetype=2) +
  ylim(10.25,12.25) +
  # theme(axis.title.y = element_blank(),
  #       axis.text.y = element_blank()) +
  theme(legend.position='none') +
  geom_signif(y_position = c(12.0), 
              xmin = c(1),
              xmax = c(2),
              annotation = c("P = 0.98"), tip_length = 0.01)
b4

b_ag = (b1|b2)/(b3|b4)

b_fin = wrap_elements(panel=b_ag) +
  labs(tag = "% N content of spiders") +
  theme(
    plot.tag = element_text(size = rel(1.5), angle = 90),
    plot.tag.position = "left"
  )

ggsave(filename = "./Figures/Fig_4_4p.png", plot = b_fin , device = "png",
       width = 10, height = 10, units = "in", scale = 0.9)


# Fig_4cd <- ggarrange(SN_plot, model_4d_plot, labels = c("4B", ""), nrow = 1,
# common.legend = FALSE, widths = c(1.75, 0.5))
# 
# ggsave(filename = "./Figures/Fig_4.png", plot = SN_plot, device = "png",
#        width = 8, height = 4, units = "in", scale = 0.9)

