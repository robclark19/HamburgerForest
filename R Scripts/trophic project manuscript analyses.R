# Trophic project 2021 Primary Figures
# Libraries ####
library("tidyverse")
library("readr")
library("lme4")
library("car")
library("multcomp")
library("ggplot2")
library("emmeans")




# Data ######
# Branch data called "pilot" branch data until it is validated
# Branch data #####
hotf_dat <- read.csv("./Data/Originals/pilot branch data.csv")

# make a guiding table
native_dat <- tibble(
  id = c("Honeysuckle", "Barberry", "Burning Bush", "Autumn Olive", "Musclewood", "Sweet Birch", "Beech", "Black Cherry", "Witch-hazel", "Striped Maple"),
  exo = c("Non-native", "Non-native", "Non-native", "Non-native", "Native", "Native", "Native", "Native", "Native", "Native")
)

# merge guiding table and raw data
hotf_dat <- hotf_dat %>%
  left_join(y = native_dat, by = c(tree = "id"))

# Shadbush fix #####
# exclude cherry pairs 5 6 and 7 in which black cherry was accidentally sampled
# All shadbush were then misproperly labeled as cherry in the field, fixed at the next step
bc_drop <- data.frame(branch_code = c("BC5B", "BC5C","BC6B", "BC6C","BC7B", "BC7C"))

# drop cherries and change column to the correct name "shadbush"
hotf_dat <- hotf_dat %>% anti_join(bc_drop, by = "branch_code", copy=TRUE) %>%
  mutate(tree = replace(tree, tree == "Black Cherry", "Shadbush"))






# Model 1: Bird x Tree #####
# Figure 1: Bird effect across 10 host plants
model_1 <- lmer(log(wet_mass_g) ~ tree * treatment + (1 | branch_code), data = hotf_dat)

# Table for Figure 1
Anova(model_1)

# Parameter estimates and posthoc tests
tree.lsm <- emmeans(model_1, ~ treatment | tree, type = "response") %>% cld()


# Fig 1 #####
# Modified from 10 species newsletter figure in hotf pilot analysis
Fig_1 <- ggplot(data=tree.lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = tree)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Average arthropod biomass (grams)") +
  xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ tree, nrow=2) 
Fig_1


# ggsave(filename = "./Figures/Fig_1.png", plot = Fig_1, device = "png",
#        width = 6, height = 4, units = "in")



# Figure 2: Effect size analysis of bird exclusion on native and non-natives
# uses hedges g based on Figure 1 




# Figure 3: bag effects on different feeding guilds
# What are birds eating on invasive shrubs vs. native shrubs?
# Supplemental figures show the real effects (lengthy), then this figure mimics figure 2



# Figure 4: C:N content among native and non-native plants for spiders and insect herbivores
# should be a simple clustered bar chart with no treatment effects




# Figures s1-s4: Major insect groups variation in counts among host plants






# Figure s5: ruling out leaf area as a primary determinant of biomass patterns


