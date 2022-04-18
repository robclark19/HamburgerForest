# Trophic project 2021 Primary Figures
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



# Data ######
# Branch data called "pilot" branch data until it is validated
# Branch data #####
hotf_dat <- read.csv("./Data/Originals/pilot branch data.csv")
trophic_dat <- read.csv("./Data/Output/clean_trophic_groups.csv")

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


# merge the cleaned insect community data (trophic_dat) with the branch-level data (hotf_dat)
ht_dat <- left_join(hotf_dat, trophic_dat)
str(ht_dat)

# gastropod fix
ht_dat$gastro_mass <- ht_dat$gastropods * (0.01)
ht_dat

#
ht_dat$wet_mass_g <- ht_dat$wet_mass_g - ht_dat$gastro_mass 

# Model 1a: Bird x Tree #####
# Figure 1: Bird effect across 10 host plants
model_1 <- lmer(log(wet_mass_g) ~ tree * treatment +  (1 | branch_code), data = ht_dat)

# Table for Figure 1
Anova(model_1)

# Parameter estimates and posthoc tests
# this should be "pairs" and report p-values
tree.lsm <- emmeans(model_1, ~ treatment | tree, type = "response") %>% cld(adjust="scheffe")

# effect size calculations using emmeans
tree.lsm <- emmeans(model_1, ~ treatment | tree, type = "response")
eff_size(tree.lsm, sigma = sigma(model_1), edf = 205)
plot(eff_size(tree.lsm, sigma = sigma(model_1), edf = 205))
cld(eff_size(tree.lsm, sigma = sigma(model_1), edf = 205), adjust="none")

# Exo model
# emmeans(model_1, ~ treatment | exo, type = "response") %>% cld() %>% plot()

# Fig S1 #####
# Modified from 10 species newsletter figure in hotf pilot analysis
Fig_1 <- ggplot(data=tree.lsm, aes(x = treatment, y = response)) +
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
Fig_1

# Model 1b
model_1b <- lmer(log(wet_mass_g) ~ exo * treatment + (1|tree)  + (1 | branch_code), data = hotf_dat)

# Table for Figure 1b
Anova(model_1b)

# Parameter estimates and posthoc tests
tree.lsm <- emmeans(model_1b, ~ treatment | exo, type = "response") %>% cld()

# Fig 1 #####
# Modified from 10 species newsletter figure in hotf pilot analysis
Fig_1b <- ggplot(data=tree.lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = exo)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Average arthropod biomass (g)") +
  xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
Fig_1b


# Arrange 1ab
Fig_1ab <- ggarrange(Fig_1, Fig_1b, labels = c("A", "B"), nrow = 2,
                    common.legend = FALSE, legend = "bottom")



# Output 1ab ####
 ggsave(filename = "./Figures/Fig_1ab.png", plot = Fig_1ab, device = "png",
        width = 6, height = 9, units = "in")



# Secondary analysis: Effect size analysis of bird exclusion on native and non-natives
# uses hedges g based on Figure 1 



# Fig 2 ######
# Reports bag effects on different feeding guilds
# What are birds eating on invasive shrubs vs. native shrubs?
# Supplemental figures may show the variation in abundance among all plants like from seminars
# 2A ####
spider.glm <- glmer.nb(arachnids ~ exo * treatment + (1|branch_code) + (1|tree), data=trophic_dat)
# mean, total, SEM
# spider_summary <- trophic_dat %>% 
#   filter(treatment == 'bag') %>%
#   group_by(tree,exo) %>% 
#   summarise(guild_mean = mean(guild), SE = std.error(guild, na.rm=TRUE)) 

spider_lsm <- cld(emmeans(spider.glm, ~  treatment|exo, type="response"))

# spider pub fig #####
Fig_2a <- ggplot(data=spider_lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = exo)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Average spider abundance per branch") +
  xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
Fig_2a

# 2B ####
hemiptera.glm <- glmer.nb(hemiptera ~ exo * treatment + (1|branch_code) + (1|tree), data=trophic_dat)
# mean, total, SEM
# hemiptera_summary <- trophic_dat %>% 
#   filter(treatment == 'bag') %>%
#   group_by(tree,exo) %>% 
#   summarise(hemiptera = mean(hemiptera), SE = std.error(hemiptera, na.rm=TRUE)) 

hemiptera_lsm <- cld(emmeans(hemiptera.glm, ~  treatment|exo, type="response"))

# hemiptera pub fig #####
Fig_2b <- ggplot(data=hemiptera_lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = exo)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Average true bug abundance per branch") +
  xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
Fig_2b

# 2c ####
lepidoptera.glm <- glmer.nb(lepidoptera ~ exo * treatment + (1|branch_code) + (1|tree), data=trophic_dat)
# mean, total, SEM
# hemiptera_summary <- trophic_dat %>% 
#   filter(treatment == 'bag') %>%
#   group_by(tree,exo) %>% 
#   summarise(hemiptera = mean(hemiptera), SE = std.error(hemiptera, na.rm=TRUE)) 

lepidoptera_lsm <- cld(emmeans(lepidoptera.glm, ~  treatment*exo, type="response"))

# hemiptera pub fig #####
Fig_2c <- ggplot(data=lepidoptera_lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = exo)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Average caterpillar abundance per branch") +
  xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
Fig_2c



# 2d ####
orthopterids.glm <- glmer.nb(orthopterids ~ exo * treatment + (1|branch_code) + (1|tree), data=trophic_dat)
# mean, total, SEM
# hemiptera_summary <- trophic_dat %>% 
#   filter(treatment == 'bag') %>%
#   group_by(tree,exo) %>% 
#   summarise(hemiptera = mean(hemiptera), SE = std.error(hemiptera, na.rm=TRUE)) 

orthopterids_lsm <- cld(emmeans(orthopterids.glm, ~  treatment|exo, type="response"), adjust="scheffe")
# https://en.wikipedia.org/wiki/Scheff%C3%A9%27s_method


# tree cricket pub fig #####
Fig_2d <- ggplot(data=orthopterids_lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_line(aes(group = exo)) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Average tree cricket abundance per branch") +
  xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
Fig_2d


# arrange figure 2
Fig_2abcd <- ggarrange(Fig_2a, Fig_2b, Fig_2c, Fig_2d,
                         labels = c("A","B","C","D"), 
                         nrow = 2, ncol = 2)
Fig_2abcd 

# write figure 2 to folder
ggsave(filename = "./Figures/Fig2abcd.png", plot = Fig_2abcd, device = "png",
       width = 10, height = 8, units = "in")








# Figure 3: C:N content among native and non-native plants for spiders and insect herbivores
# should be a simple clustered bar chart with no treatment effects






