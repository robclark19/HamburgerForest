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


# Data ###
ht_dat <- read.csv(x=ht_dat, file="./Data/Output/manuscript_dat.csv")


# Analysis #####


# Model 1a: Bird x Tree #####
# Figure 1: Bird effect across 10 host plants
model_1 <- lmer(log(wet_mass_g) ~ tree * treatment +  (1 | branch_code), data = ht_dat)

# Table for Figure 1
Anova(model_1)

# Parameter estimates and posthoc tests
# this should be "pairs" and report p-values
tree.lsm <- emmeans(model_1, ~ treatment | tree, type = "response") %>% cld(adjust="scheffe")

# effect size calculations using emmeans
# not included for now since it introduces unnecessary complexity to the paper
# tree.lsm <- emmeans(model_1, ~ treatment | tree, type = "response")
# eff_size(tree.lsm, sigma = sigma(model_1), edf = 205)
# plot(eff_size(tree.lsm, sigma = sigma(model_1), edf = 205))
# cld(eff_size(tree.lsm, sigma = sigma(model_1), edf = 205), adjust="none")

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







# Models 3ab CN ####
# Figure 3: C:N content among native and non-native plants for spiders and insect herbivores
# should be a simple clustered bar chart with no treatment effects

head(ht_dat)


# Herbivore CN ####
tree_cn_mod_1 <- glm(herbivore_average_n ~ tree, data=ht_dat)
Anova(tree_cn_mod_1)
plot(emmeans(tree_cn_mod, ~ tree))

tree_cn_mod_1a <- glm(herbivore_average_n ~ exo, data=ht_dat)
Anova(tree_cn_mod_1a)
plot(emmeans(tree_cn_mod_1a, ~ exo))

tree_cn_mod_2 <- glm(herbivore_average_c ~ tree, data=ht_dat)
Anova(tree_cn_mod_2)
plot(emmeans(tree_cn_mod_2, ~ tree))

tree_cn_mod_2a <- glm(herbivore_average_c ~ exo, data=ht_dat)
Anova(tree_cn_mod_2a)
plot(emmeans(tree_cn_mod_2a, ~ exo))

tree_cn_mod_3 <- glm(herbivore_average_ratio ~ exo, data=ht_dat)
Anova(tree_cn_mod_3)
plot(emmeans(tree_cn_mod_3, ~ exo))

tree_cn_mod_4 <- glm(herbivore_average_ratio ~ tree, data=ht_dat)
Anova(tree_cn_mod_4)
plot(emmeans(tree_cn_mod_4, ~ tree))

hist(ht_dat$herbivore_average_ratio)

# CN by herbivores

cnbh_glm <- glm(herbivore_average_n ~ hemiptera + orthopterids + lepidoptera + log(coleoptera+1), data=ht_dat)
Anova(cnbh_glm)
summary(cnbh_glm)

plot(ht_dat$herbivore_average_ratio, ht_dat$coleoptera)
abline(lm(ht_dat$herbivore_average_ratio ~ log(ht_dat$coleoptera+1)))

# Spider CN ####
# remove spider high value on barberry
ht_dat_2 <- subset(ht_dat, spider_average_ratio < 7)

tree_cn_mod_5 <- glm(spider_average_ratio ~ exo, data=ht_dat_2)
Anova(tree_cn_mod_5)
plot(emmeans(tree_cn_mod_5, ~ exo))

tree_cn_mod_6 <- glm(spider_average_ratio ~ tree, data=ht_dat_2)
Anova(tree_cn_mod_6)
plot(emmeans(tree_cn_mod_6, ~ tree))

hist(ht_dat_2$spider_average_ratio)


# Figures 3AB #####


