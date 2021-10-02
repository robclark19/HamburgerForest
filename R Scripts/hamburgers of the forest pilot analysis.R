# packages
# set working directory as R project directory "HamburgerForest"

library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("multcompView")
library("tidyverse")
library("janitor")
library("ggpubr")
library("dplyr")
library("readr")
library("lme4")

# pilot branch data #####
hotf_dat <- read.csv("./Data/Originals/pilot branch data.csv")

# pilot glmms ####
# make a new column that is "native" vs "non-native"
# make a guiding table
native_dat <- tibble(id = c("Honeysuckle", "Barberry", "Burning Bush", "Autumn Olive", "Musclewood", "Sweet Birch", "Beech", "Black Cherry", "Witch-hazel", "Striped Maple"), 
              exo = c("Non-native", "Non-native", "Non-native", "Non-native", "Native", "Native", "Native", "Native", "Native", "Native"))

# merge guiding table and raw data 
hotf_dat <- hotf_dat %>%
  left_join(y = native_dat, by = c(tree = "id"))


# line 69 in raw data we8c was incorrectly specified as a "bag" but this is a typo (only we8c was written in the field)
# i changed raw data on 10 1 2021 from bag to control

# summarize wet_mass_g by time_block
hotf_summary <- hotf_dat %>%
  group_by(treatment, tree, branch_code) %>%
  summarize(sum_wet_mass = sum(wet_mass_g))
hotf_summary

hotf_dat$wet_mass_g <- as.numeric(hotf_dat$wet_mass_g)
hotf_dat$time_block <- as.factor(hotf_dat$time_block)

write.csv(hotf_summary, "hotf_mass.csv")

# multiple high values
arrange(hotf_summary, -sum_wet_mass)

# 

# drop 7.74 and 3.08
hotf_summary <- subset(hotf_summary, sum_wet_mass < 2)


# do the wet mass glm with sum of masses
hotf_glm <- glm(sum_wet_mass ~ tree * treatment, data=hotf_summary)

summary(hotf_glm)
Anova(hotf_glm)

plot(emmeans(hotf_glm, ~ tree, type="response"))
plot(emmeans(hotf_glm, ~ treatment, type="response"))
plot(emmeans(hotf_glm, ~ treatment*tree, type="response"))
emmeans(hotf_glm, ~ treatment)

# non-pooled biomass
arrange(hotf_dat, -wet_mass_g)
hotf_chop <- subset(hotf_dat, wet_mass_g < 2)


hotf_glm_1 <- lmer(log(wet_mass_g) ~ tree*treatment + (1|branch_code), data=hotf_chop)
Anova(hotf_glm_1)
plot(emmeans(hotf_glm_1, ~ treatment*tree), type="response")

cld(emmeans(hotf_glm_1, ~treatment|tree), type="response", adjust="none")








# native vs. non-native
native_glm_1 <- lmer(log(wet_mass_g) ~ exo*treatment + (1|branch_code), data=hotf_dat)
Anova(native_glm_1)

plot(emmeans(native_glm_1, ~ treatment*exo), type="response")

# bug count
hotf_dat$bug_count <- as.numeric(hotf_dat$bug_count)


hotf_glm_2 <- glmer(bug_count ~ tree*treatment + time_block + (1|branch_code), family="poisson", data=hotf_dat)
Anova(hotf_glm_2)
summary(hotf_glm_2)

plot(emmeans(hotf_glm_2, ~ treatment*tree, type="response"))

cld(emmeans(hotf_glm_2, ~ treatment|tree))

plot(emmeans(hotf_glm_2, ~ treatment, type="response"))
plot(emmeans(hotf_glm_2, ~ tree, type="response"))
plot(emmeans(hotf_glm_2, ~ time_block, type="response"))


hotf_glm_3 <- glmer.nb(morphospecies ~ tree + treatment + (1|branch_code), data=hotf_dat)
Anova(hotf_glm_3)

# full model (interaction terms and time block had AIC of 3959.811)
# reduced model (just two treatment terms, has 3943.67)
AIC(hotf_glm_3)

plot(emmeans(hotf_glm_3, ~ treatment*tree, type="response"))

plot(emmeans(hotf_glm_3, ~ tree, type="response"))

# pilot leaf analysis #####

leaf_dat <- read.csv("./Data/Originals/pilot leaf count data.csv")

hist(leaf_dat$leaf.count)
# plot(vcd::goodfit(leaf_dat$leaf.count, type="poisson"))

leaf_dat$leaf.count <- log(leaf_dat$leaf.count)

# poisson is terrifically bad, data is log-normal
leaf_glm <- glm(log(leaf.count) ~ tree/(treatment + log(bug_count) + log(morphospecies.) + log(wet_mass_g)), data=leaf_dat)
Anova(leaf_glm)
summary(leaf_glm)

plot(emmeans(leaf_glm, ~ treatment|tree, nesting = NULL, type="response"))
plot(emmeans(leaf_glm, ~ treatment|tree|bug_count, nesting = NULL))
plot(emmeans(leaf_glm, ~ treatment|morphospecies., nesting = NULL))
plot(emmeans(leaf_glm, ~ treatment|wet_mass_g, nesting = NULL))
plot(emmeans(leaf_glm, ~tree, type="response" ))

# emmean reg plots ####
emmip(leaf_glm, ~ morphospecies.|tree, cov.reduce = range, type="response")

hist(log(leaf_dat$bug_count))
hist(log(leaf_dat$morphospecies.))
hist(log(leaf_dat$leaf.count))
hist(log(leaf_dat$wet_mass_g))


# example model selection to see if we can drop leaf count

yup.glm <- glm(wet_mass_g ~ tree * treatment + leaf.count, data=leaf_dat)
Anova(yup.glm)
AIC(yup.glm)

yup.glm.2 <- glm(wet_mass_g ~ treatment, data=leaf_dat)
AIC(yup.glm.2)

anova(yup.glm, yup.glm.2)
