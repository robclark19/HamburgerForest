# packages
# set working directory as R project directory "HamburgerForest"

# pilot analysis is just used to evaluate main treatment effects and some

library("car")
library("multcomp")
library("ggplot2")
library("emmeans")
library("multcompView")
library("tidyverse")
library("xlsx")
library("janitor")
library("ggpubr")
library("dplyr")
library("readr")
library("lme4")
library("vegan")

# pilot branch data #####
hotf_dat <- read.csv("./Data/Originals/pilot branch data.csv")

# pilot glmms ####
# make a new column that is "native" vs "non-native"
# make a guiding table
native_dat <- tibble(
  id = c("Honeysuckle", "Barberry", "Burning Bush", "Autumn Olive", "Musclewood", "Black Birch", "Beech", "Black Cherry", "Witch-hazel", "Striped Maple"),
  exo = c("Non-native", "Non-native", "Non-native", "Non-native", "Native", "Native", "Native", "Native", "Native", "Native")
)

# merge guiding table and raw data
hotf_dat <- hotf_dat %>%
  left_join(y = native_dat, by = c(tree = "id"))


# line 69 in raw data we8c was incorrectly specified as a "bag" 
# but this is a typo (only we8c was written in the field)
# i changed raw data on 10 1 2021 from bag to control

# summarize wet_mass_g by time_block
hotf_summary <- hotf_dat %>%
  group_by(treatment, tree, branch_code) %>%
  summarize(sum_wet_mass = sum(wet_mass_g))
hotf_summary

hotf_dat$wet_mass_g <- as.numeric(hotf_dat$wet_mass_g)
hotf_dat$time_block <- as.factor(hotf_dat$time_block)

write.csv(hotf_summary, "./Data/Output/hotf_mass.csv")

# multiple high values
arrange(hotf_summary, -sum_wet_mass)

# drop 7.74 and 3.08
hotf_summary <- subset(hotf_summary, sum_wet_mass < 2)


# do the wet mass glm with sum of masses
hotf_glm <- glm(sum_wet_mass ~ tree * treatment, data = hotf_summary)

summary(hotf_glm)
Anova(hotf_glm)

plot(emmeans(hotf_glm, ~tree, type = "response"))
plot(emmeans(hotf_glm, ~treatment, type = "response"))
plot(emmeans(hotf_glm, ~ treatment * tree, type = "response"))
emmeans(hotf_glm, ~treatment)

# non-pooled biomass
arrange(hotf_dat, -wet_mass_g)
hotf_chop <- subset(hotf_dat, wet_mass_g < 2)


hotf_glm_1 <- lmer(log(wet_mass_g) ~ tree * treatment + (1 | branch_code), data = hotf_chop)
Anova(hotf_glm_1)
plot(emmeans(hotf_glm_1, ~ treatment * tree), type = "response")

cld(emmeans(hotf_glm_1, ~ treatment | tree), type = "response", adjust = "none")

plot(emmeans(hotf_glm_1, ~ tree), type = "response", adjust = "none", sort="TRUE")



# native vs. non-native
native_glm_1 <- lmer(log(wet_mass_g) ~ exo * treatment + (1 | branch_code), data = hotf_dat)
Anova(native_glm_1)

plot(emmeans(native_glm_1, ~ treatment * exo), type = "response")

exo.lsm <- emmeans(native_glm_1, ~ treatment * exo, type = "response") %>% cld()


# bird and native plant effects plot for GH newsletters

newsletter_plot <- ggplot(data=exo.lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Average Insect Biomass (grams)") +
  xlab("Bird Exclusion") +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  facet_wrap( ~ exo, nrow=1)
newsletter_plot



# bug count
hotf_dat$bug_count <- as.numeric(hotf_dat$bug_count)


hotf_glm_2 <- glmer(bug_count ~ tree * treatment + time_block + (1 | branch_code), family = "poisson", data = hotf_dat)
Anova(hotf_glm_2)
summary(hotf_glm_2)

plot(emmeans(hotf_glm_2, ~ treatment * tree, type = "response"))

cld(emmeans(hotf_glm_2, ~ treatment | tree))

plot(emmeans(hotf_glm_2, ~treatment, type = "response"))
plot(emmeans(hotf_glm_2, ~tree, type = "response"))
plot(emmeans(hotf_glm_2, ~time_block, type = "response"))


hotf_glm_3 <- glmer.nb(morphospecies ~ tree + treatment + (1 | branch_code), data = hotf_dat)
Anova(hotf_glm_3)

# full model (interaction terms and time block had AIC of 3959.811)
# reduced model (just two treatment terms, has 3943.67)
AIC(hotf_glm_3)

plot(emmeans(hotf_glm_3, ~ treatment * tree, type = "response"))

plot(emmeans(hotf_glm_3, ~tree, type = "response"))

# pilot leaf analysis #####

leaf_dat <- read.csv("./Data/Originals/pilot leaf count data.csv")

hist(leaf_dat$leaf.count)
# plot(vcd::goodfit(leaf_dat$leaf.count, type="poisson"))

leaf_dat$leaf.count <- log(leaf_dat$leaf.count)

# poisson is terrifically bad, data is log-normal
leaf_glm <- glm(log(leaf.count) ~ tree / (treatment + log(bug_count) + log(morphospecies.) + log(wet_mass_g)), data = leaf_dat)
Anova(leaf_glm)
summary(leaf_glm)

plot(emmeans(leaf_glm, ~ treatment | tree, nesting = NULL, type = "response"))
plot(emmeans(leaf_glm, ~ treatment | tree | bug_count, nesting = NULL))
plot(emmeans(leaf_glm, ~ treatment | morphospecies., nesting = NULL))
plot(emmeans(leaf_glm, ~ treatment | wet_mass_g, nesting = NULL))
plot(emmeans(leaf_glm, ~tree, type = "response"))

# emmean reg plots ####
emmip(leaf_glm, ~ morphospecies. | tree, cov.reduce = range, type = "response")

hist(log(leaf_dat$bug_count))
hist(log(leaf_dat$morphospecies.))
hist(log(leaf_dat$leaf.count))
hist(log(leaf_dat$wet_mass_g))


# example model selection to see if we can drop leaf count

yup.glm <- glm(wet_mass_g ~ tree * treatment + leaf.count, data = leaf_dat)
Anova(yup.glm)
AIC(yup.glm)

yup.glm.2 <- glm(wet_mass_g ~ treatment, data = leaf_dat)
AIC(yup.glm.2)

anova(yup.glm, yup.glm.2)





# pilot NMDS ######
# also some glms for major functional groups

# bug diveristy data
trophic_nmds_dat <- read.csv("./Data/Output/trophicnmds1.csv", header=TRUE)

# 
#make new object so commands below dont edit primary data
matrix.dat <- trophic_nmds_dat

#drop env columns to make a species matrix
matrix.dat$branch_code <- NULL
matrix.dat$time_block <- NULL
matrix.dat$tree <- NULL
matrix.dat$treatment <- NULL

colSums(matrix.dat)


#nmds requires you drop any rows with far too many zeroes
# matrix.dat$ERA <- NULL


#check
colSums(matrix.dat)


# K=2 NMDS #####
#run nmds with k=2 dimensions
trophic.mds <- metaMDS(matrix.dat, k=2, plot=TRUE)
trophic.mds

# attach puts this in memory so if you call groups = treatment later,
# it knows to pull from this frame
attach(trophic_nmds_dat)
#just plot points (sites and species)


plot(trophic.mds)

#ordination plot (starts with blank ordination that layers can be added on)
ordiplot(trophic.mds, type="none")
#species plot (add insect species first)
orditorp(trophic.mds,display="species",col="black",air=0.2,cex=1)

# Bag effect NMDS plot ####
# draws a shape around it based on the environmental variable of interest
ordihull(trophic.mds, groups=treatment, draw="polygon",col="grey90",label=T)

# now for tree
ordihull(trophic.mds, groups=tree, draw="polygon",col="grey90",label=T)

#air determines how much space is between, <1 means it will let things overlap.
#dropped overlapping text shown as a circle


#try to add arrows that indicate impact of a species on ordination
# ordiarrows(trophic.mds, groups=treatment, label=TRUE)

#site plot (no species data shown)
ordiplot(trophic.mds, type="n")
orditorp(trophic.mds,display="sites",col="black",air=1,cex=1)

ordiplot(trophic.mds, type="n")
ordiellipse(trophic.mds, site, label=T,air=0.01,cex=0.5)


# Bag effect test ####
# actual statistical test to for bag vs. bird treatment
ord.fit <- envfit(trophic.mds ~ treatment, data=trophic_nmds_dat, perm=999)
ord.fit

ord.fit.tree <- envfit(trophic.mds ~ tree, data=trophic_nmds_dat, perm=999)
ord.fit.tree

# do this analysis with comparing natives 

# Stress plot shows how well the data fit:
# it should cluster around the red line (sort of like ANOVA residuals)
stressplot(trophic.mds)


# Invert GLMs ####
# merge guiding table and raw data
trophic_dat <- trophic_nmds_dat %>%
  left_join(y = native_dat, by = c(tree = "id"))


# basic glmms on invert groups
str(trophic_dat)

# aquatic glmm ####
aquatics.glm <- glm.nb(aquatics ~ treatment*exo, data=trophic_dat)
summary(aquatics.glm)

plot(emmeans(aquatics.glm, ~ treatment), type="response")
plot(emmeans(aquatics.glm, ~ treatment*exo), type="response")



# arachnids glmm ####

spider.glm <- glm.nb(arachnids ~ exo + treatment, data=trophic_dat)
summary(spider.glm)

plot(emmeans(spider.glm, ~ treatment*exo), type="response")
plot(emmeans(spider.glm, ~ exo,), type="response")


spider.glm.2 <- glm.nb(arachnids ~ tree + as.factor(time_block) + treatment, data=trophic_dat)
summary(spider.glm.2)

plot(emmeans(spider.glm.2, ~ treatment|tree))

plot(emmeans(spider.glm.2, ~ time_block|tree), type="response")

# is there a bird-arachnid-herbivore trophic cascade?


# hymenoptera glmm ####
hymenoptera.glm <- glm.nb(hymenoptera ~ treatment + tree, data=trophic_nmds_dat)
summary(hymenoptera.glm)

plot(emmeans(hymenoptera.glm, ~ treatment), type="response")
plot(emmeans(hymenoptera.glm, ~ tree), type="response")


# lepidoptera glmm #####
lepidoptera.glm <- glm.nb(lepidoptera  ~ treatment + exo , data=trophic_dat)
summary(lepidoptera.glm)

plot(emmeans(lepidoptera.glm, ~ treatment), type="response")
plot(emmeans(lepidoptera.glm, ~ exo), type="response")



# hemiptera ######
hemiptera.glm <- glm.nb(hemiptera ~ treatment * exo, data=trophic_dat)
summary(hemiptera.glm)
Anova(hemiptera.glm)

plot(emmeans(hemiptera.glm, ~ treatment), type="response")
plot(emmeans(hemiptera.glm, ~ exo), type="response")
plot(emmeans(hemiptera.glm, ~ treatment*exo), type="response")

# coleoptera ###
coleoptera.glm <- glmer.nb(coleoptera ~ treatment*exo + (1|branch_code), data=trophic_dat)
summary(coleoptera.glm)

plot(emmeans(coleoptera.glm, ~ treatment*exo), type="response")


# gastropubs
gastropods.glm <- glmer.nb(gastropods ~ treatment + (1|branch_code), data=trophic_nmds_dat)
summary(gastropods.glm)

plot(emmeans(gastropods.glm, ~ treatment), type="response")

# orthoptera ##### 
orthopterids.glm <- glmer.nb(orthopterids ~ treatment*exo + (1|branch_code), data=trophic_dat)
summary(orthopterids.glm)

plot(emmeans(orthopterids.glm, ~ treatment*exo), type="response")
