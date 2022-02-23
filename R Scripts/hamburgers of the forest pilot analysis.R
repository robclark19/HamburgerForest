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
library("plotrix") # for std.error function

# pilot branch data #####
hotf_dat <- read.csv("./Data/Originals/pilot branch data.csv")

# pilot glmms ####
# make a new column that is "native" vs "non-native"
# make a guiding table
native_dat <- tibble(
  id = c("Honeysuckle", "Barberry", "Burning Bush", "Autumn Olive", "Musclewood", "Sweet Birch", "Beech", "Black Cherry", "Witch-hazel", "Striped Maple"),
  exo = c("Non-native", "Non-native", "Non-native", "Non-native", "Native", "Native", "Native", "Native", "Native", "Native")
)

# merge guiding table and raw data
hotf_dat <- hotf_dat %>%
  left_join(y = native_dat, by = c(tree = "id"))

# Shadbush fix #####
# replace all instances of black cherry with shadbush
# exclude the handful in which black cherry was actually sampled
# was is cherry pair 5 6 and 7
bc_drop <- data.frame(branch_code = c("BC5B", "BC5C","BC6B", "BC6C","BC7B", "BC7C"))

hotf_dat <- hotf_dat %>% anti_join(bc_drop, by = "branch_code", copy=TRUE) %>%                               # Replacing values
  mutate(tree = replace(tree, tree == "Black Cherry", "Shadbush"))




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

# write.csv(hotf_summary, "./Data/Output/hotf_mass.csv")

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
  theme_bw(base_size=14) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Average Insect Biomass (grams)") +
  xlab("") +
  scale_x_discrete(labels=c("bag" = "Birds Excluded", "control" = "Birds Present")) +
  # geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  facet_wrap( ~ exo, nrow=1) +
  ylab("Average arthropod biomass (grams)") +
  xlab("Bird Exclusion") +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ exo, nrow=1) 
newsletter_plot


# newsletter plot 2 with host plants
# native vs. non-native
native_glm_2 <- lmer(log(bug_count) ~ tree * treatment + (1 | branch_code), data = hotf_dat)
Anova(native_glm_2)

plot(emmeans(native_glm_2, ~ treatment * tree), type = "response")

tree.lsm <- emmeans(native_glm_2, ~ treatment | tree, type = "response") %>% cld()


# bird and native plant effects plot for GH newsletters

newsletter_plot_2 <- ggplot(data=tree.lsm, aes(x = treatment, y = response)) +
  theme_bw(base_size=12) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin=response-(SE), ymax=response+(SE), width=0)) +
  ylab("Average arthropod biomass (grams)") +
  xlab("Bird Exclusion") +
  scale_x_discrete(labels=c("bag" = "- Birds", "control" = "+ Birds")) +
  geom_text(aes(x = treatment, y = (response+SE), label = .group, hjust=-.5)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap( ~ tree, nrow=2) 
newsletter_plot_2


# bag only biomass figure






# musclewood over time
hotf_dat$time_block <- as.factor(hotf_dat$time_block)

mt_glm <- glm(log(bug_count) ~ tree * time_block, data = hotf_dat)
Anova(mt_glm)


cld(emmeans(mt_glm, ~ time_block|tree, adjust = "none"), adjust = "none")










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



# count x biomass regression ######

cb_glm <- lmer(log(bug_count) ~ log(wet_mass_g) + (1 | branch_code), data = hotf_dat)
summary(cb_glm)
Anova(cb_glm)

plot(hotf_dat$bug_count, hotf_dat$wet_mass_g)

ggplot(hotf_dat, aes(x=log(bug_count), y=log(wet_mass_g))) + geom_point() +
  geom_smooth(method=lm)



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


# just species. yah we did a good job with bagging
leaf_glm_2 <- glm(leaf.count ~ tree*treatment, data=leaf_dat)
plot(emmeans(leaf_glm_2, ~ treatment | tree, nesting = NULL, type = "response"))

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






# effect size analysis using keenwa code
cross.irr.lsm <- cld(emmeans(keenwa2r.mix, ~ Irrigation*Year, type="response"),
                     sort=FALSE, Letters=c("bac"))
cross.irr.lsm$.group=gsub(" ", "", cross.irr.lsm$.group)
cross.irr.lsm$response <- cross.irr.lsm$rate

# % change#####
as.data.frame(cross.irr.lsm)
Irr.2018 <- cross.irr.lsm[3,"response"]
NI.2018 <- cross.irr.lsm[4,"response"]

NI.2018.percent.increase <- ((NI.2018 / Irr.2018)-1)*100
NI.2018.percent.increase






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
f
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
# Change black birch to sweet birch
# bug diveristy data
trophic_dat <- read.csv("./Data/Output/trophicnmds1.csv", header=TRUE)


trophic_dat <- trophic_dat %>% mutate(tree = replace(tree, tree == "Black Birch", "Sweet Birch"))

# merge guiding table and raw data
trophic_dat <- trophic_dat %>%
  left_join(y = native_dat, by = c(tree = "id"))


# update names for invert level analysis
bc_drop <- data.frame(branch_code = c("BC5B", "BC5C","BC6B", "BC6C","BC7B", "BC7C"))

trophic_dat <- trophic_dat %>% anti_join(bc_drop, by = "branch_code", copy=TRUE) %>%                               # Replacing values
  mutate(tree = replace(tree, tree == "Black Cherry", "Shadbush"))


# basic glmms on invert groups
str(trophic_dat)

# aquatic glmm ####
aquatics.glm <- glm.nb(aquatics ~ treatment*tree, data=trophic_dat)
summary(aquatics.glm)

plot(emmeans(aquatics.glm, ~ treatment), type="response")
plot(emmeans(aquatics.glm, ~ treatment*tree), type="response")
plot(emmeans(aquatics.glm, ~ tree), type="response")

cld(emmeans(aquatics.glm, ~ tree), type="response", adjust="none")

# aquatics plot #####
cld(emmeans(aquatics.glm, ~ tree), type="response", adjust="none")


# write a summary table of totals and means for insects by host plant
# mean, total, SEM
aquatics_summary <- trophic_dat %>% 
  filter(treatment == 'bag') %>%
  group_by(tree,exo) %>% 
  summarise(Aquatics = mean(aquatics), SE = std.error(aquatics, na.rm=TRUE)) 




# aquatics pub fig #####
#aphid density fig for 2018 non-crop legume transects
aquatics_fig <- ggplot(aquatics_summary, aes(x=Aquatics,y=reorder(tree,-desc(Aquatics)),fill=exo)) +
  geom_bar(stat="identity", width=0.6, position="dodge") +
  theme_bw(base_size = 16) + 
  scale_fill_manual(values = c("gray79", "gray45")) +
  geom_errorbar(aes(xmin=Aquatics-(SE/2), xmax=Aquatics+(SE/2)), width=0.1) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Plant species", x="Average aquatic insect # (bag branches)", fill="Plant type") + 
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position=c(0.8,0.2)) +
  theme(legend.title = element_blank())
aquatics_fig


# write figure 3 to folder, use arguments to modify size or file type!
ggsave(filename = "./Figures/Aquatics.svg", plot = aquatics_fig, device = "svg",
       width = 6, height = 5, units = "in")







# arachnids glmm ####

spider.glm <- glm.nb(arachnids ~ exo + treatment, data=trophic_dat)
summary(spider.glm)

plot(emmeans(spider.glm, ~ treatment*exo), type="response")
plot(emmeans(spider.glm, ~ exo,), type="response")


spider.glm.2 <- glm.nb(arachnids ~ tree + as.factor(time_block) + treatment, data=trophic_dat)
summary(spider.glm.2)

plot(emmeans(spider.glm.2, ~ treatment|tree))

plot(emmeans(spider.glm.2, ~ time_block|tree), type="response")

plot(emmeans(spider.glm.2, ~tree, type="response"))

# spider plot
# pooled and unpooled across tree species (native vs. nonnative, all 10 together)

# write a summary table of totals and means for insects by host plant
# mean, total, SEM
spider_summary <- trophic_dat %>% 
  filter(treatment == 'bag') %>%
  group_by(tree,exo) %>% 
  summarise(spiders = mean(arachnids), SE = std.error(arachnids, na.rm=TRUE)) 

# spider pub fig #####
spiders_fig <- ggplot(spider_summary, aes(x=spiders,y=reorder(tree,-desc(spiders)),fill=exo)) +
  geom_bar(stat="identity", width=0.6, position="dodge") +
  theme_bw(base_size = 16) +
  scale_fill_manual(values = c("gray79", "gray45")) +
  geom_errorbar(aes(xmin=spiders-(SE/2), xmax=spiders+(SE/2)), width=0.1) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Plant species", x="Average spider # (bag branches)", fill="Plant type") + 
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position=c(0.8,0.1)) +
  theme(legend.title = element_blank())
spiders_fig


# write figure 3 to folder, use arguments to modify size or file type!
ggsave(filename = "./Figures/spiders.svg", plot = spiders_fig, device = "svg",
       width = 6, height = 5, units = "in")













# is there a bird-arachnid-herbivore trophic cascade?


# hymenoptera glmm ####
hymenoptera.glm <- glm.nb(hymenoptera ~ treatment + tree, data=trophic_nmds_dat)
summary(hymenoptera.glm)

plot(emmeans(hymenoptera.glm, ~ treatment), type="response")
plot(emmeans(hymenoptera.glm, ~ tree), type="response")


# lepidoptera glmm #####
lepidoptera.glm <- glmer.nb(lepidoptera  ~ treatment * tree + (1|branch_code), data=trophic_dat)
summary(lepidoptera.glm)

plot(emmeans(lepidoptera.glm, ~ treatment*tree), type="response")

cld(emmeans(lepidoptera.glm, ~ treatment|tree), type="response")

# make a lep figure just showing the 6 trees with bird effects





# lep abundance pub fig #####
# write a summary table of totals and means for insects by host plant
# mean, total, SEM
lep_summary <- trophic_dat %>% 
  filter(treatment == 'bag') %>%
  group_by(tree,exo) %>% 
  summarise(Lepidoptera = mean(lepidoptera), SE = std.error(lepidoptera, na.rm=TRUE))

# lep pub fig #####
Lepidoptera_fig <- ggplot(lep_summary, aes(x=Lepidoptera,y=reorder(tree,-desc(Lepidoptera)),fill=exo)) +
  geom_bar(stat="identity", width=0.6, position="dodge") +
  theme_bw(base_size = 16) + 
  scale_fill_manual(values = c("gray79", "gray45")) +
  geom_errorbar(aes(xmin=Lepidoptera-(SE/2), xmax=Lepidoptera+(SE/2)), width=0.1) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Plant species", x="Average Lepidoptera # (bag branches)", fill="Plant type") + 
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position=c(0.8,0.2)) +
  theme(legend.title = element_blank())
Lepidoptera_fig


# write figure to folder, use arguments to modify size or file type!
ggsave(filename = "./Figures/Lepidoptera.svg", plot = Lepidoptera_fig, device = "svg",
       width = 6, height = 5, units = "in")








# hemiptera ######
hemiptera.glm <- glm.nb(hemiptera ~ treatment * tree, data=trophic_dat)
summary(hemiptera.glm)
Anova(hemiptera.glm)

plot(emmeans(hemiptera.glm, ~ treatment), type="response")
plot(emmeans(hemiptera.glm, ~ tree), type="response")
plot(emmeans(hemiptera.glm, ~ treatment*tree), type="response")



# hemiptera pub fig
hemiptera_summary <- trophic_dat %>% 
  filter(treatment == 'bag') %>%
  group_by(tree,exo) %>% 
  summarise(Hemiptera = mean(hemiptera), SE = std.error(hemiptera, na.rm=TRUE))


# hemi pub fig #####
hemiptera_fig <- ggplot(hemiptera_summary, aes(x=Hemiptera,y=reorder(tree,-desc(Hemiptera)),fill=exo)) +
  geom_bar(stat="identity", width=0.6, position="dodge") +
  theme_bw(base_size = 16) + 
  scale_fill_manual(values = c("gray79", "gray45")) +
  geom_errorbar(aes(xmin=Hemiptera-(SE/2), xmax=Hemiptera+(SE/2)), width=0.1) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Plant species", x="Average Hemiptera # (bag branches)", fill="Plant type") + 
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position=c(0.8,0.2)) +
  theme(legend.title = element_blank())
hemiptera_fig


# write figure to folder, use arguments to modify size or file type!
ggsave(filename = "./Figures/hemiptera.svg", plot = hemiptera_fig, device = "svg",
       width = 6, height = 5, units = "in")













# coleoptera ####
coleoptera.glm <- glmer.nb(coleoptera ~ treatment + tree + (1|branch_code), data=trophic_dat)
summary(coleoptera.glm)

plot(emmeans(coleoptera.glm, ~ tree), type="response")

# beetle pub fig
beetle_summary <- trophic_dat %>% 
  filter(treatment == 'bag') %>%
  group_by(tree,exo) %>% 
  summarise(Coleoptera = mean(coleoptera), SE = std.error(coleoptera, na.rm=TRUE))



# beetle pub fig #####
beetle_fig <- ggplot(beetle_summary, aes(x=Coleoptera,y=reorder(tree,-desc(Coleoptera)),fill=exo)) +
  geom_bar(stat="identity", width=0.6, position="dodge") +
  theme_bw(base_size = 16) + 
  scale_fill_manual(values = c("gray79", "gray45")) +
  geom_errorbar(aes(xmin=Coleoptera-(SE/2), xmax=Coleoptera+(SE/2)), width=0.1) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Plant species", x="Average Coleoptera # (bag branches)", fill="Plant type") + 
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position=c(0.8,0.2)) +
  theme(legend.title = element_blank())
beetle_fig


# write figure to folder, use arguments to modify size or file type!
ggsave(filename = "./Figures/beetles.svg", plot = beetle_fig, device = "svg",
       width = 6, height = 5, units = "in")








# gastropubs
# too many zeroes for tree-level analysis
gastropods.glm <- glmer.nb(gastropods ~ treatment + (1|branch_code), data=trophic_nmds_dat)
summary(gastropods.glm)

plot(emmeans(gastropods.glm, ~ treatment), type="response")


# orthoptera ##### 
orthopterids.glm <- glm.nb(orthopterids ~ treatment*tree, data=trophic_dat)
summary(orthopterids.glm)

plot(emmeans(orthopterids.glm, ~ treatment*tree), type="response")

# orthoptera pub fig ######
orthoptera_summary <- trophic_dat %>% 
  filter(treatment == 'bag') %>%
  group_by(tree,exo) %>% 
  summarise(orthoptera = mean(orthopterids), SE = std.error(orthopterids, na.rm = TRUE)) 

# spider pub fig #####
orthoptera_fig <- ggplot(orthoptera_summary, aes(x=orthoptera,y=reorder(tree,-desc(orthoptera)),fill=exo)) +
  geom_bar(stat="identity", width=0.6, position="dodge") +
  theme_bw(base_size = 16) + 
  scale_fill_manual(values = c("gray79", "gray45")) +
  geom_errorbar(aes(xmin=orthoptera-(SE/2), xmax=orthoptera+(SE/2)), width=0.1) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Plant species", x="Average Orthoptera # (bag branches)", fill="Plant type") + 
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position=c(0.8,0.2)) +
  theme(legend.title = element_blank())
orthoptera_fig


# write figure to folder, use arguments to modify size or file type!
ggsave(filename = "./Figures/orthoptera.svg", plot = orthoptera_fig, device = "svg",
       width = 6, height = 5, units = "in")


