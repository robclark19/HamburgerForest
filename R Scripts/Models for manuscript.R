# refactored modeling code for ecology report / PNAS paper
# models pulled from "trophic project manuscript analyses", that file is now the old one
# only glmms and lookup tables for plotting

# Libraries ####
library("tidyverse")
library("readr")
library("lme4")
library("car")
library("multcomp")
library("emmeans")
library("plotrix") # for std.error function

# Data ####
ht_dat <- read.csv(file="./Data/Output/manuscript_dat.csv")

# Native list ######
# list of native and non-natives following the order of invasives that appear in EMMEANS
native_list <- c("Non-native","Non-native","Native","Non-native","Non-native",
                 "Native","Native","Native","Native", "Native")


# Manuscript Body Analyses #####
# These are models that go into the main figures or are connected to the main text of the manuscript

# Model 1: Bagged biomass #####
model_1 <- lmer(log(wet_mass_g) ~ tree +  (1 | branch_code), data = subset(ht_dat, treatment=="bag"))

biomass_lsm <- emmeans(model_1, ~ tree, type="response")

biomass_cld <- cld(biomass_lsm, adjust="none", type="response")

# figure generation
# write a summary table of totals and means for insects by host plant
# mean, total, SEM

biomass_summary <- ht_dat %>% 
  filter(treatment == 'bag') %>%
  group_by(tree,exo) %>% 
  summarise(biomass_mean = mean(wet_mass_g), sem = std.error(wet_mass_g, na.rm=TRUE)) 

# merge biomass cld with biomass_summary
biomass_summary <- biomass_summary %>%
  left_join(y=biomass_cld, by = c("tree"))%>%
  as.data.frame()

# write the arranged model outputs 
write.csv(biomass_summary, "./Data/Models/model1.csv")

# Planned contrast method ######
# biomass posthoc test
Anova(model_1)
summary(model_1)


# apply native vs non-native list to lsm table
biomass_group <- add_grouping(biomass_lsm, "Exo", "tree", native_list)
str(biomass_group)

# write the contrast, then save the contrast values and round to nearest 3rd decimal
biomass_contrast <- emmeans(biomass_group, pairwise ~ Exo)

biomass_contrast$emmeans %>% 
  as.data.frame() %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  write.csv("./Data/Models/model1_posthoc.csv")



# Model 2 ver 1: Bag effects #####
#  Bird effect across 10 host plants
model_2 <- lmer(log(wet_mass_g) ~ tree * treatment +  (1 | branch_code), data = ht_dat)

# Table for Figure 1
Anova(model_2)

# Parameter estimates and posthoc tests
# this should be "pairs" and report p-values
tree.lsm <- emmeans(model_2, ~ treatment | tree, type = "response") %>% 
  cld(adjust="scheffe", Letters=c("abcd")) %>%
  as.data.frame()

# Write the lsm object to a .csv for ggplot to use
write.csv(tree.lsm, "./Data/Models/model2.csv")


# Model 2 ver 2: Bag LRR #####
# Alternative version of figure for showing bird effects

# make a dataframe for wide form LRR calcs
ht_dat_wide <- ht_dat

# make a variable called "pair"
ht_dat_wide$pair <- substr(ht_dat_wide$branch_code,1,nchar(ht_dat_wide$branch_code)-1) 


# first pool by tree_id and sampling day

ht_dat_wide <- ht_dat_wide %>%
  group_by(tree, exo, treatment, pair) %>% 
  summarise(sum_biomass = sum(wet_mass_g)) %>%
  as.data.frame()
ht_dat_wide

# then pivot to wide format with counts per each combination of treatment*exo

ht_dat_wide <- ht_dat_wide %>% 
  pivot_wider(names_from = c("treatment"),
              values_from =  sum_biomass,
              names_sep = ".",
              names_prefix = "biomass.")

# calc LRR and drop irrational values
ht_dat_wide$LRR<- log(ht_dat_wide$biomass.bag / ht_dat_wide$biomass.control)
ht_dat_wide$LRR[is.infinite(ht_dat_wide$LRR)] <- NA

# glm for tree effect on LRR
model_2a <- glm(LRR ~ tree, data=ht_dat_wide)
plot(emmeans(model_2a, ~tree))

# Table for Figure 2
Anova(model_2a)
summary(model_2a)

# Parameter estimates and posthoc tests
# this should be "pairs" and report p-values
lrr_lsm <- emmeans(model_2a, ~ tree, type = "response")

lrr_cld <- lrr_lsm %>% 
  cld(adjust="scheffe", Letters=c("abcd")) %>%
  as.data.frame()

# mean, total, SEM
lrr_summary <- ht_dat_wide %>% 
  group_by(tree,exo) %>% 
  summarise(LRR_mean = mean(LRR, na.rm=TRUE), LRR_sem = std.error(LRR, na.rm=TRUE)) 

# merge biomass cld with biomass_summary
lrr_summary <- lrr_summary %>%
  left_join(y=lrr_cld, by = c("tree"))%>%
  as.data.frame()

# Write the lsm object to a .csv for ggplot to use
write.csv(lrr_summary, "./Data/Models/model2a.csv")


# posthoc test
# apply native vs non-native list to lsm table
lrr_group <- add_grouping(lrr_lsm, "Exo", "tree", native_list)
lrr_group

# write the contrast, then save the contrast values and round to nearest 3rd decimal
lrr_contrast <- emmeans(lrr_group, pairwise ~ Exo)

lrr_contrast$contrasts %>% 
  as.data.frame() %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  write.csv("./Data/Models/model2_posthoc.csv")

# final check - is it normally distributed?
# Yes hooray
hist(ht_dat_wide$LRR)
shapiro.test(ht_dat_wide$LRR)


# Model 3: Araneae #####
spider_glm <- glmer.nb(arachnids ~ exo * treatment + (1|branch_code) + (1|tree), data=ht_dat)

spider_lsm <- cld(emmeans(spider.glm, ~  treatment|exo, type="response"))

spider_lsm %>%
  as.data.frame() %>%
  write.csv("./Data/Models/spider_model.csv")

# Model 4: Hemiptera ####

hemiptera_glm <- glmer.nb(hemiptera ~ exo * treatment + (1|branch_code) + (1|tree), data=ht_dat)

hemiptera_lsm <- cld(emmeans(hemiptera_glm, ~  treatment|exo, type="response"))

hemiptera_lsm %>%
  as.data.frame() %>%
  write.csv("./Data/Models/hemiptera_model.csv")

# Model 5: Lepidoptera ####
lepidoptera_glm <- glmer.nb(lepidoptera ~ exo * treatment + (1|branch_code) + (1|tree), data=ht_dat)
 

lepidoptera_lsm <- cld(emmeans(lepidoptera_glm, ~  treatment*exo, type="response"))

lepidoptera_lsm %>%
  as.data.frame() %>%
  write.csv("./Data/Models/lepidoptera_model.csv")



# Model 6: Orthoptera ####
orthoptera_glm <- glmer.nb(orthopterids ~ exo * treatment + (1|branch_code) + (1|tree), data=ht_dat)

orthoptera_lsm <- cld(emmeans(orthoptera_glm, ~  treatment|exo, type="response"), adjust="scheffe")
# https://en.wikipedia.org/wiki/Scheff%C3%A9%27s_method

orthoptera_lsm %>%
  as.data.frame() %>%
  write.csv("./Data/Models/orthoptera_model.csv")


# Model 7: Herbivore N ####
model_7 <- glm(herbivore_average_n ~ tree, data=ht_dat)

mod7_lsm <- emmeans(model_7, ~ tree, type="response")

mod7_cld <- cld(mod7_lsm, type="response")

# mean, total, SEM
mod7_summary <- ht_dat %>% 
  filter(treatment == 'bag') %>%
  group_by(tree,exo) %>% 
  summarise(mean_nitrogen = mean(herbivore_average_n), sem = std.error(herbivore_average_n, na.rm=TRUE)) %>%
  left_join(y=mod7_cld , by = c("tree")) %>%
  as.data.frame()
# write csv for summary table to use in figure generation
write.csv(mod7_summary, "./Data/Models/model7.csv")


# posthoc test
# apply native vs non-native list to lsm table
mod7_group <- add_grouping(mod7_lsm, "Exo", "tree", native_list)
mod7_group

# write the contrast, then save the contrast values and round to nearest 3rd decimal
mod7_contrast <- emmeans(mod7_group, pairwise ~ Exo)

mod7_contrast$emmeans %>% 
  as.data.frame() %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  write.csv("./Data/Models/mod7_posthoc.csv")





# Model 8: Herbivore C ####
model_8 <- glm(herbivore_average_c ~ tree, data=ht_dat)


mod8_cld <- cld(emmeans(model_8, ~ tree, type="response"), adjust="scheffe", type="response", sort=FALSE)


# mean, total, SEM
mod8_summary <- ht_dat %>% 
  filter(treatment == 'bag') %>%
  group_by(tree,exo) %>% 
  summarise(mean_carbon = mean(herbivore_average_c), sem = std.error(herbivore_average_c, na.rm=TRUE)) %>%
  left_join(y=mod8_cld , by = c("tree")) %>%
  as.data.frame()
# write csv for summary table to use in figure generation
write.csv(mod8_summary, "./Data/Models/model8.csv")


# Model 9: Spider N ####
model_9 <- glm(spider_average_n ~ tree, data=ht_dat)

mod9_lsm <- emmeans(model_9, ~ tree, type="response")

mod9_cld <- cld(mod9_lsm, adjust="scheffe", type="response", sort=FALSE)


# mean, total, SEM
mod9_summary <- ht_dat %>% 
  filter(treatment == 'bag') %>%
  group_by(tree,exo) %>% 
  summarise(mean_nitrogen = mean(spider_average_n), sem = std.error(spider_average_n, na.rm=TRUE)) %>%
  left_join(y=mod9_cld , by = c("tree")) %>%
  as.data.frame()
# write csv for summary table to use in figure generation
write.csv(mod9_summary, "./Data/Models/model9.csv")


# posthoc test
# apply native vs non-native list to lsm table
mod9_group <- add_grouping(mod9_lsm, "Exo", "tree", native_list)
mod9_group

# write the contrast, then save the contrast values and round to nearest 3rd decimal
mod9_contrast <- emmeans(mod9_group, pairwise ~ Exo)

mod9_contrast$emmeans %>% 
  as.data.frame() %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  write.csv("./Data/Models/mod9_posthoc.csv")








# Model 10: Spider C ####
model_10 <- glm(spider_average_c ~ tree, data=ht_dat)

mod10_lsm <- emmeans(model_10, ~ tree, type="response")

mod10_cld <- cld(mod10_lsm, adjust="scheffe", type="response", sort=FALSE)


# mean, total, SEM
mod10_summary <- ht_dat %>% 
  filter(treatment == 'bag') %>%
  group_by(tree,exo) %>% 
  summarise(mean_carbon = mean(spider_average_c), sem = std.error(spider_average_c, na.rm=TRUE)) %>%
  left_join(y=mod10_cld , by = c("tree")) %>%
  as.data.frame()
# write csv for summary table to use in figure generation
write.csv(mod10_summary, "./Data/Models/model10.csv")












# Supplemental Analyses #####
# These are related analyses that are primarily found in the appendix and NOT the body of the manuscript

# Model 11: Leaf count AIC #####

# Model 12: NMDS #####
