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

# biomass posthoc test
Anova(model_1)
summary(model_1)

native_list <- c("Non-native","Non-native","Native","Non-native","Non-native",
                 "Native","Native","Native","Native", "Native")

biomass_group <- add_grouping(biomass_lsm, "Exo", "tree", native_list)
str(biomass_group)

biomass_contrast <- emmeans(biomass_group, pairwise ~ Exo)

biomass_contrast$contrasts %>% 
  as.data.frame() %>% 
  mutate(across(where(is.numeric), ~ round(., 3)))



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
lrr.lsm <- emmeans(model_2a, ~ tree, type = "response") %>% 
  cld(adjust="scheffe", Letters=c("abcd")) %>%
  as.data.frame()

# mean, total, SEM
lrr_summary <- ht_dat_wide %>% 
  group_by(tree,exo) %>% 
  summarise(LRR_mean = mean(LRR, na.rm=TRUE), LRR_sem = std.error(LRR, na.rm=TRUE)) 

# merge biomass cld with biomass_summary
lrr_summary <- lrr_summary %>%
  left_join(y=lrr.lsm, by = c("tree"))%>%
  as.data.frame()

# Write the lsm object to a .csv for ggplot to use
write.csv(lrr_summary, "./Data/Models/model2a.csv")


# posthoc test
# fix this contrast later ######

model_2a_posthoc <- glm(LRR ~ exo, data=ht_dat_wide)

model_2a_posthoc <- emmeans(model_2a_posthoc, ~exo) %>% as.data.frame()

write.csv(model_2a_posthoc, "./Data/Models/model2a_posthoc.csv")

# is it normally distributed?
# Yes hooray
hist(ht_dat_wide$LRR)
shapiro.test(ht_dat_wide$LRR)




# Model 3: Araneae #####

# Model 4: Hemiptera ####

# Model 5: Lepidoptera ####

# Model 6: Orthoptera ####




# Model 7: Herbivore N ####
model_7 <- glm(herbivore_average_n ~ tree, data=ht_dat)
Anova(model_7)
plot(emmeans(model_7, ~ tree))

mod7_cld <- cld(emmeans(model_7, ~ tree, type="response"), adjust="scheffe", type="response")

# pooled contrast of natives vs. non-natives using an emmeans reference grid
Native = c(0, 0, 1, 0, 0, 1, 1, 1, 1, 1)
Exotic = c(1, 1, 0, 1, 1, 0, 0, 0, 0, 0)

# do contrast among native and non-native plants
# I dont think this is doing the contrast correct, the p-value is absurdly low
mod7_contrast <- contrast(emmeans(model_7, ~ tree, type="response"), 
            method = list("Native - Exotic" = Native - Exotic))
mod7_contrast


# mean, total, SEM

mod7_summary <- ht_dat %>% 
  filter(treatment == 'bag') %>%
  group_by(tree,exo) %>% 
  summarise(mean_nitrogen = mean(herbivore_average_n), sem = std.error(herbivore_average_n, na.rm=TRUE)) 

# merge biomass cld with summary
mod7_summary  <- mod7_summary  %>%
  left_join(y=mod7_cld , by = c("tree"))%>%
  as.data.frame()

# write csv for summary table to use in figure generation
write.csv(mod7_summary, "./Data/Models/model7.csv")


# Model 8: Herbivore CN ####


# Model 9: Spider N ####



# Model 10: Spider CN ####






# Supplemental Analyses #####
# These are related analyses that are primarily found in the appendix and NOT the body of the manuscript

# Model 11: Leaf count AIC #####

# Model 12: NMDS #####
