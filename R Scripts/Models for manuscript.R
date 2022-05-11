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

# Model 1: Bagged branch biomass #####
model_1 <- lmer(log(wet_mass_g) ~ tree +  (1 | branch_code), data = subset(ht_dat, treatment="bag"))

biomass_cld <- cld(emmeans(model_1, ~ tree, type="response"), adjust="scheffe", type="response")

# pooled contrast of natives vs. non-natives using an emmeans reference grid



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

# write the arranged model outputs (both posthoc tests and mean/SE)
write.csv(biomass_summary, "./Data/Models/model1.csv")






# Model 2: Bag effects #####
# Figure 1: Bird effect across 10 host plants
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

