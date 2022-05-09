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


# Supplemental Analyses #####
# These are related analyses that are primarily found in the appendix and NOT the body of the manuscript

