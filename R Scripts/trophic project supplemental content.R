# Analysis, figures, and tables for Appendix 1 in GH Trophic Project 2022



# Libraries



# Data

# same inputs as Figure 1b in main manuscript, just change exo to tree

# fig s1
# Figure s1: Biomass among 10 host plant species
# Drop control branches
bagged_dat <- subset(hotf_dat, treatment == "bag")

model_1 <- lmer(log(wet_mass_g) ~ tree +  (1 | branch_code), data = bagged_dat)

cld(emmeans(model_1, ~ tree, type="response"), adjust="scheffe", type="response")

# pooled contrast of natives vs. non-natives using an emmeans reference grid

# figure generation
# write a summary table of totals and means for insects by host plant
# mean, total, SEM
biomass_summary <- hotf_dat %>% 
  filter(treatment == 'bag') %>%
  group_by(tree,exo) %>% 
  summarise(biomass_mean = mean(wet_mass_g), SE = std.error(wet_mass_g, na.rm=TRUE)) 

# biomass pub fig #####
biomass_fig <- ggplot(biomass_summary, aes(x=biomass_mean,y=reorder(tree,-desc(biomass_mean)),fill=exo)) +
  geom_bar(stat="identity", width=0.6, position="dodge") +
  theme_bw(base_size = 16) +
  scale_fill_manual(values = c("gray79", "gray45")) +
  geom_errorbar(aes(xmin=biomass_mean-(SE/2), xmax=biomass_mean+(SE/2)), width=0.1) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y="Plant species", x="Average arthropod biomass on bagged branches (g)", fill="Plant type") + 
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  theme(legend.position=c(0.8,0.1)) +
  theme(legend.title = element_blank())
biomass_fig



# both analyses below have the same problem of showing no difference in biomass among species
# mostly due to type 2 error from p-value adjustments with posthoc tests
# same results no matter which adjust= you use

# pilot analysis with only native bagged
bagged_dat_nat <- subset(bagged_dat, exo == "Native")

model_1 <- lmer(log(wet_mass_g) ~ tree +  (1 | branch_code), data = bagged_dat_nat)

cld(emmeans(model_1, ~ tree, type="response"), adjust="scheffe", type="response")

# pilot analysis with only non-native bagged
bagged_dat_non <- subset(bagged_dat, exo == "Non-native")

model_1 <- lmer(log(wet_mass_g) ~ tree +  (1 | branch_code), data = bagged_dat_non)

cld(emmeans(model_1, ~ tree, type="response"), adjust="scheffe", type="response")










# Figures s1-s4: Major insect groups variation in counts among host plants






# Figure s5: ruling out leaf area as a primary determinant of biomass patterns