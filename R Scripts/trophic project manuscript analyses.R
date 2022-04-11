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




# Data ######
# Branch data called "pilot" branch data until it is validated
# Branch data #####
hotf_dat <- read.csv("./Data/Originals/pilot branch data.csv")

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






# Model 1a: Bird x Tree #####
# Figure 1: Bird effect across 10 host plants
model_1 <- lmer(log(wet_mass_g) ~ tree * treatment +  (1 | branch_code), data = hotf_dat)

# Table for Figure 1
Anova(model_1)

# Parameter estimates and posthoc tests
tree.lsm <- emmeans(model_1, ~ treatment | tree, type = "response") %>% cld()

# Exo model
# emmeans(model_1, ~ treatment | exo, type = "response") %>% cld() %>% plot()

# Fig 1 #####
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
# Supplemental figures show the variation in abundance among all plants

bag_effect <- function(data, insect_group){
  output <- mean(hotf_dat[,insect_group], na.rm=TRUE)
  return(output)}


mean(hotf_dat[,"wet_mass_g"], na.rm=TRUE)
C
bag_effect(data=hotf_dat, insect_group = "wet_mass_g")

# did a bad thing and replaced "non-native" with "nonnative" to prevent syntax errors
trophic_bug_dat <- read.csv(file="./Data/Output/clean_trophic_groups.csv")
str(trophic_bug_dat)

# arachnids example ####

spider.glm <- glmer.nb(arachnids ~ exo * treatment + (1|branch_code), data=trophic_bug_dat)
summary(spider.glm)

plot(emmeans(spider.glm, ~ treatment*exo), type="response")
plot(emmeans(spider.glm, ~ exo,), type="response")

# get hedged g for spiders with the treatment*exo pairs

# install.packages("effsize")
# library("effsize")

# make a dataframe for hedges g calculation
str(trophic_bug_dat)

# make a variable called "pair"
trophic_bug_dat$pair <- substr(trophic_bug_dat$branch_code,1,nchar(trophic_bug_dat$branch_code)-1) 


# first pool by tree_id and sampling day

spider_dat <- trophic_bug_dat %>%
  group_by(tree, exo, treatment, pair) %>% 
  summarise(sum_spiders = sum(arachnids)) %>%
  as.data.frame()
spider_dat


# then pivot to wide format with counts per each combination of treatment*exo

spider_dat_wide <- spider_dat %>% 
    pivot_wider(names_from = c("treatment"),
                values_from =  sum_spiders,
                names_sep = ".",
                names_prefix = "spider.")

# then split by exo and non-exo
spider_dat_exo <- subset(spider_dat_wide, exo=="Nonnative")
spider_dat_nat <- subset(spider_dat_wide, exo=="Native")

# then do a cohen.d for each paired comparison of bagged (among exotic, then native)

cd_1 <- cohen.d(spider_dat_exo$spider.bag, spider_dat_exo$spider.control, paired=TRUE, )
cd_1

cd_2 <- cohen.d(spider_dat_nat$spider.bag, spider_dat_nat$spider.control, paired=TRUE)
cd_2

# make a table from the cd_1 and cd_2
# stuck here #################
a <- c(1,2)
df <- data.frame(a)
df

df$exo_ci <- cd_1$conf.int


df



# trying LRRR ######
str(spider_dat_wide)

spider_dat_wide$LRR<- log(spider_dat_wide$spider.bag / spider_dat_wide$spider.control)
spider_dat_wide$LRR[is.infinite(spider_dat_wide$LRR)] <- NA

model_a <- glm(LRR ~ exo, data=spider_dat_wide)

plot(emmeans(model_a, ~exo))

# Figure 3: C:N content among native and non-native plants for spiders and insect herbivores
# should be a simple clustered bar chart with no treatment effects






