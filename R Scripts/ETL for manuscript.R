# Trophic project 2021 Primary Figures
# Libraries ####
library("tidyverse")
library("readr")



# Data ######
# Branch data called "pilot" branch data until it is validated
# Branch data #####
hotf_dat <- read.csv("./Data/Originals/pilot branch data.csv")
trophic_dat <- read.csv("./Data/Output/clean_trophic_groups.csv")
cn_dat <- read.csv("./Data/Originals/GH21_Tritrophic_branch_CN.csv")
slug_dat <- read.csv("./Data/Originals/slug biomass data.csv")

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


# merge the cleaned insect community data (trophic_dat) with the branch-level data (hotf_dat)
ht_dat <- left_join(hotf_dat, trophic_dat)
str(ht_dat)

# Slug biomass fix #####
# Merge slug biomass data, change NAs to zero slug biomass
ht_dat <- left_join(ht_dat, slug_dat, by = c("branch_code","time_block"))
ht_dat$gastropod_biomass[is.na(ht_dat$gastropod_biomass)] <- 0  
# subtract from total biomass to get arthropod only biomass
ht_dat$wet_mass_g <- ht_dat$wet_mass_g - ht_dat$gastropod_biomass


# C:N branch level merge ######
# change tree_id2 to branch_code
cn_dat <- cn_dat %>% 
  rename(branch_code = tree_id2)

# merge the total trophic data with cn data
ht_dat <- left_join(ht_dat, cn_dat, by = c("branch_code"))

# Write merged data file #####
# Contains biomass data, arthropod community data, fixes with slugs & shadbush
# Merged with CN analysis

write.csv(x=ht_dat, file="./Data/Output/manuscript_dat.csv")
