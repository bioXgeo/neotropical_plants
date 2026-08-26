# title: "Frugivoria frugivore trait subset"
# author: "Hazel J. Anderson, Jenna Baljunas"
# project: "Plant-Frugivore Diversity"
# collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske, Kelly Kaspar"
# overview: "Subsetting Frugivoria to the species and traits of interest."
# data input: "TropicalAndes_GBIF_frugivore_occ_cleaned.csv", "TropicalAndes_Frugivoria_mammal_traits.csv", "TropicalAndes_Frugivoria_bird_traits.csv"
# data output: "TropicalAndes_Frugivoria_traits_subset.csv", "TropicalAndes_mammal_traits_subset.csv", "TropicalAndes_bird_traits_subset.csv"
# date: "2023-10-31; 2025-03-10"
# notes: JB used HPCC


# Load required packages
library(tidyr); library(dplyr); library(funbiogeo); library(visdat); library(mice)


# set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')


# load functions
source("C:/GitHub_projects/neotropical_plants/code/Functions.R")


# Read in data
Frugivoria_m <- read.csv(file.path(data_path_L0, file = "TropicalAndes_Frugivoria_mammal_traits.csv"))
Frugivoria_b <- read.csv(file.path(data_path_L0, file = "TropicalAndes_Frugivoria_bird_traits.csv"))
frugivore_occ <- read.csv(file.path(data_path_L1, file = "TropicalAndes_GBIF_frugivore_occ_cleaned.csv"))


# Get species list from frugivore_occ data
frugivore_species <- unique(frugivore_occ$species)


#### Subset Frugivoria data by species list ####

mammal_filtered <- Frugivoria_m %>%
  filter(IUCN_species_name %in% frugivore_species)

bird_filtered <- Frugivoria_b %>%
  filter(IUCN_species_name %in% frugivore_species)


#### Mammals: keep only traits of interest: Body Mass (g), Diet Category, Diet Breadth, Habitat Breadth, and Generation Time (years) ####

mammal_subset <- mammal_filtered[ , c("species", "body_mass_e",  "diet_cat", "diet_breadth", "habitat_breadth", "generation_time")]

long_mammal_subset <- mammal_subset %>%
  gather(TraitName, TraitValue, -species)


na_records_count_mammals <- sum(is.na(long_mammal_subset$TraitValue))

mammal_species_with_NAs <- long_mammal_subset %>%
  filter(is.na(TraitValue)) %>%
  distinct(species) %>%
  nrow()

mammal_traits_with_NAs <- long_mammal_subset %>%
  filter(is.na(TraitValue)) %>%
  distinct(TraitName) %>%
  nrow()


# print results
cat("Number of NA records:", na_records_count_mammals, "\n")
cat("Number of species with NA records:", mammal_species_with_NAs, "\n")
cat("Number of traits with NA records:", mammal_traits_with_NAs, "\n")

# percentage of trait data
100*na_records_count_mammals/nrow(long_mammal_subset)


#### Birds: keep only traits of interest: Body Mass (g), Diet Category, Diet Breadth, Habitat Breadth, and Generation Time (years) ####

bird_subset <- bird_filtered[ , c("species", "body_mass_e",  "diet_cat", "diet_breadth", "habitat_breadth", "generation_time")]

long_bird_subset <- bird_subset %>%
  gather(TraitName, TraitValue, -species)


na_records_count_birds <- sum(is.na(long_bird_subset$TraitValue))

bird_species_with_NAs <- long_bird_subset %>%
  filter(is.na(TraitValue)) %>%
  distinct(species) %>%
  nrow()

bird_traits_with_NAs <- long_bird_subset %>%
  filter(is.na(TraitValue)) %>%
  distinct(TraitName) %>%
  nrow()


# print results
cat("Number of NA records:", na_records_count_birds, "\n")
cat("Number of species with NA records:", bird_species_with_NAs, "\n")
cat("Number of traits with NA records:", bird_traits_with_NAs, "\n")

# percentage of trait data
100*na_records_count_birds/nrow(long_bird_subset)


# summary
data_summary(mammal_subset, mammal_subset$species, mammal_subset$genus, mammal_subset$family)
data_summary(bird_subset, bird_subset$species, bird_subset$genus, bird_subset$family)


#### imputation ####

# mammals
vis_dat(mammal_subset)
vis_miss(mammal_subset)

mammal_subset_old <- mammal_subset

imp_model <- mice(mammal_subset, method='cart', maxit=20)
imputed_data <- complete(imp_model)
mammal_subset$generation_time <- imputed_data$generation_time
mammal_subset$body_mass_e <- imputed_data$body_mass_e

vis_dat(mammal_subset)
vis_miss(mammal_subset)


# birds
vis_dat(bird_subset)
vis_miss(bird_subset)

bird_subset_old <- bird_subset

imp_model <- mice(bird_subset, method='cart', maxit=20)
imputed_data <- complete(imp_model)
bird_subset$generation_time <- imputed_data$generation_time
bird_subset$body_mass_e <- imputed_data$body_mass_e

vis_dat(bird_subset)
vis_miss(bird_subset)


# write data to csv
write.csv(mammal_subset, file.path(output_path_L1,"TropicalAndes_mammal_traits_subset.csv"))
write.csv(bird_subset, file.path(output_path_L1,"TropicalAndes_bird_traits_subset.csv"))


# package citations and session info
library(report)
report::cite_packages()

devtools::session_info()
