#title: "Frugivore occurrence subset by species with complete trait records"
#author: "Hazel J. Anderson, Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske, Kelly Kapsar"
#overview: "This script subsets frugivore occurrence data to species with complete trait coverage."
# data input: "TropicalAndes_mammal_traits_subset.csv", "TropicalAndes_bird_traits_subset.csv", "TropicalAndes_GBIF_frugivore_occ_cleaned.csv"
#data output: "TropicalAndes_GBIF_frugivore_occ_cleaned_subset.csv"
#date: "2023-10-04; 2025-03-10"
#output: html_document
#notes: JB used HPCC


# load required packages
library(dplyr)
#run if not installed remotes::install_github("FRBCesab/funbiogeo")
library(funbiogeo)


# set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')


# load functions
source("C:/GitHub_projects/neotropical_plants/code/Functions.R")


# read in data
mammal_subset <- read.csv(file.path(data_path_L1, file = "TropicalAndes_mammal_traits_subset.csv"))
bird_subset <- read.csv(file.path(data_path_L1, file = "TropicalAndes_bird_traits_subset.csv"))
frugivore_occ <- read.csv(file.path(data_path_L1, file = "TropicalAndes_GBIF_frugivore_occ_cleaned.csv"))


#### check trait coverage ####
mammal_unique <- mammal_subset %>%
  distinct(IUCN_species_name, .keep_all = TRUE)

bird_unique <- bird_subset %>%
  distinct(IUCN_species_name, .keep_all = TRUE)


mammal_complete <- na.omit(mammal_unique)
bird_complete <- na.omit(bird_unique)


summary(Frugivoria_complete)


#### create list of species with complete trait records ####
mammal_species <- unique(mammal_complete$IUCN_species_name)
bird_species <- unique(bird_complete$IUCN_species_name)


# subset frugivore occurrence data by species list
mammal_occ_subset <- frugivore_occ %>%
  filter(species %in% mammal_species)

bird_occ_subset <- frugivore_occ %>%
  filter(species %in% bird_species)


# summary
glimpse(frugivore_occ_subset)


data_summary(frugivore_occ_subset, frugivore_occ_subset$species, frugivore_occ_subset$genus, frugivore_occ_subset$family)

data_summary(mammal_occ_subset, mammal_occ_subset$species, mammal_occ_subset$genus, mammal_occ_subset$family)

data_summary(bird_occ_subset, bird_occ_subset$species, bird_occ_subset$genus, bird_occ_subset$family)


# Write data to csv
write.csv(mammal_occ_subset, file.path(output_path_L1,"TropicalAndes_GBIF_mammal_occ_cleaned_subset.csv"))
write.csv(bird_occ_subset, file.path(output_path_L1,"TropicalAndes_GBIF_bird_occ_cleaned_subset.csv"))

