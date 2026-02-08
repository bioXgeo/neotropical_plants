#title: "Subsetting plant species list by fruiting species"
#author: "Hazel J. Anderson, Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "This script subsets plant occurrence and trait data by fruiting plant species."
#data input: "TropicalAndes_GBIF_plant_occ_harmonized.csv", "TropicalAndes_TRY_plant_traits_harmonized.csv", "TropicalAndes_BIEN_plant_traits_harmonized.csv", "TropicalAndes_GIFT_plant_traits_harmonized.csv"
#data output: "TropicalAndes_GBIF_plant_occ_harmonized_subset.csv", "TropicalAndes_all_plant_traits_harmonized_subset.csv"
#date: "2023-07-25; 2025-10-03"
#output: html_document
#notes: JB used HPCC


# Load required packages
library(dplyr); library(tidyr)


# Set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')

##HPCC
#data_path_L0 <- file.path('/mnt/research/nasabio/data_2025/plants/L0')
#data_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')
#output_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')
#figure_path <- file.path('/mnt/research/nasabio/data_2025/plants/figures')


# read in harmonized data
plant_occ <- read.csv(file.path(data_path_L1, "TropicalAndes_GBIF_plant_occ_harmonized.csv"))
TRY_traits <- read.csv(file.path(data_path_L1,"TropicalAndes_TRY_plant_traits_harmonized.csv"))
BIEN_traits <- read.csv(file.path(data_path_L1,"TropicalAndes_BIEN_plant_traits_harmonized.csv"))
GIFT_traits <- read.csv(file.path(data_path_L1,"TropicalAndes_GIFT_plant_traits_harmonized.csv"))


# get species list from plant_occ data
plant_species <- unique(plant_occ$Accepted_species)
length(plant_species)


# subset TRY, BIEN, and GIFT by species list
# TRY
nrow(TRY_traits)
TRY_filtered <- TRY_traits %>%
  filter(Accepted_species %in% plant_species)
nrow(TRY_filtered)

TRY_traits %>%
  distinct(Accepted_species, TraitName) %>%
  count(TraitName)


# BIEN
nrow(BIEN_traits)
BIEN_filtered <- BIEN_traits %>%
  filter(Accepted_species %in% plant_species)
nrow(BIEN_filtered)

BIEN_traits %>%
  count(trait_name)

BIEN_traits %>%
  distinct(Accepted_species, trait_name) %>%
  count(trait_name)


# GIFT
nrow(GIFT_traits)
GIFT_filtered <- GIFT_traits %>%
  filter(Accepted_species %in% plant_species)
nrow(GIFT_filtered)

GIFT_traits %>%
  count(trait_name)

GIFT_traits %>%
  distinct(Accepted_species, trait_name) %>%
  count(trait_name)


# combine trait information from TRY, BIEN, GIFT

# TRY
TRYTraitTypes <- TRY_filtered %>%
  group_by(TraitName) %>%
  distinct(DataName)

unique(TRYTraitTypes$TraitName)

colnames(TRY_filtered)

TRY_filtered_subset <- TRY_filtered[ , c("Accepted_species", "TraitName", "OrigValueStr", "UnitName")]
colnames(TRY_filtered_subset) <- c("Accepted_species", "TraitName", "TraitValue", "Unit")

# save sources/references
TRY_references <- unique(TRY_filtered[ , c("Dataset", "Reference")])
write.csv(TRY_references, file.path(output_path_L1,"TRY_references.csv"))


# BIEN
unique(BIEN_filtered$trait_name)

colnames(BIEN_filtered)

BIEN_filtered_subset <- BIEN_filtered[ , c("Accepted_species", "trait_name", "trait_value", "unit")]
colnames(BIEN_filtered_subset) <- c("Accepted_species", "TraitName", "TraitValue", "Unit")

# save sources/references
BIEN_references <- unique(BIEN_filtered[ , c("url_source", "source_citation")])
write.csv(BIEN_references, file.path(output_path_L1,"BIEN_references.csv"))


# GIFT
unique(GIFT_filtered$trait_name)

colnames(GIFT_filtered)

GIFT_filtered_subset <- GIFT_filtered[ , c("Accepted_species", "trait_name", "trait_value")]
colnames(GIFT_filtered_subset) <- c("Accepted_species", "TraitName", "TraitValue")

#Add Unit column to GIFT dataframe

GIFT_filtered_subset <- GIFT_filtered_subset %>%
  mutate(Unit = case_when(
    TraitName == "Plant_height_mean" ~ "m",
    TraitName == "Plant_height_max" ~ "m",
    TraitName == "Plant_height_min" ~ "m",
    TraitName == "Seed_mass_mean" ~ "g",
    TraitName == "Seed_mass_min" ~ "g",
    TraitName == "Seed_mass_max" ~ "g",
    TraitName == "Fruit_length_min" ~ "cm",
    TraitName == "Fruit_length_max" ~ "cm",
    TraitName == "Fruit_length_mean" ~ "cm",
    TraitName == "Seed_length_max" ~ "mm",
    TraitName == "Seed_length_min" ~ "mm",
    TraitName == "Seed_length_mean" ~ "mm",
    TraitName == "Seed_width_max" ~ "mm",
    TraitName == "Seed_width_min" ~ "mm",
    TraitName == "Seed_width_mean" ~ "mm",
    TraitName == "Plant_lifespan" ~ "years",
    .default = ""
  ))


# add trait database source column to each subset
TRY_filtered_subset$DatabaseSource <- "TRY"
BIEN_filtered_subset$DatabaseSource <- "BIEN"
GIFT_filtered_subset$DatabaseSource <- "GIFT"


# combine all subsets into one dataframe
traits <- rbind(TRY_filtered_subset, BIEN_filtered_subset, GIFT_filtered_subset)
traits$Unit <- ifelse(traits$Unit=='',NA,traits$Unit)
dim(traits)


# remove rows without accepted species names
traits_clean <- traits[!(is.na(traits$Accepted_species) | traits$Accepted_species==""), ]
nrow(traits_clean)


traits_clean %>%
  count(TraitName)

traits_clean %>%
  distinct(Accepted_species, TraitName) %>%
  count(TraitName)


# create a species list of species with fruiting traits
fruit_traits <- c("Dispersal syndrome","Dispersal_syndrome_1","Dispersal_syndrome_2","Fruit dry mass", "Fruit length", "Fruit type", "Fruit/seed color", "Fruit/seed conspicuous", "Fruit_colour", "Fruit_dryness_1", "Fruit_length_max", "Fruit_length_mean", "Fruit_length_min", "Fruit_type_1", "Fruiting_end","Fruiting_start","fruit type","maximum fruit length", "minimum fruit length","plant fruiting duration", "whole plant dispersal syndrome")


# filter the dataframe for fruiting traits
fruiting_df <- traits_clean %>%
  filter(TraitName %in% fruit_traits)
dim(fruiting_df)

fruiting_df %>%
  distinct(Accepted_species, TraitName) %>%
  count(TraitName)


# extract unique species names
fruiting_species <- unique(fruiting_df$Accepted_species)
length(fruiting_species)


# subset trait data and occurrence data by fruiting species list
dim(plant_occ)
plant_occ_subset <- plant_occ %>%
  filter(Accepted_species %in% fruiting_species)
dim(plant_occ_subset)

dim(traits_clean)
traits_subset <- traits_clean%>%
  filter(Accepted_species %in% fruiting_species)
dim(traits_subset)

traits_subset %>%
  distinct(Accepted_species, TraitName) %>%
  count(TraitName)


# summary
cat("Number of occurrence records:", nrow(plant_occ_subset), "\n")
cat("Number of species with occurrence records:", length(unique(plant_occ_subset$Accepted_species)), "\n")


# the number of records and species per trait database
database_summary <- traits_subset %>%
  group_by(DatabaseSource) %>%
  summarise(
    num_records = n(),
    num_species = n_distinct(Accepted_species)
  )

# print the result
print(database_summary)


# write data to csv
write.csv(plant_occ_subset, file.path(output_path_L1, "TropicalAndes_GBIF_plant_occ_harmonized_subset.csv"))
write.csv(traits_subset, file.path(output_path_L1, "TropicalAndes_all_plant_traits_harmonized_subset.csv"))



