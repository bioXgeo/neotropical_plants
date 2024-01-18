# Trouble shooting adding accepted names from lookup table to occurrence data

library(dplyr)

## Set file paths
data_path_L0<-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')

## Load files
GBIF_occ <- read.csv(file.path(data_path_L1, "TropicalAndes_GBIF_plant_occ_cleaned.csv"))
lookup_table <- read.csv(file.path(output_path,"plant_lookup_table.csv"))

#small subset for testing
lookup_table_5 <- lookup_table[1:5, 2:3]

GBIF_occ_subset_5 <- GBIF_occ %>%
  filter(species %in% lookup_table_5$Name_submitted)
nrow(GBIF_occ_subset_5)

# x is lookuptable  y is occurrences
GBIF_occ_subset_harmonized_5 <- right_join(lookup_table_5, GBIF_occ_subset_5, join_by(Name_submitted == species), keep = TRUE)
nrow(GBIF_occ_subset_harmonized_5)

## filter data to species in lookup_table$Names_submitted
GBIF_occ_subset <- GBIF_occ %>%
  filter(species %in% lookup_table$Name_submitted)
nrow(GBIF_occ_subset)

## add accepted species column from lookup table based on species column
GBIF_occ_subset_harmonized <- right_join(lookup_table, GBIF_occ_subset, join_by(Name_submitted == species), keep = TRUE)
nrow(GBIF_occ_subset_harmonized)
