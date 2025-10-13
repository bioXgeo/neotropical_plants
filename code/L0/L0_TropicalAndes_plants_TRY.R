# title: "Tropical Andes plant trait data TRY"
# author: "Hazel J. Anderson"
# project: "Plant-Frugivore Diversity"
# collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske, and Jenna B. Baljunas"
# overview: "This script retrieves plant trait data from the TRY database for plant species list."
# data input: "none"
# data output: "TropicalAndes_TRY_traits.csv"
# date: "2023-07-18"
# output: html_document
# notes: JB ran on HPCC

  
# Set file paths
data_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
output_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')

## HPCC
# data_path_L0 <- file.path('/mnt/research/nasabio/data_2025/plants/L0')
# output_path_L0 <- file.path('/mnt/research/nasabio/data_2025/plants/L0')


# Load required packages
library(rtry); library(dplyr); library(tidyr)


# Load TRY data
#Data was obtained from https://www.try-db.org/TryWeb/Prop0.php on 2025-09-23
try_data <- rtry_import("G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0/TRY/44066_23092025231532/44066.txt")

##HPCC
#try_data <- rtry_import('/mnt/research/nasabio/data_2025/plants/L0/44066.txt')


# Summary
glimpse(try_data)


# Keep only record with non-NA trait IDs
try_data <- try_data %>%
  drop_na(TraitID)
nrow(try_data)

print("The number of records is")
nrow(try_data)
print("The number of species is" )
length(unique(try_data$AccSpeciesName))

try_data %>% count(TraitName)


# Get units for each trait
try_data %>% distinct(TraitName, UnitName)


# Write data to csv
write.csv(try_data, file = file.path(output_path_L0,"TropicalAndes_TRY_plant_traits.csv"))
