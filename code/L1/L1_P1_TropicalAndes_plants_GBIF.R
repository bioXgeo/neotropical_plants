# title: "Cleaning Tropical Andes plant GBIF occurrence records"
# author: "Hazel J. Anderson"
# project: "Plant-Frugivore Diversity"
# collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske, Jenna B. Baljunas"
# overview: "Uses CoordinateCleaner to flag and remove problematic records from GBIF plant occurrence records."
# data input: "TropicalAndes_GBIF_plant_occ.csv"
# data output: "TropicalAndes_GBIF_plant_occ_cleaned.csv", "TropicalAndes_GBIF_plant_occ_flagged.csv"
# date: "2023-07-25; 2025-10-21"
# output: html_document
# notes: JB used HPCC


# load required packages
library(countrycode); library(CoordinateCleaner); library(dplyr)


# set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
output_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')

## HPCC
# data_path_L0 <- file.path('/mnt/research/nasabio/data_2025/plants/L0')
# output_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')


# load functions
source("C:/GitHub_projects/plant-frugivore diversity/neotropical_plants/code/Functions.R")

## HPCC
# source("/mnt/ffs24/home/baljunas/Documents/neotropical_plants/code/Functions.R")


# read in data
TropicalAndes_GBIF_plant_occ <- read.csv(file.path(data_path_L0,"TropicalAndes_GBIF_plant_occ.csv"))

# clean data
# Adapted code from https://ropensci.github.io/CoordinateCleaner/articles/Cleaning_GBIF_data_with_CoordinateCleaner.html 


# remove records without coordinates
TropicalAndes_GBIF_plant_occ <- TropicalAndes_GBIF_plant_occ %>%
  filter(!is.na(decimalLongitude)) %>%
  filter(!is.na(decimalLatitude))


flags <- clean_coordinates(x = TropicalAndes_GBIF_plant_occ,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids",
                                     "duplicates", "equal", "gbif",
                                     "institutions", "seas","zeros"))
summary(flags)



# exclude problematic records
TropicalAndes_GBIF_plant_occ_cleaned <- TropicalAndes_GBIF_plant_occ[flags$.summary,]
TropicalAndes_GBIF_plant_occ_flagged <- TropicalAndes_GBIF_plant_occ[!flags$.summary,]


# summary
glimpse(TropicalAndes_GBIF_plant_occ_cleaned)

data_summary(TropicalAndes_GBIF_plant_occ_cleaned, TropicalAndes_GBIF_plant_occ_cleaned$species, TropicalAndes_GBIF_plant_occ_cleaned$genus, TropicalAndes_GBIF_plant_occ_cleaned$family)


# write data to csv
write.csv(TropicalAndes_GBIF_plant_occ_cleaned, file = file.path(output_path_L1,"TropicalAndes_GBIF_plant_occ_cleaned.csv"))

write.csv(TropicalAndes_GBIF_plant_occ_flagged, file = file.path(output_path_L1,"TropicalAndes_GBIF_plant_occ_flagged.csv"))
