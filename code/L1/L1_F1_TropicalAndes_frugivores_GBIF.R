# title: "Cleaning Tropical Andes frugivore GBIF occurrence records"
# author: "Hazel J. Anderson"
# project: "Plant-Frugivore Diversity"
# collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske, and Jenna B. Baljunas"
# overview: "Uses CoordinateCleaner to flag and remove problematic records from GBIF frugivore occurrence records."
# data input: "TropicalAndes_GBIF_frugivore_occ.csv"
# data output: "TropicalAndes_GBIF_frugivore_occ_cleaned.csv, TropicalAndes_GBIF_frugivore_occ_flagged.csv"
# date: "2023-09-24"
# notes: JB used HPCC


# set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
output_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')

##HPCC
#data_path <- file.path('/mnt/research/nasabio/data_2025/plants/L0')
#output_path <- file.path('/mnt/research/nasabio/data_2025/plants/L1')


# load required packages
library(countrycode); library(CoordinateCleaner); library(dplyr)


# read in data
TropicalAndes_GBIF_frugivore_occ <- read.csv(file.path(data_path_L0,"TropicalAndes_GBIF_frugivore_occ.csv"))


# clean data
# adapted code from https://ropensci.github.io/CoordinateCleaner/articles/Cleaning_GBIF_data_with_CoordinateCleaner.html 

# remove records without coordinates
TropicalAndes_GBIF_frugivore_occ <- TropicalAndes_GBIF_frugivore_occ %>%
  filter(!is.na(decimalLongitude)) %>%
  filter(!is.na(decimalLatitude))


flags <- clean_coordinates(x = TropicalAndes_GBIF_frugivore_occ,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids",
                                     "duplicates", "equal", "gbif",
                                     "institutions", "seas","zeros"))
summary(flags)


#exclude problematic records
TropicalAndes_GBIF_frugivore_occ_cleaned <- TropicalAndes_GBIF_frugivore_occ[flags$.summary,]
TropicalAndes_GBIF_frugivore_occ_flagged <- TropicalAndes_GBIF_frugivore_occ[!flags$.summary,]


# summary
glimpse(TropicalAndes_GBIF_frugivore_occ_cleaned)

#source("~/GitHub/neotropical_plants/code/data_summary.R")

data_summary <- function(records, species, genera, families){
  num_records <- nrow(records)
  num_species <- length(unique(species))
  num_genera <- length(unique(genera))
  num_families <- length(unique(families))
  return(cat("The number of records is", num_records, "\n", "The number of species is", num_species, "\n","The number of genera is", num_genera, "\n", "The number of families is", num_families))
}

data_summary(TropicalAndes_GBIF_frugivore_occ_cleaned, TropicalAndes_GBIF_frugivore_occ_cleaned$species, TropicalAndes_GBIF_frugivore_occ_cleaned$genus, TropicalAndes_GBIF_frugivore_occ_cleaned$family)


# write data to csv
write.csv(TropicalAndes_GBIF_frugivore_occ_cleaned, file = file.path(output_path_L1,"TropicalAndes_GBIF_frugivore_occ_cleaned.csv"))
