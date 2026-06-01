# title: "Tropical Andes plant occurrence data GBIF"
# author: "Hazel J. Anderson, Jenna B. Baljunas"
# project: "Plant-Frugivore Diversity"
# collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
# overview: "This script retrieves plant occurrence data from GBIF using a subset of species with IUCN habitat designations ‘Forest-Subtropical/Tropical Moist Montane’ and/or ‘Forest-Subtropical/Tropical Moist Lowland’. Data is also retrived without the subset for species in the countries that make up the Tropical Andes to be spatially subset later, as not many plant species are assessed on the IUCN RedList. "
# data input: "IUCN/Tropical Andes Plants - Search Results/habitats.csv"
# data output: "TropAndes_GBIF_plant_occ.csv"
# date: "2023-07-18; 2025-09-22"
# notes: JB used HPCC


# Load required packages
library(dplyr); library(rgbif); library(raster); library(sf)


# Set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
output_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')

# # HPCC
# data_path_L0 <- file.path('/mnt/research/nasabio/data_2025/plants/L0')
# output_path_L0 <- file.path('/mnt/research/nasabio/data_2025/plants/L0')


# load functions
source("C:/GitHub_projects/neotropical_plants/code/Functions.R")

# # HPCC
# source("/mnt/ffs24/home/baljunas/Documents/neotropical_plants/code/Functions.R")


# # download data-- data downloaded on 2025-09-22
# occ_download(
# pred_in("taxonKey", c(220, 196)),
# pred("hasCoordinate", TRUE),
# pred("hasGeospatialIssue", FALSE),
# pred_in("country",c("EC","CO","VE", "PE", "BO")),
# format = "SIMPLE_CSV")
## creates '0006533-250920141307145'


# retrieve GBIF download, save file, and load into r
d <- occ_download_get('0006533-250920141307145', path = output_path_L0, overwrite = TRUE) %>%
  occ_download_import()


# read in Forest shape
TropicalAndes_IUCNHabitat_Forest <- read_sf(file.path(data_path_L0, "Forest_sf.shp"), layer = "Forest_sf")


# convert data frame to sf object
TropicalAndes_plant_occ <- st_as_sf(x = d, coords = c("decimalLongitude", "decimalLatitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")


# crop occurrence data to forest extent
TropicalAndes_plant_occ_forest <- TropicalAndes_plant_occ[TropicalAndes_IUCNHabitat_Forest,]


# extract lat and long to separate columns
TropicalAndes_plant_occ_forest <- TropicalAndes_plant_occ_forest %>%
  mutate(decimalLongitude = st_coordinates(.)[,1],
         decimalLatitude = st_coordinates(.)[,2])

TropicalAndes_plant_occ_forest <- as.data.frame(TropicalAndes_plant_occ_forest)


# fix column types 
TropicalAndes_plant_occ_forest$gbifID <- as.character(TropicalAndes_plant_occ_forest$gbifID)
TropicalAndes_plant_occ_forest$geometry <- NULL


# summary 
glimpse(TropicalAndes_plant_occ_forest)

data_summary(TropicalAndes_plant_occ_forest, TropicalAndes_plant_occ_forest$species, TropicalAndes_plant_occ_forest$genus, TropicalAndes_plant_occ_forest$family)


# extract species list
GBIF_SpeciesList <- unique(TropicalAndes_plant_occ_forest$species)


# write data to csv
write.csv(TropicalAndes_plant_occ_forest, file.path(output_path_L0,"TropicalAndes_GBIF_plant_occ.csv"))
write.csv(GBIF_SpeciesList, file.path(output_path_L0,"TropicalAndes_GBIF_plant_species.csv"))