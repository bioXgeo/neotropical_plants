# title: "Tropical Andes montane and lowland forest bird and mammal occurrence data from GBIF"
# author: "Hazel J. Anderson, Jenna B. Baljunas"
# project: "Plant-Frugivore Diversity"
# collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
# overview: "This script retrieves plant occurrence data from GBIF using the species list from Frugivoria."
# data input: "TropicalAndes_Frugivoria_frugivore_traits_species.csv, Forest_sf.shp"
# data output: "TropicalAndes_GBIF_frugivore_occ.csv, TropicalAndes_GBIF_frugivore_occ_species.csv"
# date: "2023-07-25; 2025-09-22"
# notes: "JB used HPCC"


# load required packages
library(dplyr); library(rgbif); library(sf)


# set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
output_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')

## HPCC
#data_path_L0 <- file.path('/mnt/research/nasabio/data_2025/plants/L0')
#output_path_L0 <- file.path('/mnt/research/nasabio/data_2025/plants/L0')


# load functions
source("C:/GitHub_projects/neotropical_plants/code/Functions.R")

# # HPCC
# source("/mnt/ffs24/home/baljunas/Documents/neotropical_plants/code/Functions.R")


# read in Frugivoria species list
frugivore_species <- read.csv(file.path(data_path_L0,"TropicalAndes_Frugivoria_frugivore_traits_species.csv"))


# download data-- data downloaded on 2025-09-22
# gbif_taxon_keys <- frugivore_species$x %>% 
#   name_backbone_checklist() %>% # match to backbone 
#   filter(!matchType == "NONE") %>% # get matched names
#   pull(usageKey) 
# # Make GBIF request
# occ_download(
#   pred_in("taxonKey", gbif_taxon_keys),
#   pred("hasCoordinate", TRUE),
#   pred("hasGeospatialIssue", FALSE),
#   pred_in("country",c("EC","CO","VE", "PE", "BO")), 
#   format = "SIMPLE_CSV")
## creates '0006431-250920141307145'


# import data
d <- occ_download_get('0006431-250920141307145',
                      path = output_path_L0, overwrite = TRUE) %>%
  occ_download_import()


# subset occurrence to Tropical Andes IUCN Montane and Lowland forest

# read in Forest shape
TropicalAndes_IUCNHabitat_Forest <- read_sf(file.path(data_path_L0, "Forest_sf.shp"), layer = "Forest_sf")

# convert data frame to sf object
TropicalAndes_frugivore_occ <- st_as_sf(x = d, coords = c("decimalLongitude", "decimalLatitude"), crs = "+proj=longlat +datum=WGS84 +no_defs")

# crop occurrence data to forest extent
TropicalAndes_frugivore_occ_forest <- TropicalAndes_frugivore_occ[TropicalAndes_IUCNHabitat_Forest,]

# extract lat and long to separate columns
TropicalAndes_frugivore_occ_forest <- TropicalAndes_frugivore_occ_forest %>%
  mutate(decimalLongitude = sf::st_coordinates(.)[,1],
         decimalLatitude = sf::st_coordinates(.)[,2])

TropicalAndes_frugivore_occ_forest <- as.data.frame(TropicalAndes_frugivore_occ_forest)

# fix column type 
TropicalAndes_frugivore_occ_forest$gbifID <- as.character(TropicalAndes_frugivore_occ_forest$gbifID)
TropicalAndes_frugivore_occ_forest$geometry <- NULL


# summary
glimpse(TropicalAndes_frugivore_occ_forest)

data_summary(TropicalAndes_frugivore_occ_forest, TropicalAndes_frugivore_occ_forest$species, TropicalAndes_frugivore_occ_forest$genus, TropicalAndes_frugivore_occ_forest$family)

# extract species list
GBIF_occ_SpeciesList <- unique(TropicalAndes_frugivore_occ_forest$species)


# write data to csvs
write.csv(TropicalAndes_frugivore_occ_forest, file = file.path(output_path_L0,"TropicalAndes_GBIF_frugivore_occ.csv"))
write.csv(GBIF_occ_SpeciesList, file = file.path(output_path_L0,"TropicalAndes_GBIF_frugivore_occ_species.csv"))


# Package Citations and Session Info
library(report)
report::cite_packages()

devtools::session_info()
