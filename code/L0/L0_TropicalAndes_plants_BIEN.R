# title: "Tropical Andes plant trait data BIEN"
# author: "Hazel J. Anderson"
# project: "Plant-Frugivore Diversity"
# collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske, Jenna B. Baljunas"
# overview: "This script retrieves plant trait data from the BIEN database for Tropical Andean countries."
# data input: "none"
# data output: "TropicalAndes_BIEN_traits.csv, AllDesired_BIEN_plant_traits.csv"
# date: "2023-10-17; 2025-09-22"
# notes: PLZ ran, JB couldn't on laptop and HPCC


# load required packages
library(BIEN); library(tidyr); library(dplyr)


# set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
output_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')

# PLZ filepaths
data_path_L0 <- file.path('~/Google Drive/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
output_path_L0 <- file.path('~/Google Drive/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')


# load functions
source("C:/GitHub_projects/plant-frugivore diversity/neotropical_plants/code/Functions.R")


# retrieved desired trait data from BIEN-- data downloaded on 2025-09-22
BIEN_trait_list()

trait_list <- c("whole plant dispersal syndrome", "whole plant height", "maximum whole plant longevity", "maximum whole plant height", "minimum whole plant height", "longest whole plant longevity", "fruit type", "maximum fruit length", "minimum fruit length", "seed mass", "whole plant growth form", "whole plant growth form diversity", "plant fruiting duration")


# list of desired countries
TA_countries <- c("Bolivia", "Ecuador", "Venezuela", "Colombia", "Peru")


# retrieve all BIEN records for desired traits
AllDesired_BIEN_plant_traits <- BIEN_trait_trait(trait_list, all.taxonomy = TRUE, political.boundaries = TRUE, source.citation = TRUE)


# retrieve all BIEN records for desired traits in Tropical Andes countries
TropicalAndes_BIEN_plant_traits <- BIEN_trait_country(TA_countries, trait_list, all.taxonomy = TRUE, political.boundaries = TRUE, source.citation = TRUE)


# data summary
glimpse(AllDesired_BIEN_plant_traits)

data_summary(AllDesired_BIEN_plant_traits, AllDesired_BIEN_plant_traits$scrubbed_species_binomial, AllDesired_BIEN_plant_traits$scrubbed_genus, AllDesired_BIEN_plant_traits$scrubbed_family)

AllDesired_BIEN_plant_traits %>% count(trait_name)

# desired traits in Tropical Andes countries
glimpse(TropicalAndes_BIEN_plant_traits)

data_summary(AllDesired_BIEN_plant_traits, TropicalAndes_BIEN_plant_traits$scrubbed_species_binomial, TropicalAndes_BIEN_plant_traits$scrubbed_genus, TropicalAndes_BIEN_plant_traits$scrubbed_family)

TropicalAndes_BIEN_plant_traits %>% count(trait_name)


# write data to csv
write.csv(AllDesired_BIEN_plant_traits, file.path(output_path_L0,"AllDesired_BIEN_plant_traits.csv"))
write.csv(TropicalAndes_BIEN_plant_traits, file.path(output_path_L0,"TropicalAndes_BIEN_plant_traits.csv"))
