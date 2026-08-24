# title: "Creating Diversity Input Objects"
# author: "Hazel J. Anderson, Jenna B. Baljunas"
# project: "Plant-Frugivore Diversity"
# collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
# overview: "Projecting occurrence data and creating presence-absence matrix for plants and frugivores. "
# data input: "TropicalAndes_GBIF_plant_occ_harmonized_subset_final.csv", "TropicalAndes_GBIF_frugivore_occ_cleaned_subset.csv", "TropicalAndes_GBIF_mammal_occ_cleaned_subset.csv", "TropicalAndes_GBIF_bird_occ_cleaned_subset.csv", "Forest_sf.shp", "TropicalAndes_Frugivoria_traits_subset.csv", "TropicalAndes_bird_traits_subset.csv", "TropicalAndes_mammal_traits_subset.csv", "TropicalAndes_imputed_plant_traits2.csv"
# data output: "all_points_maps.png", "plants_sf_species.rds", "frugivores_sf_species.rds", "mammals_sf_species.rds", "birds_sf_species.rds", "Americas.rds", "TApoly.rds", "TropicalAndes_IUCNHabitat_Forest.rds", "plant_PAM_100km.rds", "frugivore_PAM_100km.rds", "mammal_PAM_100km.rds", "bird_PAM_100km.rds", "plant_PAM_75km.rds", "frugivore_PAM_75km.rds", "mammal_PAM_75km.rds", "bird_PAM_75km.rds", "plant_PAM_50km.rds", "frugivore_PAM_50km.rds", "mammal_PAM_50km.rds", "bird_PAM_50km.rds", "plant_PAM_25km.rds", "frugivore_PAM_25km.rds", "mammal_PAM_25km.rds", "bird_PAM_25km.rds", "plant_PAM_10km.rds", "frugivore_PAM_10km.rds", "mammal_PAM_10km.rds", "bird_PAM_10km.rds", "plant_PAM_5km.rds", "frugivore_PAM_5km.rds", "mammal_PAM_5km.rds", "bird_PAM_5km.rds", "plant_traits_df_final.rds", "frugivore_traits_df_final.rds", "bird_traits_df_final.rds", "mammal_traits_df_final.rds", "site_loc_key_plant_100km.rds", "site_loc_key_frugivore_100km.rds", "site_loc_key_mammal_100km.rds", "site_loc_key_bird_100km.rds", "PAM_plant_site_final_100km.rds", "PAM_frugivore_site_final_100km.rds", "PAM_mammal_site_final_100km.rds", "PAM_bird_site_final_100km.rds", "site_loc_key_plant_75km.rds", "site_loc_key_frugivore_75km.rds", "site_loc_key_mammal_75km.rds", "site_loc_key_bird_75km.rds", "PAM_plant_site_final_75km.rds", "PAM_frugivore_site_final_75km.rds", "PAM_mammal_site_final_75km.rds", "PAM_bird_site_final_75km.rds", "site_loc_key_plant_50km.rds", "site_loc_key_frugivore_50km.rds", "site_loc_key_mammal_50km.rds", "site_loc_key_bird_50km.rds", "PAM_plant_site_final_50km.rds", "PAM_frugivore_site_final_50km.rds", "PAM_mammal_site_final_50km.rds", "PAM_bird_site_final_50km.rds", "site_loc_key_plant_25km.rds", "site_loc_key_frugivore_25km.rds", "site_loc_key_mammal_25km.rds", "site_loc_key_bird_25km.rds", "PAM_plant_site_final_25km.rds", "PAM_frugivore_site_final_25km.rds", "PAM_mammal_site_final_25km.rds", "PAM_bird_site_final_25km.rds", "site_loc_key_plant_10km.rds", "site_loc_key_frugivore_10km.rds", "site_loc_key_mammal_10km.rds", "site_loc_key_bird_10km.rds", "PAM_plant_site_final_10km.rds", "PAM_frugivore_site_final_10km.rds", "PAM_mammal_site_final_10km.rds", "PAM_bird_site_final_10km.rds", "site_loc_key_plant_5km.rds", "site_loc_key_frugivore_5km.rds", "site_loc_key_mammal_5km.rds", "site_loc_key_bird_5km.rds", "PAM_plant_site_final_5km.rds", "PAM_frugivore_site_final_5km.rds", "PAM_mammal_site_final_5km.rds", "PAM_bird_site_final_5km.rds"
# date: "2024-05-13; 2025-10-21"
# notes: JB used HPCC


# load required packages
library(letsR); library(mFD); library(vegan); library(rnaturalearth); library(sf); library(raster); library(fasterize); library(funbiogeo); library(dplyr); library(tidyr); library(ggspatial); library(ggplot2); library(ggpubr); library(rphylopic); library(patchwork); library(tibble); library(iNEXT)


# set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')

# new file paths
all_data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1/all_data')
all_output_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1/all_data')
all_data_figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures/all_data')

filtered_data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1/filtered_data')
filtered_output_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1/filtered_data')
filtered_data_figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures/filtered_data')

# # HPCC
# data_path_L0 <- file.path('/mnt/research/nasabio/data_2025/plants/L0')
# data_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')
# output_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')
# figure_path <- file.path('/mnt/research/nasabio/data_2025/plants/figures')


# load functions
source("C:/GitHub_projects/neotropical_plants/code/Functions.R")

# # HPCC
# source("/mnt/ffs24/home/baljunas/Documents/neotropical_plants/code/Functions.R")


# read in data

# occurrence records
TropicalAndes_plant_occ_forest <- read.csv(file.path(data_path_L1,"TropicalAndes_GBIF_plant_occ_harmonized_subset_final.csv"))
TropicalAndes_frugivore_occ_forest <- read.csv(file.path(data_path_L1,"TropicalAndes_GBIF_frugivore_occ_cleaned_subset.csv"))
TropicalAndes_mammal_occ_forest <- read.csv(file.path(data_path_L1, "TropicalAndes_GBIF_mammal_occ_cleaned_subset.csv"))
TropicalAndes_bird_occ_forest <- read.csv(file.path(data_path_L1, "TropicalAndes_GBIF_bird_occ_cleaned_subset.csv"))
TropicalAndes_IUCNHabitat_Forest <- read_sf(file.path(data_path_L0, "Forest_sf.shp"), layer = "Forest_sf")

# traits
bird_traits <- read.csv(file.path(data_path_L1,"TropicalAndes_bird_traits_subset.csv"))
mammal_traits <- read.csv(file.path(data_path_L1,"TropicalAndes_mammal_traits_subset.csv"))
plant_traits <- read.csv(file.path(data_path_L1,"TropicalAndes_imputed_plant_traits2.csv"))


# convert data to spatial data
plants.sf <- st_as_sf(TropicalAndes_plant_occ_forest, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

mammals.sf <- st_as_sf(TropicalAndes_mammal_occ_forest, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

birds.sf <- st_as_sf(TropicalAndes_bird_occ_forest, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)


# polygons of countries
worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")
Americas <- ne_countries(continent = c("North America", "South America"), returnclass = "sf")
#polygon of Tropical Andes
TApoly <- worldMap %>% filter(sovereignt == "Bolivia" |sovereignt == "Ecuador" | sovereignt == "Venezuela" | sovereignt == "Colombia" | sovereignt == "Peru")


# transform to projected coordinate reference system (units from degrees to meters)
Americas <- st_transform(Americas, 32719)
TApoly <- st_transform(TApoly, 32719)
TropicalAndes_IUCNHabitat_Forest <- st_transform(TropicalAndes_IUCNHabitat_Forest, 32719)
plants.sf <- st_transform(plants.sf, 32719)
frugivores.sf <- st_transform(frugivores.sf, 32719)
mammals.sf <- st_transform(mammals.sf, 32719)
birds.sf <- st_transform(birds.sf, 32719)


# import sfs (if already saved)
Americas <- readRDS(file.path(data_path_L1, "Americas.rds"))
TApoly <- readRDS(file.path(data_path_L1,"TApoly.rds"))
TropicalAndes_IUCNHabitat_Forest <- readRDS(file.path(data_path_L1,"TropicalAndes_IUCNHabitat_Forest.rds"))
plants.sf <- readRDS(file.path(data_path_L1, "plants.sf"))
mammals.sf <- readRDS(file.path(data_path_L1, "mammals.sf"))
birds.sf <- readRDS(file.path(data_path_L1, "birds.sf"))


#### filtering records by time ####

hist(plants.sf$year[plants.sf$year>1970])
hist(mammals.sf$year[mammals.sf$year>1970])
hist(birds.sf$year[birds.sf$year>1970])

# check out peak in plant observations
obs_years <- plants.sf %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarize(n = n())

plants_2018 <- plants.sf %>% 
  filter(year == 2018)

plot(plants_2018$geometry)

# number of species (total) x time
mammal_sp_time <- mammals.sf %>%
  group_by(year) %>%
  summarize(n = length(unique(species)))

ggplot(mammal_sp_time, aes(x = year, y = n)) +
  geom_col()


# figure out when species were first and last observed

# plants
plants_sf_species <- plants.sf %>%
  st_drop_geometry() %>% 
  filter(!is.na(year)) %>%
  group_by(species) %>%
  summarise(
    first_obs = min(year, na.rm=T), 
    last_obs = max(year, na.rm=T), 
    n_obs = n()
  ) # based on year cutoff, we can see which species are lost by any with last_obs < cutoff

# mammals
mammals_sf_species <- mammals.sf %>%
  st_drop_geometry() %>% 
  filter(!is.na(year)) %>%
  group_by(species) %>%
  summarise(
    first_obs = min(year, na.rm=T), 
    last_obs = max(year, na.rm=T), 
    n_obs = n()
  )

# birds
birds_sf_species <- birds.sf %>%
  st_drop_geometry() %>% 
  filter(!is.na(year)) %>%
  group_by(species) %>%
  summarise(
    first_obs = min(year, na.rm=T), 
    last_obs = max(year, na.rm=T), 
    n_obs = n()
  )

# # total number of species based on first observation year
# plants_first_obs_year <- plants_sf_species %>%
#   group_by(first_obs) %>%
#   summarize(n_species = n()) %>%
#   rename(year = first_obs)
# 
# # joined by total number of observations of each year
# plants_per_year <- plants.sf %>%
#   st_drop_geometry() %>% 
#   group_by(year) %>%
#   summarize(n_obs = n()) %>%
#   left_join(plants_first_obs_year)
# 
# # convert NAs to 0s
# plants_per_year$n_species[is.na(plants_per_year$n_species)] <- 0
# 
# # plot number of species x number of observations
# ggplot(plants_per_year, aes(x = log(n_obs), y = n_species, color = year)) +
#   geom_point() +
#   theme_classic()


# species accumulation over time based on first obs date

# plants
sp_acc_plants <- plants_sf_species %>%
  group_by(first_obs) %>%
  summarize(n_species = n()) %>% 
  mutate(acc = cumsum(n_species))

ggplot(sp_acc_plants, aes(x = first_obs, y = acc))+
  geom_line(linewidth=1)+
  geom_vline(xintercept = 1970, color = "blue") +
  theme_classic()


# mammals
sp_acc_mammals <- mammals_sf_species %>%
  group_by(first_obs) %>%
  summarize(n_species = n()) %>% 
  mutate(acc = cumsum(n_species))

ggplot(sp_acc_mammals, aes(x = first_obs, y = acc))+
  geom_line(linewidth=1)+
  geom_vline(xintercept = 1970, color = "blue") +
  theme_classic()


# birds
sp_acc_birds <- birds_sf_species %>%
  group_by(first_obs) %>%
  summarize(n_species = n()) %>% 
  mutate(acc = cumsum(n_species))

ggplot(sp_acc_birds, aes(x = first_obs, y = acc))+
  geom_line(linewidth=1)+
  geom_vline(xintercept = 1970, color = "blue") +
  theme_classic()


# report species lost at 1970 cutoff

# plants
plant_records_1970_cutoff <- plants.sf %>%
  st_drop_geometry() %>% 
  filter(!is.na(year), year < 1970)
nrow(plant_records_1970_cutoff)
length(unique(plant_records_1970_cutoff$species))

plant_sp_1970_cutoff <- plants_sf_species |> 
  filter(last_obs < 1970)
nrow(plant_sp_1970_cutoff)


# mammals
mammal_records_1970_cutoff <- mammals.sf %>%
  st_drop_geometry() %>% 
  filter(!is.na(year), year < 1970)
nrow(mammal_records_1970_cutoff)
length(unique(mammal_records_1970_cutoff$species))

mammal_sp_1970_cutoff <- mammals_sf_species |> 
  filter(last_obs < 1970)
nrow(mammal_sp_1970_cutoff)


# birds
bird_records_1970_cutoff <- birds.sf %>%
  st_drop_geometry() %>% 
  filter(!is.na(year), year < 1970)
nrow(bird_records_1970_cutoff)
length(unique(bird_records_1970_cutoff$species))

bird_sp_1970_cutoff <- birds_sf_species |> 
  filter(last_obs < 1970)
nrow(bird_sp_1970_cutoff)


#### species observation records after 1970 ####

# plants
plants_sf_species2 <- plants.sf %>%
  filter(year > 1970) %>% 
  dplyr::select(species)

saveRDS(plants_sf_species2, file = file.path(all_output_path_L1, "plant_sp_obs.rds"))


# mammals
mammals_sf_species2 <- mammals.sf %>%
  filter(year > 1970) %>% 
  dplyr::select(species)

saveRDS(mammals_sf_species2, file = file.path(all_output_path_L1, "mammal_sp_obs.rds"))


# birds
birds_sf_species2 <- birds.sf %>%
  filter(year > 1970) %>% 
  dplyr::select(species) 

saveRDS(birds_sf_species2, file = file.path(all_output_path_L1, "bird_sp_obs.rds"))


#### species occurrence matrices ####

#### 100 km #### 

# matrix of observations (species (includes duplicates if observed more than once) x cell), cells with 0 observations removed
plant_obs_grid_100km <- obs_grid(100000, plants_sf_species2)
mammal_obs_grid_100km <- obs_grid(100000, mammals_sf_species2)
bird_obs_grid_100km <- obs_grid(100000, birds_sf_species2)

# matrix of species presence-absence (counts total number of observations of each species in each cell)
plant_sp_grid_100km <- sp_grid(plant_obs_grid_100km)
mammal_sp_grid_100km <- sp_grid(mammal_obs_grid_100km)
bird_sp_grid_100km <- sp_grid(bird_obs_grid_100km)

# save data 
saveRDS(plant_sp_grid_100km, file = file.path(all_output_path_L1, "plant_sp_grid_100km.rds"))
saveRDS(mammal_sp_grid_100km, file = file.path(all_output_path_L1, "mammal_sp_grid_100km.rds"))
saveRDS(bird_sp_grid_100km, file = file.path(all_output_path_L1, "bird_sp_grid_100km.rds"))

# import saved data
plant_sp_grid_100km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_100km.rds"))
mammal_sp_grid_100km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_100km.rds"))
bird_sp_grid_100km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_100km.rds"))


#### 75 km #### 

# matrix of observations (species (includes duplicates if observed more than once) x cell), cells with 0 observations removed
plant_obs_grid_75km <- obs_grid(75000, plants_sf_species2)
mammal_obs_grid_75km <- obs_grid(75000, mammals_sf_species2)
bird_obs_grid_75km <- obs_grid(75000, birds_sf_species2)

# matrix of species presence-absence (counts total number of observations of each species in each cell)
plant_sp_grid_75km <- sp_grid(plant_obs_grid_75km)
mammal_sp_grid_75km <- sp_grid(mammal_obs_grid_75km)
bird_sp_grid_75km <- sp_grid(bird_obs_grid_75km)

# save data 
saveRDS(plant_sp_grid_75km, file = file.path(all_output_path_L1, "plant_sp_grid_75km.rds"))
saveRDS(mammal_sp_grid_75km, file = file.path(all_output_path_L1, "mammal_sp_grid_75km.rds"))
saveRDS(bird_sp_grid_75km, file = file.path(all_output_path_L1, "bird_sp_grid_75km.rds"))

# import saved data
plant_sp_grid_75km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_75km.rds"))
mammal_sp_grid_75km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_75km.rds"))
bird_sp_grid_75km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_75km.rds"))


#### 50 km #### 

# matrix of observations (species (includes duplicates if observed more than once) x cell), cells with 0 observations removed
plant_obs_grid_50km <- obs_grid(50000, plants_sf_species2)
mammal_obs_grid_50km <- obs_grid(50000, mammals_sf_species2)
bird_obs_grid_50km <- obs_grid(50000, birds_sf_species2)

# matrix of species presence-absence (counts total number of observations of each species in each cell)
plant_sp_grid_50km <- sp_grid(plant_obs_grid_50km)
mammal_sp_grid_50km <- sp_grid(mammal_obs_grid_50km)
bird_sp_grid_50km <- sp_grid(bird_obs_grid_50km)

# save data 
saveRDS(plant_sp_grid_50km, file = file.path(all_output_path_L1, "plant_sp_grid_50km.rds"))
saveRDS(mammal_sp_grid_50km, file = file.path(all_output_path_L1, "mammal_sp_grid_50km.rds"))
saveRDS(bird_sp_grid_50km, file = file.path(all_output_path_L1, "bird_sp_grid_50km.rds"))

# import saved data
plant_sp_grid_50km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_50km.rds"))
mammal_sp_grid_50km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_50km.rds"))
bird_sp_grid_50km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_50km.rds"))


#### 25 km #### 

# matrix of observations (species (includes duplicates if observed more than once) x cell), cells with 0 observations removed
plant_obs_grid_25km <- obs_grid(25000, plants_sf_species2)
mammal_obs_grid_25km <- obs_grid(25000, mammals_sf_species2)
bird_obs_grid_25km <- obs_grid(25000, birds_sf_species2)

# matrix of species presence-absence (counts total number of observations of each species in each cell)
plant_sp_grid_25km <- sp_grid(plant_obs_grid_25km)
mammal_sp_grid_25km <- sp_grid(mammal_obs_grid_25km)
bird_sp_grid_25km <- sp_grid(bird_obs_grid_25km)

# save data 
saveRDS(plant_sp_grid_25km, file = file.path(all_output_path_L1, "plant_sp_grid_25km.rds"))
saveRDS(mammal_sp_grid_25km, file = file.path(all_output_path_L1, "mammal_sp_grid_25km.rds"))
saveRDS(bird_sp_grid_25km, file = file.path(all_output_path_L1, "bird_sp_grid_25km.rds"))

# import saved data
plant_sp_grid_25km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_25km.rds"))
mammal_sp_grid_25km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_25km.rds"))
bird_sp_grid_25km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_25km.rds"))


#### 10 km #### 

# matrix of observations (species (includes duplicates if observed more than once) x cell), cells with 0 observations removed
plant_obs_grid_10km <- obs_grid(10000, plants_sf_species2)
mammal_obs_grid_10km <- obs_grid(10000, mammals_sf_species2)
bird_obs_grid_10km <- obs_grid(10000, birds_sf_species2)

# matrix of species presence-absence (counts total number of observations of each species in each cell)
plant_sp_grid_10km <- sp_grid(plant_obs_grid_10km)
mammal_sp_grid_10km <- sp_grid(mammal_obs_grid_10km)
bird_sp_grid_10km <- sp_grid(bird_obs_grid_10km)

# save data 
saveRDS(plant_sp_grid_10km, file = file.path(all_output_path_L1, "plant_sp_grid_10km.rds"))
saveRDS(mammal_sp_grid_10km, file = file.path(all_output_path_L1, "mammal_sp_grid_10km.rds"))
saveRDS(bird_sp_grid_10km, file = file.path(all_output_path_L1, "bird_sp_grid_10km.rds"))

# import saved data
plant_sp_grid_10km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_10km.rds"))
mammal_sp_grid_10km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_10km.rds"))
bird_sp_grid_10km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_10km.rds"))


#### 5 km #### 

# matrix of observations (species (includes duplicates if observed more than once) x cell), cells with 0 observations removed
plant_obs_grid_5km <- obs_grid(5000, plants_sf_species2)
mammal_obs_grid_5km <- obs_grid(5000, mammals_sf_species2)
bird_obs_grid_5km <- obs_grid(5000, birds_sf_species2)

# matrix of species presence-absence (counts total number of observations of each species in each cell)
plant_sp_grid_5km <- sp_grid(plant_obs_grid_5km)
mammal_sp_grid_5km <- sp_grid(mammal_obs_grid_5km)
bird_sp_grid_5km <- sp_grid(bird_obs_grid_5km)

# save data 
saveRDS(plant_sp_grid_5km, file = file.path(all_output_path_L1, "plant_sp_grid_5km.rds"))
saveRDS(mammal_sp_grid_5km, file = file.path(all_output_path_L1, "mammal_sp_grid_5km.rds"))
saveRDS(bird_sp_grid_5km, file = file.path(all_output_path_L1, "bird_sp_grid_5km.rds"))

# import saved data
plant_sp_grid_5km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_5km.rds"))
mammal_sp_grid_5km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_5km.rds"))
bird_sp_grid_5km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_5km.rds"))


#### species accumulation curves at 5km ####

# plants
cell_summary_plants <- data.frame( cell = rownames(plant_sp_grid_5km), observations = rowSums(plant_sp_grid_5km), richness = specnumber(plant_sp_grid_5km))

plot(richness ~ observations, data = cell_summary_plants[cell_summary_plants$observations < 1000,], xlab = "Observations per cell", ylab = "Species richness per cell")
abline(v=20, col="blue")

# look at relationship between number of observations and sample coverage
iNEXT_plant <- calc_coverage(plant_sp_grid_5km)
iNEXT_plant$coverage_by_obs + geom_vline(xintercept = 20, color = "blue", linewidth = 1.5)


# mammals
cell_summary_mammals <- data.frame( cell = rownames(mammal_sp_grid_5km), observations = rowSums(mammal_sp_grid_5km), richness = specnumber(mammal_sp_grid_5km))

plot(richness ~ observations, data = cell_summary_mammals, xlab = "Observations per cell", ylab = "Species richness per cell")
abline(v=20, col="blue")

# look at relationship between number of observations and sample coverage
iNEXT_mammal <- calc_coverage(mammal_sp_grid_5km)
iNEXT_mammal$coverage_by_obs + geom_vline(xintercept = 20, color = "blue", linewidth = 1.5)


# birds
cell_summary_birds <- data.frame( cell = rownames(bird_sp_grid_5km), observations = rowSums(bird_sp_grid_5km), richness = specnumber(bird_sp_grid_5km))

plot(richness ~ observations, data = cell_summary_birds, xlab = "Observations per cell", ylab = "Species richness per cell")
abline(v=20, col="blue")

# look at relationship between number of observations and sample coverage
iNEXT_bird <- calc_coverage(bird_sp_grid_5km)
iNEXT_bird$coverage_by_obs + geom_vline(xintercept = 20, color = "blue", linewidth = 1.5)


#### run through previous code with low-end cutoff ####

#### species observation records after 1970 and specified cutoff ####

cutoff_obs <- 20

#### species occurrence matrices ####

#### 100 km #### 

# matrix of species presence-absence (counts total number of observations of each species in each cell)
plant_cutoff_sp_grid_100km <- plant_sp_grid_100km[rowSums(plant_sp_grid_100km > 0) >= cutoff_obs,]
mammal_cutoff_sp_grid_100km <- mammal_sp_grid_100km[rowSums(mammal_sp_grid_100km > 0) >= cutoff_obs, ]
bird_cutoff_sp_grid_100km <- bird_sp_grid_100km[rowSums(bird_sp_grid_100km > 0) >= cutoff_obs, ]

# save data 
saveRDS(plant_cutoff_sp_grid_100km, file = file.path(filtered_output_path_L1, paste0("plant_", cutoff_obs, "_sp_grid_100km.rds")))
saveRDS(mammal_cutoff_sp_grid_100km, file = file.path(filtered_output_path_L1, paste0("mammal_", cutoff_obs, "_sp_grid_100km.rds")))
saveRDS(bird_cutoff_sp_grid_100km, file = file.path(filtered_output_path_L1, paste0("bird_", cutoff_obs, "_sp_grid_100km.rds")))

# import saved data
plant_cutoff_sp_grid_100km <- readRDS(file.path(filtered_data_path_L1, paste0("plant_", cutoff_obs, "_sp_grid_100km.rds")))
mammal_cutoff_sp_grid_100km <- readRDS(file.path(filtered_data_path_L1, paste0("mammal_", cutoff_obs, "_sp_grid_100km.rds")))
bird_cutoff_sp_grid_100km <- readRDS(file.path(filtered_data_path_L1, paste0("bird_", cutoff_obs, "_sp_grid_100km.rds")))


#### 75 km #### 

# matrix of species presence-absence (counts total number of observations of each species in each cell)
plant_cutoff_sp_grid_75km <- plant_sp_grid_75km[rowSums(plant_sp_grid_75km > 0) >= cutoff_obs,]
mammal_cutoff_sp_grid_75km <- mammal_sp_grid_75km[rowSums(mammal_sp_grid_75km > 0) >= cutoff_obs, ]
bird_cutoff_sp_grid_75km <- bird_sp_grid_75km[rowSums(bird_sp_grid_75km > 0) >= cutoff_obs, ]

# save data 
saveRDS(plant_cutoff_sp_grid_75km, file = file.path(filtered_output_path_L1, paste0("plant_", cutoff_obs, "_sp_grid_75km.rds")))
saveRDS(mammal_cutoff_sp_grid_75km, file = file.path(filtered_output_path_L1, paste0("mammal_", cutoff_obs, "_sp_grid_75km.rds")))
saveRDS(bird_cutoff_sp_grid_75km, file = file.path(filtered_output_path_L1, paste0("bird_", cutoff_obs, "_sp_grid_75km.rds")))

# import saved data
plant_cutoff_sp_grid_75km <- readRDS(file.path(filtered_data_path_L1, paste0("plant_", cutoff_obs, "_sp_grid_75km.rds")))
mammal_cutoff_sp_grid_75km <- readRDS(file.path(filtered_data_path_L1, paste0("mammal_", cutoff_obs, "_sp_grid_75km.rds")))
bird_cutoff_sp_grid_75km <- readRDS(file.path(filtered_data_path_L1, paste0("bird_", cutoff_obs, "_sp_grid_75km.rds")))


#### 50 km #### 

# matrix of species presence-absence (counts total number of observations of each species in each cell)
plant_cutoff_sp_grid_50km <- plant_sp_grid_50km[rowSums(plant_sp_grid_50km > 0) >= cutoff_obs,]
mammal_cutoff_sp_grid_50km <- mammal_sp_grid_50km[rowSums(mammal_sp_grid_50km > 0) >= cutoff_obs, ]
bird_cutoff_sp_grid_50km <- bird_sp_grid_50km[rowSums(bird_sp_grid_50km > 0) >= cutoff_obs, ]

# save data 
saveRDS(plant_cutoff_sp_grid_50km, file = file.path(filtered_output_path_L1, paste0("plant_", cutoff_obs, "_sp_grid_50km.rds")))
saveRDS(mammal_cutoff_sp_grid_50km, file = file.path(filtered_output_path_L1, paste0("mammal_", cutoff_obs, "_sp_grid_50km.rds")))
saveRDS(bird_cutoff_sp_grid_50km, file = file.path(filtered_output_path_L1, paste0("bird_", cutoff_obs, "_sp_grid_50km.rds")))

# import saved data
plant_cutoff_sp_grid_50km <- readRDS(file.path(filtered_data_path_L1, paste0("plant_", cutoff_obs, "_sp_grid_50km.rds")))
mammal_cutoff_sp_grid_50km <- readRDS(file.path(filtered_data_path_L1, paste0("mammal_", cutoff_obs, "_sp_grid_50km.rds")))
bird_cutoff_sp_grid_50km <- readRDS(file.path(filtered_data_path_L1, paste0("bird_", cutoff_obs, "_sp_grid_50km.rds")))


#### 25 km #### 

# matrix of species presence-absence (counts total number of observations of each species in each cell)
plant_cutoff_sp_grid_25km <- plant_sp_grid_25km[rowSums(plant_sp_grid_25km > 0) >= cutoff_obs,]
mammal_cutoff_sp_grid_25km <- mammal_sp_grid_25km[rowSums(mammal_sp_grid_25km > 0) >= cutoff_obs, ]
bird_cutoff_sp_grid_25km <- bird_sp_grid_25km[rowSums(bird_sp_grid_25km > 0) >= cutoff_obs, ]

# save data 
saveRDS(plant_cutoff_sp_grid_25km, file = file.path(filtered_output_path_L1, paste0("plant_", cutoff_obs, "_sp_grid_25km.rds")))
saveRDS(mammal_cutoff_sp_grid_25km, file = file.path(filtered_output_path_L1, paste0("mammal_", cutoff_obs, "_sp_grid_25km.rds")))
saveRDS(bird_cutoff_sp_grid_25km, file = file.path(filtered_output_path_L1, paste0("bird_", cutoff_obs, "_sp_grid_25km.rds")))

# import saved data
plant_cutoff_sp_grid_25km <- readRDS(file.path(filtered_data_path_L1, paste0("plant_", cutoff_obs, "_sp_grid_25km.rds")))
mammal_cutoff_sp_grid_25km <- readRDS(file.path(filtered_data_path_L1, paste0("mammal_", cutoff_obs, "_sp_grid_25km.rds")))
bird_cutoff_sp_grid_25km <- readRDS(file.path(filtered_data_path_L1, paste0("bird_", cutoff_obs, "_sp_grid_25km.rds")))


#### 10 km #### 

# matrix of species presence-absence (counts total number of observations of each species in each cell)
plant_cutoff_sp_grid_10km <- plant_sp_grid_10km[rowSums(plant_sp_grid_10km > 0) >= cutoff_obs,]
mammal_cutoff_sp_grid_10km <- mammal_sp_grid_10km[rowSums(mammal_sp_grid_10km > 0) >= cutoff_obs, ]
bird_cutoff_sp_grid_10km <- bird_sp_grid_10km[rowSums(bird_sp_grid_10km > 0) >= cutoff_obs, ]

# save data 
saveRDS(plant_cutoff_sp_grid_10km, file = file.path(filtered_output_path_L1, paste0("plant_", cutoff_obs, "_sp_grid_10km.rds")))
saveRDS(mammal_cutoff_sp_grid_10km, file = file.path(filtered_output_path_L1, paste0("mammal_", cutoff_obs, "_sp_grid_10km.rds")))
saveRDS(bird_cutoff_sp_grid_10km, file = file.path(filtered_output_path_L1, paste0("bird_", cutoff_obs, "_sp_grid_10km.rds")))

# import saved data
plant_cutoff_sp_grid_10km <- readRDS(file.path(filtered_data_path_L1, paste0("plant_", cutoff_obs, "_sp_grid_10km.rds")))
mammal_cutoff_sp_grid_10km <- readRDS(file.path(filtered_data_path_L1, paste0("mammal_", cutoff_obs, "_sp_grid_10km.rds")))
bird_cutoff_sp_grid_10km <- readRDS(file.path(filtered_data_path_L1, paste0("bird_", cutoff_obs, "_sp_grid_10km.rds")))


#### 5 km #### 

# matrix of species presence-absence (counts total number of observations of each species in each cell)
plant_cutoff_sp_grid_5km <- plant_sp_grid_5km[rowSums(plant_sp_grid_5km > 0) >= cutoff_obs,]
mammal_cutoff_sp_grid_5km <- mammal_sp_grid_5km[rowSums(mammal_sp_grid_5km > 0) >= cutoff_obs, ]
bird_cutoff_sp_grid_5km <- bird_sp_grid_5km[rowSums(bird_sp_grid_5km > 0) >= cutoff_obs, ]

# save data 
saveRDS(plant_cutoff_sp_grid_5km, file = file.path(filtered_output_path_L1, paste0("plant_", cutoff_obs, "_sp_grid_5km.rds")))
saveRDS(mammal_cutoff_sp_grid_5km, file = file.path(filtered_output_path_L1, paste0("mammal_", cutoff_obs, "_sp_grid_5km.rds")))
saveRDS(bird_cutoff_sp_grid_5km, file = file.path(filtered_output_path_L1, paste0("bird_", cutoff_obs, "_sp_grid_5km.rds")))

# import saved data
plant_cutoff_sp_grid_5km <- readRDS(file.path(filtered_data_path_L1, paste0("plant_", cutoff_obs, "_sp_grid_5km.rds")))
mammal_cutoff_sp_grid_5km <- readRDS(file.path(filtered_data_path_L1, paste0("mammal_", cutoff_obs, "_sp_grid_5km.rds")))
bird_cutoff_sp_grid_5km <- readRDS(file.path(filtered_data_path_L1, paste0("bird_", cutoff_obs, "_sp_grid_5km.rds")))

iNEXT_plant_cutoff <- calc_coverage(plant_cutoff_sp_grid_5km)
iNEXT_plant_cutoff$coverage_by_obs

iNEXT_mammal_cutoff <- calc_coverage(mammal_cutoff_sp_grid_5km)
iNEXT_mammal_cutoff$coverage_by_obs

iNEXT_bird_cutoff <- calc_coverage(bird_cutoff_sp_grid_5km)
iNEXT_bird_cutoff$coverage_by_obs

# report which species were lost
plants_lost <- setdiff(colnames(plant_sp_grid_5km), colnames(plant_cutoff_sp_grid_5km))
length(setdiff(colnames(plant_sp_grid_5km), colnames(plant_cutoff_sp_grid_5km)))

mammals_lost <- setdiff(colnames(mammal_sp_grid_5km), colnames(mammal_cutoff_sp_grid_5km))
length(setdiff(colnames(mammal_sp_grid_5km), colnames(mammal_cutoff_sp_grid_5km)))

birds_lost <- setdiff(colnames(bird_sp_grid_5km), colnames(bird_cutoff_sp_grid_5km))
length(setdiff(colnames(plant_sp_grid_5km), colnames(plant_cutoff_sp_grid_5km)))

# no species lost

# report number of cells lost at 5km
plant_cells_lost <- setdiff(rownames(plant_sp_grid_5km), rownames(plant_cutoff_sp_grid_5km))
length(setdiff(rownames(plant_sp_grid_5km), rownames(plant_cutoff_sp_grid_5km)))

mammal_cells_lost <- setdiff(rownames(mammal_sp_grid_5km), rownames(mammal_cutoff_sp_grid_5km))
length(setdiff(rownames(mammal_sp_grid_5km), rownames(mammal_cutoff_sp_grid_5km)))

bird_cells_lost <- setdiff(rownames(bird_sp_grid_5km), rownames(bird_cutoff_sp_grid_5km))
length(setdiff(rownames(bird_sp_grid_5km), rownames(bird_cutoff_sp_grid_5km)))


#### adjust trait data ####

# 1970 cutoff

# remove species names from trait matrix not included in matrix
plant_traits_df_subset <- plant_traits %>%
  filter(species %in% colnames(plant_sp_grid_100km)) %>%
  distinct(species, .keep_all = TRUE)

mammal_traits_df_subset <- mammal_traits %>%
  filter(IUCN_species_name %in% colnames(mammal_sp_grid_100km)) %>%
  distinct(IUCN_species_name, .keep_all = TRUE)

bird_traits_df_subset <- bird_traits %>%
  filter(IUCN_species_name %in% colnames(bird_sp_grid_100km)) %>%
  distinct(IUCN_species_name, .keep_all = TRUE)

dim(plant_traits)
dim(plant_traits_df_subset)

dim(mammal_traits)
dim(mammal_traits_df_subset)

dim(bird_traits)
dim(bird_traits_df_subset)

# define row names as species names
row_names_plant <- plant_traits_df_subset$species
row_names_mammal <- mammal_traits_df_subset$IUCN_species_name
row_names_bird <- bird_traits_df_subset$IUCN_species_name

# assign row names to the matrix
rownames(plant_traits_df_subset) <- row_names_plant
rownames(mammal_traits_df_subset) <- row_names_mammal
rownames(bird_traits_df_subset) <- row_names_bird

plant_traits_df_subset$X <-NULL
mammal_traits_df_subset$X <- NULL
bird_traits_df_subset$X <- NULL

# remove duplicate species name column
plant_traits_df_subset$species <- NULL
mammal_traits_df_subset <- mammal_traits_df_subset[, c("body_mass_e",  "diet_cat", "diet_breadth", "habitat_breadth", "generation_time")]
bird_traits_df_subset <- bird_traits_df_subset[, c("body_mass_e",  "diet_cat", "diet_breadth", "habitat_breadth", "generation_time")]

# save new trait data
saveRDS(plant_traits_df_subset, file = file.path(all_output_path_L1,"plant_traits_df_final.rds"))
saveRDS(bird_traits_df_subset, file = file.path(all_output_path_L1,"bird_traits_df_final.rds"))
saveRDS(mammal_traits_df_subset, file = file.path(all_output_path_L1,"mammal_traits_df_final.rds"))


# 1970 and specified cutoff

# remove species names from trait matrix not included in matrix
plant_cutoff_traits_df_subset <- plant_traits %>%
  filter(species %in% colnames(plant_cutoff_sp_grid_100km)) %>%
  distinct(species, .keep_all = TRUE)

mammal_cutoff_traits_df_subset <- mammal_traits %>%
  filter(IUCN_species_name %in% colnames(mammal_cutoff_sp_grid_100km)) %>%
  distinct(IUCN_species_name, .keep_all = TRUE)

bird_cutoff_traits_df_subset <- bird_traits %>%
  filter(IUCN_species_name %in% colnames(bird_cutoff_sp_grid_100km)) %>%
  distinct(IUCN_species_name, .keep_all = TRUE)

dim(plant_traits)
dim(plant_traits_df_subset)
dim(plant_cutoff_traits_df_subset)

dim(mammal_traits)
dim(mammal_traits_df_subset)
dim(mammal_cutoff_traits_df_subset)

dim(bird_traits)
dim(bird_traits_df_subset)
dim(bird_cutoff_traits_df_subset)

# define row names as species names
row_names_plant <- plant_cutoff_traits_df_subset$species
row_names_mammal <- mammal_cutoff_traits_df_subset$IUCN_species_name
row_names_bird <- bird_cutoff_traits_df_subset$IUCN_species_name

# assign row names to the matrix
rownames(plant_cutoff_traits_df_subset) <- row_names_plant
rownames(mammal_cutoff_traits_df_subset) <- row_names_mammal
rownames(bird_cutoff_traits_df_subset) <- row_names_bird

plant_cutoff_traits_df_subset$X <-NULL
mammal_cutoff_traits_df_subset$X <- NULL
bird_cutoff_traits_df_subset$X <- NULL

# remove duplicate species name column
plant_cutoff_traits_df_subset$species <- NULL
mammal_cutoff_traits_df_subset <- mammal_cutoff_traits_df_subset[, c("body_mass_e",  "diet_cat", "diet_breadth", "habitat_breadth", "generation_time")]
bird_cutoff_traits_df_subset <- bird_cutoff_traits_df_subset[, c("body_mass_e",  "diet_cat", "diet_breadth", "habitat_breadth", "generation_time")]

# save new trait data
saveRDS(plant_cutoff_traits_df_subset, file = file.path(filtered_output_path_L1, paste0("plant_", cutoff_obs, "_traits_df_subset.rds")))
saveRDS(mammal_cutoff_traits_df_subset, file = file.path(filtered_output_path_L1, paste0("mammal_", cutoff_obs, "_traits_df_subset.rds")))
saveRDS(bird_cutoff_traits_df_subset, file = file.path(filtered_output_path_L1, paste0("bird_", cutoff_obs, "_traits_df_subset.rds")))


#### maps #### 

# plot base map
(basePlot <-
  ggplot() +
  geom_sf(data = Americas, fill = "white") +
  geom_sf(data = TApoly) +
  geom_sf(data = TropicalAndes_IUCNHabitat_Forest, fill = "gray50") +
  labs(title = "Tropical Andes Forest", x = "Latitude", y = "Longitude") +
  coord_sf(xlim = c(-85, -54), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
  annotation_scale(location = "bl", width_hint = 0.3, style = "ticks") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) +
  theme(panel.background = element_rect(fill = "lightblue")))

# plants

# plant picture
plant <- pick_phylopic(name='Coffea alleizettei')

# map of data filtered by 1970
(plantsPointsPlot <-
  ggplot() +
  geom_sf(data = Americas, fill = "white") +
  geom_sf(data = TApoly) +
  geom_sf(data = TropicalAndes_IUCNHabitat_Forest, fill = "gray50") + 
  geom_sf(data = plants_sf_species2, pch = 16, size = 0.05, color='darkseagreen3') +
  labs(title = "Fruiting plants") +
  coord_sf(xlim = c(-85, -54), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
  scale_x_continuous(breaks = seq(-85, -54, by = 10)) + 
  scale_y_continuous(breaks = seq(-24, 14, by = 10)) +
  add_phylopic(img=plant, x=-82, y=17, height=5)+
  annotation_scale(location = "bl",width_hint = 0.2, style = "bar") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(0.3, "in"), width = unit(0.3, "in"),
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.background = element_rect(fill = "lightblue"), plot.title=element_text(hjust=0.5))+
  xlab('')+ 
  ylab('Longitude'))

ggsave("plant_occurrence_points_map.png", plot = last_plot(), path = all_data_figure_path)


# map of data filtered by 1970 and specified cutoff
(plantsPointsPlot2 <-
    ggplot() +
    geom_sf(data = Americas, fill = "white") +
    geom_sf(data = TApoly) +
    geom_sf(data = TropicalAndes_IUCNHabitat_Forest, fill = "gray50") + 
    geom_sf(data = plants_sf_species3, pch = 16, size = 0.05, color='darkseagreen3') +
    labs(title = "Fruiting plants") +
    coord_sf(xlim = c(-85, -54), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
    scale_x_continuous(breaks = seq(-85, -54, by = 10)) + 
    scale_y_continuous(breaks = seq(-24, 14, by = 10)) +
    add_phylopic(img=plant, x=-82, y=17, height=5)+
    annotation_scale(location = "bl",width_hint = 0.2, style = "bar") +
    annotation_north_arrow(location = "bl", which_north = "true",
                           height = unit(0.3, "in"), width = unit(0.3, "in"),
                           pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                           style = north_arrow_fancy_orienteering) +
    theme(panel.background = element_rect(fill = "lightblue"), plot.title=element_text(hjust=0.5))+
    xlab('')+ 
    ylab('Longitude'))

ggsave(paste0("plant_", cutoff_obs, "obs_occurrence_points_map.png"), plot = last_plot(), path = filtered_data_figure_path)


# mammals

# mammal picture
mammal <- pick_phylopic(name='Potos flavus', n=2, auto=2)

# map of data filtered by 1970
(mammalsPointsPlot <-
  ggplot() +
  geom_sf(data = Americas, fill = "white") +
  geom_sf(data = TApoly) +
  geom_sf(data = TropicalAndes_IUCNHabitat_Forest, fill = "gray50") +
  geom_sf(data = mammals_sf_species2, pch = 16, size = 0.01, color='burlywood3') +
  labs(title = "Mammals") +
  coord_sf(xlim = c(-85, -54), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
  scale_x_continuous(breaks = seq(-85, -54, by = 10)) + 
  scale_y_continuous(breaks = seq(-24, 14, by = 10)) +
  add_phylopic(img=mammal, x=-78, y=16, height=6)+
  # annotation_scale(location = "bl",width_hint = 0.3, style = "bar") +
  # annotation_north_arrow(location = "bl", which_north = "true",
  #                        height = unit(0.5, "in"), width = unit(0.5, "in"),
  #                        pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  theme(panel.background = element_rect(fill = "lightblue"), plot.title=element_text(hjust=0.5))+
  xlab('Latitude')+ 
  ylab(''))

ggsave("mammal_occurrence_points_map.png", plot = last_plot(), path = all_data_figure_path)


# map of data filtered by 1970 and specified cutoff
(mammalsPointsPlot2 <-
    ggplot() +
    geom_sf(data = Americas, fill = "white") +
    geom_sf(data = TApoly) +
    geom_sf(data = TropicalAndes_IUCNHabitat_Forest, fill = "gray50") +
    geom_sf(data = mammals_sf_species3, pch = 16, size = 0.01, color='burlywood3') +
    labs(title = "Mammals") +
    coord_sf(xlim = c(-85, -54), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
    scale_x_continuous(breaks = seq(-85, -54, by = 10)) + 
    scale_y_continuous(breaks = seq(-24, 14, by = 10)) +
    add_phylopic(img=mammal, x=-78, y=16, height=6)+
    # annotation_scale(location = "bl",width_hint = 0.3, style = "bar") +
    # annotation_north_arrow(location = "bl", which_north = "true",
    #                        height = unit(0.5, "in"), width = unit(0.5, "in"),
    #                        pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
    #                        style = north_arrow_fancy_orienteering) +
    theme(panel.background = element_rect(fill = "lightblue"), plot.title=element_text(hjust=0.5))+
    xlab('Latitude')+ 
    ylab(''))

ggsave(paste0("mammal_", cutoff_obs, "obs_occurrence_points_map.png"), plot = last_plot(), path = filtered_data_figure_path)


# birds

# bird picture
bird <- pick_phylopic(name='Ramphastos sulfuratus', n=2, auto=1)

# map of data filtered by 1970
(birdsPointsPlot <-
  ggplot() +
  geom_sf(data = Americas, fill = "white") +
  geom_sf(data = TApoly) +
  geom_sf(data = TropicalAndes_IUCNHabitat_Forest, fill = "gray50")+
  geom_sf(data = birds_sf_species2, pch = 16, size = 0.01, color='lightsteelblue2') +
  labs(title = "Birds") +
  coord_sf(xlim = c(-85, -54), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
  scale_x_continuous(breaks = seq(-85, -54, by = 10)) + 
  scale_y_continuous(breaks = seq(-24, 14, by = 10)) +
  add_phylopic(img=bird, x=-76, y=17, height=6)+
  # annotation_scale(location = "bl",width_hint = 0.3, style = "bar") +
  # annotation_north_arrow(location = "bl", which_north = "true",
  #                        height = unit(0.5, "in"), width = unit(0.5, "in"),
  #                        pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
  #                        style = north_arrow_fancy_orienteering) +
  theme(panel.background = element_rect(fill = "lightblue"), plot.title=element_text(hjust=0.5))+
  xlab('')+ 
  ylab(''))

ggsave("bird_occurrence_points_map.png", plot = last_plot(), path = all_data_figure_path)


# map of data filtered by 1970 and specified cutoff
(birdsPointsPlot2 <-
    ggplot() +
    geom_sf(data = Americas, fill = "white") +
    geom_sf(data = TApoly) +
    geom_sf(data = TropicalAndes_IUCNHabitat_Forest, fill = "gray50")+
    geom_sf(data = birds_sf_species3, pch = 16, size = 0.01, color='lightsteelblue2') +
    labs(title = "Birds") +
    coord_sf(xlim = c(-85, -54), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
    scale_x_continuous(breaks = seq(-85, -54, by = 10)) + 
    scale_y_continuous(breaks = seq(-24, 14, by = 10)) +
    add_phylopic(img=bird, x=-76, y=17, height=6)+
    # annotation_scale(location = "bl",width_hint = 0.3, style = "bar") +
    # annotation_north_arrow(location = "bl", which_north = "true",
    #                        height = unit(0.5, "in"), width = unit(0.5, "in"),
    #                        pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
    #                        style = north_arrow_fancy_orienteering) +
    theme(panel.background = element_rect(fill = "lightblue"), plot.title=element_text(hjust=0.5))+
    xlab('')+ 
    ylab(''))

ggsave(paste0("bird_", cutoff_obs, "obs_occurrence_points_map.png"), plot = last_plot(), path = filtered_data_figure_path)


# plot all three maps

# data filtered by 1970
(all_points_maps <- wrap_plots(plantsPointsPlot, mammalsPointsPlot, birdsPointsPlot, ncol = 3, nrow = 1) + plot_annotation(tag_levels=list(c('(a)','(b)','(c)'))))

ggsave("all_points_maps.png", all_points_maps, path = all_data_figure_path, height =  7, width = 8, units = "in", dpi=1000)

# data filtered by 1970 and specified cutoff
(all_points_maps2 <- wrap_plots(plantsPointsPlot2, mammalsPointsPlot2, birdsPointsPlot2, ncol = 3, nrow = 1) + plot_annotation(tag_levels=list(c('(a)','(b)','(c)'))))

ggsave(paste0("all_", cutoff_obs, "obs_points_maps.png"), all_points_maps2, path = filtered_data_figure_path, height =  7, width = 8, units = "in", dpi=1000)
