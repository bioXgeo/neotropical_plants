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
library(letsR); library(mFD); library(vegan); library(rnaturalearth); library(sf); library(raster); library(fasterize); library(funbiogeo); library(dplyr); library(tidyr); library(ggspatial); library(ggplot2); library(ggpubr); library(rphylopic); library(patchwork)


# set file paths
data_path_L0<-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')

# # HPCC
# data_path_L0 <- file.path('/mnt/research/nasabio/data_2025/plants/L0')
# data_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')
# output_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')
# figure_path <- file.path('/mnt/research/nasabio/data_2025/plants/figures')


# load functions
source("C:/GitHub_projects/plant-frugivore diversity/neotropical_plants/code/Functions.R")

# # HPCC
# source("/mnt/ffs24/home/baljunas/Documents/neotropical_plants/code/Functions.R")


# read in data
TropicalAndes_plant_occ_forest <- read.csv(file.path(data_path_L1,"TropicalAndes_GBIF_plant_occ_harmonized_subset_final.csv"))
TropicalAndes_frugivore_occ_forest <- read.csv(file.path(data_path_L1,"TropicalAndes_GBIF_frugivore_occ_cleaned_subset.csv"))
TropicalAndes_mammal_occ_forest <- read.csv(file.path(data_path_L1, "TropicalAndes_GBIF_mammal_occ_cleaned_subset.csv"))
TropicalAndes_bird_occ_forest <- read.csv(file.path(data_path_L1, "TropicalAndes_GBIF_bird_occ_cleaned_subset.csv"))
TropicalAndes_IUCNHabitat_Forest <- read_sf(file.path(data_path_L0, "Forest_sf.shp"), layer = "Forest_sf")
frugivore_traits <- read.csv(file.path(data_path_L1,"TropicalAndes_Frugivoria_traits_subset.csv"))
bird_traits <- read.csv(file.path(data_path_L1,"TropicalAndes_bird_traits_subset.csv"))
mammal_traits <- read.csv(file.path(data_path_L1,"TropicalAndes_mammal_traits_subset.csv"))
plant_traits <- read.csv(file.path(data_path_L1,"TropicalAndes_imputed_plant_traits2.csv"))


# convert data to spatial data
plants.sf <- st_as_sf(TropicalAndes_plant_occ_forest, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

frugivores.sf <- st_as_sf(TropicalAndes_frugivore_occ_forest, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

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


# check units
st_crs(TApoly, parameters = TRUE)$units_gdal
st_crs(TApoly)
st_crs(TropicalAndes_IUCNHabitat_Forest)


# group by species
plants_sf_species <- plants.sf %>%
  group_by(species) %>%
  summarise()

frugivores_sf_species <- frugivores.sf %>%
  group_by(species) %>%
  summarise()

mammals_sf_species <- mammals.sf %>%
  group_by(species) %>%
  summarise()

birds_sf_species <- birds.sf %>%
  group_by(species) %>%
  summarise()

# data (if already saved)
Americas <- readRDS(file.path(data_path_L1, "Americas.rds"))
TApoly <- readRDS(file.path(data_path_L1,"TApoly.rds"))
TropicalAndes_IUCNHabitat_Forest <- readRDS(file.path(data_path_L1,"TropicalAndes_IUCNHabitat_Forest.rds"))

# plot base map
basePlot <-
  ggplot() +
  geom_sf(data = Americas, fill = "white") +
  geom_sf(data = TApoly) +
  geom_sf(data = TropicalAndes_IUCNHabitat_Forest, fill = "gray50") +
  labs(title = "Tropical Andes Forest", x = "Latitude", y = "Longitude") +
  coord_sf(xlim = c(-85, -54), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
  annotation_scale(location = "bl", width_hint = 0.3, style = "ticks") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.3, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) +
  theme(panel.background = element_rect(fill = "lightblue"))

basePlot
ggsave("tropical_andes_forest_map.png", plot = last_plot(), path = figure_path)


# data (if already saved)
plants_sf_species <- readRDS(file.path(data_path_L1,"plants_sf_species.rds"))

# plant picture
plant <- pick_phylopic(name='Coffea alleizettei')

# plot points
plantsPointsPlot <-
  ggplot() +
  geom_sf(data = Americas, fill = "white") +
  geom_sf(data = TApoly) +
  geom_sf(data = TropicalAndes_IUCNHabitat_Forest, fill = "gray50") + 
  geom_sf(data = plants_sf_species, pch = 16, size = 0.05, color='darkseagreen3') +
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
  ylab('Longitude')

plantsPointsPlot
ggsave("plant_occurrence_points_map2.png", plot = last_plot(), path = figure_path)


frugivoresPointsPlot <-
  ggplot() +
  geom_sf(data = Americas, fill = "white") +
  geom_sf(data = TApoly) +
  geom_sf(data = TropicalAndes_IUCNHabitat_Forest, fill = "gray50") +
  geom_sf(data = frugivores_sf_species, pch = 16, size = 0.01, color='salmon') +
  labs(title = "Frugivore Occurrences") +
  coord_sf(xlim = c(-85, -54), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
  scale_x_continuous(breaks = seq(-85, -54, by = 10)) + 
  scale_y_continuous(breaks = seq(-24, 14, by = 10)) +
  annotation_scale(location = "bl",width_hint = 0.3, style = "bar") +
  annotation_north_arrow(location = "bl", which_north = "true",
                         height = unit(0.5, "in"), width = unit(0.5, "in"),
                         pad_x = unit(0.1, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(panel.background = element_rect(fill = "lightblue"))+
  xlab('Latitude')+ 
  ylab('Longitude')

frugivoresPointsPlot
ggsave("frugivore_occurrence_points_map2.png", plot = last_plot(), path = figure_path)


# data (if already saved)
mammals_sf_species <- readRDS(file.path(data_path_L1,"mammals_sf_species.rds"))

# mammal picture
mammal <- pick_phylopic(name='Potos flavus', n=2, auto=2)

mammalsPointsPlot <-
  ggplot() +
  geom_sf(data = Americas, fill = "white") +
  geom_sf(data = TApoly) +
  geom_sf(data = TropicalAndes_IUCNHabitat_Forest, fill = "gray50") +
  geom_sf(data = mammals_sf_species, pch = 16, size = 0.01, color='burlywood3') +
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
  ylab('')
mammalsPointsPlot
ggsave("mammal_occurrence_points_map2.png", plot = last_plot(), path = figure_path)


# data (if already saved)
birds_sf_species <- readRDS(file.path(data_path_L1,"birds_sf_species.rds"))

# bird picture
bird <- pick_phylopic(name='Ramphastos sulfuratus', n=2, auto=1)

birdsPointsPlot <-
  ggplot() +
  geom_sf(data = Americas, fill = "white") +
  geom_sf(data = TApoly) +
  geom_sf(data = TropicalAndes_IUCNHabitat_Forest, fill = "gray50")+
  geom_sf(data = birds_sf_species, pch = 16, size = 0.01, color='lightsteelblue2') +
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
  ylab('')
birdsPointsPlot 
ggsave("bird_occurrence_points2_map.png", plot = last_plot(), path = figure_path)


all_points_maps <- wrap_plots(plantsPointsPlot, mammalsPointsPlot, birdsPointsPlot, ncol = 3, nrow = 1) + plot_annotation(tag_levels=list(c('(a)','(b)','(c)')))
all_points_maps
ggsave("all_points_maps2.png", all_points_maps, path = figure_path, height =  7, width = 8, units = "in", dpi=1000)


#### presence-absence matrices ####

#### 100 km #### 
plant_PAM_100km <- create_presence_absence_matrix(100000, plants_sf_species)
#frugivore_PAM_100km <- create_presence_absence_matrix(100000, frugivores_sf_species)
mammal_PAM_100km <- create_presence_absence_matrix(100000, mammals_sf_species)
bird_PAM_100km <- create_presence_absence_matrix(100000, birds_sf_species)

# check str 
plant_PAM_100km[1:4, 1:4]
#frugivore_PAM_100km[1:4, 1:4]

# remove the species from PAM that have no occurrences
# remove columns with sum equal to zero
PAM_plant_site_final_100km <- plant_PAM_100km[, colSums(plant_PAM_100km) != 0]
#PAM_frugivore_site_final_100km <- frugivore_PAM_100km[, colSums(frugivore_PAM_100km) != 0]
PAM_mammal_site_final_100km <- mammal_PAM_100km[, colSums(mammal_PAM_100km) != 0]
PAM_bird_site_final_100km <- bird_PAM_100km[, colSums(bird_PAM_100km) != 0]

# save coordinates for later
site_loc_key_plant_100km <- PAM_plant_site_final_100km[,1:2]
#site_loc_key_frugivore_100km <- PAM_frugivore_site_final_100km[,1:2]
site_loc_key_mammal_100km <- PAM_mammal_site_final_100km[,1:2]
site_loc_key_bird_100km <- PAM_bird_site_final_100km[,1:2]

PAM_plant_site_final_100km <- PAM_plant_site_final_100km[,-c(1:2)]
#PAM_frugivore_site_final_100km <- PAM_frugivore_site_final_100km[,-(1:2)]
PAM_mammal_site_final_100km <- PAM_mammal_site_final_100km[,-(1:2)]
PAM_bird_site_final_100km <- PAM_bird_site_final_100km[,-(1:2)]

colnames_plant_100km <- colnames(PAM_plant_site_final_100km)
#colnames_frugivore_100km <- colnames(PAM_frugivore_site_final_100km)
colnames_mammal_100km <- colnames(PAM_mammal_site_final_100km)
colnames_bird_100km <- colnames(PAM_bird_site_final_100km)

str(PAM_plant_site_final_100km)
#str(PAM_frugivore_site_final_100km)
str(PAM_mammal_site_final_100km)
str(PAM_bird_site_final_100km)


#### 75 km #### 
plant_PAM_75km <- create_presence_absence_matrix(75000, plants_sf_species)
#frugivore_PAM_75km <- create_presence_absence_matrix(75000, frugivores_sf_species)
mammal_PAM_75km <- create_presence_absence_matrix(75000, mammals_sf_species)
bird_PAM_75km <- create_presence_absence_matrix(75000, birds_sf_species)

# check str 
plant_PAM_75km[1:4, 1:4]
#frugivore_PAM_75km[1:4, 1:4]

# remove the species from PAM that have no occurrences
# remove columns with sum equal to zero
PAM_plant_site_final_75km <- plant_PAM_75km[, colSums(plant_PAM_75km) != 0]
#PAM_frugivore_site_final_75km <- frugivore_PAM_75km[, colSums(frugivore_PAM_75km) != 0]
PAM_mammal_site_final_75km <- mammal_PAM_75km[, colSums(mammal_PAM_75km) != 0]
PAM_bird_site_final_75km <- bird_PAM_75km[, colSums(bird_PAM_75km) != 0]

# save coordinates for later
site_loc_key_plant_75km <- PAM_plant_site_final_75km[,1:2]
#site_loc_key_frugivore_75km <- PAM_frugivore_site_final_75km[,1:2]
site_loc_key_mammal_75km <- PAM_mammal_site_final_75km[,1:2]
site_loc_key_bird_75km <- PAM_bird_site_final_75km[,1:2]

PAM_plant_site_final_75km <- PAM_plant_site_final_75km[,-c(1:2)]
#PAM_frugivore_site_final_75km <- PAM_frugivore_site_final_75km[,-(1:2)]
PAM_mammal_site_final_75km <- PAM_mammal_site_final_75km[,-(1:2)]
PAM_bird_site_final_75km <- PAM_bird_site_final_75km[,-(1:2)]

colnames_plant_75km <- colnames(PAM_plant_site_final_75km)
#colnames_frugivore_75km <- colnames(PAM_frugivore_site_final_75km)
colnames_mammal_75km <- colnames(PAM_mammal_site_final_75km)
colnames_bird_75km <- colnames(PAM_bird_site_final_75km)

str(PAM_plant_site_final_75km)
#str(PAM_frugivore_site_final_75km)
str(PAM_mammal_site_final_75km)
str(PAM_bird_site_final_75km)


#### 50 km #### 
plant_PAM_50km <- create_presence_absence_matrix(50000, plants_sf_species)
#frugivore_PAM_50km <- create_presence_absence_matrix(50000, frugivores_sf_species)
mammal_PAM_50km <- create_presence_absence_matrix(50000, mammals_sf_species)
bird_PAM_50km <- create_presence_absence_matrix(50000, birds_sf_species)

# check str 
plant_PAM_50km[1:4, 1:4]
#frugivore_PAM_50km[1:4, 1:4]

# remove the species from PAM that have no occurrences
# remove columns with sum equal to zero
PAM_plant_site_final_50km <- plant_PAM_50km[, colSums(plant_PAM_50km) != 0]
#PAM_frugivore_site_final_50km <- frugivore_PAM_50km[, colSums(frugivore_PAM_50km) != 0]
PAM_mammal_site_final_50km <- mammal_PAM_50km[, colSums(mammal_PAM_50km) != 0]
PAM_bird_site_final_50km <- bird_PAM_50km[, colSums(bird_PAM_50km) != 0]

# save coordinates for later
site_loc_key_plant_50km <- PAM_plant_site_final_50km[,1:2]
#site_loc_key_frugivore_50km <- PAM_frugivore_site_final_50km[,1:2]
site_loc_key_mammal_50km <- PAM_mammal_site_final_50km[,1:2]
site_loc_key_bird_50km <- PAM_bird_site_final_50km[,1:2]

PAM_plant_site_final_50km <- PAM_plant_site_final_50km[,-c(1:2)]
#PAM_frugivore_site_final_50km <- PAM_frugivore_site_final_50km[,-(1:2)]
PAM_mammal_site_final_50km <- PAM_mammal_site_final_50km[,-(1:2)]
PAM_bird_site_final_50km <- PAM_bird_site_final_50km[,-(1:2)]

colnames_plant_50km <- colnames(PAM_plant_site_final_50km)
#colnames_frugivore_50km <- colnames(PAM_frugivore_site_final_50km)
colnames_mammal_50km <- colnames(PAM_mammal_site_final_50km)
colnames_bird_50km <- colnames(PAM_bird_site_final_50km)

str(PAM_plant_site_final_50km)
#str(PAM_frugivore_site_final_50km)
str(PAM_mammal_site_final_50km)
str(PAM_bird_site_final_50km)


#### 25 km #### 
plant_PAM_25km <- create_presence_absence_matrix(25000, plants_sf_species)
#frugivore_PAM_25km <- create_presence_absence_matrix(25000, frugivores_sf_species)
mammal_PAM_25km <- create_presence_absence_matrix(25000, mammals_sf_species)
bird_PAM_25km <- create_presence_absence_matrix(25000, birds_sf_species)

# check str 
plant_PAM_25km[1:4, 1:4]
#frugivore_PAM_25km[1:4, 1:4]


# remove the species from PAM that have no occurrences
# remove columns with sum equal to zero
PAM_plant_site_final_25km <- plant_PAM_25km[, colSums(plant_PAM_25km) != 0]
#PAM_frugivore_site_final_25km <- frugivore_PAM_25km[, colSums(frugivore_PAM_25km) != 0]
PAM_mammal_site_final_25km <- mammal_PAM_25km[, colSums(mammal_PAM_25km) != 0]
PAM_bird_site_final_25km <- bird_PAM_25km[, colSums(bird_PAM_25km) != 0]

# save coordinates for later
site_loc_key_plant_25km <- PAM_plant_site_final_25km[,1:2]
#site_loc_key_frugivore_25km <- PAM_frugivore_site_final_25km[,1:2]
site_loc_key_mammal_25km <- PAM_mammal_site_final_25km[,1:2]
site_loc_key_bird_25km <- PAM_bird_site_final_25km[,1:2]

PAM_plant_site_final_25km <- PAM_plant_site_final_25km[,-c(1:2)]
#PAM_frugivore_site_final_25km <- PAM_frugivore_site_final_25km[,-(1:2)]
PAM_mammal_site_final_25km <- PAM_mammal_site_final_25km[,-(1:2)]
PAM_bird_site_final_25km <- PAM_bird_site_final_25km[,-(1:2)]

colnames_plant_25km <- colnames(PAM_plant_site_final_25km)
#colnames_frugivore_25km <- colnames(PAM_frugivore_site_final_25km)
colnames_mammal_25km <- colnames(PAM_mammal_site_final_25km)
colnames_bird_25km <- colnames(PAM_bird_site_final_25km)

str(PAM_plant_site_final_25km)
#str(PAM_frugivore_site_final_25km)
str(PAM_mammal_site_final_25km)
str(PAM_bird_site_final_25km)


#### 10 km #### 
plant_PAM_10km <- create_presence_absence_matrix(10000, plants_sf_species)
#frugivore_PAM_10km <- create_presence_absence_matrix(10000, frugivores_sf_species)
mammal_PAM_10km <- create_presence_absence_matrix(10000, mammals_sf_species)
bird_PAM_10km <- create_presence_absence_matrix(10000, birds_sf_species)

# check str 
plant_PAM_10km[1:4, 1:4]
#frugivore_PAM_10km[1:4, 1:4]

# remove the species from PAM that have no occurrences
# remove columns with sum equal to zero
PAM_plant_site_final_10km <- plant_PAM_10km[, colSums(plant_PAM_10km) != 0]
#PAM_frugivore_site_final_10km <- frugivore_PAM_10km[, colSums(frugivore_PAM_10km) != 0]
PAM_mammal_site_final_10km <- mammal_PAM_10km[, colSums(mammal_PAM_10km) != 0]
PAM_bird_site_final_10km <- bird_PAM_10km[, colSums(bird_PAM_10km) != 0]

# save coordinates for later
site_loc_key_plant_10km <- PAM_plant_site_final_10km[,1:2]
#site_loc_key_frugivore_10km <- PAM_frugivore_site_final_10km[,1:2]
site_loc_key_mammal_10km <- PAM_mammal_site_final_10km[,1:2]
site_loc_key_bird_10km <- PAM_bird_site_final_10km[,1:2]

PAM_plant_site_final_10km <- PAM_plant_site_final_10km[,-c(1:2)]
#PAM_frugivore_site_final_10km <- PAM_frugivore_site_final_10km[,-(1:2)]
PAM_mammal_site_final_10km <- PAM_mammal_site_final_10km[,-(1:2)]
PAM_bird_site_final_10km <- PAM_bird_site_final_10km[,-(1:2)]

colnames_plant_10km <- colnames(PAM_plant_site_final_10km)
#colnames_frugivore_10km <- colnames(PAM_frugivore_site_final_10km)
colnames_mammal_10km <- colnames(PAM_mammal_site_final_10km)
colnames_bird_10km <- colnames(PAM_bird_site_final_10km)

str(PAM_plant_site_final_10km)
#str(PAM_frugivore_site_final_10km)
str(PAM_mammal_site_final_10km)
str(PAM_bird_site_final_10km)


#### 5 km #### 
plant_PAM_5km <- create_presence_absence_matrix(5000, plants_sf_species)
#frugivore_PAM_5km <- create_presence_absence_matrix(5000, frugivores_sf_species)
mammal_PAM_5km <- create_presence_absence_matrix(5000, mammals_sf_species)
bird_PAM_5km <- create_presence_absence_matrix(5000, birds_sf_species)

# check str 
plant_PAM_5km[1:4, 1:4]
#frugivore_PAM_5km[1:4, 1:4]

# remove the species from PAM that have no occurrences
# remove columns with sum equal to zero
PAM_plant_site_final_5km <- plant_PAM_5km[, colSums(plant_PAM_5km) != 0]
#PAM_frugivore_site_final_5km <- frugivore_PAM_5km[, colSums(frugivore_PAM_5km) != 0]
PAM_mammal_site_final_5km <- mammal_PAM_5km[, colSums(mammal_PAM_5km) != 0]
PAM_bird_site_final_5km <- bird_PAM_5km[, colSums(bird_PAM_5km) != 0]

# save coordinates for later
site_loc_key_plant_5km <- PAM_plant_site_final_5km[,1:2]
#site_loc_key_frugivore_5km <- PAM_frugivore_site_final_5km[,1:2]
site_loc_key_mammal_5km <- PAM_mammal_site_final_5km[,1:2]
site_loc_key_bird_5km <- PAM_bird_site_final_5km[,1:2]

PAM_plant_site_final_5km <- PAM_plant_site_final_5km[,-c(1:2)]
#PAM_frugivore_site_final_5km <- PAM_frugivore_site_final_5km[,-(1:2)]
PAM_mammal_site_final_5km <- PAM_mammal_site_final_5km[,-(1:2)]
PAM_bird_site_final_5km <- PAM_bird_site_final_5km[,-(1:2)]

colnames_plant_5km <- colnames(PAM_plant_site_final_5km)
#colnames_frugivore_5km <- colnames(PAM_frugivore_site_final_5km)
colnames_mammal_5km <- colnames(PAM_mammal_site_final_5km)
colnames_bird_5km <- colnames(PAM_bird_site_final_5km)

str(PAM_plant_site_final_5km)
#str(PAM_frugivore_site_final_5km)
str(PAM_mammal_site_final_5km)
str(PAM_bird_site_final_5km)


# remove species names from trait matrix not in the PAM
plant_traits_df_subset <- plant_traits %>%
  filter(species %in% colnames_plant_100km) %>%
  distinct(species, .keep_all = TRUE)

#frugivore_traits_df_subset <- frugivore_traits %>%
#  filter(IUCN_species_name %in% colnames_frugivore_100km) %>%
#  distinct(IUCN_species_name, .keep_all = TRUE)

mammal_traits_df_subset <- mammal_traits %>%
  filter(IUCN_species_name %in% colnames_mammal_100km) %>%
  distinct(IUCN_species_name, .keep_all = TRUE)

bird_traits_df_subset <- bird_traits %>%
  filter(IUCN_species_name %in% colnames_bird_100km) %>%
  distinct(IUCN_species_name, .keep_all = TRUE)

dim(plant_traits)
dim(plant_traits_df_subset)

#dim(frugivore_traits)
#dim(frugivore_traits_df_subset)

dim(mammal_traits)
dim(mammal_traits_df_subset)

dim(bird_traits)
dim(bird_traits_df_subset)

# define row names as species names
row_names_plant <- plant_traits_df_subset$species
#row_names_frugivore <- frugivore_traits_df_subset$IUCN_species_name
row_names_mammal <- mammal_traits_df_subset$IUCN_species_name
row_names_bird <- bird_traits_df_subset$IUCN_species_name

# assign row names to the matrix
rownames(plant_traits_df_subset) <- row_names_plant
#rownames(frugivore_traits_df_subset) <- row_names_frugivore
rownames(mammal_traits_df_subset) <- row_names_mammal
rownames(bird_traits_df_subset) <- row_names_bird

plant_traits_df_subset$X <-NULL
#frugivore_traits_df_subset$X <-NULL
mammal_traits_df_subset$X <- NULL
bird_traits_df_subset$X <- NULL

# remove duplicate species name column
plant_traits_df_subset$species <- NULL

#frugivore_traits_df_subset <- frugivore_traits_df_subset[, c("body_mass_e",  "diet_cat", "diet_breadth", "habitat_breadth", "generation_time")]
mammal_traits_df_subset <- mammal_traits_df_subset[, c("body_mass_e",  "diet_cat", "diet_breadth", "habitat_breadth", "generation_time")]
bird_traits_df_subset <- bird_traits_df_subset[, c("body_mass_e",  "diet_cat", "diet_breadth", "habitat_breadth", "generation_time")]

str(plant_traits_df_subset)
#str(frugivore_traits_df_subset)


# export data

# sf objects
saveRDS(plants_sf_species, file = file.path(data_path_L1,"plants_sf_species.rds"))
#saveRDS(frugivores_sf_species, file = file.path(data_path_L1,"frugivores_sf_species.rds"))
saveRDS(mammals_sf_species, file = file.path(data_path_L1,"mammals_sf_species.rds"))
saveRDS(birds_sf_species, file = file.path(data_path_L1,"birds_sf_species.rds"))
saveRDS(Americas, file = file.path(data_path_L1, "Americas.rds"))
saveRDS(TApoly, file = file.path(data_path_L1,"TApoly.rds"))
saveRDS(TropicalAndes_IUCNHabitat_Forest, file = file.path(data_path_L1,"TropicalAndes_IUCNHabitat_Forest.rds"))


# PAM objects
saveRDS(plant_PAM_100km, file = file.path(data_path_L1,"plant_PAM_100km.rds"))
#saveRDS(frugivore_PAM_100km, file = file.path(data_path_L1,"frugivore_PAM_100km.rds"))
saveRDS(mammal_PAM_100km, file = file.path(data_path_L1,"mammal_PAM_100km.rds"))
saveRDS(bird_PAM_100km, file = file.path(data_path_L1,"bird_PAM_100km.rds"))

saveRDS(plant_PAM_75km, file = file.path(data_path_L1,"plant_PAM_75km.rds"))
#saveRDS(frugivore_PAM_75km, file = file.path(data_path_L1,"frugivore_PAM_75km.rds"))
saveRDS(mammal_PAM_75km, file = file.path(data_path_L1,"mammal_PAM_75km.rds"))
saveRDS(bird_PAM_75km, file = file.path(data_path_L1,"bird_PAM_75km.rds"))

saveRDS(plant_PAM_50km, file = file.path(data_path_L1,"plant_PAM_50km.rds"))
#saveRDS(frugivore_PAM_50km, file = file.path(data_path_L1,"frugivore_PAM_50km.rds"))
saveRDS(mammal_PAM_50km, file = file.path(data_path_L1,"mammal_PAM_50km.rds"))
saveRDS(bird_PAM_50km, file = file.path(data_path_L1,"bird_PAM_50km.rds"))

saveRDS(plant_PAM_25km, file = file.path(data_path_L1,"plant_PAM_25km.rds"))
#saveRDS(frugivore_PAM_25km, file = file.path(data_path_L1,"frugivore_PAM_25km.rds"))
saveRDS(mammal_PAM_25km, file = file.path(data_path_L1,"mammal_PAM_25km.rds"))
saveRDS(bird_PAM_25km, file = file.path(data_path_L1,"bird_PAM_25km.rds"))

saveRDS(plant_PAM_10km, file = file.path(data_path_L1,"plant_PAM_10km.rds"))
#saveRDS(frugivore_PAM_10km, file = file.path(data_path_L1,"frugivore_PAM_10km.rds"))
saveRDS(mammal_PAM_10km, file = file.path(data_path_L1,"mammal_PAM_10km.rds"))
saveRDS(bird_PAM_10km, file = file.path(data_path_L1,"bird_PAM_10km.rds"))

saveRDS(plant_PAM_5km, file = file.path(data_path_L1,"plant_PAM_5km.rds"))
#saveRDS(frugivore_PAM_5km, file = file.path(data_path_L1,"frugivore_PAM_5km.rds"))
saveRDS(mammal_PAM_5km, file = file.path(data_path_L1,"mammal_PAM_5km.rds"))
saveRDS(bird_PAM_5km, file = file.path(data_path_L1,"bird_PAM_5km.rds"))


# objects for functional diversity
# traits
saveRDS(plant_traits_df_subset, file = file.path(data_path_L1,"plant_traits_df_final.rds"))
#saveRDS(frugivore_traits_df_subset, file = file.path(data_path_L1,"frugivore_traits_df_final.rds"))
saveRDS(bird_traits_df_subset, file = file.path(data_path_L1,"bird_traits_df_final.rds"))
saveRDS(mammal_traits_df_subset, file = file.path(data_path_L1,"mammal_traits_df_final.rds"))

# 100km
saveRDS(site_loc_key_plant_100km, file = file.path(data_path_L1,"site_loc_key_plant_100km.rds"))
#saveRDS(site_loc_key_frugivore_100km, file = file.path(data_path_L1,"site_loc_key_frugivore_100km.rds"))
saveRDS(site_loc_key_mammal_100km, file = file.path(data_path_L1,"site_loc_key_mammal_100km.rds"))
saveRDS(site_loc_key_bird_100km, file = file.path(data_path_L1,"site_loc_key_bird_100km.rds"))

saveRDS(PAM_plant_site_final_100km, file = file.path(data_path_L1,"PAM_plant_site_final_100km.rds"))
#saveRDS(PAM_frugivore_site_final_100km, file = file.path(data_path_L1,"PAM_frugivore_site_final_100km.rds"))
saveRDS(PAM_mammal_site_final_100km, file = file.path(data_path_L1,"PAM_mammal_site_final_100km.rds"))
saveRDS(PAM_bird_site_final_100km, file = file.path(data_path_L1,"PAM_bird_site_final_100km.rds"))

# 75km
saveRDS(site_loc_key_plant_75km, file = file.path(data_path_L1,"site_loc_key_plant_75km.rds"))
#saveRDS(site_loc_key_frugivore_75km, file = file.path(data_path_L1,"site_loc_key_frugivore_75km.rds"))
saveRDS(site_loc_key_mammal_75km, file = file.path(data_path_L1,"site_loc_key_mammal_75km.rds"))
saveRDS(site_loc_key_bird_75km, file = file.path(data_path_L1,"site_loc_key_bird_75km.rds"))

saveRDS(PAM_plant_site_final_75km, file = file.path(data_path_L1,"PAM_plant_site_final_75km.rds"))
#saveRDS(PAM_frugivore_site_final_75km, file = file.path(data_path_L1,"PAM_frugivore_site_final_75km.rds"))
saveRDS(PAM_mammal_site_final_75km, file = file.path(data_path_L1,"PAM_mammal_site_final_75km.rds"))
saveRDS(PAM_bird_site_final_75km, file = file.path(data_path_L1,"PAM_bird_site_final_75km.rds"))

# 50km
saveRDS(site_loc_key_plant_50km, file = file.path(data_path_L1,"site_loc_key_plant_50km.rds"))
#saveRDS(site_loc_key_frugivore_50km, file = file.path(data_path_L1,"site_loc_key_frugivore_50km.rds"))
saveRDS(site_loc_key_mammal_50km, file = file.path(data_path_L1,"site_loc_key_mammal_50km.rds"))
saveRDS(site_loc_key_bird_50km, file = file.path(data_path_L1,"site_loc_key_bird_50km.rds"))

saveRDS(PAM_plant_site_final_50km, file = file.path(data_path_L1,"PAM_plant_site_final_50km.rds"))
#saveRDS(PAM_frugivore_site_final_50km, file = file.path(data_path_L1,"PAM_frugivore_site_final_50km.rds"))
saveRDS(PAM_mammal_site_final_50km, file = file.path(data_path_L1,"PAM_mammal_site_final_50km.rds"))
saveRDS(PAM_bird_site_final_50km, file = file.path(data_path_L1,"PAM_bird_site_final_50km.rds"))

# 25km
saveRDS(site_loc_key_plant_25km, file = file.path(data_path_L1,"site_loc_key_plant_25km.rds"))
#saveRDS(site_loc_key_frugivore_25km, file = file.path(data_path_L1,"site_loc_key_frugivore_25km.rds"))
saveRDS(site_loc_key_mammal_25km, file = file.path(data_path_L1,"site_loc_key_mammal_25km.rds"))
saveRDS(site_loc_key_bird_25km, file = file.path(data_path_L1,"site_loc_key_bird_25km.rds"))

saveRDS(PAM_plant_site_final_25km, file = file.path(data_path_L1,"PAM_plant_site_final_25km.rds"))
#saveRDS(PAM_frugivore_site_final_25km, file = file.path(data_path_L1,"PAM_frugivore_site_final_25km.rds"))
saveRDS(PAM_mammal_site_final_25km, file = file.path(data_path_L1,"PAM_mammal_site_final_25km.rds"))
saveRDS(PAM_bird_site_final_25km, file = file.path(data_path_L1,"PAM_bird_site_final_25km.rds"))

# 10km
saveRDS(site_loc_key_plant_10km, file = file.path(data_path_L1,"site_loc_key_plant_10km.rds"))
#saveRDS(site_loc_key_frugivore_10km, file = file.path(data_path_L1,"site_loc_key_frugivore_10km.rds"))
saveRDS(site_loc_key_mammal_10km, file = file.path(data_path_L1,"site_loc_key_mammal_10km.rds"))
saveRDS(site_loc_key_bird_10km, file = file.path(data_path_L1,"site_loc_key_bird_10km.rds"))

saveRDS(PAM_plant_site_final_10km, file = file.path(data_path_L1,"PAM_plant_site_final_10km.rds"))
#saveRDS(PAM_frugivore_site_final_10km, file = file.path(data_path_L1,"PAM_frugivore_site_final_10km.rds"))
saveRDS(PAM_mammal_site_final_10km, file = file.path(data_path_L1,"PAM_mammal_site_final_10km.rds"))
saveRDS(PAM_bird_site_final_10km, file = file.path(data_path_L1,"PAM_bird_site_final_10km.rds"))

# 5km
saveRDS(site_loc_key_plant_5km, file = file.path(data_path_L1,"site_loc_key_plant_5km.rds"))
#saveRDS(site_loc_key_frugivore_5km, file = file.path(data_path_L1,"site_loc_key_frugivore_5km.rds"))
saveRDS(site_loc_key_mammal_5km, file = file.path(data_path_L1,"site_loc_key_mammal_5km.rds"))
saveRDS(site_loc_key_bird_5km, file = file.path(data_path_L1,"site_loc_key_bird_5km.rds"))

saveRDS(PAM_plant_site_final_5km, file = file.path(data_path_L1,"PAM_plant_site_final_5km.rds"))
#saveRDS(PAM_frugivore_site_final_5km, file = file.path(data_path_L1,"PAM_frugivore_site_final_5km.rds"))
saveRDS(PAM_mammal_site_final_5km, file = file.path(data_path_L1,"PAM_mammal_site_final_5km.rds"))
saveRDS(PAM_bird_site_final_5km, file = file.path(data_path_L1,"PAM_bird_site_final_5km.rds"))
