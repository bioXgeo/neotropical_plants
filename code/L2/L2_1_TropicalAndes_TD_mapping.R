#title: "Tropical Andes Taxonomic Diversity of Plants and Frugivores"
#author: "Hazel J. Anderson, Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "Calculating and mapping taxonomic diversity as species richness using occurrence data for plants and frugivores in the Tropical Andes Moist Lowland and Montane forests. Note: some code is adapted from https://luisdva.github.io/rstats/richness/."
#data input: "plants_sf_species.rds", "frugivores_sf_species.rds", "mammals_sf_species.rds", "birds_sf_species.rds", "Americas.rds", "TApoly.rds", "TropicalAndes_IUCNHabitat_Forest.rds"
#data output: "richness_5km2.rds", "richness_10km2.rds", "richness_25km2.rds", "richness_50km2.rds", "richness_75km2.rds", "richness_100km2.rds", "all_plant_richness_plots.png", "all_frugivore_richness_plots.png", "all_mammal_richness_plots.png", "all_bird_richness_plots.png", "all_richness_plots.png", "TropicalAndes_plantRichness_5km.csv", "TropicalAndes_frugivoreRichness_5km.csv", "TropicalAndes_mammalRichness_5km.csv", "TropicalAndes_birdRichness_5km.csv", "TropicalAndes_plantRichness_10km.csv", "TropicalAndes_frugivoreRichness_10km.csv", "TropicalAndes_mammalRichness_10km.csv", "TropicalAndes_birdRichness_10km.csv", "TropicalAndes_plantRichness_25km.csv", "TropicalAndes_frugivoreRichness_25km.csv", "TropicalAndes_mammalRichness_25km.csv", "TropicalAndes_birdRichness_25km.csv", "TropicalAndes_plantRichness_50km.csv", "TropicalAndes_frugivoreRichness_50km.csv", "TropicalAndes_mammalRichness_50km.csv", "TropicalAndes_birdRichness_50km.csv", "TropicalAndes_plantRichness_75km.csv", "TropicalAndes_frugivoreRichness_75km.csv", "TropicalAndes_mammalRichness_75km.csv", "TropicalAndes_birdRichness_75km.csv", "TropicalAndes_plantRichness_100km.csv", "TropicalAndes_frugivoreRichness_100km.csv", "TropicalAndes_mammalRichness_100km.csv", "TropicalAndes_birdRichness_100km.csv"
#date: "2023-08-01; 2025-10-27"
#notes: JB used HPCC


# load required packages
library(sf); library(dplyr); library(ggplot2); library(parallel); library(foreach); library(doParallel); library(ggspatial); library(ggpubr); library(patchwork); library(rphylopic); library(iNEXT); library(stringr)

data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')

# set file paths
all_data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1/all_data')
all_output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2/all_data')
all_data_figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures/all_data')


# load functions
source("C:/GitHub_projects/neotropical_plants/code/Functions.R")


# read in data

# projected sf objects
Americas <- readRDS(file = file.path(data_path_L1, "Americas.rds"))
TApoly <- readRDS(file = file.path(data_path_L1,"TApoly.rds"))
TropicalAndes_IUCNHabitat_Forest <- readRDS(file = file.path(data_path_L1,"TropicalAndes_IUCNHabitat_Forest.rds"))


#### TD of data filtered by 1970 ####

# set file paths 
filtered_data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1/filtered_data')
filtered_output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2/filtered_data')
filtered_data_figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures/filtered_data')


# mammals
#### 100 km ####

# species occurrence data
mammal_sp_grid_100km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_100km.rds"))

# richness calculation
iNEXT_mammal_100km <- calc_coverage(mammal_sp_grid_100km)
mammal_TD_100km <- iNEXT_mammal_100km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
mammal_TD_map_100km <- TD_map(mammal_TD_100km, 100000, 'mammal')
saveRDS(mammal_TD_map_100km, file = file.path(all_output_path_L2,"mammal_TD_map_100km.rds"))

(mammal_TD_plot_100km <- mammal_TD_map_100km$gridTDTA)
mammal_cell_TD_100km <- mammal_TD_map_100km$spatial_TA_grid

# save data
saveRDS(mammal_cell_TD_100km, file.path(all_output_path_L2,"mammal_cell_TD_100km.rds"))
ggsave('mammal_TD_plot_100km.png', mammal_TD_plot_100km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 75 km ####

# species occurrence data
mammal_sp_grid_75km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_75km.rds"))

# richness calculation
iNEXT_mammal_75km <- calc_coverage(mammal_sp_grid_75km)
mammal_TD_75km <- iNEXT_mammal_75km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
mammal_TD_map_75km <- TD_map(mammal_TD_75km, 75000, 'mammal')
saveRDS(mammal_TD_map_75km, file = file.path(all_output_path_L2,"mammal_TD_map_75km.rds"))

(mammal_TD_plot_75km <- mammal_TD_map_75km$gridTDTA)
mammal_cell_TD_75km <- mammal_TD_map_75km$spatial_TA_grid

# save data
saveRDS(mammal_cell_TD_75km, file.path(all_output_path_L2,"mammal_cell_TD_75km.rds"))
ggsave('mammal_TD_plot_75km.png', mammal_TD_plot_75km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 50 km ####

# species occurrence data
mammal_sp_grid_50km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_50km.rds"))

# richness calculation
iNEXT_mammal_50km <- calc_coverage(mammal_sp_grid_50km)
mammal_TD_50km <- iNEXT_mammal_50km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
mammal_TD_map_50km <- TD_map(mammal_TD_50km, 50000, 'mammal')
saveRDS(mammal_TD_map_50km, file = file.path(all_output_path_L2,"mammal_TD_map_50km.rds"))

(mammal_TD_plot_50km <- mammal_TD_map_50km$gridTDTA)
mammal_cell_TD_50km <- mammal_TD_map_50km$spatial_TA_grid

# save data
saveRDS(mammal_cell_TD_50km, file.path(all_output_path_L2,"mammal_cell_TD_50km.rds"))
ggsave('mammal_TD_plot_50km.png', mammal_TD_plot_50km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 25 km ####

# species occurrence data
mammal_sp_grid_25km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_25km.rds"))

# richness calculation
iNEXT_mammal_25km <- calc_coverage(mammal_sp_grid_25km)
mammal_TD_25km <- iNEXT_mammal_25km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
mammal_TD_map_25km <- TD_map(mammal_TD_25km, 25000, 'mammal')
saveRDS(mammal_TD_map_25km, file = file.path(all_output_path_L2,"mammal_TD_map_25km.rds"))

(mammal_TD_plot_25km <- mammal_TD_map_25km$gridTDTA)
mammal_cell_TD_25km <- mammal_TD_map_25km$spatial_TA_grid

# save data
saveRDS(mammal_cell_TD_25km, file.path(all_output_path_L2,"mammal_cell_TD_25km.rds"))
ggsave('mammal_TD_plot_25km.png', mammal_TD_plot_25km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 10 km ####

# species occurrence data
mammal_sp_grid_10km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_10km.rds"))

# richness calculation
iNEXT_mammal_10km <- calc_coverage(mammal_sp_grid_10km)
mammal_TD_10km <- iNEXT_mammal_10km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
mammal_TD_map_10km <- TD_map(mammal_TD_10km, 10000, 'mammal')
saveRDS(mammal_TD_map_10km, file = file.path(all_output_path_L2,"mammal_TD_map_10km.rds"))

(mammal_TD_plot_10km <- mammal_TD_map_10km$gridTDTA)
mammal_cell_TD_10km <- mammal_TD_map_10km$spatial_TA_grid

# save data
saveRDS(mammal_cell_TD_10km, file.path(all_output_path_L2,"mammal_cell_TD_10km.rds"))
ggsave('mammal_TD_plot_10km.png', mammal_TD_plot_10km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 5 km ####

# species occurrence data
mammal_sp_grid_5km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_5km.rds"))

# richness calculation
iNEXT_mammal_5km <- calc_coverage(mammal_sp_grid_5km)
mammal_TD_5km <- iNEXT_mammal_5km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
mammal_TD_map_5km <- TD_map(mammal_TD_5km, 5000, 'mammal')
saveRDS(mammal_TD_map_5km, file = file.path(all_output_path_L2,"mammal_TD_map_5km.rds"))

(mammal_TD_plot_5km <- mammal_TD_map_5km$gridTDTA)
mammal_cell_TD_5km <- mammal_TD_map_5km$spatial_TA_grid

# save data
saveRDS(mammal_cell_TD_5km, file.path(all_output_path_L2,"mammal_cell_TD_5km.rds"))
ggsave('mammal_TD_plot_5km.png', mammal_TD_plot_5km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


# plants
#### 100 km ####

# species occurrence data
plant_sp_grid_100km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_100km.rds"))

# richness calculation
iNEXT_plant_100km <- calc_coverage(plant_sp_grid_100km)
plant_TD_100km <- iNEXT_plant_100km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
plant_TD_map_100km <- TD_map(plant_TD_100km, 100000, 'plant')
saveRDS(plant_TD_map_100km, file = file.path(all_output_path_L2,"plant_TD_map_100km.rds"))

(plant_TD_plot_100km <- plant_TD_map_100km$gridTDTA)
plant_cell_TD_100km <- plant_TD_map_100km$spatial_TA_grid

# save data
saveRDS(plant_cell_TD_100km, file.path(all_output_path_L2,"plant_cell_TD_100km.rds"))
ggsave('plant_TD_plot_100km.png', plant_TD_plot_100km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 75 km ####

# species occurrence data
plant_sp_grid_75km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_75km.rds"))

# richness calculation
iNEXT_plant_75km <- calc_coverage(plant_sp_grid_75km)
plant_TD_75km <- iNEXT_plant_75km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
plant_TD_map_75km <- TD_map(plant_TD_75km, 75000, 'plant')
saveRDS(plant_TD_map_75km, file = file.path(all_output_path_L2,"plant_TD_map_75km.rds"))

(plant_TD_plot_75km <- plant_TD_map_75km$gridTDTA)
plant_cell_TD_75km <- plant_TD_map_75km$spatial_TA_grid

# save data
saveRDS(plant_cell_TD_75km, file.path(all_output_path_L2,"plant_cell_TD_75km.rds"))
ggsave('plant_TD_plot_75km.png', plant_TD_plot_75km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 50 km ####

# species occurrence data
plant_sp_grid_50km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_50km.rds"))

# richness calculation
iNEXT_plant_50km <- calc_coverage(plant_sp_grid_50km)
plant_TD_50km <- iNEXT_plant_50km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
plant_TD_map_50km <- TD_map(plant_TD_50km, 50000, 'plant')
saveRDS(plant_TD_map_50km, file = file.path(all_output_path_L2,"plant_TD_map_50km.rds"))

(plant_TD_plot_50km <- plant_TD_map_50km$gridTDTA)
plant_cell_TD_50km <- plant_TD_map_50km$spatial_TA_grid

# save data
saveRDS(plant_cell_TD_50km, file.path(all_output_path_L2,"plant_cell_TD_50km.rds"))
ggsave('plant_TD_plot_50km.png', plant_TD_plot_50km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 25 km ####

# species occurrence data
plant_sp_grid_25km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_25km.rds"))

# richness calculation
iNEXT_plant_25km <- calc_coverage(plant_sp_grid_25km)
plant_TD_25km <- iNEXT_plant_25km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
plant_TD_map_25km <- TD_map(plant_TD_25km, 25000, 'plant')
saveRDS(plant_TD_map_25km, file = file.path(all_output_path_L2,"plant_TD_map_25km.rds"))

(plant_TD_plot_25km <- plant_TD_map_25km$gridTDTA)
plant_cell_TD_25km <- plant_TD_map_25km$spatial_TA_grid

# save data
saveRDS(plant_cell_TD_25km, file.path(all_output_path_L2,"plant_cell_TD_25km.rds"))
ggsave('plant_TD_plot_25km.png', plant_TD_plot_25km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 10 km ####

# species occurrence data
plant_sp_grid_10km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_10km.rds"))

# richness calculation
iNEXT_plant_10km <- calc_coverage(plant_sp_grid_10km)
plant_TD_10km <- iNEXT_plant_10km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
plant_TD_map_10km <- TD_map(plant_TD_10km, 10000, 'plant')
saveRDS(plant_TD_map_10km, file = file.path(all_output_path_L2,"plant_TD_map_10km.rds"))

(plant_TD_plot_10km <- plant_TD_map_10km$gridTDTA)
plant_cell_TD_10km <- plant_TD_map_10km$spatial_TA_grid

# save data
saveRDS(plant_cell_TD_10km, file.path(all_output_path_L2,"plant_cell_TD_10km.rds"))
ggsave('plant_TD_plot_10km.png', plant_TD_plot_10km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 5 km ####

# species occurrence data
plant_sp_grid_5km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_5km.rds"))

# richness calculation
iNEXT_plant_5km <- calc_coverage(plant_sp_grid_5km)
plant_TD_5km <- iNEXT_plant_5km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
plant_TD_map_5km <- TD_map(plant_TD_5km, 5000, 'plant')
saveRDS(plant_TD_map_5km, file = file.path(all_output_path_L2,"plant_TD_map_5km.rds"))

(plant_TD_plot_5km <- plant_TD_map_5km$gridTDTA)
plant_cell_TD_5km <- plant_TD_map_5km$spatial_TA_grid

# save data
saveRDS(plant_cell_TD_5km, file.path(all_output_path_L2,"plant_cell_TD_5km.rds"))
ggsave('plant_TD_plot_5km.png', plant_TD_plot_5km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


# birds
#### 100 km ####

# species occurrence data
bird_sp_grid_100km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_100km.rds"))

# richness calculation
iNEXT_bird_100km <- calc_coverage(bird_sp_grid_100km)
bird_TD_100km <- iNEXT_bird_100km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
bird_TD_map_100km <- TD_map(bird_TD_100km, 100000, 'bird')
saveRDS(bird_TD_map_100km, file = file.path(all_output_path_L2,"bird_TD_map_100km.rds"))

(bird_TD_plot_100km <- bird_TD_map_100km$gridTDTA)
bird_cell_TD_100km <- bird_TD_map_100km$spatial_TA_grid

# save data
saveRDS(bird_cell_TD_100km, file.path(all_output_path_L2,"bird_cell_TD_100km.rds"))
ggsave('bird_TD_plot_100km.png', bird_TD_plot_100km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 75 km ####

# species occurrence data
bird_sp_grid_75km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_75km.rds"))

# richness calculation
iNEXT_bird_75km <- calc_coverage(bird_sp_grid_75km)
bird_TD_75km <- iNEXT_bird_75km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
bird_TD_map_75km <- TD_map(bird_TD_75km, 75000, 'bird')
saveRDS(bird_TD_map_75km, file = file.path(all_output_path_L2,"bird_TD_map_75km.rds"))

(bird_TD_plot_75km <- bird_TD_map_75km$gridTDTA)
bird_cell_TD_75km <- bird_TD_map_75km$spatial_TA_grid

# save data
saveRDS(bird_cell_TD_75km, file.path(all_output_path_L2,"bird_cell_TD_75km.rds"))
ggsave('bird_TD_plot_75km.png', bird_TD_plot_75km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 50 km ####

# species occurrence data
bird_sp_grid_50km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_50km.rds"))

# richness calculation
iNEXT_bird_50km <- calc_coverage(bird_sp_grid_50km)
bird_TD_50km <- iNEXT_bird_50km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
bird_TD_map_50km <- TD_map(bird_TD_50km, 50000, 'bird')
saveRDS(bird_TD_map_50km, file = file.path(all_output_path_L2,"bird_TD_map_50km.rds"))

(bird_TD_plot_50km <- bird_TD_map_50km$gridTDTA)
bird_cell_TD_50km <- bird_TD_map_50km$spatial_TA_grid

# save data
saveRDS(bird_cell_TD_50km, file.path(all_output_path_L2,"bird_cell_TD_50km.rds"))
ggsave('bird_TD_plot_50km.png', bird_TD_plot_50km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 25 km ####

# species occurrence data
bird_sp_grid_25km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_25km.rds"))

# richness calculation
iNEXT_bird_25km <- calc_coverage(bird_sp_grid_25km)
bird_TD_25km <- iNEXT_bird_25km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
bird_TD_map_25km <- TD_map(bird_TD_25km, 25000, 'bird')
saveRDS(bird_TD_map_25km, file = file.path(all_output_path_L2,"bird_TD_map_25km.rds"))

(bird_TD_plot_25km <- bird_TD_map_25km$gridTDTA)
bird_cell_TD_25km <- bird_TD_map_25km$spatial_TA_grid

# save data
saveRDS(bird_cell_TD_25km, file.path(all_output_path_L2,"bird_cell_TD_25km.rds"))
ggsave('bird_TD_plot_25km.png', bird_TD_plot_25km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 10 km ####

# species occurrence data
bird_sp_grid_10km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_10km.rds"))

# richness calculation
iNEXT_bird_10km <- calc_coverage(bird_sp_grid_10km)
bird_TD_10km <- iNEXT_bird_10km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
bird_TD_map_10km <- TD_map(bird_TD_10km, 10000, 'bird')
saveRDS(bird_TD_map_10km, file = file.path(all_output_path_L2,"bird_TD_map_10km.rds"))

(bird_TD_plot_10km <- bird_TD_map_10km$gridTDTA)
bird_cell_TD_10km <- bird_TD_map_10km$spatial_TA_grid

# save data
saveRDS(bird_cell_TD_10km, file.path(all_output_path_L2,"bird_cell_TD_10km.rds"))
ggsave('bird_TD_plot_10km.png', bird_TD_plot_10km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 5 km ####

# species occurrence data
bird_sp_grid_5km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_5km.rds"))

# richness calculation
iNEXT_bird_5km <- calc_coverage(bird_sp_grid_5km)
bird_TD_5km <- iNEXT_bird_5km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
bird_TD_map_5km <- TD_map(bird_TD_5km, 5000, 'bird')
saveRDS(bird_TD_map_5km, file = file.path(all_output_path_L2,"bird_TD_map_5km.rds"))

(bird_TD_plot_5km <- bird_TD_map_5km$gridTDTA)
bird_cell_TD_5km <- bird_TD_map_5km$spatial_TA_grid

# save data
saveRDS(bird_cell_TD_5km, file.path(all_output_path_L2,"bird_cell_TD_5km.rds"))
ggsave('bird_TD_plot_5km.png', bird_TD_plot_5km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### final figure ####

# all map data
plant_TD_map_100km <- readRDS(file.path(all_output_path_L2, "plant_TD_map_100km.rds"))
plant_TD_plot_100km <- plant_TD_map_100km$gridTDTA

plant_TD_map_75km <- readRDS(file.path(all_output_path_L2, "plant_TD_map_75km.rds"))
plant_TD_plot_75km <- plant_TD_map_75km$gridTDTA

plant_TD_map_50km <- readRDS(file.path(all_output_path_L2, "plant_TD_map_50km.rds"))
plant_TD_plot_50km <- plant_TD_map_50km$gridTDTA

plant_TD_map_25km <- readRDS(file.path(all_output_path_L2, "plant_TD_map_25km.rds"))
plant_TD_plot_25km <- plant_TD_map_25km$gridTDTA

plant_TD_map_10km <- readRDS(file.path(all_output_path_L2, "plant_TD_map_10km.rds"))
plant_TD_plot_10km <- plant_TD_map_10km$gridTDTA

plant_TD_map_5km <- readRDS(file.path(all_output_path_L2, "plant_TD_map_5km.rds"))
plant_TD_plot_5km <- plant_TD_map_5km$gridTDTA

mammal_TD_map_100km <- readRDS(file.path(all_output_path_L2, "mammal_TD_map_100km.rds"))
mammal_TD_plot_100km <- mammal_TD_map_100km$gridTDTA

mammal_TD_map_75km <- readRDS(file.path(all_output_path_L2, "mammal_TD_map_75km.rds"))
mammal_TD_plot_75km <- mammal_TD_map_75km$gridTDTA

mammal_TD_map_50km <- readRDS(file.path(all_output_path_L2, "mammal_TD_map_50km.rds"))
mammal_TD_plot_50km <- mammal_TD_map_50km$gridTDTA

mammal_TD_map_25km <- readRDS(file.path(all_output_path_L2, "mammal_TD_map_25km.rds"))
mammal_TD_plot_25km <- mammal_TD_map_25km$gridTDTA

mammal_TD_map_10km <- readRDS(file.path(all_output_path_L2, "mammal_TD_map_10km.rds"))
mammal_TD_plot_10km <- mammal_TD_map_10km$gridTDTA

mammal_TD_map_5km <- readRDS(file.path(all_output_path_L2, "mammal_TD_map_5km.rds"))
mammal_TD_plot_5km <- mammal_TD_map_5km$gridTDTA

bird_TD_map_100km <- readRDS(file.path(all_output_path_L2, "bird_TD_map_100km.rds"))
bird_TD_plot_100km <- bird_TD_map_100km$gridTDTA

bird_TD_map_75km <- readRDS(file.path(all_output_path_L2, "bird_TD_map_75km.rds"))
bird_TD_plot_75km <- bird_TD_map_75km$gridTDTA

bird_TD_map_50km <- readRDS(file.path(all_output_path_L2, "bird_TD_map_50km.rds"))
bird_TD_plot_50km <- bird_TD_map_50km$gridTDTA

bird_TD_map_25km <- readRDS(file.path(all_output_path_L2, "bird_TD_map_25km.rds"))
bird_TD_plot_25km <- bird_TD_map_25km$gridTDTA

bird_TD_map_10km <- readRDS(file.path(all_output_path_L2, "bird_TD_map_10km.rds"))
bird_TD_plot_10km <- bird_TD_map_10km$gridTDTA

bird_TD_map_5km <- readRDS(file.path(all_output_path_L2, "bird_TD_map_5km.rds"))
bird_TD_plot_5km <- bird_TD_map_5km$gridTDTA

# individual plot edits
plant_TD_plot_100km <- plant_TD_plot_100km + labs(title='[100km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_TD_plot_100km <- mammal_TD_plot_100km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_TD_plot_100km <- bird_TD_plot_100km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


plant_TD_plot_75km <- plant_TD_plot_75km + labs(title='[75km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_TD_plot_75km <- mammal_TD_plot_75km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_TD_plot_75km <- bird_TD_plot_75km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(size = 16)) + theme(plot.margin = margin(0,0,0,0))


plant_TD_plot_50km <- plant_TD_plot_50km + labs(title='[50km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_TD_plot_50km <- mammal_TD_plot_50km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_TD_plot_50km <- bird_TD_plot_50km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


plant_TD_plot_25km <- plant_TD_plot_25km + labs(title='[25km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_TD_plot_25km <- mammal_TD_plot_25km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_TD_plot_25km <- bird_TD_plot_25km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))+ labs(x = "Longitude")


plant_TD_plot_10km <- plant_TD_plot_10km + labs(title='[10km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_TD_plot_10km <- mammal_TD_plot_10km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_TD_plot_10km <- bird_TD_plot_10km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


# plant picture
plant <- pick_phylopic(name='Coffea alleizettei')

plant_TD_plot_5km <- plant_TD_plot_5km + labs(title='[5km]') + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20)) + add_phylopic(img=plant, x=-79.5, y=13, height=8)

# mammal picture
mammal <- pick_phylopic(name='Potos flavus', n=2, auto=2)

mammal_TD_plot_5km <- mammal_TD_plot_5km + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16)) + labs(y = "Latitude") + add_phylopic(img=mammal, x=-79, y=12, height=8)

# bird picture
bird <- pick_phylopic(name='Ramphastos sulfuratus', n=2, auto=1)

bird_TD_plot_5km <- bird_TD_plot_5km + annotation_scale(location = "bl",width_hint = 0.4, style = "bar") + annotation_north_arrow(location = "bl", which_north = "true", height = unit(0.5, "in"), width = unit(0.5, "in"), pad_x = unit(0.05, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) + add_phylopic(img=bird, x=-80, y=12.5, height=8) + theme(plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


# arrange
all_richness_plots <- plant_TD_plot_5km + plant_TD_plot_10km + plant_TD_plot_25km + plant_TD_plot_50km + plant_TD_plot_75km + plant_TD_plot_100km + mammal_TD_plot_5km + mammal_TD_plot_10km + mammal_TD_plot_25km + mammal_TD_plot_50km + mammal_TD_plot_75km + mammal_TD_plot_100km + bird_TD_plot_5km + bird_TD_plot_10km + bird_TD_plot_25km + bird_TD_plot_50km + bird_TD_plot_75km + bird_TD_plot_100km + plot_layout(ncol = 6, nrow = 3, guides = 'collect', axis_titles = 'collect', heights = c(1, 1, 1)) & theme(legend.position = 'left', axis.title = element_text(size = 20)) & plot_annotation(title='Species richness', theme = theme(plot.title = element_text(hjust = 0.5, size=30, face='bold')))

all_richness_plots

ggsave('all_richness_plots.png', all_richness_plots, path = all_data_figure_path, width = 14, height = 12, units = "in", dpi=1000)


#### repeat FD calculations and mapping with obs cutoff ####

# set file paths
filtered_data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1/filtered_data')
filtered_output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2/filtered_data')
filtered_data_figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures/filtered_data')


# set cutoff
cutoff_obs <- 20


# mammals
#### 100 km ####

# species occurrence data
mammal_cutoff_sp_grid_100km <- readRDS(file.path(filtered_data_path_L1,paste0("mammal_", cutoff_obs, "_sp_grid_100km.rds")))

# richness calculation
iNEXT_mammal_cutoff_100km <- calc_coverage(mammal_cutoff_sp_grid_100km)
mammal_cutoff_TD_100km <- iNEXT_mammal_cutoff_100km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
mammal_cutoff_TD_map_100km <- TD_map(mammal_cutoff_TD_100km, 100000, 'mammal')
saveRDS(mammal_cutoff_TD_map_100km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_map_100km.rds")))

(mammal_cutoff_TD_plot_100km <- mammal_cutoff_TD_map_100km$gridTDTA)
mammal_cutoff_cell_TD_100km <- mammal_cutoff_TD_map_100km$spatial_TA_grid

# save data
saveRDS(mammal_cutoff_cell_TD_100km, file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_cell_TD_100km.rds")))
ggsave(paste0('mammal_', cutoff_obs, '_TD_plot_100km.png'), mammal_TD_plot_100km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 75 km ####

# species occurrence data
mammal_cutoff_sp_grid_75km <- readRDS(file.path(filtered_data_path_L1,paste0("mammal_", cutoff_obs, "_sp_grid_75km.rds")))

# richness calculation
iNEXT_mammal_cutoff_75km <- calc_coverage(mammal_cutoff_sp_grid_75km)
mammal_cutoff_TD_75km <- iNEXT_mammal_cutoff_75km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
mammal_cutoff_TD_map_75km <- TD_map(mammal_cutoff_TD_75km, 75000, 'mammal')
saveRDS(mammal_cutoff_TD_map_75km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_map_75km.rds")))

(mammal_cutoff_TD_plot_75km <- mammal_cutoff_TD_map_75km$gridTDTA)
mammal_cutoff_cell_TD_75km <- mammal_cutoff_TD_map_75km$spatial_TA_grid

# save data
saveRDS(mammal_cutoff_cell_TD_75km, file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_cell_TD_75km.rds")))
ggsave(paste0('mammal_', cutoff_obs, '_TD_plot_75km.png'), mammal_TD_plot_75km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 50 km ####

# species occurrence data
mammal_cutoff_sp_grid_50km <- readRDS(file.path(filtered_data_path_L1,paste0("mammal_", cutoff_obs, "_sp_grid_50km.rds")))

# richness calculation
iNEXT_mammal_cutoff_50km <- calc_coverage(mammal_cutoff_sp_grid_50km)
mammal_cutoff_TD_50km <- iNEXT_mammal_cutoff_50km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
mammal_cutoff_TD_map_50km <- TD_map(mammal_cutoff_TD_50km, 50000, 'mammal')
saveRDS(mammal_cutoff_TD_map_50km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_map_50km.rds")))

(mammal_cutoff_TD_plot_50km <- mammal_cutoff_TD_map_50km$gridTDTA)
mammal_cutoff_cell_TD_50km <- mammal_cutoff_TD_map_50km$spatial_TA_grid

# save data
saveRDS(mammal_cutoff_cell_TD_50km, file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_cell_TD_50km.rds")))
ggsave(paste0('mammal_', cutoff_obs, '_TD_plot_50km.png'), mammal_TD_plot_50km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 25 km ####

# species occurrence data
mammal_cutoff_sp_grid_25km <- readRDS(file.path(filtered_data_path_L1,paste0("mammal_", cutoff_obs, "_sp_grid_25km.rds")))

# richness calculation
iNEXT_mammal_cutoff_25km <- calc_coverage(mammal_cutoff_sp_grid_25km)
mammal_cutoff_TD_25km <- iNEXT_mammal_cutoff_25km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
mammal_cutoff_TD_map_25km <- TD_map(mammal_cutoff_TD_25km, 25000, 'mammal')
saveRDS(mammal_cutoff_TD_map_25km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_map_25km.rds")))

(mammal_cutoff_TD_plot_25km <- mammal_cutoff_TD_map_25km$gridTDTA)
mammal_cutoff_cell_TD_25km <- mammal_cutoff_TD_map_25km$spatial_TA_grid

# save data
saveRDS(mammal_cutoff_cell_TD_25km, file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_cell_TD_25km.rds")))
ggsave(paste0('mammal_', cutoff_obs, '_TD_plot_25km.png'), mammal_TD_plot_25km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 10 km ####

# species occurrence data
mammal_cutoff_sp_grid_10km <- readRDS(file.path(filtered_data_path_L1,paste0("mammal_", cutoff_obs, "_sp_grid_10km.rds")))

# richness calculation
iNEXT_mammal_cutoff_10km <- calc_coverage(mammal_cutoff_sp_grid_10km)
mammal_cutoff_TD_10km <- iNEXT_mammal_cutoff_10km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
mammal_cutoff_TD_map_10km <- TD_map(mammal_cutoff_TD_10km, 10000, 'mammal')
saveRDS(mammal_cutoff_TD_map_10km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_map_10km.rds")))

(mammal_cutoff_TD_plot_10km <- mammal_cutoff_TD_map_10km$gridTDTA)
mammal_cutoff_cell_TD_10km <- mammal_cutoff_TD_map_10km$spatial_TA_grid

# save data
saveRDS(mammal_cutoff_cell_TD_10km, file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_cell_TD_10km.rds")))
ggsave(paste0('mammal_', cutoff_obs, '_TD_plot_10km.png'), mammal_TD_plot_10km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 5 km ####

# species occurrence data
mammal_cutoff_sp_grid_5km <- readRDS(file.path(filtered_data_path_L1,paste0("mammal_", cutoff_obs, "_sp_grid_5km.rds")))

# richness calculation
iNEXT_mammal_cutoff_5km <- calc_coverage(mammal_cutoff_sp_grid_5km)
mammal_cutoff_TD_5km <- iNEXT_mammal_cutoff_5km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
mammal_cutoff_TD_map_5km <- TD_map(mammal_cutoff_TD_5km, 5000, 'mammal')
saveRDS(mammal_cutoff_TD_map_5km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_map_5km.rds")))

(mammal_cutoff_TD_plot_5km <- mammal_cutoff_TD_map_5km$gridTDTA)
mammal_cutoff_cell_TD_5km <- mammal_cutoff_TD_map_5km$spatial_TA_grid

# save data
saveRDS(mammal_cutoff_cell_TD_5km, file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_cell_TD_5km.rds")))
ggsave(paste0('mammal_', cutoff_obs, '_TD_plot_5km.png'), mammal_TD_plot_5km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


# plants
#### 100 km ####

# species occurrence data
plant_cutoff_sp_grid_100km <- readRDS(file.path(filtered_data_path_L1,paste0("plant_", cutoff_obs, "_sp_grid_100km.rds")))

# richness calculation
iNEXT_plant_cutoff_100km <- calc_coverage(plant_cutoff_sp_grid_100km)
plant_cutoff_TD_100km <- iNEXT_plant_cutoff_100km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
plant_cutoff_TD_map_100km <- TD_map(plant_cutoff_TD_100km, 100000, 'plant')
saveRDS(plant_cutoff_TD_map_100km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_map_100km.rds")))

(plant_cutoff_TD_plot_100km <- plant_cutoff_TD_map_100km$gridTDTA)
plant_cutoff_cell_TD_100km <- plant_cutoff_TD_map_100km$spatial_TA_grid

# save data
saveRDS(plant_cutoff_cell_TD_100km, file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_cell_TD_100km.rds")))
ggsave(paste0('plant_', cutoff_obs, '_TD_plot_100km.png'), plant_TD_plot_100km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 75 km ####

# species occurrence data
plant_cutoff_sp_grid_75km <- readRDS(file.path(filtered_data_path_L1,paste0("plant_", cutoff_obs, "_sp_grid_75km.rds")))

# richness calculation
iNEXT_plant_cutoff_75km <- calc_coverage(plant_cutoff_sp_grid_75km)
plant_cutoff_TD_75km <- iNEXT_plant_cutoff_75km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
plant_cutoff_TD_map_75km <- TD_map(plant_cutoff_TD_75km, 75000, 'plant')
saveRDS(plant_cutoff_TD_map_75km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_map_75km.rds")))

(plant_cutoff_TD_plot_75km <- plant_cutoff_TD_map_75km$gridTDTA)
plant_cutoff_cell_TD_75km <- plant_cutoff_TD_map_75km$spatial_TA_grid

# save data
saveRDS(plant_cutoff_cell_TD_75km, file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_cell_TD_75km.rds")))
ggsave(paste0('plant_', cutoff_obs, '_TD_plot_75km.png'), plant_TD_plot_75km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 50 km ####

# species occurrence data
plant_cutoff_sp_grid_50km <- readRDS(file.path(filtered_data_path_L1,paste0("plant_", cutoff_obs, "_sp_grid_50km.rds")))

# richness calculation
iNEXT_plant_cutoff_50km <- calc_coverage(plant_cutoff_sp_grid_50km)
plant_cutoff_TD_50km <- iNEXT_plant_cutoff_50km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
plant_cutoff_TD_map_50km <- TD_map(plant_cutoff_TD_50km, 50000, 'plant')
saveRDS(plant_cutoff_TD_map_50km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_map_50km.rds")))

(plant_cutoff_TD_plot_50km <- plant_cutoff_TD_map_50km$gridTDTA)
plant_cutoff_cell_TD_50km <- plant_cutoff_TD_map_50km$spatial_TA_grid

# save data
saveRDS(plant_cutoff_cell_TD_50km, file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_cell_TD_50km.rds")))
ggsave(paste0('plant_', cutoff_obs, '_TD_plot_50km.png'), plant_TD_plot_50km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 25 km ####

# species occurrence data
plant_cutoff_sp_grid_25km <- readRDS(file.path(filtered_data_path_L1,paste0("plant_", cutoff_obs, "_sp_grid_25km.rds")))

# richness calculation
iNEXT_plant_cutoff_25km <- calc_coverage(plant_cutoff_sp_grid_25km)
plant_cutoff_TD_25km <- iNEXT_plant_cutoff_25km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
plant_cutoff_TD_map_25km <- TD_map(plant_cutoff_TD_25km, 25000, 'plant')
saveRDS(plant_cutoff_TD_map_25km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_map_25km.rds")))

(plant_cutoff_TD_plot_25km <- plant_cutoff_TD_map_25km$gridTDTA)
plant_cutoff_cell_TD_25km <- plant_cutoff_TD_map_25km$spatial_TA_grid

# save data
saveRDS(plant_cutoff_cell_TD_25km, file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_cell_TD_25km.rds")))
ggsave(paste0('plant_', cutoff_obs, '_TD_plot_25km.png'), plant_TD_plot_25km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 10 km ####

# species occurrence data
plant_cutoff_sp_grid_10km <- readRDS(file.path(filtered_data_path_L1,paste0("plant_", cutoff_obs, "_sp_grid_10km.rds")))

# richness calculation
iNEXT_plant_cutoff_10km <- calc_coverage(plant_cutoff_sp_grid_10km)
plant_cutoff_TD_10km <- iNEXT_plant_cutoff_10km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
plant_cutoff_TD_map_10km <- TD_map(plant_cutoff_TD_10km, 10000, 'plant')
saveRDS(plant_cutoff_TD_map_10km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_map_10km.rds")))

(plant_cutoff_TD_plot_10km <- plant_cutoff_TD_map_10km$gridTDTA)
plant_cutoff_cell_TD_10km <- plant_cutoff_TD_map_10km$spatial_TA_grid

# save data
saveRDS(plant_cutoff_cell_TD_10km, file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_cell_TD_10km.rds")))
ggsave(paste0('plant_', cutoff_obs, '_TD_plot_10km.png'), plant_TD_plot_10km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 5 km ####

# species occurrence data
plant_cutoff_sp_grid_5km <- readRDS(file.path(filtered_data_path_L1,paste0("plant_", cutoff_obs, "_sp_grid_5km.rds")))

# richness calculation
iNEXT_plant_cutoff_5km <- calc_coverage(plant_cutoff_sp_grid_5km)
plant_cutoff_TD_5km <- iNEXT_plant_cutoff_5km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
plant_cutoff_TD_map_5km <- TD_map(plant_cutoff_TD_5km, 5000, 'plant')
saveRDS(plant_cutoff_TD_map_5km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_map_5km.rds")))

(plant_cutoff_TD_plot_5km <- plant_cutoff_TD_map_5km$gridTDTA)
plant_cutoff_cell_TD_5km <- plant_cutoff_TD_map_5km$spatial_TA_grid

# save data
saveRDS(plant_cutoff_cell_TD_5km, file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_cell_TD_5km.rds")))
ggsave(paste0('plant_', cutoff_obs, '_TD_plot_5km.png'), plant_TD_plot_5km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


# birds
#### 100 km ####

# species occurrence data
bird_cutoff_sp_grid_100km <- readRDS(file.path(filtered_data_path_L1,paste0("bird_", cutoff_obs, "_sp_grid_100km.rds")))

# richness calculation
iNEXT_bird_cutoff_100km <- calc_coverage(bird_cutoff_sp_grid_100km)
bird_cutoff_TD_100km <- iNEXT_bird_cutoff_100km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
bird_cutoff_TD_map_100km <- TD_map(bird_cutoff_TD_100km, 100000, 'bird')
saveRDS(bird_cutoff_TD_map_100km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_map_100km.rds")))

(bird_cutoff_TD_plot_100km <- bird_cutoff_TD_map_100km$gridTDTA)
bird_cutoff_cell_TD_100km <- bird_cutoff_TD_map_100km$spatial_TA_grid

# save data
saveRDS(bird_cutoff_cell_TD_100km, file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_cell_TD_100km.rds")))
ggsave(paste0('bird_', cutoff_obs, '_TD_plot_100km.png'), bird_TD_plot_100km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 75 km ####

# species occurrence data
bird_cutoff_sp_grid_75km <- readRDS(file.path(filtered_data_path_L1,paste0("bird_", cutoff_obs, "_sp_grid_75km.rds")))

# richness calculation
iNEXT_bird_cutoff_75km <- calc_coverage(bird_cutoff_sp_grid_75km)
bird_cutoff_TD_75km <- iNEXT_bird_cutoff_75km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
bird_cutoff_TD_map_75km <- TD_map(bird_cutoff_TD_75km, 75000, 'bird')
saveRDS(bird_cutoff_TD_map_75km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_map_75km.rds")))

(bird_cutoff_TD_plot_75km <- bird_cutoff_TD_map_75km$gridTDTA)
bird_cutoff_cell_TD_75km <- bird_cutoff_TD_map_75km$spatial_TA_grid

# save data
saveRDS(bird_cutoff_cell_TD_75km, file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_cell_TD_75km.rds")))
ggsave(paste0('bird_', cutoff_obs, '_TD_plot_75km.png'), bird_TD_plot_75km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 50 km ####

# species occurrence data
bird_cutoff_sp_grid_50km <- readRDS(file.path(filtered_data_path_L1,paste0("bird_", cutoff_obs, "_sp_grid_50km.rds")))

# richness calculation
iNEXT_bird_cutoff_50km <- calc_coverage(bird_cutoff_sp_grid_50km)
bird_cutoff_TD_50km <- iNEXT_bird_cutoff_50km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
bird_cutoff_TD_map_50km <- TD_map(bird_cutoff_TD_50km, 50000, 'bird')
saveRDS(bird_cutoff_TD_map_50km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_map_50km.rds")))

(bird_cutoff_TD_plot_50km <- bird_cutoff_TD_map_50km$gridTDTA)
bird_cutoff_cell_TD_50km <- bird_cutoff_TD_map_50km$spatial_TA_grid

# save data
saveRDS(bird_cutoff_cell_TD_50km, file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_cell_TD_50km.rds")))
ggsave(paste0('bird_', cutoff_obs, '_TD_plot_50km.png'), bird_TD_plot_50km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 25 km ####

# species occurrence data
bird_cutoff_sp_grid_25km <- readRDS(file.path(filtered_data_path_L1,paste0("bird_", cutoff_obs, "_sp_grid_25km.rds")))

# richness calculation
iNEXT_bird_cutoff_25km <- calc_coverage(bird_cutoff_sp_grid_25km)
bird_cutoff_TD_25km <- iNEXT_bird_cutoff_25km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
bird_cutoff_TD_map_25km <- TD_map(bird_cutoff_TD_25km, 25000, 'bird')
saveRDS(bird_cutoff_TD_map_25km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_map_25km.rds")))

(bird_cutoff_TD_plot_25km <- bird_cutoff_TD_map_25km$gridTDTA)
bird_cutoff_cell_TD_25km <- bird_cutoff_TD_map_25km$spatial_TA_grid

# save data
saveRDS(bird_cutoff_cell_TD_25km, file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_cell_TD_25km.rds")))
ggsave(paste0('bird_', cutoff_obs, '_TD_plot_25km.png'), bird_TD_plot_25km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 10 km ####

# species occurrence data
bird_cutoff_sp_grid_10km <- readRDS(file.path(filtered_data_path_L1,paste0("bird_", cutoff_obs, "_sp_grid_10km.rds")))

# richness calculation
iNEXT_bird_cutoff_10km <- calc_coverage(bird_cutoff_sp_grid_10km)
bird_cutoff_TD_10km <- iNEXT_bird_cutoff_10km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
bird_cutoff_TD_map_10km <- TD_map(bird_cutoff_TD_10km, 10000, 'bird')
saveRDS(bird_cutoff_TD_map_10km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_map_10km.rds")))

(bird_cutoff_TD_plot_10km <- bird_cutoff_TD_map_10km$gridTDTA)
bird_cutoff_cell_TD_10km <- bird_cutoff_TD_map_10km$spatial_TA_grid

# save data
saveRDS(bird_cutoff_cell_TD_10km, file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_cell_TD_10km.rds")))
ggsave(paste0('bird_', cutoff_obs, '_TD_plot_10km.png'), bird_TD_plot_10km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 5 km ####

# species occurrence data
bird_cutoff_sp_grid_5km <- readRDS(file.path(filtered_data_path_L1,paste0("bird_", cutoff_obs, "_sp_grid_5km.rds")))

# richness calculation
iNEXT_bird_cutoff_5km <- calc_coverage(bird_cutoff_sp_grid_5km)
bird_cutoff_TD_5km <- iNEXT_bird_cutoff_5km$iNEXT_calcs |> 
  rename(richness_raw = S.obs, cellid = Assemblage) |> 
  select(cellid, richness_raw)

# mapping
bird_cutoff_TD_map_5km <- TD_map(bird_cutoff_TD_5km, 5000, 'bird')
saveRDS(bird_cutoff_TD_map_5km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_map_5km.rds")))

(bird_cutoff_TD_plot_5km <- bird_cutoff_TD_map_5km$gridTDTA)
bird_cutoff_cell_TD_5km <- bird_cutoff_TD_map_5km$spatial_TA_grid

# save data
saveRDS(bird_cutoff_cell_TD_5km, file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_cell_TD_5km.rds")))
ggsave(paste0('bird_', cutoff_obs, '_TD_plot_5km.png'), bird_TD_plot_5km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### final figure ####

# all map data
plant_cutoff_TD_map_100km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_map_100km.rds")))
plant_cutoff_TD_plot_100km <- plant_cutoff_TD_map_100km$gridTDTA

plant_cutoff_TD_map_75km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_map_75km.rds")))
plant_cutoff_TD_plot_75km <- plant_cutoff_TD_map_75km$gridTDTA

plant_cutoff_TD_map_50km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_map_50km.rds")))
plant_cutoff_TD_plot_50km <- plant_cutoff_TD_map_50km$gridTDTA

plant_cutoff_TD_map_25km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_map_25km.rds")))
plant_cutoff_TD_plot_25km <- plant_cutoff_TD_map_25km$gridTDTA

plant_cutoff_TD_map_10km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_map_10km.rds")))
plant_cutoff_TD_plot_10km <- plant_cutoff_TD_map_10km$gridTDTA

plant_cutoff_TD_map_5km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_map_5km.rds")))
plant_cutoff_TD_plot_5km <- plant_cutoff_TD_map_5km$gridTDTA

mammal_cutoff_TD_map_100km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_map_100km.rds")))
mammal_cutoff_TD_plot_100km <- mammal_cutoff_TD_map_100km$gridTDTA

mammal_cutoff_TD_map_75km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_map_75km.rds")))
mammal_cutoff_TD_plot_75km <- mammal_cutoff_TD_map_75km$gridTDTA

mammal_cutoff_TD_map_50km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_map_50km.rds")))
mammal_cutoff_TD_plot_50km <- mammal_cutoff_TD_map_50km$gridTDTA

mammal_cutoff_TD_map_25km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_map_25km.rds")))
mammal_cutoff_TD_plot_25km <- mammal_cutoff_TD_map_25km$gridTDTA

mammal_cutoff_TD_map_10km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_map_10km.rds")))
mammal_cutoff_TD_plot_10km <- mammal_cutoff_TD_map_10km$gridTDTA

mammal_cutoff_TD_map_5km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_map_5km.rds")))
mammal_cutoff_TD_plot_5km <- mammal_cutoff_TD_map_5km$gridTDTA

bird_cutoff_TD_map_100km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_map_100km.rds")))
bird_cutoff_TD_plot_100km <- bird_cutoff_TD_map_100km$gridTDTA

bird_cutoff_TD_map_75km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_map_75km.rds")))
bird_cutoff_TD_plot_75km <- bird_cutoff_TD_map_75km$gridTDTA

bird_cutoff_TD_map_50km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_map_50km.rds")))
bird_cutoff_TD_plot_50km <- bird_cutoff_TD_map_50km$gridTDTA

bird_cutoff_TD_map_25km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_map_25km.rds")))
bird_cutoff_TD_plot_25km <- bird_cutoff_TD_map_25km$gridTDTA

bird_cutoff_TD_map_10km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_map_10km.rds")))
bird_cutoff_TD_plot_10km <- bird_cutoff_TD_map_10km$gridTDTA

bird_cutoff_TD_map_5km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_map_5km.rds")))
bird_cutoff_TD_plot_5km <- bird_cutoff_TD_map_5km$gridTDTA


# individual plot edits
plant_cutoff_TD_plot_100km <- plant_cutoff_TD_plot_100km + labs(title='[100km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_cutoff_TD_plot_100km <- mammal_cutoff_TD_plot_100km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_TD_plot_100km <- bird_cutoff_TD_plot_100km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


plant_cutoff_TD_plot_75km <- plant_cutoff_TD_plot_75km + labs(title='[75km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_cutoff_TD_plot_75km <- mammal_cutoff_TD_plot_75km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_TD_plot_75km <- bird_cutoff_TD_plot_75km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(size = 16)) + theme(plot.margin = margin(0,0,0,0))


plant_cutoff_TD_plot_50km <- plant_cutoff_TD_plot_50km + labs(title='[50km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_cutoff_TD_plot_50km <- mammal_cutoff_TD_plot_50km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_TD_plot_50km <- bird_cutoff_TD_plot_50km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


plant_cutoff_TD_plot_25km <- plant_cutoff_TD_plot_25km + labs(title='[25km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_cutoff_TD_plot_25km <- mammal_cutoff_TD_plot_25km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_TD_plot_25km <- bird_cutoff_TD_plot_25km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))+ labs(x = "Longitude")


plant_cutoff_TD_plot_10km <- plant_cutoff_TD_plot_10km + labs(title='[10km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_cutoff_TD_plot_10km <- mammal_cutoff_TD_plot_10km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_TD_plot_10km <- bird_cutoff_TD_plot_10km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


# plant picture
plant <- pick_phylopic(name='Coffea alleizettei')

plant_cutoff_TD_plot_5km <- plant_cutoff_TD_plot_5km + labs(title='[5km]') + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20)) + add_phylopic(img=plant, x=-79.5, y=13, height=8)

# mammal picture
mammal <- pick_phylopic(name='Potos flavus', n=2, auto=2)

mammal_cutoff_TD_plot_5km <- mammal_cutoff_TD_plot_5km + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16)) + labs(y = "Latitude") + add_phylopic(img=mammal, x=-79, y=12, height=8)

# bird picture
bird <- pick_phylopic(name='Ramphastos sulfuratus', n=2, auto=1)

bird_cutoff_TD_plot_5km <- bird_cutoff_TD_plot_5km + annotation_scale(location = "bl",width_hint = 0.4, style = "bar") + annotation_north_arrow(location = "bl", which_north = "true", height = unit(0.5, "in"), width = unit(0.5, "in"), pad_x = unit(0.05, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) + add_phylopic(img=bird, x=-80, y=12.5, height=8) + theme(plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


# arrange
all_cutoff_richness_plots <- plant_cutoff_TD_plot_5km + plant_cutoff_TD_plot_10km + plant_cutoff_TD_plot_25km + plant_cutoff_TD_plot_50km + plant_cutoff_TD_plot_75km + plant_cutoff_TD_plot_100km + mammal_cutoff_TD_plot_5km + mammal_cutoff_TD_plot_10km + mammal_cutoff_TD_plot_25km + mammal_cutoff_TD_plot_50km + mammal_cutoff_TD_plot_75km + mammal_cutoff_TD_plot_100km + bird_cutoff_TD_plot_5km + bird_cutoff_TD_plot_10km + bird_cutoff_TD_plot_25km + bird_cutoff_TD_plot_50km + bird_cutoff_TD_plot_75km + bird_cutoff_TD_plot_100km + plot_layout(ncol = 6, nrow = 3, guides = 'collect', axis_titles = 'collect', heights = c(1, 1, 1)) & theme(legend.position = 'left', axis.title = element_text(size = 20)) & plot_annotation(title='Species richness', theme = theme(plot.title = element_text(hjust = 0.5, size=30, face='bold')))

all_cutoff_richness_plots

ggsave(paste0('all_richness', cutoff_obs, '_plots.png'), all_cutoff_richness_plots, path = filtered_data_figure_path, width = 14, height = 12, units = "in", dpi=1000)
