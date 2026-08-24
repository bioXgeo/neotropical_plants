#title: "Tropical Andes Functional diversity for plants and Frugivores"
#author: "Hazel J. Anderson, Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "Calculating and mapping functional diversity as functional dispersion for plants and frugivores."
#data input: "plants_sf_species.rds", "frugivores_sf_species.rds", "Americas.rds", "TApoly.rds", "TropicalAndes_IUCNHabitat_Forest.rds", "plant_traits_df_final.rds", "frugivore_traits_df_final.rds", "mammal_traits_df_final.rds", "bird_traits_df_final.rds", "site_loc_key_plant_100km.rds", "site_loc_key_frugivore_100km.rds", "site_loc_key_mammal_100km.rds", "site_loc_key_bird_100km.rds", "PAM_plant_site_final_100km.rds", "PAM_frugivore_site_final_100km.rds", "PAM_mammal_site_final_100km.rds", "PAM_bird_site_final_100km.rds", "site_loc_key_plant_75km.rds", "site_loc_key_frugivore_75km.rds", "site_loc_key_mammal_75km.rds", "site_loc_key_bird_75km.rds", "PAM_plant_site_final_75km.rds", "PAM_frugivore_site_final_75km.rds", "PAM_mammal_site_final_75km.rds", "PAM_bird_site_final_75km.rds", "site_loc_key_plant_50km.rds", "site_loc_key_mammal_50km.rds", "site_loc_key_bird_50km.rds", "site_loc_key_frugivore_50km.rds", "PAM_plant_site_final_50km.rds", "PAM_frugivore_site_final_50km.rds", "PAM_mammal_site_final_50km.rds", "PAM_bird_site_final_50km.rds", "site_loc_key_plant_25km.rds", "site_loc_key_frugivore_25km.rds", "site_loc_key_mammal_25km.rds", "site_loc_key_bird_25km.rds", "PAM_plant_site_final_25km.rds", "PAM_frugivore_site_final_25km.rds", "PAM_mammal_site_final_25km.rds", "PAM_bird_site_final_25km.rds", "site_loc_key_plant_10km.rds", "site_loc_key_frugivore_10km.rds", "site_loc_key_mammal_10km.rds", "site_loc_key_bird_10km.rds", "PAM_plant_site_final_10km.rds", "PAM_frugivore_site_final_10km.rds", "PAM_mammal_site_final_10km.rds", "PAM_bird_site_final_10km.rds", "site_loc_key_plant_5km.rds", "site_loc_key_frugivore_5km.rds", "site_loc_key_mammal_5km.rds", "site_loc_key_bird_5km.rds", "PAM_plant_site_final_5km.rds", "PAM_frugivore_site_final_5km.rds", "PAM_mammal_site_final_5km.rds", "PAM_bird_site_final_5km.rds"
#data output: "fdis_frugivore_100km.rds", "fdis_mammal_100km.rds", "fdis_bird_100km.rds", "fdis_plant_100km.rds", "fdis_frugivore_75km.rds", "fdis_mammal_75km.rds", "fdis_bird_75km.rds", "fdis_plant_75km.rds", "fdis_frugivore_50km.rds", "fdis_mammal_50km.rds", "fdis_bird_50km.rds", "fdis_plant_50km.rds", "fdis_frugivore_25km.rds", "fdis_mammal_25km.rds", "fdis_bird_25km.rds", "fdis_plant_25km.rds", "fdis_frugivore_10km.rds", "fdis_mammal_10km.rds", "fdis_bird_10km.rds", "fdis_plant_10km.rds", "fdis_frugivore_5km.rds", "fdis_mammal_5km.rds", "fdis_bird_5km.rds", "fdis_plant_5km.rds", "all_frugivore_fdis_plots.png", "all_mammal_fdis_plots.png", "all_bird_fdis_plots.png", "all_plant_fdis_plots.png"
#date: "2023-08-03; 2025-11-03"
#notes: JB used HPCC


# load required packages
library(mFD); library(sf); library(dplyr); library(ggplot2); library(rnaturalearth); library(ggspatial); library(rlang); library(doParallel); library(foreach); library(purrr); library(ggpubr); library(patchwork); library(stringr); library(rphylopic)


# set file paths
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')


# load functions
source("C:/GitHub_projects/neotropical_plants/code/Functions.R")


# read in Data

# projected sf objects
Americas <- readRDS(file = file.path(data_path_L1, "Americas.rds"))
TApoly <- readRDS(file = file.path(data_path_L1,"TApoly.rds"))
TropicalAndes_IUCNHabitat_Forest <- readRDS(file = file.path(data_path_L1,"TropicalAndes_IUCNHabitat_Forest.rds"))


#### FD of data filtered by 1970 ####

# set file paths
all_data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1/all_data')
all_output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2/all_data')
all_data_figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures/all_data')


# trait data
plant_traits_df_final <- readRDS(file = file.path(all_data_path_L1,"plant_traits_df_final.rds"))
mammal_traits_df_final <- readRDS(file=file.path(all_data_path_L1, 'mammal_traits_df_final.rds'))
bird_traits_df_final <- readRDS(file=file.path(all_data_path_L1, 'bird_traits_df_final.rds'))


# mammals
#### 100 km ####

# species occurrence data
mammal_sp_grid_100km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_100km.rds"))

# quality of functional spaces
fspaces_quality2(mammal_sp_grid_100km, mammal_traits_df_final, 'mammal')

fspace_quality_plot(fspaces_quality2_mammal)

pc_coords2(fspaces_quality2_mammal, mammal_traits_df_final, 'mammal')
fspace_corr_plots(sp_faxes2_coord_mammal, tr_faxes2_mammal)
saveRDS(sp_faxes2_coord_mammal, file = file.path(all_output_path_L2, "sp_faxes2_coord_mammal.rds"))
sp_faxes2_coord_mammal <- readRDS(file.path(all_output_path_L2, "sp_faxes2_coord_mammal.rds"))


# FDis calculation
fdis_mammal2_100km <- FDis2(mammal_sp_grid_100km, sp_faxes2_coord_mammal)
saveRDS(fdis_mammal2_100km, file.path(all_output_path_L2,"fdis_mammal_100km.rds"))

# mapping

# set limits for all mammal maps based off of 100 km
lims <- c(0, max(fdis_mammal2_100km$fdis))
mpt <- max(fdis_mammal2_100km$fdis)/2

mammal_FD_map_100km <- FD_map2(fdis_mammal2_100km, 100000, 'mammal')
saveRDS(mammal_FD_map_100km, file = file.path(all_output_path_L2,"mammal_FD_map_100km.rds"))

(mammal_FD_plot_100km <- mammal_FD_map_100km$gridFDisTA)
mammal_cell_FD_100km <- mammal_FD_map_100km$spatial_fdis_grid

# save data
saveRDS(mammal_cell_FD_100km, file.path(all_output_path_L2,"mammal_cell_FD_100km.rds"))
ggsave('mammal_FD_plot_100km.png', mammal_FD_plot_100km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 75km ####

# species occurrence data
mammal_sp_grid_75km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_75km.rds"))

# FDis calculation
fdis_mammal2_75km <- FDis2(mammal_sp_grid_75km, sp_faxes2_coord_mammal)
saveRDS(fdis_mammal2_75km, file.path(all_output_path_L2,"fdis_mammal_75km.rds"))

# mapping
mammal_FD_map_75km <- FD_map2(fdis_mammal2_75km, 75000, 'mammal')
saveRDS(mammal_FD_map_75km, file = file.path(all_output_path_L2,"mammal_FD_map_75km.rds"))

(mammal_FD_plot_75km <- mammal_FD_map_75km$gridFDisTA)
mammal_cell_FD_75km <- mammal_FD_map_75km$spatial_fdis_grid

# save data
saveRDS(mammal_cell_FD_75km, file.path(all_output_path_L2,"mammal_cell_FD_75km.rds"))
ggsave('mammal_FD_plot_75km.png', mammal_FD_plot_75km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 50km ####

# species occurrence data
mammal_sp_grid_50km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_50km.rds"))

# FDis calculation
fdis_mammal2_50km <- FDis2(mammal_sp_grid_50km, sp_faxes2_coord_mammal)
saveRDS(fdis_mammal2_50km, file.path(all_output_path_L2,"fdis_mammal_50km.rds"))

# mapping 
mammal_FD_map_50km <- FD_map2(fdis_mammal2_50km, 50000, 'mammal')
saveRDS(mammal_FD_map_50km, file = file.path(all_output_path_L2,"mammal_FD_map_50km.rds"))

(mammal_FD_plot_50km <- mammal_FD_map_50km$gridFDisTA)
mammal_cell_FD_50km <- mammal_FD_map_50km$spatial_fdis_grid

# save data
saveRDS(mammal_cell_FD_50km, file.path(all_output_path_L2,"mammal_cell_FD_50km.rds"))
ggsave('mammal_FD_plot_50km.png', mammal_FD_plot_50km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 25km ####

# species occurrence data
mammal_sp_grid_25km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_25km.rds"))

# FDis calculation
fdis_mammal2_25km <- FDis2(mammal_sp_grid_25km, sp_faxes2_coord_mammal)
saveRDS(fdis_mammal2_25km, file.path(all_output_path_L2,"fdis_mammal_25km.rds"))

# mapping
mammal_FD_map_25km <- FD_map2(fdis_mammal2_25km, 25000, 'mammal')
saveRDS(mammal_FD_map_25km, file = file.path(all_output_path_L2,"mammal_FD_map_25km.rds"))

(mammal_FD_plot_25km <- mammal_FD_map_25km$gridFDisTA)
mammal_cell_FD_25km <- mammal_FD_map_25km$spatial_fdis_grid

# save data
saveRDS(mammal_cell_FD_25km, file.path(all_output_path_L2,"mammal_cell_FD_25km.rds"))
ggsave('mammal_FD_plot_25km.png', mammal_FD_plot_25km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 10km ####

# species occurrence data
mammal_sp_grid_10km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_10km.rds"))

# FDis calculation
fdis_mammal2_10km <- FDis2(mammal_sp_grid_10km, sp_faxes2_coord_mammal)
saveRDS(fdis_mammal2_10km, file.path(all_output_path_L2,"fdis_mammal_10km.rds"))

# mapping
mammal_FD_map_10km <- FD_map2(fdis_mammal2_10km, 10000, 'mammal')
saveRDS(mammal_FD_map_10km, file = file.path(all_output_path_L2,"mammal_FD_map_10km.rds"))

(mammal_FD_plot_10km <- mammal_FD_map_10km$gridFDisTA)
mammal_cell_FD_10km <- mammal_FD_map_10km$spatial_fdis_grid

# save data
saveRDS(mammal_cell_FD_10km, file.path(all_output_path_L2,"mammal_cell_FD_10km.rds"))
ggsave('mammal_FD_plot_10km.png', mammal_FD_plot_10km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 5km ####

# species occurrence data
mammal_sp_grid_5km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_5km.rds"))

# FDis calculation 
fdis_mammal2_5km <- FDis2(mammal_sp_grid_5km, sp_faxes2_coord_mammal)
saveRDS(fdis_mammal2_5km, file.path(all_output_path_L2,"fdis_mammal_5km.rds"))

# mapping
mammal_FD_map_5km <- FD_map2(fdis_mammal2_5km, 5000, 'mammal')
saveRDS(mammal_FD_map_5km, file = file.path(all_output_path_L2,"mammal_FD_map_5km.rds"))

(mammal_FD_plot_5km <- mammal_FD_map_5km$gridFDisTA)
mammal_cell_FD_5km <- mammal_FD_map_5km$spatial_fdis_grid

# save data
saveRDS(mammal_cell_FD_5km, file.path(all_output_path_L2,"mammal_cell_FD_5km.rds"))
ggsave('mammal_FD_plot_5km.png', mammal_FD_plot_5km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


# plants
#### 100 km ####

# species occurrence data
plant_sp_grid_100km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_100km.rds"))

# quality of functional spaces
fspaces_quality2(plant_sp_grid_100km, plant_traits_df_final, 'plant')

fspace_quality_plot(fspaces_quality2_plant)

pc_coords2(fspaces_quality2_plant, plant_traits_df_final, 'plant')
fspace_corr_plots(sp_faxes2_coord_plant, tr_faxes2_plant)
saveRDS(sp_faxes2_coord_plant, file = file.path(all_output_path_L2, "sp_faxes2_coord_plant.rds"))
sp_faxes2_coord_plant <- readRDS(file.path(all_output_path_L2, "sp_faxes2_coord_plant.rds"))


# FDis calculation
fdis_plant2_100km <- FDis2(plant_sp_grid_100km, sp_faxes2_coord_plant)
saveRDS(fdis_plant2_100km, file.path(all_output_path_L2,"fdis_plant_100km.rds"))

# mapping

# set limits for all plant maps based off of 100 km
lims <- c(0, max(fdis_plant2_100km$fdis))
mpt <- max(fdis_plant2_100km$fdis)/2

plant_FD_map_100km <- FD_map2(fdis_plant2_100km, 100000, 'plant')
saveRDS(plant_FD_map_100km, file = file.path(all_output_path_L2,"plant_FD_map_100km.rds"))

(plant_FD_plot_100km <- plant_FD_map_100km$gridFDisTA)
plant_cell_FD_100km <- plant_FD_map_100km$spatial_fdis_grid

# save data
saveRDS(plant_cell_FD_100km, file.path(all_output_path_L2,"plant_cell_FD_100km.rds"))
ggsave('plant_FD_plot_100km.png', plant_FD_plot_100km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 75km ####

# species occurrence data
plant_sp_grid_75km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_75km.rds"))

# FDis calculation
fdis_plant2_75km <- FDis2(plant_sp_grid_75km, sp_faxes2_coord_plant)
saveRDS(fdis_plant2_75km, file.path(all_output_path_L2,"fdis_plant_75km.rds"))

# mapping
plant_FD_map_75km <- FD_map2(fdis_plant2_75km, 75000, 'plant')
saveRDS(plant_FD_map_75km, file = file.path(all_output_path_L2,"plant_FD_map_75km.rds"))

(plant_FD_plot_75km <- plant_FD_map_75km$gridFDisTA)
plant_cell_FD_75km <- plant_FD_map_75km$spatial_fdis_grid

# save data
saveRDS(plant_cell_FD_75km, file.path(all_output_path_L2,"plant_cell_FD_75km.rds"))
ggsave('plant_FD_plot_75km.png', plant_FD_plot_75km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 50km ####

# species occurrence data
plant_sp_grid_50km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_50km.rds"))

# FDis calculation
fdis_plant2_50km <- FDis2(plant_sp_grid_50km, sp_faxes2_coord_plant)
saveRDS(fdis_plant2_50km, file.path(all_output_path_L2,"fdis_plant_50km.rds"))

# mapping 
plant_FD_map_50km <- FD_map2(fdis_plant2_50km, 50000, 'plant')
saveRDS(plant_FD_map_50km, file = file.path(all_output_path_L2,"plant_FD_map_50km.rds"))

(plant_FD_plot_50km <- plant_FD_map_50km$gridFDisTA)
plant_cell_FD_50km <- plant_FD_map_50km$spatial_fdis_grid

# save data
saveRDS(plant_cell_FD_50km, file.path(all_output_path_L2,"plant_cell_FD_50km.rds"))
ggsave('plant_FD_plot_50km.png', plant_FD_plot_50km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 25km ####

# species occurrence data
plant_sp_grid_25km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_25km.rds"))

# FDis calculation
fdis_plant2_25km <- FDis2(plant_sp_grid_25km, sp_faxes2_coord_plant)
saveRDS(fdis_plant2_25km, file.path(all_output_path_L2,"fdis_plant_25km.rds"))

# mapping
plant_FD_map_25km <- FD_map2(fdis_plant2_25km, 25000, 'plant')
saveRDS(plant_FD_map_25km, file = file.path(all_output_path_L2,"plant_FD_map_25km.rds"))

(plant_FD_plot_25km <- plant_FD_map_25km$gridFDisTA)
plant_cell_FD_25km <- plant_FD_map_25km$spatial_fdis_grid

# save data
saveRDS(plant_cell_FD_25km, file.path(all_output_path_L2,"plant_cell_FD_25km.rds"))
ggsave('plant_FD_plot_25km.png', plant_FD_plot_25km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 10km ####

# species occurrence data
plant_sp_grid_10km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_10km.rds"))

# FDis calculation
fdis_plant2_10km <- FDis2(plant_sp_grid_10km, sp_faxes2_coord_plant)
saveRDS(fdis_plant2_10km, file.path(all_output_path_L2,"fdis_plant_10km.rds"))

# mapping
plant_FD_map_10km <- FD_map2(fdis_plant2_10km, 10000, 'plant')
saveRDS(plant_FD_map_10km, file = file.path(all_output_path_L2,"plant_FD_map_10km.rds"))

(plant_FD_plot_10km <- plant_FD_map_10km$gridFDisTA)
plant_cell_FD_10km <- plant_FD_map_10km$spatial_fdis_grid

# save data
saveRDS(plant_cell_FD_10km, file.path(all_output_path_L2,"plant_cell_FD_10km.rds"))
ggsave('plant_FD_plot_10km.png', plant_FD_plot_10km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 5km ####

# species occurrence data
plant_sp_grid_5km <- readRDS(file.path(all_data_path_L1,"plant_sp_grid_5km.rds"))

# FDis calculation 
fdis_plant2_5km <- FDis2(plant_sp_grid_5km, sp_faxes2_coord_plant)
saveRDS(fdis_plant2_5km, file.path(all_output_path_L2,"fdis_plant_5km.rds"))

# mapping
plant_FD_map_5km <- FD_map2(fdis_plant2_5km, 5000, 'plant')
saveRDS(plant_FD_map_5km, file = file.path(all_output_path_L2,"plant_FD_map_5km.rds"))

(plant_FD_plot_5km <- plant_FD_map_5km$gridFDisTA)
plant_cell_FD_5km <- plant_FD_map_5km$spatial_fdis_grid

# save data
saveRDS(plant_cell_FD_5km, file.path(all_output_path_L2,"plant_cell_FD_5km.rds"))
ggsave('plant_FD_plot_5km.png', plant_FD_plot_5km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


# birds
#### 100 km ####

# species occurrence data
bird_sp_grid_100km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_100km.rds"))

# quality of functional spaces
fspaces_quality2(bird_sp_grid_100km, bird_traits_df_final, 'bird')

fspace_quality_plot(fspaces_quality2_bird)

pc_coords2(fspaces_quality2_bird, bird_traits_df_final, 'bird')
fspace_corr_plots(sp_faxes2_coord_bird, tr_faxes2_bird)
saveRDS(fspaces_quality2_bird, file = file.path(all_output_path_L2, "fspaces_quality2_bird.rds"))
fspaces_quality2_bird <- readRDS(file.path(all_output_path_L2, "fspaces_quality2_bird.rds"))


# FDis calculation
fdis_bird2_100km <- FDis2(bird_sp_grid_100km, sp_faxes2_coord_bird)
saveRDS(fdis_bird2_100km, file.path(all_output_path_L2,"fdis_bird_100km.rds"))

# mapping

# set limits for all bird maps based off of 100 km
lims <- c(0, max(fdis_bird2_100km$fdis))
mpt <- max(fdis_bird2_100km$fdis)/2


bird_FD_map_100km <- FD_map2(fdis_bird2_100km, 100000, 'bird')
saveRDS(bird_FD_map_100km, file = file.path(all_output_path_L2,"bird_FD_map_100km.rds"))

(bird_FD_plot_100km <- bird_FD_map_100km$gridFDisTA)
bird_cell_FD_100km <- bird_FD_map_100km$spatial_fdis_grid

# save data
saveRDS(bird_cell_FD_100km, file.path(all_output_path_L2,"bird_cell_FD_100km.rds"))
ggsave('bird_FD_plot_100km.png', bird_FD_plot_100km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 75km ####

# species occurrence data
bird_sp_grid_75km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_75km.rds"))

# FDis calculation
fdis_bird2_75km <- FDis2(bird_sp_grid_75km, sp_faxes2_coord_bird)
saveRDS(fdis_bird2_75km, file.path(all_output_path_L2,"fdis_bird_75km.rds"))

# mapping
bird_FD_map_75km <- FD_map2(fdis_bird2_75km, 75000, 'bird')
saveRDS(bird_FD_map_75km, file = file.path(all_output_path_L2,"bird_FD_map_75km.rds"))

(bird_FD_plot_75km <- bird_FD_map_75km$gridFDisTA)
bird_cell_FD_75km <- bird_FD_map_75km$spatial_fdis_grid

# save data
saveRDS(bird_cell_FD_75km, file.path(all_output_path_L2,"bird_cell_FD_75km.rds"))
ggsave('bird_FD_plot_75km.png', bird_FD_plot_75km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 50km ####

# species occurrence data
bird_sp_grid_50km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_50km.rds"))

# FDis calculation
fdis_bird2_50km <- FDis2(bird_sp_grid_50km, sp_faxes2_coord_bird)
saveRDS(fdis_bird2_50km, file.path(all_output_path_L2,"fdis_bird_50km.rds"))

# mapping 
bird_FD_map_50km <- FD_map2(fdis_bird2_50km, 50000, 'bird')
saveRDS(bird_FD_map_50km, file = file.path(all_output_path_L2,"bird_FD_map_50km.rds"))

(bird_FD_plot_50km <- bird_FD_map_50km$gridFDisTA)
bird_cell_FD_50km <- bird_FD_map_50km$spatial_fdis_grid

# save data
saveRDS(bird_cell_FD_50km, file.path(all_output_path_L2,"bird_cell_FD_50km.rds"))
ggsave('bird_FD_plot_50km.png', bird_FD_plot_50km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 25km ####

# species occurrence data
bird_sp_grid_25km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_25km.rds"))

# FDis calculation
fdis_bird2_25km <- FDis2(bird_sp_grid_25km, sp_faxes2_coord_bird)
saveRDS(fdis_bird2_25km, file.path(all_output_path_L2,"fdis_bird_25km.rds"))

# mapping
bird_FD_map_25km <- FD_map2(fdis_bird2_25km, 25000, 'bird')
saveRDS(bird_FD_map_25km, file = file.path(all_output_path_L2,"bird_FD_map_25km.rds"))

(bird_FD_plot_25km <- bird_FD_map_25km$gridFDisTA)
bird_cell_FD_25km <- bird_FD_map_25km$spatial_fdis_grid

# save data
saveRDS(bird_cell_FD_25km, file.path(all_output_path_L2,"bird_cell_FD_25km.rds"))
ggsave('bird_FD_plot_25km.png', bird_FD_plot_25km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 10km ####

# species occurrence data
bird_sp_grid_10km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_10km.rds"))

# FDis calculation
fdis_bird2_10km <- FDis2(bird_sp_grid_10km, sp_faxes2_coord_bird)
saveRDS(fdis_bird2_10km, file.path(all_output_path_L2,"fdis_bird_10km.rds"))

# mapping
bird_FD_map_10km <- FD_map2(fdis_bird2_10km, 10000, 'bird')
saveRDS(bird_FD_map_10km, file = file.path(all_output_path_L2,"bird_FD_map_10km.rds"))

(bird_FD_plot_10km <- bird_FD_map_10km$gridFDisTA)
bird_cell_FD_10km <- bird_FD_map_10km$spatial_fdis_grid

# save data
saveRDS(bird_cell_FD_10km, file.path(all_output_path_L2,"bird_cell_FD_10km.rds"))
ggsave('bird_FD_plot_10km.png', bird_FD_plot_10km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 5km ####

# species occurrence data
bird_sp_grid_5km <- readRDS(file.path(all_data_path_L1,"bird_sp_grid_5km.rds"))

# FDis calculation 
fdis_bird2_5km <- FDis2(bird_sp_grid_5km, sp_faxes2_coord_bird)
saveRDS(fdis_bird2_5km, file.path(all_output_path_L2,"fdis_bird_5km.rds"))

# mapping
bird_FD_map_5km <- FD_map2(fdis_bird2_5km, 5000, 'bird')
saveRDS(bird_FD_map_5km, file = file.path(all_output_path_L2,"bird_FD_map_5km.rds"))

(bird_FD_plot_5km <- bird_FD_map_5km$gridFDisTA)
bird_cell_FD_5km <- bird_FD_map_5km$spatial_fdis_grid

# save data
saveRDS(bird_cell_FD_5km, file.path(all_output_path_L2,"bird_cell_FD_5km.rds"))
ggsave('bird_FD_plot_5km.png', bird_FD_plot_5km, path = all_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### final figure ####

# all map data
plant_FD_map_100km <- readRDS(file.path(all_output_path_L2, "plant_FD_map_100km.rds"))
plant_FD_plot_100km <- plant_FD_map_100km$gridFDisTA

plant_FD_map_75km <- readRDS(file.path(all_output_path_L2, "plant_FD_map_75km.rds"))
plant_FD_plot_75km <- plant_FD_map_75km$gridFDisTA

plant_FD_map_50km <- readRDS(file.path(all_output_path_L2, "plant_FD_map_50km.rds"))
plant_FD_plot_50km <- plant_FD_map_50km$gridFDisTA

plant_FD_map_25km <- readRDS(file.path(all_output_path_L2, "plant_FD_map_25km.rds"))
plant_FD_plot_25km <- plant_FD_map_25km$gridFDisTA

plant_FD_map_10km <- readRDS(file.path(all_output_path_L2, "plant_FD_map_10km.rds"))
plant_FD_plot_10km <- plant_FD_map_10km$gridFDisTA

plant_FD_map_5km <- readRDS(file.path(all_output_path_L2, "plant_FD_map_5km.rds"))
plant_FD_plot_5km <- plant_FD_map_5km$gridFDisTA

mammal_FD_map_100km <- readRDS(file.path(all_output_path_L2, "mammal_FD_map_100km.rds"))
mammal_FD_plot_100km <- mammal_FD_map_100km$gridFDisTA

mammal_FD_map_75km <- readRDS(file.path(all_output_path_L2, "mammal_FD_map_75km.rds"))
mammal_FD_plot_75km <- mammal_FD_map_75km$gridFDisTA

mammal_FD_map_50km <- readRDS(file.path(all_output_path_L2, "mammal_FD_map_50km.rds"))
mammal_FD_plot_50km <- mammal_FD_map_50km$gridFDisTA

mammal_FD_map_25km <- readRDS(file.path(all_output_path_L2, "mammal_FD_map_25km.rds"))
mammal_FD_plot_25km <- mammal_FD_map_25km$gridFDisTA

mammal_FD_map_10km <- readRDS(file.path(all_output_path_L2, "mammal_FD_map_10km.rds"))
mammal_FD_plot_10km <- mammal_FD_map_10km$gridFDisTA

mammal_FD_map_5km <- readRDS(file.path(all_output_path_L2, "mammal_FD_map_5km.rds"))
mammal_FD_plot_5km <- mammal_FD_map_5km$gridFDisTA

bird_FD_map_100km <- readRDS(file.path(all_output_path_L2, "bird_FD_map_100km.rds"))
bird_FD_plot_100km <- bird_FD_map_100km$gridFDisTA

bird_FD_map_75km <- readRDS(file.path(all_output_path_L2, "bird_FD_map_75km.rds"))
bird_FD_plot_75km <- bird_FD_map_75km$gridFDisTA

bird_FD_map_50km <- readRDS(file.path(all_output_path_L2, "bird_FD_map_50km.rds"))
bird_FD_plot_50km <- bird_FD_map_50km$gridFDisTA

bird_FD_map_25km <- readRDS(file.path(all_output_path_L2, "bird_FD_map_25km.rds"))
bird_FD_plot_25km <- bird_FD_map_25km$gridFDisTA

bird_FD_map_10km <- readRDS(file.path(all_output_path_L2, "bird_FD_map_10km.rds"))
bird_FD_plot_10km <- bird_FD_map_10km$gridFDisTA

bird_FD_map_5km <- readRDS(file.path(all_output_path_L2, "bird_FD_map_5km.rds"))
bird_FD_plot_5km <- bird_FD_map_5km$gridFDisTA


# individual plot edits
plant_FD_plot_100km <- plant_FD_plot_100km + labs(title='[100km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20)) 

mammal_FD_plot_100km <- mammal_FD_plot_100km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_FD_plot_100km <- bird_FD_plot_100km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


plant_FD_plot_75km <- plant_FD_plot_75km + labs(title='[75km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_FD_plot_75km <- mammal_FD_plot_75km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_FD_plot_75km <- bird_FD_plot_75km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


plant_FD_plot_50km <- plant_FD_plot_50km + labs(title='[50km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_FD_plot_50km <- mammal_FD_plot_50km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_FD_plot_50km <- bird_FD_plot_50km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


plant_FD_plot_25km <- plant_FD_plot_25km + labs(title='[25km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_FD_plot_25km <- mammal_FD_plot_25km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_FD_plot_25km <- bird_FD_plot_25km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + labs(x = "Longitude")


plant_FD_plot_10km <- plant_FD_plot_10km + labs(title='[10km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_FD_plot_10km <- mammal_FD_plot_10km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_FD_plot_10km <- bird_FD_plot_10km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


# plant picture
plant <- pick_phylopic(name='Coffea alleizettei')

plant_FD_plot_5km <- plant_FD_plot_5km + labs(title='[5km]') + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20), axis.text = element_text(size=16)) + add_phylopic(img=plant, x=-79, y=12, height=8)

# mammal picture
mammal <- pick_phylopic(name='Potos flavus', n=2, auto=2)

mammal_FD_plot_5km <- mammal_FD_plot_5km + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + labs(y = "Latitude") + add_phylopic(img=mammal, x=-79, y=12, height=8)

# bird picture
bird <- pick_phylopic(name='Ramphastos sulfuratus', n=2, auto=1)

bird_FD_plot_5km <- bird_FD_plot_5km + annotation_scale(location = "bl",width_hint = 0.4, style = "bar") + annotation_north_arrow(location = "bl", which_north = "true", height = unit(0.5, "in"), width = unit(0.5, "in"), pad_x = unit(0.05, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) + theme(plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + add_phylopic(img=bird, x=-79, y=12, height=8)

# combine all plots
all_fdis_plots <- plant_FD_plot_5km + plant_FD_plot_10km + plant_FD_plot_25km + plant_FD_plot_50km + plant_FD_plot_75km + plant_FD_plot_100km + mammal_FD_plot_5km + mammal_FD_plot_10km + mammal_FD_plot_25km + mammal_FD_plot_50km + mammal_FD_plot_75km + mammal_FD_plot_100km + bird_FD_plot_5km + bird_FD_plot_10km + bird_FD_plot_25km + bird_FD_plot_50km + bird_FD_plot_75km + bird_FD_plot_100km + plot_layout(ncol = 6, nrow = 3, guides = 'collect', axis_titles = 'collect', heights = c(1, 1, 1)) & theme(legend.position = 'left', axis.title = element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=16)) & plot_annotation(title='FDis', theme = theme(plot.title = element_text(hjust = 0.5, size=30, face='bold')))

all_fdis_plots

ggsave('all_fdis_plots.png', all_fdis_plots, path = all_data_figure_path, width = 14, height = 11.8, units = "in", dpi=1000)


#### repeat FD calculations and mapping with obs cutoff ####

# set file paths
filtered_data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1/filtered_data')
filtered_output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2/filtered_data')
filtered_data_figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures/filtered_data')


# set cutoff
cutoff_obs <- 20


# trait data at cutoff
plant_traits_df_final_cutoff <- readRDS(file = file.path(filtered_data_path_L1, paste0("plant_", cutoff_obs,"_traits_df_subset.rds")))
mammal_traits_df_final_cutoff <- readRDS(file=file.path(filtered_data_path_L1, paste0("mammal_", cutoff_obs,"_traits_df_subset.rds")))
bird_traits_df_final_cutoff <- readRDS(file=file.path(filtered_data_path_L1, paste0("bird_", cutoff_obs,"_traits_df_subset.rds")))


# mammals
#### 100 km ####

# secies occurrence data
mammal_cutoff_sp_grid_100km <- readRDS(file.path(filtered_data_path_L1,paste0("mammal_", cutoff_obs, "_sp_grid_100km.rds")))

# quality of functional spaces
fspaces_quality2(mammal_cutoff_sp_grid_100km, mammal_traits_df_final_cutoff, 'mammal')

pc_coords2(fspaces_quality2_mammal, mammal_traits_df_final_cutoff, 'mammal')
fspace_corr_plots(sp_faxes2_coord_mammal, tr_faxes2_mammal)

saveRDS(sp_faxes2_coord_mammal, file = file.path(filtered_output_path_L2, paste0("sp_faxes2_coord_mammal_", cutoff_obs, ".rds")))

# FDis calculation
fdis_mammal2_100km_cutoff <- FDis2(mammal_cutoff_sp_grid_100km, sp_faxes2_coord_mammal)
saveRDS(fdis_mammal2_100km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_mammal_", cutoff_obs, "_100km.rds")))

# mapping
mammal_cutoff_FD_map_100km <- FD_map2(fdis_mammal2_100km_cutoff, 100000, 'mammal')
saveRDS(mammal_cutoff_FD_map_100km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_FD_map_100km.rds")))

(mammal_cutoff_FD_plot_100km <- mammal_cutoff_FD_map_100km$gridFDisTA)
mammal_cutoff_cell_FD_100km <- mammal_cutoff_FD_map_100km$spatial_fdis_grid

# save data
saveRDS(mammal_cutoff_cell_FD_100km, file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_cell_FD_100km.rds")))
ggsave(paste0('mammal_', cutoff_obs, '_FD_plot_100km.png'), mammal_cutoff_FD_plot_100km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 75 km ####

# species occurrence data
mammal_cutoff_sp_grid_75km <- readRDS(file.path(filtered_data_path_L1,paste0("mammal_", cutoff_obs, "_sp_grid_75km.rds")))

# FDis calculation
fdis_mammal2_75km_cutoff <- FDis2(mammal_cutoff_sp_grid_75km, sp_faxes2_coord_mammal)
saveRDS(fdis_mammal2_75km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_mammal_", cutoff_obs, "_75km.rds")))

# mapping
mammal_cutoff_FD_map_75km <- FD_map2(fdis_mammal2_75km_cutoff, 75000, 'mammal')
saveRDS(mammal_cutoff_FD_map_75km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_FD_map_75km.rds")))

(mammal_cutoff_FD_plot_75km <- mammal_cutoff_FD_map_75km$gridFDisTA)
mammal_cutoff_cell_FD_75km <- mammal_cutoff_FD_map_75km$spatial_fdis_grid

# save data
saveRDS(mammal_cutoff_cell_FD_75km, file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_cell_FD_75km.rds")))
ggsave(paste0('mammal_', cutoff_obs, '_FD_plot_75km.png'), mammal_cutoff_FD_plot_75km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 50 km ####

# species occurrence data
mammal_cutoff_sp_grid_50km <- readRDS(file.path(filtered_data_path_L1,paste0("mammal_", cutoff_obs, "_sp_grid_50km.rds")))

# FDis calculation
fdis_mammal2_50km_cutoff <- FDis2(mammal_cutoff_sp_grid_50km, sp_faxes2_coord_mammal)
saveRDS(fdis_mammal2_50km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_mammal_", cutoff_obs, "_50km.rds")))

# mapping
mammal_cutoff_FD_map_50km <- FD_map2(fdis_mammal2_50km_cutoff, 50000, 'mammal')
saveRDS(mammal_cutoff_FD_map_50km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_FD_map_50km.rds")))

(mammal_cutoff_FD_plot_50km <- mammal_cutoff_FD_map_50km$gridFDisTA)
mammal_cutoff_cell_FD_50km <- mammal_cutoff_FD_map_50km$spatial_fdis_grid

# save data
saveRDS(mammal_cutoff_cell_FD_50km, file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_cell_FD_50km.rds")))
ggsave(paste0('mammal_', cutoff_obs, '_FD_plot_50km.png'), mammal_cutoff_FD_plot_50km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 25 km ####

# species occurrence data
mammal_cutoff_sp_grid_25km <- readRDS(file.path(filtered_data_path_L1,paste0("mammal_", cutoff_obs, "_sp_grid_25km.rds")))

# FDis calculation
fdis_mammal2_25km_cutoff <- FDis2(mammal_cutoff_sp_grid_25km, sp_faxes2_coord_mammal)
saveRDS(fdis_mammal2_25km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_mammal_", cutoff_obs, "_25km.rds")))

# mapping
mammal_cutoff_FD_map_25km <- FD_map2(fdis_mammal2_25km_cutoff, 25000, 'mammal')
saveRDS(mammal_cutoff_FD_map_25km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_FD_map_25km.rds")))

(mammal_cutoff_FD_plot_25km <- mammal_cutoff_FD_map_25km$gridFDisTA)
mammal_cutoff_cell_FD_25km <- mammal_cutoff_FD_map_25km$spatial_fdis_grid

# save data
saveRDS(mammal_cutoff_cell_FD_25km, file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_cell_FD_25km.rds")))
ggsave(paste0('mammal_', cutoff_obs, '_FD_plot_25km.png'), mammal_cutoff_FD_plot_25km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 10 km ####

# species occurrence data
mammal_cutoff_sp_grid_10km <- readRDS(file.path(filtered_data_path_L1,paste0("mammal_", cutoff_obs, "_sp_grid_10km.rds")))

# FDis calculation
fdis_mammal2_10km_cutoff <- FDis2(mammal_cutoff_sp_grid_10km, sp_faxes2_coord_mammal)
saveRDS(fdis_mammal2_10km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_mammal_", cutoff_obs, "_10km.rds")))

# mapping
mammal_cutoff_FD_map_10km <- FD_map2(fdis_mammal2_10km_cutoff, 10000, 'mammal')
saveRDS(mammal_cutoff_FD_map_10km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_FD_map_10km.rds")))

(mammal_cutoff_FD_plot_10km <- mammal_cutoff_FD_map_10km$gridFDisTA)
mammal_cutoff_cell_FD_10km <- mammal_cutoff_FD_map_10km$spatial_fdis_grid

# save data
saveRDS(mammal_cutoff_cell_FD_10km, file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_cell_FD_10km.rds")))
ggsave(paste0('mammal_', cutoff_obs, '_FD_plot_10km.png'), mammal_cutoff_FD_plot_10km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 5 km ####

# species occurrence data
mammal_cutoff_sp_grid_5km <- readRDS(file.path(filtered_data_path_L1,paste0("mammal_", cutoff_obs, "_sp_grid_5km.rds")))

# FDis calculation
fdis_mammal2_5km_cutoff <- FDis2(mammal_cutoff_sp_grid_5km, sp_faxes2_coord_mammal)
saveRDS(fdis_mammal2_5km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_mammal_", cutoff_obs, "_5km.rds")))

# mapping
mammal_cutoff_FD_map_5km <- FD_map2(fdis_mammal2_5km_cutoff, 5000, 'mammal')
saveRDS(mammal_cutoff_FD_map_5km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_FD_map_5km.rds")))

(mammal_cutoff_FD_plot_5km <- mammal_cutoff_FD_map_5km$gridFDisTA)
mammal_cutoff_cell_FD_5km <- mammal_cutoff_FD_map_5km$spatial_fdis_grid

# save data
saveRDS(mammal_cutoff_cell_FD_5km, file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_cell_FD_5km.rds")))
ggsave(paste0('mammal_', cutoff_obs, '_FD_plot_5km.png'), mammal_cutoff_FD_plot_5km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


# plants
#### 100 km ####

# secies occurrence data
plant_cutoff_sp_grid_100km <- readRDS(file.path(filtered_data_path_L1,paste0("plant_", cutoff_obs, "_sp_grid_100km.rds")))

# quality of functional spaces
fspaces_quality2(plant_cutoff_sp_grid_100km, plant_traits_df_final_cutoff, 'plant')

pc_coords2(fspaces_quality2_plant, plant_traits_df_final_cutoff, 'plant')
fspace_corr_plots(sp_faxes2_coord_plant, tr_faxes2_plant)

saveRDS(sp_faxes2_coord_plant, file = file.path(filtered_output_path_L2, paste0("sp_faxes2_coord_plant_", cutoff_obs, ".rds")))

# FDis calculation
fdis_plant2_100km_cutoff <- FDis2(plant_cutoff_sp_grid_100km, sp_faxes2_coord_plant)
saveRDS(fdis_plant2_100km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_plant_", cutoff_obs, "_100km.rds")))

# mapping
plant_cutoff_FD_map_100km <- FD_map2(fdis_plant2_100km_cutoff, 100000, 'plant')
saveRDS(plant_cutoff_FD_map_100km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_FD_map_100km.rds")))

(plant_cutoff_FD_plot_100km <- plant_cutoff_FD_map_100km$gridFDisTA)
plant_cutoff_cell_FD_100km <- plant_cutoff_FD_map_100km$spatial_fdis_grid

# save data
saveRDS(plant_cutoff_cell_FD_100km, file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_cell_FD_100km.rds")))
ggsave(paste0('plant_', cutoff_obs, '_FD_plot_100km.png'), plant_cutoff_FD_plot_100km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 75 km ####

# species occurrence data
plant_cutoff_sp_grid_75km <- readRDS(file.path(filtered_data_path_L1,paste0("plant_", cutoff_obs, "_sp_grid_75km.rds")))

# FDis calculation
fdis_plant2_75km_cutoff <- FDis2(plant_cutoff_sp_grid_75km, sp_faxes2_coord_plant)
saveRDS(fdis_plant2_75km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_plant_", cutoff_obs, "_75km.rds")))

# mapping
plant_cutoff_FD_map_75km <- FD_map2(fdis_plant2_75km_cutoff, 75000, 'plant')
saveRDS(plant_cutoff_FD_map_75km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_FD_map_75km.rds")))

(plant_cutoff_FD_plot_75km <- plant_cutoff_FD_map_75km$gridFDisTA)
plant_cutoff_cell_FD_75km <- plant_cutoff_FD_map_75km$spatial_fdis_grid

# save data
saveRDS(plant_cutoff_cell_FD_75km, file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_cell_FD_75km.rds")))
ggsave(paste0('plant_', cutoff_obs, '_FD_plot_75km.png'), plant_cutoff_FD_plot_75km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 50 km ####

# species occurrence data
plant_cutoff_sp_grid_50km <- readRDS(file.path(filtered_data_path_L1,paste0("plant_", cutoff_obs, "_sp_grid_50km.rds")))

# FDis calculation
fdis_plant2_50km_cutoff <- FDis2(plant_cutoff_sp_grid_50km, sp_faxes2_coord_plant)
saveRDS(fdis_plant2_50km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_plant_", cutoff_obs, "_50km.rds")))

# mapping
plant_cutoff_FD_map_50km <- FD_map2(fdis_plant2_50km_cutoff, 50000, 'plant')
saveRDS(plant_cutoff_FD_map_50km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_FD_map_50km.rds")))

(plant_cutoff_FD_plot_50km <- plant_cutoff_FD_map_50km$gridFDisTA)
plant_cutoff_cell_FD_50km <- plant_cutoff_FD_map_50km$spatial_fdis_grid

# save data
saveRDS(plant_cutoff_cell_FD_50km, file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_cell_FD_50km.rds")))
ggsave(paste0('plant_', cutoff_obs, '_FD_plot_50km.png'), plant_cutoff_FD_plot_50km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 25 km ####

# species occurrence data
plant_cutoff_sp_grid_25km <- readRDS(file.path(filtered_data_path_L1,paste0("plant_", cutoff_obs, "_sp_grid_25km.rds")))

# FDis calculation
fdis_plant2_25km_cutoff <- FDis2(plant_cutoff_sp_grid_25km, sp_faxes2_coord_plant)
saveRDS(fdis_plant2_25km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_plant_", cutoff_obs, "_25km.rds")))

# mapping
plant_cutoff_FD_map_25km <- FD_map2(fdis_plant2_25km_cutoff, 25000, 'plant')
saveRDS(plant_cutoff_FD_map_25km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_FD_map_25km.rds")))

(plant_cutoff_FD_plot_25km <- plant_cutoff_FD_map_25km$gridFDisTA)
plant_cutoff_cell_FD_25km <- plant_cutoff_FD_map_25km$spatial_fdis_grid

# save data
saveRDS(plant_cutoff_cell_FD_25km, file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_cell_FD_25km.rds")))
ggsave(paste0('plant_', cutoff_obs, '_FD_plot_25km.png'), plant_cutoff_FD_plot_25km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 10 km ####

# species occurrence data
plant_cutoff_sp_grid_10km <- readRDS(file.path(filtered_data_path_L1,paste0("plant_", cutoff_obs, "_sp_grid_10km.rds")))

# FDis calculation
fdis_plant2_10km_cutoff <- FDis2(plant_cutoff_sp_grid_10km, sp_faxes2_coord_plant)
saveRDS(fdis_plant2_10km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_plant_", cutoff_obs, "_10km.rds")))

# mapping
plant_cutoff_FD_map_10km <- FD_map2(fdis_plant2_10km_cutoff, 10000, 'plant')
saveRDS(plant_cutoff_FD_map_10km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_FD_map_10km.rds")))

(plant_cutoff_FD_plot_10km <- plant_cutoff_FD_map_10km$gridFDisTA)
plant_cutoff_cell_FD_10km <- plant_cutoff_FD_map_10km$spatial_fdis_grid

# save data
saveRDS(plant_cutoff_cell_FD_10km, file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_cell_FD_10km.rds")))
ggsave(paste0('plant_', cutoff_obs, '_FD_plot_10km.png'), plant_cutoff_FD_plot_10km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 5 km ####

# species occurrence data
plant_cutoff_sp_grid_5km <- readRDS(file.path(filtered_data_path_L1,paste0("plant_", cutoff_obs, "_sp_grid_5km.rds")))

# FDis calculation
fdis_plant2_5km_cutoff <- FDis2(plant_cutoff_sp_grid_5km, sp_faxes2_coord_plant)
saveRDS(fdis_plant2_5km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_plant_", cutoff_obs, "_5km.rds")))

# mapping
plant_cutoff_FD_map_5km <- FD_map2(fdis_plant2_5km_cutoff, 5000, 'plant')
saveRDS(plant_cutoff_FD_map_5km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_FD_map_5km.rds")))

(plant_cutoff_FD_plot_5km <- plant_cutoff_FD_map_5km$gridFDisTA)
plant_cutoff_cell_FD_5km <- plant_cutoff_FD_map_5km$spatial_fdis_grid

# save data
saveRDS(plant_cutoff_cell_FD_5km, file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_cell_FD_5km.rds")))
ggsave(paste0('plant_', cutoff_obs, '_FD_plot_5km.png'), plant_cutoff_FD_plot_5km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


# birds
#### 100 km ####

# secies occurrence data
bird_cutoff_sp_grid_100km <- readRDS(file.path(filtered_data_path_L1,paste0("bird_", cutoff_obs, "_sp_grid_100km.rds")))

# quality of functional spaces
fspaces_quality2(bird_cutoff_sp_grid_100km, bird_traits_df_final_cutoff, 'bird')

pc_coords2(fspaces_quality2_bird, bird_traits_df_final_cutoff, 'bird')
fspace_corr_plots(sp_faxes2_coord_bird, tr_faxes2_bird)

saveRDS(sp_faxes2_coord_bird, file = file.path(filtered_output_path_L2, paste0("sp_faxes2_coord_bird_", cutoff_obs, ".rds")))

# FDis calculation
fdis_bird2_100km_cutoff <- FDis2(bird_cutoff_sp_grid_100km, sp_faxes2_coord_bird)
saveRDS(fdis_bird2_100km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_bird_", cutoff_obs, "_100km.rds")))

# mapping
bird_cutoff_FD_map_100km <- FD_map2(fdis_bird2_100km_cutoff, 100000, 'bird')
saveRDS(bird_cutoff_FD_map_100km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_FD_map_100km.rds")))

(bird_cutoff_FD_plot_100km <- bird_cutoff_FD_map_100km$gridFDisTA)
bird_cutoff_cell_FD_100km <- bird_cutoff_FD_map_100km$spatial_fdis_grid

# save data
saveRDS(bird_cutoff_cell_FD_100km, file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_cell_FD_100km.rds")))
ggsave(paste0('bird_', cutoff_obs, '_FD_plot_100km.png'), bird_cutoff_FD_plot_100km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 75 km ####

# species occurrence data
bird_cutoff_sp_grid_75km <- readRDS(file.path(filtered_data_path_L1,paste0("bird_", cutoff_obs, "_sp_grid_75km.rds")))

# FDis calculation
fdis_bird2_75km_cutoff <- FDis2(bird_cutoff_sp_grid_75km, sp_faxes2_coord_bird)
saveRDS(fdis_bird2_75km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_bird_", cutoff_obs, "_75km.rds")))

# mapping
bird_cutoff_FD_map_75km <- FD_map2(fdis_bird2_75km_cutoff, 75000, 'bird')
saveRDS(bird_cutoff_FD_map_75km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_FD_map_75km.rds")))

(bird_cutoff_FD_plot_75km <- bird_cutoff_FD_map_75km$gridFDisTA)
bird_cutoff_cell_FD_75km <- bird_cutoff_FD_map_75km$spatial_fdis_grid

# save data
saveRDS(bird_cutoff_cell_FD_75km, file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_cell_FD_75km.rds")))
ggsave(paste0('bird_', cutoff_obs, '_FD_plot_75km.png'), bird_cutoff_FD_plot_75km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)



#### 50 km ####

# species occurrence data
bird_cutoff_sp_grid_50km <- readRDS(file.path(filtered_data_path_L1,paste0("bird_", cutoff_obs, "_sp_grid_50km.rds")))

# FDis calculation
fdis_bird2_50km_cutoff <- FDis2(bird_cutoff_sp_grid_50km, sp_faxes2_coord_bird)
saveRDS(fdis_bird2_50km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_bird_", cutoff_obs, "_50km.rds")))

# mapping
bird_cutoff_FD_map_50km <- FD_map2(fdis_bird2_50km_cutoff, 50000, 'bird')
saveRDS(bird_cutoff_FD_map_50km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_FD_map_50km.rds")))

(bird_cutoff_FD_plot_50km <- bird_cutoff_FD_map_50km$gridFDisTA)
bird_cutoff_cell_FD_50km <- bird_cutoff_FD_map_50km$spatial_fdis_grid

# save data
saveRDS(bird_cutoff_cell_FD_50km, file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_cell_FD_50km.rds")))
ggsave(paste0('bird_', cutoff_obs, '_FD_plot_50km.png'), bird_cutoff_FD_plot_50km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 25 km ####

# species occurrence data
bird_cutoff_sp_grid_25km <- readRDS(file.path(filtered_data_path_L1,paste0("bird_", cutoff_obs, "_sp_grid_25km.rds")))

# FDis calculation
fdis_bird2_25km_cutoff <- FDis2(bird_cutoff_sp_grid_25km, sp_faxes2_coord_bird)
saveRDS(fdis_bird2_25km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_bird_", cutoff_obs, "_25km.rds")))

# mapping
bird_cutoff_FD_map_25km <- FD_map2(fdis_bird2_25km_cutoff, 25000, 'bird')
saveRDS(bird_cutoff_FD_map_25km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_FD_map_25km.rds")))

(bird_cutoff_FD_plot_25km <- bird_cutoff_FD_map_25km$gridFDisTA)
bird_cutoff_cell_FD_25km <- bird_cutoff_FD_map_25km$spatial_fdis_grid

# save data
saveRDS(bird_cutoff_cell_FD_25km, file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_cell_FD_25km.rds")))
ggsave(paste0('bird_', cutoff_obs, '_FD_plot_25km.png'), bird_cutoff_FD_plot_25km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 10 km ####

# species occurrence data
bird_cutoff_sp_grid_10km <- readRDS(file.path(filtered_data_path_L1,paste0("bird_", cutoff_obs, "_sp_grid_10km.rds")))

# FDis calculation
fdis_bird2_10km_cutoff <- FDis2(bird_cutoff_sp_grid_10km, sp_faxes2_coord_bird)
saveRDS(fdis_bird2_10km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_bird_", cutoff_obs, "_10km.rds")))

# mapping
bird_cutoff_FD_map_10km <- FD_map2(fdis_bird2_10km_cutoff, 10000, 'bird')
saveRDS(bird_cutoff_FD_map_10km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_FD_map_10km.rds")))

(bird_cutoff_FD_plot_10km <- bird_cutoff_FD_map_10km$gridFDisTA)
bird_cutoff_cell_FD_10km <- bird_cutoff_FD_map_10km$spatial_fdis_grid

# save data
saveRDS(bird_cutoff_cell_FD_10km, file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_cell_FD_10km.rds")))
ggsave(paste0('bird_', cutoff_obs, '_FD_plot_10km.png'), bird_cutoff_FD_plot_10km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### 5 km ####

# species occurrence data
bird_cutoff_sp_grid_5km <- readRDS(file.path(filtered_data_path_L1,paste0("bird_", cutoff_obs, "_sp_grid_5km.rds")))

# FDis calculation
fdis_bird2_5km_cutoff <- FDis2(bird_cutoff_sp_grid_5km, sp_faxes2_coord_bird)
saveRDS(fdis_bird2_5km_cutoff, file.path(filtered_output_path_L2, paste0("fdis_bird_", cutoff_obs, "_5km.rds")))

# mapping
bird_cutoff_FD_map_5km <- FD_map2(fdis_bird2_5km_cutoff, 5000, 'bird')
saveRDS(bird_cutoff_FD_map_5km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_FD_map_5km.rds")))

(bird_cutoff_FD_plot_5km <- bird_cutoff_FD_map_5km$gridFDisTA)
bird_cutoff_cell_FD_5km <- bird_cutoff_FD_map_5km$spatial_fdis_grid

# save data
saveRDS(bird_cutoff_cell_FD_5km, file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_cell_FD_5km.rds")))
ggsave(paste0('bird_', cutoff_obs, '_FD_plot_5km.png'), bird_cutoff_FD_plot_5km, path = filtered_data_figure_path, width = 6, height = 8, units = "in", dpi=1000)


#### final figure ####

# all map data
plant_cutoff_FD_map_100km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_FD_map_100km.rds")))
plant_cutoff_FD_plot_100km <- plant_cutoff_FD_map_100km$gridFDisTA

plant_cutoff_FD_map_75km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_FD_map_75km.rds")))
plant_cutoff_FD_plot_75km <- plant_cutoff_FD_map_75km$gridFDisTA

plant_cutoff_FD_map_50km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_FD_map_50km.rds")))
plant_cutoff_FD_plot_50km <- plant_cutoff_FD_map_50km$gridFDisTA

plant_cutoff_FD_map_25km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_FD_map_25km.rds")))
plant_cutoff_FD_plot_25km <- plant_cutoff_FD_map_25km$gridFDisTA

plant_cutoff_FD_map_10km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_FD_map_10km.rds")))
plant_cutoff_FD_plot_10km <- plant_cutoff_FD_map_10km$gridFDisTA

plant_cutoff_FD_map_5km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_FD_map_5km.rds")))
plant_cutoff_FD_plot_5km <- plant_cutoff_FD_map_5km$gridFDisTA

mammal_cutoff_FD_map_100km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_FD_map_100km.rds")))
mammal_cutoff_FD_plot_100km <- mammal_cutoff_FD_map_100km$gridFDisTA

mammal_cutoff_FD_map_75km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_FD_map_75km.rds")))
mammal_cutoff_FD_plot_75km <- mammal_cutoff_FD_map_75km$gridFDisTA

mammal_cutoff_FD_map_50km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_FD_map_50km.rds")))
mammal_cutoff_FD_plot_50km <- mammal_cutoff_FD_map_50km$gridFDisTA

mammal_cutoff_FD_map_25km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_FD_map_25km.rds")))
mammal_cutoff_FD_plot_25km <- mammal_cutoff_FD_map_25km$gridFDisTA

mammal_cutoff_FD_map_10km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_FD_map_10km.rds")))
mammal_cutoff_FD_plot_10km <- mammal_cutoff_FD_map_10km$gridFDisTA

mammal_cutoff_FD_map_5km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_FD_map_5km.rds")))
mammal_cutoff_FD_plot_5km <- mammal_cutoff_FD_map_5km$gridFDisTA

bird_cutoff_FD_map_100km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_FD_map_100km.rds")))
bird_cutoff_FD_plot_100km <- bird_cutoff_FD_map_100km$gridFDisTA

bird_cutoff_FD_map_75km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_FD_map_75km.rds")))
bird_cutoff_FD_plot_75km <- bird_cutoff_FD_map_75km$gridFDisTA

bird_cutoff_FD_map_50km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_FD_map_50km.rds")))
bird_cutoff_FD_plot_50km <- bird_cutoff_FD_map_50km$gridFDisTA

bird_cutoff_FD_map_25km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_FD_map_25km.rds")))
bird_cutoff_FD_plot_25km <- bird_cutoff_FD_map_25km$gridFDisTA

bird_cutoff_FD_map_10km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_FD_map_10km.rds")))
bird_cutoff_FD_plot_10km <- bird_cutoff_FD_map_10km$gridFDisTA

bird_cutoff_FD_map_5km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_FD_map_5km.rds")))
bird_cutoff_FD_plot_5km <- bird_cutoff_FD_map_5km$gridFDisTA


# individual plot edits
plant_cutoff_FD_plot_100km <- plant_cutoff_FD_plot_100km + labs(title='[100km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_cutoff_FD_plot_100km <- mammal_cutoff_FD_plot_100km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_FD_plot_100km <- bird_cutoff_FD_plot_100km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


plant_cutoff_FD_plot_75km <- plant_cutoff_FD_plot_75km + labs(title='[75km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_cutoff_FD_plot_75km <- mammal_cutoff_FD_plot_75km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_FD_plot_75km <- bird_cutoff_FD_plot_75km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(size = 16)) + theme(plot.margin = margin(0,0,0,0))


plant_cutoff_FD_plot_50km <- plant_cutoff_FD_plot_50km + labs(title='[50km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_cutoff_FD_plot_50km <- mammal_cutoff_FD_plot_50km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_FD_plot_50km <- bird_cutoff_FD_plot_50km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


plant_cutoff_FD_plot_25km <- plant_cutoff_FD_plot_25km + labs(title='[25km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_cutoff_FD_plot_25km <- mammal_cutoff_FD_plot_25km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_FD_plot_25km <- bird_cutoff_FD_plot_25km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))+ labs(x = "Longitude")


plant_cutoff_FD_plot_10km <- plant_cutoff_FD_plot_10km + labs(title='[10km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_cutoff_FD_plot_10km <- mammal_cutoff_FD_plot_10km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_FD_plot_10km <- bird_cutoff_FD_plot_10km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


# plant picture
plant <- pick_phylopic(name='Coffea alleizettei')

plant_cutoff_FD_plot_5km <- plant_cutoff_FD_plot_5km + labs(title='[5km]') + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20)) + add_phylopic(img=plant, x=-79.5, y=13, height=8)

# mammal picture
mammal <- pick_phylopic(name='Potos flavus', n=2, auto=2)

mammal_cutoff_FD_plot_5km <- mammal_cutoff_FD_plot_5km + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16)) + labs(y = "Latitude") + add_phylopic(img=mammal, x=-79, y=12, height=8)

# bird picture
bird <- pick_phylopic(name='Ramphastos sulfuratus', n=2, auto=1)

bird_cutoff_FD_plot_5km <- bird_cutoff_FD_plot_5km + annotation_scale(location = "bl",width_hint = 0.4, style = "bar") + annotation_north_arrow(location = "bl", which_north = "true", height = unit(0.5, "in"), width = unit(0.5, "in"), pad_x = unit(0.05, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) + add_phylopic(img=bird, x=-80, y=12.5, height=8) + theme(plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


# arrange
all_cutoff_fdis_plots <- plant_cutoff_FD_plot_5km + plant_cutoff_FD_plot_10km + plant_cutoff_FD_plot_25km + plant_cutoff_FD_plot_50km + plant_cutoff_FD_plot_75km + plant_cutoff_FD_plot_100km + mammal_cutoff_FD_plot_5km + mammal_cutoff_FD_plot_10km + mammal_cutoff_FD_plot_25km + mammal_cutoff_FD_plot_50km + mammal_cutoff_FD_plot_75km + mammal_cutoff_FD_plot_100km + bird_cutoff_FD_plot_5km + bird_cutoff_FD_plot_10km + bird_cutoff_FD_plot_25km + bird_cutoff_FD_plot_50km + bird_cutoff_FD_plot_75km + bird_cutoff_FD_plot_100km + plot_layout(ncol = 6, nrow = 3, guides = 'collect', axis_titles = 'collect', heights = c(1, 1, 1)) & theme(legend.position = 'left', axis.title = element_text(size = 20)) & plot_annotation(title='FDis', theme = theme(plot.title = element_text(hjust = 0.5, size=30, face='bold')))

all_cutoff_fdis_plots

ggsave(paste0('all_fdis', cutoff_obs, '_plots.png'), all_cutoff_fdis_plots, path = filtered_data_figure_path, width = 14, height = 12, units = "in", dpi=1000)
