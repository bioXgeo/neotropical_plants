#title: "Tropical Andes Functional diversity for plants and Frugivores"
#author: "Hazel J. Anderson, Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "This script calculates functional diversity as functional dispersion for plants and frugivores."
#date: "2025-11-03"
#output: html_document
#notes: JB used HPCC


# load required packages
library(mFD); library(sf); library(dplyr); library(ggplot2); library(rnaturalearth); library(ggspatial); library(rlang); library(doParallel); library(foreach); library(purrr); library(ggpubr); library(patchwork)


# set file paths
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')

#HPCC
data_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')
output_path_L2 <- file.path('/mnt/research/nasabio/data_2025/plants/L2')
figure_path <- file.path('/mnt/research/nasabio/data_2025/plants/figures')


# load functions
setwd("/mnt/ffs24/home/baljunas/Documents/neotropical_plants/code")
source("Functions.R")


# read in Data

# projected sf objects
plants_sf_species <- readRDS(file = file.path(data_path_L1,"plants_sf_species.rds"))
frugivores_sf_species <- readRDS(file = file.path(data_path_L1,"frugivores_sf_species.rds"))
Americas <- readRDS(file = file.path(data_path_L1, "Americas.rds"))
TApoly <- readRDS(file = file.path(data_path_L1,"TApoly.rds"))
TropicalAndes_IUCNHabitat_Forest <- readRDS(file = file.path(data_path_L1,"TropicalAndes_IUCNHabitat_Forest.rds"))

# trait data
plant_traits_df_final <- readRDS(file = file.path(data_path_L1,"plant_traits_df_final.rds"))
frugivore_traits_df_final <- readRDS(file = file.path(data_path_L1,"frugivore_traits_df_final.rds"))
mammal_traits_df_final <- readRDS(file=file.path(data_path_L1, 'mammal_traits_df_final.rds'))
bird_traits_df_final <- readRDS(file=file.path(data_path_L1, 'bird_traits_df_final.rds'))

# 100km
site_loc_key_plant_100km <- readRDS(file = file.path(data_path_L1,"site_loc_key_plant_100km.rds"))
site_loc_key_frugivore_100km <- readRDS(file = file.path(data_path_L1,"site_loc_key_frugivore_100km.rds"))
site_loc_key_mammal_100km <- readRDS(file = file.path(data_path_L1,"site_loc_key_mammal_100km.rds"))
site_loc_key_bird_100km <- readRDS(file = file.path(data_path_L1,"site_loc_key_bird_100km.rds"))
PAM_plant_site_final_100km <- readRDS(file = file.path(data_path_L1,"PAM_plant_site_final_100km.rds"))
PAM_frugivore_site_final_100km <- readRDS(file = file.path(data_path_L1,"PAM_frugivore_site_final_100km.rds"))
PAM_mammal_site_final_100km <- readRDS(file = file.path(data_path_L1,"PAM_mammal_site_final_100km.rds"))
PAM_bird_site_final_100km <- readRDS(file = file.path(data_path_L1,"PAM_bird_site_final_100km.rds"))

# 75km
site_loc_key_plant_75km <- readRDS(file = file.path(data_path_L1,"site_loc_key_plant_75km.rds"))
site_loc_key_frugivore_75km <- readRDS(file = file.path(data_path_L1,"site_loc_key_frugivore_75km.rds"))
site_loc_key_mammal_75km <- readRDS(file = file.path(data_path_L1,"site_loc_key_mammal_75km.rds"))
site_loc_key_bird_75km <- readRDS(file = file.path(data_path_L1,"site_loc_key_bird_75km.rds"))
PAM_plant_site_final_75km <- readRDS(file = file.path(data_path_L1,"PAM_plant_site_final_75km.rds"))
PAM_frugivore_site_final_75km <- readRDS(file = file.path(data_path_L1,"PAM_frugivore_site_final_75km.rds"))
PAM_mammal_site_final_75km <- readRDS(file = file.path(data_path_L1,"PAM_mammal_site_final_75km.rds"))
PAM_bird_site_final_75km <- readRDS(file = file.path(data_path_L1,"PAM_bird_site_final_75km.rds"))

# 50km
site_loc_key_plant_50km <- readRDS(file = file.path(data_path_L1,"site_loc_key_plant_50km.rds"))
site_loc_key_mammal_50km <- readRDS(file = file.path(data_path_L1,"site_loc_key_mammal_50km.rds"))
site_loc_key_bird_50km <- readRDS(file = file.path(data_path_L1,"site_loc_key_bird_50km.rds"))
site_loc_key_frugivore_50km <- readRDS(file = file.path(data_path_L1,"site_loc_key_frugivore_50km.rds"))
PAM_plant_site_final_50km <- readRDS(file = file.path(data_path_L1,"PAM_plant_site_final_50km.rds"))
PAM_frugivore_site_final_50km <- readRDS(file = file.path(data_path_L1,"PAM_frugivore_site_final_50km.rds"))
PAM_mammal_site_final_50km <- readRDS(file = file.path(data_path_L1,"PAM_mammal_site_final_50km.rds"))
PAM_bird_site_final_50km <- readRDS(file = file.path(data_path_L1,"PAM_bird_site_final_50km.rds"))

# 25km
site_loc_key_plant_25km <- readRDS(file = file.path(data_path_L1,"site_loc_key_plant_25km.rds"))
site_loc_key_frugivore_25km <- readRDS(file = file.path(data_path_L1,"site_loc_key_frugivore_25km.rds"))
site_loc_key_mammal_25km <- readRDS(file = file.path(data_path_L1,"site_loc_key_mammal_25km.rds"))
site_loc_key_bird_25km <- readRDS(file = file.path(data_path_L1,"site_loc_key_bird_25km.rds"))
PAM_plant_site_final_25km <- readRDS(file = file.path(data_path_L1,"PAM_plant_site_final_25km.rds"))
PAM_frugivore_site_final_25km <- readRDS(file = file.path(data_path_L1,"PAM_frugivore_site_final_25km.rds"))
PAM_mammal_site_final_25km <- readRDS(file = file.path(data_path_L1,"PAM_mammal_site_final_25km.rds"))
PAM_bird_site_final_25km <- readRDS(file = file.path(data_path_L1,"PAM_bird_site_final_25km.rds"))

# 10km
site_loc_key_plant_10km <- readRDS(file = file.path(data_path_L1,"site_loc_key_plant_10km.rds"))
site_loc_key_frugivore_10km <- readRDS(file = file.path(data_path_L1,"site_loc_key_frugivore_10km.rds"))
site_loc_key_mammal_10km <- readRDS(file = file.path(data_path_L1,"site_loc_key_mammal_10km.rds"))
site_loc_key_bird_10km <- readRDS(file = file.path(data_path_L1,"site_loc_key_bird_10km.rds"))
PAM_plant_site_final_10km <- readRDS(file = file.path(data_path_L1,"PAM_plant_site_final_10km.rds"))
PAM_frugivore_site_final_10km <- readRDS(file = file.path(data_path_L1,"PAM_frugivore_site_final_10km.rds"))
PAM_mammal_site_final_10km <- readRDS(file = file.path(data_path_L1,"PAM_mammal_site_final_10km.rds"))
PAM_bird_site_final_10km <- readRDS(file = file.path(data_path_L1,"PAM_bird_site_final_10km.rds"))

# 5km
site_loc_key_plant_5km <- readRDS(file = file.path(data_path_L1,"site_loc_key_plant_5km.rds"))
site_loc_key_frugivore_5km <- readRDS(file = file.path(data_path_L1,"site_loc_key_frugivore_5km.rds"))
site_loc_key_mammal_5km <- readRDS(file = file.path(data_path_L1,"site_loc_key_mammal_5km.rds"))
site_loc_key_bird_5km <- readRDS(file = file.path(data_path_L1,"site_loc_key_bird_5km.rds"))
PAM_plant_site_final_5km <- readRDS(file = file.path(data_path_L1,"PAM_plant_site_final_5km.rds"))
PAM_frugivore_site_final_5km <- readRDS(file = file.path(data_path_L1,"PAM_frugivore_site_final_5km.rds"))
PAM_mammal_site_final_5km <- readRDS(file = file.path(data_path_L1,"PAM_mammal_site_final_5km.rds"))
PAM_bird_site_final_5km <- readRDS(file = file.path(data_path_L1,"PAM_bird_site_final_5km.rds"))


# quality of functional spaces
fspaces_quality(PAM_frugivore_site_final_100km, frugivore_traits_df_final, 'frugivore')
fspaces_quality(PAM_mammal_site_final_100km, mammal_traits_df_final, 'mammal')
fspaces_quality(PAM_bird_site_final_100km, bird_traits_df_final, 'bird')
fspaces_quality(PAM_plant_site_final_100km, plant_traits_df_final, 'plant')


# plots
fspace_quality_plot(fspaces_quality_frugivore)
fspace_quality_plot(fspaces_quality_mammal)
fspace_quality_plot(fspaces_quality_bird)
fspace_quality_plot(fspaces_quality_plant)


# correlation between functional axes and traits, functional space plots
pc_coords(fspaces_quality_frugivore, frugivore_traits_df_final, 'frugivore')
fspace_corr_plots(sp_faxes_coord_frugivore, tr_faxes_frugivore)

pc_coords(fspaces_quality_mammal, mammal_traits_df_final, 'mammal')
fspace_corr_plots(sp_faxes_coord_mammal, tr_faxes_mammal)

pc_coords(fspaces_quality_bird, bird_traits_df_final, 'bird')
fspace_corr_plots(sp_faxes_coord_bird, tr_faxes_bird)

pc_coords(fspaces_quality_plant, plant_traits_df_final, 'plant')
fspace_corr_plots(sp_faxes_coord_plant, tr_faxes_plant)


# functional dispersion calculation
fdis_frugivore_100km <- FDis(PAM_frugivore_site_final_100km, sp_faxes_coord_frugivore)
fdis_mammal_100km <- FDis(PAM_mammal_site_final_100km, sp_faxes_coord_mammal)
fdis_bird_100km <- FDis(PAM_bird_site_final_100km, sp_faxes_coord_bird)
fdis_plant_100km <- FDis(PAM_plant_site_final_100km, sp_faxes_coord_plant)

saveRDS(fdis_frugivore_100km, file = file.path(output_path_L2,"fdis_frugivore_100km.rds"))
saveRDS(fdis_mammal_100km, file = file.path(output_path_L2,"fdis_mammal_100km.rds"))
saveRDS(fdis_bird_100km, file = file.path(output_path_L2,"fdis_bird_100km.rds"))
saveRDS(fdis_plant_100km, file = file.path(output_path_L2,"fdis_plant_100km.rds"))


fdis_frugivore_75km <- FDis(PAM_frugivore_site_final_75km, sp_faxes_coord_frugivore)
fdis_mammal_75km <- FDis(PAM_mammal_site_final_75km, sp_faxes_coord_mammal)
fdis_bird_75km <- FDis(PAM_bird_site_final_75km, sp_faxes_coord_bird)
fdis_plant_75km <- FDis(PAM_plant_site_final_75km, sp_faxes_coord_plant)

saveRDS(fdis_frugivore_75km, file = file.path(output_path_L2,"fdis_frugivore_75km.rds"))
saveRDS(fdis_mammal_75km, file = file.path(output_path_L2,"fdis_mammal_75km.rds"))
saveRDS(fdis_bird_75km, file = file.path(output_path_L2,"fdis_bird_75km.rds"))
saveRDS(fdis_plant_75km, file = file.path(output_path_L2,"fdis_plant_75km.rds"))


fdis_frugivore_50km <- FDis(PAM_frugivore_site_final_50km, sp_faxes_coord_frugivore)
fdis_mammal_50km <- FDis(PAM_mammal_site_final_50km, sp_faxes_coord_mammal)
fdis_bird_50km <- FDis(PAM_bird_site_final_50km, sp_faxes_coord_bird)
fdis_plant_50km <- FDis(PAM_plant_site_final_50km, sp_faxes_coord_plant)

saveRDS(fdis_frugivore_50km, file = file.path(output_path_L2,"fdis_frugivore_50km.rds"))
saveRDS(fdis_mammal_50km, file = file.path(output_path_L2,"fdis_mammal_50km.rds"))
saveRDS(fdis_bird_50km, file = file.path(output_path_L2,"fdis_bird_50km.rds"))
saveRDS(fdis_plant_50km, file = file.path(output_path_L2,"fdis_plant_50km.rds"))


fdis_frugivore_25km <- FDis(PAM_frugivore_site_final_25km, sp_faxes_coord_frugivore)
fdis_mammal_25km <- FDis(PAM_mammal_site_final_25km, sp_faxes_coord_mammal)
fdis_bird_25km <- FDis(PAM_bird_site_final_25km, sp_faxes_coord_bird)
fdis_plant_25km <- FDis(PAM_plant_site_final_25km, sp_faxes_coord_plant)

saveRDS(fdis_frugivore_25km, file = file.path(output_path_L2,"fdis_frugivore_25km.rds"))
saveRDS(fdis_mammal_25km, file = file.path(output_path_L2,"fdis_mammal_25km.rds"))
saveRDS(fdis_bird_25km, file = file.path(output_path_L2,"fdis_bird_25km.rds"))
saveRDS(fdis_plant_25km, file = file.path(output_path_L2,"fdis_plant_25km.rds"))


fdis_frugivore_10km <- FDis(PAM_frugivore_site_final_10km, sp_faxes_coord_frugivore)
fdis_mammal_10km <- FDis(PAM_mammal_site_final_10km, sp_faxes_coord_mammal)
fdis_bird_10km <- FDis(PAM_bird_site_final_10km, sp_faxes_coord_bird)
fdis_plant_10km <- FDis(PAM_plant_site_final_10km, sp_faxes_coord_plant)

saveRDS(fdis_frugivore_10km, file = file.path(output_path_L2,"fdis_frugivore_10km.rds"))
saveRDS(fdis_mammal_10km, file = file.path(output_path_L2,"fdis_mammal_10km.rds"))
saveRDS(fdis_bird_10km, file = file.path(output_path_L2,"fdis_bird_10km.rds"))
saveRDS(fdis_plant_10km, file = file.path(output_path_L2,"fdis_plant_10km.rds"))


fdis_frugivore_5km <- FDis(PAM_frugivore_site_final_5km, sp_faxes_coord_frugivore)
fdis_mammal_5km <- FDis(PAM_mammal_site_final_5km, sp_faxes_coord_mammal)
fdis_bird_5km <- FDis(PAM_bird_site_final_5km, sp_faxes_coord_bird)
fdis_plant_5km <- FDis(PAM_plant_site_final_5km, sp_faxes_coord_plant)

saveRDS(fdis_frugivore_5km, file = file.path(output_path_L2,"fdis_frugivore_5km.rds"))
saveRDS(fdis_mammal_5km, file = file.path(output_path_L2,"fdis_mammal_5km.rds"))
saveRDS(fdis_bird_5km, file = file.path(output_path_L2,"fdis_bird_5km.rds"))
saveRDS(fdis_plant_5km, file = file.path(output_path_L2,"fdis_plant_5km.rds"))


#### Mapping ####

# 100 km
F100 <- FD_map(site_loc_key_frugivore_100km, PAM_frugivore_site_final_100km, 100000, fdis_frugivore_100km, 'frugivore')
FgridFDisTA_100km <- F100$gridFDisTA
FcellFDis_100km <- F100$spatial_fdis_grid

# mammals
M100 <- FD_map(site_loc_key_mammal_100km, PAM_mammal_site_final_100km, 100000, fdis_mammal_100km, 'mammal')
MgridFDisTA_100km <- M100$gridFDisTA
McellFDis_100km <- M100$spatial_fdis_grid

# birds
B100 <- FD_map(site_loc_key_bird_100km, PAM_bird_site_final_100km, 100000, fdis_bird_100km, 'bird')
BgridFDisTA_100km <- B100$gridFDisTA
BcellFDis_100km <- B100$spatial_fdis_grid

# plants
P100 <- FD_map(site_loc_key_plant_100km, PAM_plant_site_final_100km, 100000, fdis_plant_100km, 'plant')
PgridFDisTA_100km <- P100$gridFDisTA
PcellFDis_100km <- P100$spatial_fdis_grid


# 75km 

# frugivores
F75 <- FD_map(site_loc_key_frugivore_75km, PAM_frugivore_site_final_75km, 75000, fdis_frugivore_75km, 'frugivore')
FgridFDisTA_75km <- F75$gridFDisTA
FcellFDis_75km <- F75$spatial_fdis_grid

# mammals
M75 <- FD_map(site_loc_key_mammal_75km, PAM_mammal_site_final_75km, 75000, fdis_mammal_75km, 'mammal')
MgridFDisTA_75km <- M75$gridFDisTA
McellFDis_75km <- M75$spatial_fdis_grid

# birds
B75 <- FD_map(site_loc_key_bird_75km, PAM_bird_site_final_75km, 75000, fdis_bird_75km, 'bird')
BgridFDisTA_75km <- B75$gridFDisTA
BcellFDis_75km <- B75$spatial_fdis_grid

# plants
P75 <- FD_map(site_loc_key_plant_75km, PAM_plant_site_final_75km, 75000, fdis_plant_75km, 'plant')
PgridFDisTA_75km <- P75$gridFDisTA
PcellFDis_75km <- P75$spatial_fdis_grid


# 50km 

# frugivores
F50 <- FD_map(site_loc_key_frugivore_50km, PAM_frugivore_site_final_50km, 50000, fdis_frugivore_50km, 'frugivore')
FgridFDisTA_50km <- F50$gridFDisTA
FcellFDis_50km <- F50$spatial_fdis_grid

# mammals
M50 <- FD_map(site_loc_key_mammal_50km, PAM_mammal_site_final_50km, 50000, fdis_mammal_50km, 'mammal')
MgridFDisTA_50km <- M50$gridFDisTA
McellFDis_50km <- M50$spatial_fdis_grid

# birds
B50 <- FD_map(site_loc_key_bird_50km, PAM_bird_site_final_50km, 50000, fdis_bird_50km, 'bird')
BgridFDisTA_50km <- B50$gridFDisTA
BcellFDis_50km <- B50$spatial_fdis_grid

# plants
P50 <- FD_map(site_loc_key_plant_50km, PAM_plant_site_final_50km, 50000, fdis_plant_50km, 'plant')
PgridFDisTA_50km <- P50$gridFDisTA
PcellFDis_50km <- P50$spatial_fdis_grid


# 25km

# frugivores
F25 <- FD_map(site_loc_key_frugivore_25km, PAM_frugivore_site_final_25km, 25000, fdis_frugivore_25km, 'frugivore')
FgridFDisTA_25km <- F25$gridFDisTA
FcellFDis_25km <- F25$spatial_fdis_grid

# mammals
M25 <- FD_map(site_loc_key_mammal_25km, PAM_mammal_site_final_25km, 25000, fdis_mammal_25km, 'mammal')
MgridFDisTA_25km <- M25$gridFDisTA
McellFDis_25km <- M25$spatial_fdis_grid

# birds
B25 <- FD_map(site_loc_key_bird_25km, PAM_bird_site_final_25km, 25000, fdis_bird_25km, 'bird')
BgridFDisTA_25km <- B25$gridFDisTA
BcellFDis_25km <- B25$spatial_fdis_grid

# plants
P25 <- FD_map(site_loc_key_plant_25km, PAM_plant_site_final_25km, 25000, fdis_plant_25km, 'plant')
PgridFDisTA_25km <- P25$gridFDisTA
PcellFDis_25km <- P25$spatial_fdis_grid


# 10km

# frugivores
F10 <- FD_map(site_loc_key_frugivore_10km, PAM_frugivore_site_final_10km, 10000, fdis_frugivore_10km, 'frugivore')
FgridFDisTA_10km <- F10$gridFDisTA
FcellFDis_10km <- F10$spatial_fdis_grid

# mammals
M10 <- FD_map(site_loc_key_mammal_10km, PAM_mammal_site_final_10km, 10000, fdis_mammal_10km, 'mammal')
MgridFDisTA_10km <- M10$gridFDisTA
McellFDis_10km <- M10$spatial_fdis_grid

# birds
B10 <- FD_map(site_loc_key_bird_10km, PAM_bird_site_final_10km, 10000, fdis_bird_10km, 'bird')
BgridFDisTA_10km <- B10$gridFDisTA
BcellFDis_10km <- B10$spatial_fdis_grid

#  plants
P10 <- FD_map(site_loc_key_plant_10km, PAM_plant_site_final_10km, 10000, fdis_plant_10km, 'plant')
PgridFDisTA_10km <- P10$gridFDisTA
PcellFDis_10km <- P10$spatial_fdis_grid


# 5km

# frugivores
F5 <- FD_map(site_loc_key_frugivore_5km, PAM_frugivore_site_final_5km, 5000, fdis_frugivore_5km, 'frugivore')
FgridFDisTA_5km <- F5$gridFDisTA
FcellFDis_5km <- F5$spatial_fdis_grid

# mammals
M5 <- FD_map(site_loc_key_mammal_5km, PAM_mammal_site_final_5km, 5000, fdis_mammal_5km, 'mammal')
MgridFDisTA_5km <- M5$gridFDisTA
McellFDis_5km <- M5$spatial_fdis_grid

# birds
B5 <- FD_map(site_loc_key_bird_5km, PAM_bird_site_final_5km, 5000, fdis_bird_5km, 'bird')
BgridFDisTA_5km <- B5$gridFDisTA
BcellFDis_5km <- B5$spatial_fdis_grid

# plants
P5 <- FD_map(site_loc_key_plant_5km, PAM_plant_site_final_5km, 5000, fdis_plant_5km, 'plant')
PgridFDisTA_5km <- P5$gridFDisTA
PcellFDis_5km <- P5$spatial_fdis_grid


# all frugivores
all_frugivore_fdis_plots <- ggarrange(FgridFDisTA_5km, FgridFDisTA_10km, FgridFDisTA_25km, FgridFDisTA_50km, FgridFDisTA_75km, FgridFDisTA_100km, ncol = 6, nrow = 1, common.legend = TRUE, legend = "left")
all_frugivore_fdis_plots_labeled <- ggpubr::annotate_figure(all_frugivore_fdis_plots, left = ggpubr::text_grob("Frugivores", face = "bold", size = 20, rot = 90))
all_frugivore_fdis_plots_labeled
ggsave("all_frugivore_fdis_plots2.png", all_frugivore_fdis_plots_labeled, path = figure_path, width = 16, height = 10, units = "in", dpi=1000)


# all mammals
all_mammal_fdis_plots <- ggarrange(MgridFDisTA_5km, MgridFDisTA_10km, MgridFDisTA_25km, MgridFDisTA_50km, MgridFDisTA_75km, MgridFDisTA_100km, ncol = 6, nrow = 1, common.legend = TRUE, legend = "left")
all_mammal_fdis_plots_labeled <- ggpubr::annotate_figure(all_mammal_fdis_plots, left = ggpubr::text_grob("Mammals", face = "bold", size = 20, rot = 90))
all_mammal_fdis_plots_labeled
ggsave("all_mammal_fdis_plots2.png", all_mammal_fdis_plots_labeled, path = figure_path, width = 16, height = 10, units = "in", dpi=1000)


# all birds
all_bird_fdis_plots <- ggarrange(BgridFDisTA_5km, BgridFDisTA_10km, BgridFDisTA_25km, BgridFDisTA_50km, BgridFDisTA_75km, BgridFDisTA_100km, ncol = 6, nrow = 1, common.legend = TRUE, legend = "left")
all_bird_fdis_plots_labeled <- ggpubr::annotate_figure(all_bird_fdis_plots, left = ggpubr::text_grob("Birds", face = "bold", size = 20, rot = 90))
all_bird_fdis_plots_labeled
ggsave("all_bird_fdis_plots2.png", all_bird_fdis_plots_labeled, path = figure_path, width = 16, height = 10, units = "in", dpi=1000)


# all plants
all_plant_fdis_plots <- ggarrange(PgridFDisTA_5km, PgridFDisTA_10km, PgridFDisTA_25km, PgridFDisTA_50km, PgridFDisTA_75km, PgridFDisTA_100km, ncol = 6, nrow = 1, common.legend = TRUE, legend = "left")
all_plant_fdis_plots_labeled <- ggpubr::annotate_figure(all_plant_fdis_plots, left = ggpubr::text_grob("Plants", face = "bold", size = 20, rot = 90))
all_plant_fdis_plots_labeled
ggsave("all_plant_fdis_plots2.png", all_plant_fdis_plots_labeled, path = figure_path, width = 16, height = 10, units = "in", dpi=1000)
