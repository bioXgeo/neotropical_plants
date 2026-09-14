#title: "Tropical Andes functional and taxonomic diversity spatial patterns for plants and Frugivores"
#author: "Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Hazel J. Anderson, Beth E. Gerstner, Phoebe L. Zarnetske, Kelly Kaspar"
#overview: "Calculating and mapping the difference between taxonomic and functional diversity as functional dispersion for plants and frugivores."
#data input: 
#data output:
#date: "2026-04-22"
#notes: JB used HPCC


# load required packages
library(mFD); library(sf); library(dplyr); library(ggplot2); library(rnaturalearth); library(ggspatial); library(rlang); library(doParallel); library(foreach); library(purrr); library(ggpubr); library(patchwork); library(stringr); library(rphylopic); library(scales); library(viridis)


# set file paths
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')


# load functions
source("C:/GitHub_projects/neotropical_plants/code/Functions.R")


# read in Data

# projected sf objects
plants_sf_species <- readRDS(file = file.path(data_path_L1,"plants_sf_species.rds"))
frugivores_sf_species <- readRDS(file = file.path(data_path_L1,"frugivores_sf_species.rds"))
Americas <- readRDS(file = file.path(data_path_L1, "Americas.rds"))
TApoly <- readRDS(file = file.path(data_path_L1,"TApoly.rds"))
TropicalAndes_IUCNHabitat_Forest <- readRDS(file = file.path(data_path_L1,"TropicalAndes_IUCNHabitat_Forest.rds"))


#### mapping with data filtered by 1970 ####

# set file paths
all_data_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2/all_data')
all_output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2/all_data')
all_data_figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures/all_data')


# FD data 

# 5km
mammal_cell_FD_5km <- readRDS(file = file.path(all_data_path_L2,"mammal_cell_FD_5km.rds"))
plant_cell_FD_5km <- readRDS(file = file.path(all_data_path_L2,"plant_cell_FD_5km.rds"))
bird_cell_FD_5km <- readRDS(file = file.path(all_data_path_L2,"bird_cell_FD_5km.rds"))


# 10km
mammal_cell_FD_10km <- readRDS(file = file.path(all_data_path_L2,"mammal_cell_FD_10km.rds"))
plant_cell_FD_10km <- readRDS(file = file.path(all_data_path_L2,"plant_cell_FD_10km.rds"))
bird_cell_FD_10km <- readRDS(file = file.path(all_data_path_L2,"bird_cell_FD_10km.rds"))


# 25km
mammal_cell_FD_25km <- readRDS(file = file.path(all_data_path_L2,"mammal_cell_FD_25km.rds"))
plant_cell_FD_25km <- readRDS(file = file.path(all_data_path_L2,"plant_cell_FD_25km.rds"))
bird_cell_FD_25km <- readRDS(file = file.path(all_data_path_L2,"bird_cell_FD_25km.rds"))


# 50km
mammal_cell_FD_50km <- readRDS(file = file.path(all_data_path_L2,"mammal_cell_FD_50km.rds"))
plant_cell_FD_50km <- readRDS(file = file.path(all_data_path_L2,"plant_cell_FD_50km.rds"))
bird_cell_FD_50km <- readRDS(file = file.path(all_data_path_L2,"bird_cell_FD_50km.rds"))


# 75km
mammal_cell_FD_75km <- readRDS(file = file.path(all_data_path_L2,"mammal_cell_FD_75km.rds"))
plant_cell_FD_75km <- readRDS(file = file.path(all_data_path_L2,"plant_cell_FD_75km.rds"))
bird_cell_FD_75km <- readRDS(file = file.path(all_data_path_L2,"bird_cell_FD_75km.rds"))


# 100km
mammal_cell_FD_100km <- readRDS(file = file.path(all_data_path_L2,"mammal_cell_FD_100km.rds"))
plant_cell_FD_100km <- readRDS(file = file.path(all_data_path_L2,"plant_cell_FD_100km.rds"))
bird_cell_FD_100km <- readRDS(file = file.path(all_data_path_L2,"bird_cell_FD_100km.rds"))



# TD data (filtered by 1970)

# 5km
mammal_cell_TD_5km <- readRDS(file.path(all_data_path_L2,"mammal_cell_TD_5km.rds"))
plant_cell_TD_5km <- readRDS(file.path(all_data_path_L2,"plant_cell_TD_5km.rds"))
bird_cell_TD_5km <- readRDS(file.path(all_data_path_L2,"bird_cell_TD_5km.rds"))


# 10km 
mammal_cell_TD_10km <- readRDS(file.path(all_data_path_L2,"mammal_cell_TD_10km.rds"))
plant_cell_TD_10km <- readRDS(file.path(all_data_path_L2,"plant_cell_TD_10km.rds"))
bird_cell_TD_10km <- readRDS(file.path(all_data_path_L2,"bird_cell_TD_10km.rds"))


# 25km
mammal_cell_TD_25km <- readRDS(file.path(all_data_path_L2,"mammal_cell_TD_25km.rds"))
plant_cell_TD_25km <- readRDS(file.path(all_data_path_L2,"plant_cell_TD_25km.rds"))
bird_cell_TD_25km <- readRDS(file.path(all_data_path_L2,"bird_cell_TD_25km.rds"))


# 50km
mammal_cell_TD_50km <- readRDS(file.path(all_data_path_L2,"mammal_cell_TD_50km.rds"))
plant_cell_TD_50km <- readRDS(file.path(all_data_path_L2,"plant_cell_TD_50km.rds"))
bird_cell_TD_50km <- readRDS(file.path(all_data_path_L2,"bird_cell_TD_50km.rds"))


# 75km
mammal_cell_TD_75km <- readRDS(file.path(all_data_path_L2,"mammal_cell_TD_75km.rds"))
plant_cell_TD_75km <- readRDS(file.path(all_data_path_L2,"plant_cell_TD_75km.rds"))
bird_cell_TD_75km <- readRDS(file.path(all_data_path_L2,"bird_cell_TD_75km.rds"))


# 100km
mammal_cell_TD_100km <- readRDS(file.path(all_data_path_L2,"mammal_cell_TD_100km.rds"))
plant_cell_TD_100km <- readRDS(file.path(all_data_path_L2,"plant_cell_TD_100km.rds"))
bird_cell_TD_100km <- readRDS(file.path(all_data_path_L2,"bird_cell_TD_100km.rds"))


# Plant maps
(plant_div_map_100km <- div_diff_map(plant_cell_TD_100km, plant_cell_FD_100km, 'plant'))
(plant_div_map_75km <- div_diff_map(plant_cell_TD_75km, plant_cell_FD_75km, 'Plants'))
(plant_div_map_50km <- div_diff_map(plant_cell_TD_50km, plant_cell_FD_50km, 'Plants'))
(plant_div_map_25km <- div_diff_map(plant_cell_TD_25km, plant_cell_FD_25km, 'Plants'))
(plant_div_map_10km <- div_diff_map(plant_cell_TD_10km, plant_cell_FD_10km, 'Plants'))
(plant_div_map_5km <- div_diff_map(plant_cell_TD_5km, plant_cell_FD_5km, 'Plants'))


# Mammal maps
(mammal_div_map_100km <- div_diff_map(mammal_cell_TD_100km, mammal_cell_FD_100km, 'mammal'))
(mammal_div_map_75km <- div_diff_map(mammal_cell_TD_75km, mammal_cell_FD_75km, 'mammal'))
(mammal_div_map_50km <- div_diff_map(mammal_cell_TD_50km, mammal_cell_FD_50km, 'mammal'))
(mammal_div_map_25km <- div_diff_map(mammal_cell_TD_25km, mammal_cell_FD_25km, 'mammal'))
(mammal_div_map_10km <- div_diff_map(mammal_cell_TD_10km, mammal_cell_FD_10km, 'mammal'))
(mammal_div_map_5km <- div_diff_map(mammal_cell_TD_5km, mammal_cell_FD_5km, 'mammal'))


# Bird maps
(bird_div_map_100km <- div_diff_map(bird_cell_TD_100km, bird_cell_FD_100km, 'bird'))
(bird_div_map_75km <- div_diff_map(bird_cell_TD_75km, bird_cell_FD_75km, 'bird'))
(bird_div_map_50km <- div_diff_map(bird_cell_TD_50km, bird_cell_FD_50km, 'bird'))
(bird_div_map_25km <- div_diff_map(bird_cell_TD_25km, bird_cell_FD_25km, 'bird'))
(bird_div_map_10km <- div_diff_map(bird_cell_TD_10km, bird_cell_FD_10km, 'bird'))
(bird_div_map_5km <- div_diff_map(bird_cell_TD_5km, bird_cell_FD_5km, 'bird'))


# combine all plots 

# individual plot edits
mammal_div_map_100km <- mammal_div_map_100km + labs(title='[100km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20)) 

plant_div_map_100km <- plant_div_map_100km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_100km <- bird_div_map_100km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


mammal_div_map_75km <- mammal_div_map_75km + labs(title='[75km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

plant_div_map_75km <- plant_div_map_75km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_75km <- bird_div_map_75km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


mammal_div_map_50km <- mammal_div_map_50km + labs(title='[50km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

plant_div_map_50km <- plant_div_map_50km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_50km <- bird_div_map_50km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


mammal_div_map_25km <- mammal_div_map_25km + labs(title='[25km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

plant_div_map_25km <- plant_div_map_25km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_25km <- bird_div_map_25km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + labs(x = "Longitude")


mammal_div_map_10km <- mammal_div_map_10km + labs(title='[10km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

plant_div_map_10km <- plant_div_map_10km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_10km <- bird_div_map_10km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


# mammal picture
mammal <- pick_phylopic(name='Potos flavus', n=2, auto=2)

mammal_div_map_5km <- mammal_div_map_5km + labs(title='[5km]') + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20), axis.text = element_text(size=16)) + add_phylopic(img=mammal, x=-79, y=12, height=8)

# plant picture
plant <- pick_phylopic(name='Coffea alleizettei')

plant_div_map_5km <- plant_div_map_5km + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + labs(y = "Latitude") + add_phylopic(img=plant, x=-79, y=12, height=8)

# bird picture
bird <- pick_phylopic(name='Ramphastos sulfuratus', n=2, auto=1)

bird_div_map_5km <- bird_div_map_5km + annotation_scale(location = "bl",width_hint = 0.4, style = "bar") + annotation_north_arrow(location = "bl", which_north = "true", height = unit(0.5, "in"), width = unit(0.5, "in"), pad_x = unit(0.05, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) + theme(plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + add_phylopic(img=bird, x=-79, y=12, height=8)


# arrange
all_div_plots <-  mammal_div_map_5km + mammal_div_map_10km + mammal_div_map_25km + mammal_div_map_50km + mammal_div_map_75km + mammal_div_map_100km + plant_div_map_5km + plant_div_map_10km + plant_div_map_25km + plant_div_map_50km + plant_div_map_75km + plant_div_map_100km + bird_div_map_5km + bird_div_map_10km + bird_div_map_25km + bird_div_map_50km + bird_div_map_75km + bird_div_map_100km + plot_layout(ncol = 6, nrow = 3, guides = 'collect', axis_titles = 'collect', heights = c(1, 1, 1)) & theme(legend.position = 'left', axis.title = element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=16)) & plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size=30, face='bold')))

all_div_plots

ggsave('div_comparison_maps.png', all_div_plots, path = all_data_figure_path, width = 15, height = 12, units = "in", dpi=1000)


##### repeat mapping with obs cutoff ####

# set file paths
filtered_data_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2/filtered_data')
filtered_output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2/filtered_data')
filtered_data_figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures/filtered_data')

# set obs cutoff
cutoff_obs <- 20

# FD data

# 5km
mammal_cutoff_cell_FD_5km <- readRDS(file = file.path(filtered_data_path_L2, paste0("mammal_", cutoff_obs, "_cell_FD_5km.rds")))
plant_cutoff_cell_FD_5km <- readRDS(file = file.path(filtered_data_path_L2, paste0("plant_", cutoff_obs, "_cell_FD_5km.rds")))
bird_cutoff_cell_FD_5km <- readRDS(file = file.path(filtered_data_path_L2, paste0("bird_", cutoff_obs, "_cell_FD_5km.rds")))


# 10km
mammal_cutoff_cell_FD_10km <- readRDS(file = file.path(filtered_data_path_L2, paste0("mammal_", cutoff_obs, "_cell_FD_10km.rds")))
plant_cutoff_cell_FD_10km <- readRDS(file = file.path(filtered_data_path_L2, paste0("plant_", cutoff_obs, "_cell_FD_10km.rds")))
bird_cutoff_cell_FD_10km <- readRDS(file = file.path(filtered_data_path_L2, paste0("bird_", cutoff_obs, "_cell_FD_10km.rds")))


# 25km
mammal_cutoff_cell_FD_25km <- readRDS(file = file.path(filtered_data_path_L2, paste0("mammal_", cutoff_obs, "_cell_FD_25km.rds")))
plant_cutoff_cell_FD_25km <- readRDS(file = file.path(filtered_data_path_L2, paste0("plant_", cutoff_obs, "_cell_FD_25km.rds")))
bird_cutoff_cell_FD_25km <- readRDS(file = file.path(filtered_data_path_L2, paste0("bird_", cutoff_obs, "_cell_FD_25km.rds")))


# 50km
mammal_cutoff_cell_FD_50km <- readRDS(file = file.path(filtered_data_path_L2, paste0("mammal_", cutoff_obs, "_cell_FD_50km.rds")))
plant_cutoff_cell_FD_50km <- readRDS(file = file.path(filtered_data_path_L2, paste0("plant_", cutoff_obs, "_cell_FD_50km.rds")))
bird_cutoff_cell_FD_50km <- readRDS(file = file.path(filtered_data_path_L2, paste0("bird_", cutoff_obs, "_cell_FD_50km.rds")))


# 75km
mammal_cutoff_cell_FD_75km <- readRDS(file = file.path(filtered_data_path_L2, paste0("mammal_", cutoff_obs, "_cell_FD_75km.rds")))
plant_cutoff_cell_FD_75km <- readRDS(file = file.path(filtered_data_path_L2, paste0("plant_", cutoff_obs, "_cell_FD_75km.rds")))
bird_cutoff_cell_FD_75km <- readRDS(file = file.path(filtered_data_path_L2, paste0("bird_", cutoff_obs, "_cell_FD_75km.rds")))


# 100km
mammal_cutoff_cell_FD_100km <- readRDS(file = file.path(filtered_data_path_L2, paste0("mammal_", cutoff_obs, "_cell_FD_100km.rds")))
plant_cutoff_cell_FD_100km <- readRDS(file = file.path(filtered_data_path_L2, paste0("plant_", cutoff_obs, "_cell_FD_100km.rds")))
bird_cutoff_cell_FD_100km <- readRDS(file = file.path(filtered_data_path_L2, paste0("bird_", cutoff_obs, "_cell_FD_100km.rds")))


# TD data 

# 5km
mammal_cutoff_cell_TD_5km <- readRDS(file = file.path(filtered_data_path_L2, paste0("mammal_", cutoff_obs, "_cell_TD_5km.rds")))
plant_cutoff_cell_TD_5km <- readRDS(file = file.path(filtered_data_path_L2, paste0("plant_", cutoff_obs, "_cell_TD_5km.rds")))
bird_cutoff_cell_TD_5km <- readRDS(file = file.path(filtered_data_path_L2, paste0("bird_", cutoff_obs, "_cell_TD_5km.rds")))


# 10km
mammal_cutoff_cell_TD_10km <- readRDS(file = file.path(filtered_data_path_L2, paste0("mammal_", cutoff_obs, "_cell_TD_10km.rds")))
plant_cutoff_cell_TD_10km <- readRDS(file = file.path(filtered_data_path_L2, paste0("plant_", cutoff_obs, "_cell_TD_10km.rds")))
bird_cutoff_cell_TD_10km <- readRDS(file = file.path(filtered_data_path_L2, paste0("bird_", cutoff_obs, "_cell_TD_10km.rds")))


# 25km
mammal_cutoff_cell_TD_25km <- readRDS(file = file.path(filtered_data_path_L2, paste0("mammal_", cutoff_obs, "_cell_TD_25km.rds")))
plant_cutoff_cell_TD_25km <- readRDS(file = file.path(filtered_data_path_L2, paste0("plant_", cutoff_obs, "_cell_TD_25km.rds")))
bird_cutoff_cell_TD_25km <- readRDS(file = file.path(filtered_data_path_L2, paste0("bird_", cutoff_obs, "_cell_TD_25km.rds")))


# 50km
mammal_cutoff_cell_TD_50km <- readRDS(file = file.path(filtered_data_path_L2, paste0("mammal_", cutoff_obs, "_cell_TD_50km.rds")))
plant_cutoff_cell_TD_50km <- readRDS(file = file.path(filtered_data_path_L2, paste0("plant_", cutoff_obs, "_cell_TD_50km.rds")))
bird_cutoff_cell_TD_50km <- readRDS(file = file.path(filtered_data_path_L2, paste0("bird_", cutoff_obs, "_cell_TD_50km.rds")))


# 75km
mammal_cutoff_cell_TD_75km <- readRDS(file = file.path(filtered_data_path_L2, paste0("mammal_", cutoff_obs, "_cell_TD_75km.rds")))
plant_cutoff_cell_TD_75km <- readRDS(file = file.path(filtered_data_path_L2, paste0("plant_", cutoff_obs, "_cell_TD_75km.rds")))
bird_cutoff_cell_TD_75km <- readRDS(file = file.path(filtered_data_path_L2, paste0("bird_", cutoff_obs, "_cell_TD_75km.rds")))


# 100km
mammal_cutoff_cell_TD_100km <- readRDS(file = file.path(filtered_data_path_L2, paste0("mammal_", cutoff_obs, "_cell_TD_100km.rds")))
plant_cutoff_cell_TD_100km <- readRDS(file = file.path(filtered_data_path_L2, paste0("plant_", cutoff_obs, "_cell_TD_100km.rds")))
bird_cutoff_cell_TD_100km <- readRDS(file = file.path(filtered_data_path_L2, paste0("bird_", cutoff_obs, "_cell_TD_100km.rds")))


# Plant maps
(plant_cutoff_div_map_100km <- div_diff_map(plant_cutoff_cell_TD_100km, plant_cutoff_cell_FD_100km, 'plant'))
(plant_cutoff_div_map_75km <- div_diff_map(plant_cutoff_cell_TD_75km, plant_cutoff_cell_FD_75km, 'Plants'))
(plant_cutoff_div_map_50km <- div_diff_map(plant_cutoff_cell_TD_50km, plant_cutoff_cell_FD_50km, 'Plants'))
(plant_cutoff_div_map_25km <- div_diff_map(plant_cutoff_cell_TD_25km, plant_cutoff_cell_FD_25km, 'Plants'))
(plant_cutoff_div_map_10km <- div_diff_map(plant_cutoff_cell_TD_10km, plant_cutoff_cell_FD_10km, 'Plants'))
(plant_cutoff_div_map_5km <- div_diff_map(plant_cutoff_cell_TD_5km, plant_cutoff_cell_FD_5km, 'Plants'))


# Mammal maps
(mammal_cutoff_div_map_100km <- div_diff_map(mammal_cutoff_cell_TD_100km, mammal_cutoff_cell_FD_100km, 'mammal'))
(mammal_cutoff_div_map_75km <- div_diff_map(mammal_cutoff_cell_TD_75km, mammal_cutoff_cell_FD_75km, 'mammal'))
(mammal_cutoff_div_map_50km <- div_diff_map(mammal_cutoff_cell_TD_50km, mammal_cutoff_cell_FD_50km, 'mammal'))
(mammal_cutoff_div_map_25km <- div_diff_map(mammal_cutoff_cell_TD_25km, mammal_cutoff_cell_FD_25km, 'mammal'))
(mammal_cutoff_div_map_10km <- div_diff_map(mammal_cutoff_cell_TD_10km, mammal_cutoff_cell_FD_10km, 'mammal'))
(mammal_cutoff_div_map_5km <- div_diff_map(mammal_cutoff_cell_TD_5km, mammal_cutoff_cell_FD_5km, 'mammal'))


# Bird maps
(bird_cutoff_div_map_100km <- div_diff_map(bird_cutoff_cell_TD_100km, bird_cutoff_cell_FD_100km, 'bird'))
(bird_cutoff_div_map_75km <- div_diff_map(bird_cutoff_cell_TD_75km, bird_cutoff_cell_FD_75km, 'bird'))
(bird_cutoff_div_map_50km <- div_diff_map(bird_cutoff_cell_TD_50km, bird_cutoff_cell_FD_50km, 'bird'))
(bird_cutoff_div_map_25km <- div_diff_map(bird_cutoff_cell_TD_25km, bird_cutoff_cell_FD_25km, 'bird'))
(bird_cutoff_div_map_10km <- div_diff_map(bird_cutoff_cell_TD_10km, bird_cutoff_cell_FD_10km, 'bird'))
(bird_cutoff_div_map_5km <- div_diff_map(bird_cutoff_cell_TD_5km, bird_cutoff_cell_FD_5km, 'bird'))


# combine all plots 

# individual plot edits
mammal_cutoff_div_map_100km <- mammal_cutoff_div_map_100km + labs(title='[100km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20)) 

plant_cutoff_div_map_100km <- plant_cutoff_div_map_100km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_div_map_100km <- bird_cutoff_div_map_100km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


mammal_cutoff_div_map_75km <- mammal_cutoff_div_map_75km + labs(title='[75km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

plant_cutoff_div_map_75km <- plant_cutoff_div_map_75km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_div_map_75km <- bird_cutoff_div_map_75km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


mammal_cutoff_div_map_50km <- mammal_cutoff_div_map_50km + labs(title='[50km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

plant_cutoff_div_map_50km <- plant_cutoff_div_map_50km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_div_map_50km <- bird_cutoff_div_map_50km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


mammal_cutoff_div_map_25km <- mammal_cutoff_div_map_25km + labs(title='[25km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

plant_cutoff_div_map_25km <- plant_cutoff_div_map_25km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_div_map_25km <- bird_cutoff_div_map_25km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + labs(x = "Longitude")


mammal_cutoff_div_map_10km <- mammal_cutoff_div_map_10km + labs(title='[10km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

plant_cutoff_div_map_10km <- plant_cutoff_div_map_10km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_cutoff_div_map_10km <- bird_cutoff_div_map_10km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


# mammal picture
mammal <- pick_phylopic(name='Potos flavus', n=2, auto=2)

mammal_cutoff_div_map_5km <- mammal_cutoff_div_map_5km + labs(title='[5km]') + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20), axis.text = element_text(size=16)) + add_phylopic(img=mammal, x=-79, y=12, height=8)

# plant picture
plant <- pick_phylopic(name='Coffea alleizettei')

plant_cutoff_div_map_5km <- plant_cutoff_div_map_5km + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + labs(y = "Latitude") + add_phylopic(img=plant, x=-79, y=12, height=8)

# bird picture
bird <- pick_phylopic(name='Ramphastos sulfuratus', n=2, auto=1)

bird_cutoff_div_map_5km <- bird_cutoff_div_map_5km + annotation_scale(location = "bl",width_hint = 0.4, style = "bar") + annotation_north_arrow(location = "bl", which_north = "true", height = unit(0.5, "in"), width = unit(0.5, "in"), pad_x = unit(0.05, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) + theme(plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + add_phylopic(img=bird, x=-79, y=12, height=8)


# arrange
all_cutoff_div_plots <-  mammal_cutoff_div_map_5km + mammal_cutoff_div_map_10km + mammal_cutoff_div_map_25km + mammal_cutoff_div_map_50km + mammal_cutoff_div_map_75km + mammal_cutoff_div_map_100km + plant_cutoff_div_map_5km + plant_cutoff_div_map_10km + plant_cutoff_div_map_25km + plant_cutoff_div_map_50km + plant_cutoff_div_map_75km + plant_cutoff_div_map_100km + bird_cutoff_div_map_5km + bird_cutoff_div_map_10km + bird_cutoff_div_map_25km + bird_cutoff_div_map_50km + bird_cutoff_div_map_75km + bird_cutoff_div_map_100km + plot_layout(ncol = 6, nrow = 3, guides = 'collect', axis_titles = 'collect', heights = c(1, 1, 1)) & theme(legend.position = 'left', axis.title = element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=16)) & plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size=30, face='bold')))

all_cutoff_div_plots

ggsave(paste0('div_', cutoff_obs,'_comparison_maps.png'), all_div_plots, path = filtered_data_figure_path, width = 14, height = 11.8, units = "in", dpi=1000)

