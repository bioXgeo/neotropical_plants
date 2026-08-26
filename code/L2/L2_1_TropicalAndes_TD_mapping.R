#title: "Tropical Andes Taxonomic Diversity of Plants and Frugivores"
#author: "Hazel J. Anderson, Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske, Kelly Kaspar"
#overview: "Calculating and mapping taxonomic diversity as species richness using occurrence data for plants and frugivores in the Tropical Andes Moist Lowland and Montane forests. Note: some code is adapted from https://luisdva.github.io/rstats/richness/."
#data input: "Americas.rds", "TApoly.rds", "TropicalAndes_IUCNHabitat_Forest.rds",
#            "mammal_sp_grid_100km.rds", "mammal_sp_grid_75km.rds", "mammal_sp_grid_50km.rds", "mammal_sp_grid_25km.rds", "mammal_sp_grid_10km.rds", "mammal_sp_grid_5km.rds"
#            "plant_sp_grid_100km.rds", "plant_sp_grid_75km.rds", "plant_sp_grid_50km.rds", "plant_sp_grid_25km.rds", "plant_sp_grid_10km.rds", "plant_sp_grid_5km.rds"
#            "bird_sp_grid_100km.rds", "bird_sp_grid_75km.rds", "bird_sp_grid_50km.rds", "bird_sp_grid_25km.rds", "bird_sp_grid_10km.rds", "bird_sp_grid_5km.rds"
#data output: "mammal_TD_100km.rds", "mammal_TD_100km_Chao.rds", "mammal_TD_map_100km.rds", "mammal_cell_TD_100km.rds", "mammal_TD_plot_100km.png"
#             "mammal_TD_75km.rds", "mammal_TD_75km_Chao.rds", "mammal_TD_map_75km.rds", "mammal_cell_TD_75km.rds", "mammal_TD_plot_75km.png"
#             "mammal_TD_50km.rds", "mammal_TD_50km_Chao.rds", "mammal_TD_map_50km.rds", "mammal_cell_TD_50km.rds", "mammal_TD_plot_50km.png"
#             "mammal_TD_25km.rds", "mammal_TD_25km_Chao.rds", "mammal_TD_map_25km.rds", "mammal_cell_TD_25km.rds", "mammal_TD_plot_25km.png"
#             "mammal_TD_10km.rds", "mammal_TD_10km_Chao.rds", "mammal_TD_map_10km.rds", "mammal_cell_TD_10km.rds", "mammal_TD_plot_10km.png"
#             "mammal_TD_5km.rds", "mammal_TD_5km_Chao.rds", "mammal_TD_map_5km.rds", "mammal_cell_TD_5km.rds", "mammal_TD_plot_5km.png"
#             "plant_TD_100km.rds", "plant_TD_100km_Chao.rds", "plant_TD_map_100km.rds", "plant_cell_TD_100km.rds", "plant_TD_plot_100km.png"
#             "plant_TD_75km.rds", "plant_TD_75km_Chao.rds", "plant_TD_map_75km.rds", "plant_cell_TD_75km.rds", "plant_TD_plot_75km.png"
#             "plant_TD_50km.rds", "plant_TD_50km_Chao.rds", "plant_TD_map_50km.rds", "plant_cell_TD_50km.rds", "plant_TD_plot_50km.png"
#             "plant_TD_25km.rds", "plant_TD_25km_Chao.rds", "plant_TD_map_25km.rds", "plant_cell_TD_25km.rds", "plant_TD_plot_25km.png"
#             "plant_TD_10km.rds", "plant_TD_10km_Chao.rds", "plant_TD_map_10km.rds", "plant_cell_TD_10km.rds", "plant_TD_plot_10km.png"
#             "plant_TD_5km.rds", "plant_TD_5km_Chao.rds", "plant_TD_map_5km.rds", "plant_cell_TD_5km.rds", "plant_TD_plot_5km.png"
#             "bird_TD_100km.rds", "bird_TD_100km_Chao.rds", "bird_TD_map_100km.rds", "bird_cell_TD_100km.rds", "bird_TD_plot_100km.png"
#             "bird_TD_75km.rds", "bird_TD_75km_Chao.rds", "bird_TD_map_75km.rds", "bird_cell_TD_75km.rds", "bird_TD_plot_75km.png"
#             "bird_TD_50km.rds", "bird_TD_50km_Chao.rds", "bird_TD_map_50km.rds", "bird_cell_TD_50km.rds", "bird_TD_plot_50km.png"
#             "bird_TD_25km.rds", "bird_TD_25km_Chao.rds", "bird_TD_map_25km.rds", "bird_cell_TD_25km.rds", "bird_TD_plot_25km.png"
#             "bird_TD_10km.rds", "bird_TD_10km_Chao.rds", "bird_TD_map_10km.rds", "bird_cell_TD_10km.rds", "bird_TD_plot_10km.png"
#             "bird_TD_5km.rds", "bird_TD_5km_Chao.rds", "bird_TD_map_5km.rds", "bird_cell_TD_5km.rds", "bird_TD_plot_5km.png"
#             "all_richness_plots.png"
#             "mammal_20_TD_100km.rds", "mammal_20_TD_100km_Chao.rds", "mammal_20_TD_map_100km.rds", "mammal_20_cell_TD_100km.rds", "mammal_20_TD_plot_100km.png"
#             "mammal_20_TD_75km.rds", "mammal_20_TD_75km_Chao.rds", "mammal_20_TD_map_75km.rds", "mammal_20_cell_TD_75km.rds", "mammal_20_TD_plot_75km.png"
#             "mammal_20_TD_50km.rds", "mammal_20_TD_50km_Chao.rds", "mammal_20_TD_map_50km.rds", "mammal_20_cell_TD_50km.rds", "mammal_20_TD_plot_50km.png"
#             "mammal_20_TD_25km.rds", "mammal_20_TD_25km_Chao.rds", "mammal_20_TD_map_25km.rds", "mammal_20_cell_TD_25km.rds", "mammal_20_TD_plot_25km.png"
#             "mammal_20_TD_10km.rds", "mammal_20_TD_10km_Chao.rds", "mammal_20_TD_map_10km.rds", "mammal_20_cell_TD_10km.rds", "mammal_20_TD_plot_10km.png"
#             "mammal_20_TD_5km.rds", "mammal_20_TD_5km_Chao.rds", "mammal_20_TD_map_5km.rds", "mammal_20_cell_TD_5km.rds", "mammal_20_TD_plot_5km.png"
#             "plant_20_TD_100km.rds", "plant_20_TD_100km_Chao.rds", "plant_20_TD_map_100km.rds", "plant_20_cell_TD_100km.rds", "plant_20_TD_plot_100km.png"
#             "plant_20_TD_75km.rds", "plant_20_TD_75km_Chao.rds", "plant_20_TD_map_75km.rds", "plant_20_cell_TD_75km.rds", "plant_20_TD_plot_75km.png"
#             "plant_20_TD_50km.rds", "plant_20_TD_50km_Chao.rds", "plant_20_TD_map_50km.rds", "plant_20_cell_TD_50km.rds", "plant_20_TD_plot_50km.png"
#             "plant_20_TD_25km.rds", "plant_20_TD_25km_Chao.rds", "plant_20_TD_map_25km.rds", "plant_20_cell_TD_25km.rds", "plant_20_TD_plot_25km.png"
#             "plant_20_TD_10km.rds", "plant_20_TD_10km_Chao.rds", "plant_20_TD_map_10km.rds", "plant_20_cell_TD_10km.rds", "plant_20_TD_plot_10km.png"
#             "plant_20_TD_5km.rds", "plant_20_TD_5km_Chao.rds", "plant_20_TD_map_5km.rds", "plant_20_cell_TD_5km.rds", "plant_20_TD_plot_5km.png"
#             "bird_20_TD_100km.rds", "bird_20_TD_100km_Chao.rds", "bird_20_TD_map_100km.rds", "bird_20_cell_TD_100km.rds", "bird_20_TD_plot_100km.png"
#             "bird_20_TD_75km.rds", "bird_20_TD_75km_Chao.rds", "bird_20_TD_map_75km.rds", "bird_20_cell_TD_75km.rds", "bird_20_TD_plot_75km.png"
#             "bird_20_TD_50km.rds", "bird_20_TD_50km_Chao.rds", "bird_20_TD_map_50km.rds", "bird_20_cell_TD_50km.rds", "bird_20_TD_plot_50km.png"
#             "bird_20_TD_25km.rds", "bird_20_TD_25km_Chao.rds", "bird_20_TD_map_25km.rds", "bird_20_cell_TD_25km.rds", "bird_20_TD_plot_25km.png"
#             "bird_20_TD_10km.rds", "bird_20_TD_10km_Chao.rds", "bird_20_TD_map_10km.rds", "bird_20_cell_TD_10km.rds", "bird_20_TD_plot_10km.png"
#             "bird_20_TD_5km.rds", "bird_20_TD_5km_Chao.rds", "bird_20_TD_map_5km.rds", "bird_20_cell_TD_5km.rds", "bird_20_TD_plot_5km.png"
#             "all_richness_20_plots.png"

#date: "2023-08-01; 2025-10-27"
#notes: JB used HPCC


# load required packages
library(sf); library(dplyr); library(ggplot2); library(parallel); library(foreach); library(doParallel); library(ggspatial); library(ggpubr); library(patchwork); library(rphylopic); library(iNEXT); library(stringr); library(vegan)

# set file path
data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')

# load functions
source("C:/GitHub_projects/neotropical_plants/code/Functions.R")


# read in data

# projected sf objects
Americas <- readRDS(file = file.path(data_path_L1, "Americas.rds"))
TApoly <- readRDS(file = file.path(data_path_L1,"TApoly.rds"))
TropicalAndes_IUCNHabitat_Forest <- readRDS(file = file.path(data_path_L1,"TropicalAndes_IUCNHabitat_Forest.rds"))


#### TD of data filtered by 1970 ####

# set file paths 
all_data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1/all_data')
all_output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2/all_data')
all_data_figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures/all_data')


# mammals
#### 100 km ####

# species occurrence data
mammal_sp_grid_100km <- readRDS(file.path(all_data_path_L1,"mammal_sp_grid_100km.rds"))

# richness calculation
mammal_TD_100km <- calculate_richness(mammal_sp_grid_100km)
saveRDS(mammal_TD_100km, file = file.path(all_output_path_L2,"mammal_TD_100km.rds"))
mammal_TD_100km <- readRDS(file.path(all_output_path_L2,"mammal_TD_100km.rds"))

# after assessing relationships between sample coverage and richness using all richness estimates, we decided to eliminate data with coverage <0.3 and use the Chao1 estimator
mammal_TD_100km <- mammal_TD_100km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage,) |> 
  select(cellid, richness)
saveRDS(mammal_TD_100km, file = file.path(all_output_path_L2,"mammal_TD_100km_Chao.rds"))
mammal_TD_100km <- readRDS(file.path(all_output_path_L2,"mammal_TD_100km_Chao.rds"))

# mapping

# set limits for all mammal maps based off of 100 km
lims <- c(0, max(mammal_TD_100km$richness))
mpt <- max(mammal_TD_100km$richness)/2

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
mammal_TD_75km <- calculate_richness(mammal_sp_grid_75km)
saveRDS(mammal_TD_75km, file = file.path(all_output_path_L2,"mammal_TD_75km.rds"))
mammal_TD_75km <- readRDS(file.path(all_output_path_L2,"mammal_TD_75km.rds"))

# filter data as mentioned on line 49
mammal_TD_75km <- mammal_TD_75km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1,
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(mammal_TD_75km, file = file.path(all_output_path_L2,"mammal_TD_75km_Chao.rds"))
mammal_TD_75km <- readRDS(file.path(all_output_path_L2,"mammal_TD_75km_Chao.rds"))

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
mammal_TD_50km <- calculate_richness(mammal_sp_grid_50km)
saveRDS(mammal_TD_50km, file = file.path(all_output_path_L2,"mammal_TD_50km.rds"))
mammal_TD_50km <- readRDS(file.path(all_output_path_L2,"mammal_TD_50km.rds"))

# filter data as mentioned on line 49
mammal_TD_50km <- mammal_TD_50km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(mammal_TD_50km, file = file.path(all_output_path_L2,"mammal_TD_50km_Chao.rds"))
mammal_TD_50km <- readRDS(file.path(all_output_path_L2,"mammal_TD_50km_Chao.rds"))

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
mammal_TD_25km <- calculate_richness(mammal_sp_grid_25km)
saveRDS(mammal_TD_25km, file = file.path(all_output_path_L2,"mammal_TD_25km.rds"))
mammal_TD_25km <- readRDS(file.path(all_output_path_L2,"mammal_TD_25km.rds"))

# filter data as mentioned on line 49
mammal_TD_25km <- mammal_TD_25km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(mammal_TD_25km, file = file.path(all_output_path_L2,"mammal_TD_25km_Chao.rds"))
mammal_TD_25km <- readRDS(file.path(all_output_path_L2,"mammal_TD_25km_Chao.rds"))

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
mammal_TD_10km <- calculate_richness(mammal_sp_grid_10km)
saveRDS(mammal_TD_10km, file = file.path(all_output_path_L2,"mammal_TD_10km.rds"))
mammal_TD_10km <- readRDS(file.path(all_output_path_L2,"mammal_TD_10km.rds"))

# filter data as mentioned on line 49
mammal_TD_10km <- mammal_TD_10km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(mammal_TD_10km, file = file.path(all_output_path_L2,"mammal_TD_10km_Chao.rds"))
mammal_TD_10km <- readRDS(file.path(all_output_path_L2,"mammal_TD_10km_Chao.rds"))

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
mammal_TD_5km <- calculate_richness(mammal_sp_grid_5km)
saveRDS(mammal_TD_5km, file = file.path(all_output_path_L2,"mammal_TD_5km.rds"))
mammal_TD_5km <- readRDS(file.path(all_output_path_L2,"mammal_TD_5km.rds"))

ggplot(mammal_TD_5km, aes(x = SC, y = S.obs))+
  geom_point(alpha = 0.5) +
  theme_classic()

mammal_plot_5km <- mammal_TD_5km |>
  select(SC, richness_Chao1,
         richness_coverage_0.4,
         richness_coverage_0.5,
         richness_coverage_0.6) |>
  pivot_longer(
    cols = -SC,
    names_to = "method",
    values_to = "richness"
  )

ggplot(mammal_plot_5km, aes(x = SC, y = richness)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~method, scales = "free_y") +
  theme_classic() +
  labs(
    x = "Sample coverage",
    y = "Estimated species richness"
  )

ggplot(mammal_plot_5km[mammal_plot_5km$SC >= 0.3, ], aes(x = SC, y = richness)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~method, scales = "free_y") +
  theme_classic() +
  labs(
    x = "Sample coverage",
    y = "Estimated species richness"
  )

ggplot(mammal_TD_5km,
       aes(x = SC,
           y = richness_coverage_0.4 - S.obs)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_classic() +
  labs(
    x = "Sample coverage",
    y = "Estimated - observed richness"
  )

ggplot(mammal_TD_5km,
       aes(x = SC,
           y = richness_coverage_0.6 - S.obs)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_classic() +
  labs(
    x = "Sample coverage",
    y = "Estimated - observed richness"
  )

ggplot(mammal_TD_5km,
       aes(x = SC,
           y = richness_Chao1 - S.obs)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  theme_classic() +
  labs(
    x = "Sample coverage",
    y = "Estimated - observed richness"
  )

# filter data as mentioned on line 49
mammal_TD_5km <- mammal_TD_5km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(mammal_TD_5km, file = file.path(all_output_path_L2,"mammal_TD_5km_Chao.rds"))
mammal_TD_5km <- readRDS(file.path(all_output_path_L2,"mammal_TD_5km_Chao.rds"))

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
plant_TD_100km <- calculate_richness(plant_sp_grid_100km)
saveRDS(plant_TD_100km, file = file.path(all_output_path_L2,"plant_TD_100km.rds"))
plant_TD_100km <- readRDS(file.path(all_output_path_L2,"plant_TD_100km.rds"))

# filter data as mentioned on line 49
plant_TD_100km <- plant_TD_100km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(plant_TD_100km, file = file.path(all_output_path_L2,"plant_TD_100km_Chao.rds"))
plant_TD_100km <- readRDS(file.path(all_output_path_L2,"plant_TD_100km_Chao.rds"))

# mapping

# set limits for all plant maps based off of 100 km
lims <- c(0, max(plant_TD_100km$richness))
mpt <- max(plant_TD_100km$richness)/2

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
plant_TD_75km <- calculate_richness(plant_sp_grid_75km)
saveRDS(plant_TD_75km, file = file.path(all_output_path_L2,"plant_TD_75km.rds"))
plant_TD_75km <- readRDS(file.path(all_output_path_L2,"plant_TD_75km.rds"))

# filter data as mentioned on line 49
plant_TD_75km <- plant_TD_75km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(plant_TD_75km, file = file.path(all_output_path_L2,"plant_TD_75km_Chao.rds"))
plant_TD_75km <- readRDS(file.path(all_output_path_L2,"plant_TD_75km_Chao.rds"))

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
plant_TD_50km <- calculate_richness(plant_sp_grid_50km)
saveRDS(plant_TD_50km, file = file.path(all_output_path_L2,"plant_TD_50km.rds"))
plant_TD_50km <- readRDS(file.path(all_output_path_L2,"plant_TD_50km.rds"))

# filter data as mentioned on line 49
plant_TD_50km <- plant_TD_50km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(plant_TD_50km, file = file.path(all_output_path_L2,"plant_TD_50km_Chao.rds"))
plant_TD_50km <- readRDS(file.path(all_output_path_L2,"plant_TD_50km_Chao.rds"))

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
plant_TD_25km <- calculate_richness(plant_sp_grid_25km)
saveRDS(plant_TD_25km, file = file.path(all_output_path_L2,"plant_TD_25km.rds"))
plant_TD_25km <- readRDS(file.path(all_output_path_L2,"plant_TD_25km.rds"))

# filter data as mentioned on line 49
plant_TD_25km <- plant_TD_25km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(plant_TD_25km, file = file.path(all_output_path_L2,"plant_TD_25km_Chao.rds"))
plant_TD_25km <- readRDS(file.path(all_output_path_L2,"plant_TD_25km_Chao.rds"))

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
plant_TD_10km <- calculate_richness(plant_sp_grid_10km)
saveRDS(plant_TD_10km, file = file.path(all_output_path_L2,"plant_TD_10km.rds"))
plant_TD_10km <- readRDS(file.path(all_output_path_L2,"plant_TD_10km.rds"))

# filter data as mentioned on line 49
plant_TD_10km <- plant_TD_10km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(plant_TD_10km, file = file.path(all_output_path_L2,"plant_TD_10km_Chao.rds"))
plant_TD_10km <- readRDS(file.path(all_output_path_L2,"plant_TD_10km_Chao.rds"))

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
plant_TD_5km <- calculate_richness(plant_sp_grid_5km)
saveRDS(plant_TD_5km, file = file.path(all_output_path_L2,"plant_TD_5km.rds"))
plant_TD_5km <- readRDS(file.path(all_output_path_L2,"plant_TD_5km.rds"))

ggplot(plant_TD_5km, aes(x = SC, y = S.obs))+
  geom_point(alpha = 0.5) +
  theme_classic()

plant_plot <- plant_TD_5km |>
  select(SC, richness_Chao1,
         richness_coverage_0.4,
         richness_coverage_0.5,
         richness_coverage_0.6) |>
  pivot_longer(
    cols = -SC,
    names_to = "method",
    values_to = "richness"
  )

ggplot(plant_plot, aes(x = SC, y = richness)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~method, scales = "free_y") +
  theme_classic() +
  labs(
    x = "Sample coverage",
    y = "Estimated species richness"
  )

ggplot(plant_plot[plant_plot$SC >= 0.3, ], aes(x = SC, y = richness)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~method, scales = "free_y") +
  theme_classic() +
  labs(
    x = "Sample coverage",
    y = "Estimated species richness"
  )

# filter data as mentioned on line 49
plant_TD_5km <- plant_TD_5km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(plant_TD_5km, file = file.path(all_output_path_L2,"plant_TD_5km_Chao.rds"))
plant_TD_5km <- readRDS(file.path(all_output_path_L2,"plant_TD_5km_Chao.rds"))

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
bird_TD_100km <- calculate_richness(bird_sp_grid_100km)
saveRDS(bird_TD_100km, file = file.path(all_output_path_L2,"bird_TD_100km.rds"))
bird_TD_100km <- readRDS(file.path(all_output_path_L2,"bird_TD_100km.rds"))

# filter data as mentioned on line 49
bird_TD_100km <- bird_TD_100km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(bird_TD_100km, file = file.path(all_output_path_L2,"bird_TD_100km_Chao.rds"))
bird_TD_100km <- readRDS(file.path(all_output_path_L2,"bird_TD_100km_Chao.rds"))

# mapping

# set limits for all bird maps based off of 100 km
lims <- c(0, max(bird_TD_100km$richness))
mpt <- max(bird_TD_100km$richness)/2

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
bird_TD_75km <- calculate_richness(bird_sp_grid_75km)
saveRDS(bird_TD_75km, file = file.path(all_output_path_L2,"bird_TD_75km.rds"))
bird_TD_75km <- readRDS(file.path(all_output_path_L2,"bird_TD_75km.rds"))

# filter data as mentioned on line 49
bird_TD_75km <- bird_TD_75km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(bird_TD_75km, file = file.path(all_output_path_L2,"bird_TD_75km_Chao.rds"))
bird_TD_75km <- readRDS(file.path(all_output_path_L2,"bird_TD_75km_Chao.rds"))

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
bird_TD_50km <- calculate_richness(bird_sp_grid_50km)
saveRDS(bird_TD_50km, file = file.path(all_output_path_L2,"bird_TD_50km.rds"))
bird_TD_50km <- readRDS(file.path(all_output_path_L2,"bird_TD_50km.rds"))

# filter data as mentioned on line 49
bird_TD_50km <- bird_TD_50km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(bird_TD_50km, file = file.path(all_output_path_L2,"bird_TD_50km_Chao.rds"))
bird_TD_50km <- readRDS(file.path(all_output_path_L2,"bird_TD_50km_Chao.rds"))

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
bird_TD_25km <- calculate_richness(bird_sp_grid_25km)
saveRDS(bird_TD_25km, file = file.path(all_output_path_L2,"bird_TD_25km.rds"))
bird_TD_25km <- readRDS(file.path(all_output_path_L2,"bird_TD_25km.rds"))

# filter data as mentioned on line 49
bird_TD_25km <- bird_TD_25km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(bird_TD_25km, file = file.path(all_output_path_L2,"bird_TD_25km_Chao.rds"))
bird_TD_25km <- readRDS(file.path(all_output_path_L2,"bird_TD_25km_Chao.rds"))

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
bird_TD_10km <- calculate_richness(bird_sp_grid_10km)
saveRDS(bird_TD_10km, file = file.path(all_output_path_L2,"bird_TD_10km.rds"))
bird_TD_10km <- readRDS(file.path(all_output_path_L2,"bird_TD_10km.rds"))

# filter data as mentioned on line 49
bird_TD_10km <- bird_TD_10km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(bird_TD_10km, file = file.path(all_output_path_L2,"bird_TD_10km_Chao.rds"))
bird_TD_10km <- readRDS(file.path(all_output_path_L2,"bird_TD_10km_Chao.rds"))

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
bird_TD_5km <- calculate_richness(bird_sp_grid_5km)
saveRDS(bird_TD_5km, file = file.path(all_output_path_L2,"bird_TD_5km.rds"))
bird_TD_5km <- readRDS(file.path(all_output_path_L2,"bird_TD_5km.rds"))

ggplot(bird_TD_5km, aes(x = SC, y = S.obs))+
  geom_point(alpha = 0.5) +
  theme_classic()

bird_plot <- bird_TD_5km |>
  select(SC, richness_Chao1,
         richness_coverage_0.4,
         richness_coverage_0.5,
         richness_coverage_0.6) |>
  pivot_longer(
    cols = -SC,
    names_to = "method",
    values_to = "richness"
  )

ggplot(bird_plot, aes(x = SC, y = richness)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~method, scales = "free_y") +
  theme_classic() +
  labs(
    x = "Sample coverage",
    y = "Estimated species richness"
  )

ggplot(bird_plot[bird_plot$SC >= 0.3, ], aes(x = SC, y = richness)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~method, scales = "free_y") +
  theme_classic() +
  labs(
    x = "Sample coverage",
    y = "Estimated species richness"
  )

# filter data as mentioned on line 49
bird_TD_5km <- bird_TD_5km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(bird_TD_5km, file = file.path(all_output_path_L2,"bird_TD_5km_Chao.rds"))
bird_TD_5km <- readRDS(file.path(all_output_path_L2,"bird_TD_5km_Chao.rds"))

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
mammal_cutoff_TD_100km <- calculate_richness(mammal_cutoff_sp_grid_100km)
saveRDS(mammal_cutoff_TD_100km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_100km.rds")))
mammal_cutoff_TD_100km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_100km.rds")))

# filter data as mentioned on line 49
mammal_cutoff_TD_100km <- mammal_cutoff_TD_100km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(mammal_cutoff_TD_100km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_100km_Chao.rds")))
mammal_cutoff_TD_100km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_100km_Chao.rds")))

# mapping

# set limits for all mammal maps based off of 100 km
lims <- c(0, max(mammal_cutoff_TD_100km$richness))
mpt <- max(mammal_cutoff_TD_100km$richness)/2

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
mammal_cutoff_TD_75km <- calculate_richness(mammal_cutoff_sp_grid_75km)
saveRDS(mammal_cutoff_TD_75km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_75km.rds")))
mammal_cutoff_TD_75km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_75km.rds")))

# filter data as mentioned on line 49
mammal_cutoff_TD_75km <- mammal_cutoff_TD_75km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(mammal_cutoff_TD_75km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_75km_Chao.rds")))
mammal_cutoff_TD_75km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_75km_Chao.rds")))

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
mammal_cutoff_TD_50km <- calculate_richness(mammal_cutoff_sp_grid_50km)
saveRDS(mammal_cutoff_TD_50km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_50km.rds")))
mammal_cutoff_TD_50km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_50km.rds")))

# filter data as mentioned on line 49
mammal_cutoff_TD_50km <- mammal_cutoff_TD_50km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(mammal_cutoff_TD_50km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_50km_Chao.rds")))
mammal_cutoff_TD_50km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_50km_Chao.rds")))

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
mammal_cutoff_TD_25km <- calculate_richness(mammal_cutoff_sp_grid_25km)
saveRDS(mammal_cutoff_TD_25km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_25km.rds")))
mammal_cutoff_TD_25km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_25km.rds")))

# filter data as mentioned on line 49
mammal_cutoff_TD_25km <- mammal_cutoff_TD_25km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(mammal_cutoff_TD_25km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_25km_Chao.rds")))
mammal_cutoff_TD_25km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_25km_Chao.rds")))

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
mammal_cutoff_TD_10km <- calculate_richness(mammal_cutoff_sp_grid_10km)
saveRDS(mammal_cutoff_TD_10km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_10km.rds")))
mammal_cutoff_TD_10km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_10km.rds")))

# filter data as mentioned on line 49
mammal_cutoff_TD_10km <- mammal_cutoff_TD_10km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(mammal_cutoff_TD_10km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_10km_Chao.rds")))
mammal_cutoff_TD_10km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_10km_Chao.rds")))

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
mammal_cutoff_TD_5km <- calculate_richness(mammal_cutoff_sp_grid_5km)
saveRDS(mammal_cutoff_TD_5km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_5km.rds")))
mammal_cutoff_TD_5km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_5km.rds")))

# filter data as mentioned on line 49
mammal_cutoff_TD_5km <- mammal_cutoff_TD_5km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(mammal_cutoff_TD_5km, file = file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_5km_Chao.rds")))
mammal_cutoff_TD_5km <- readRDS(file.path(filtered_output_path_L2, paste0("mammal_", cutoff_obs, "_TD_5km_Chao.rds")))

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
plant_cutoff_TD_100km <- calculate_richness(plant_cutoff_sp_grid_100km)
saveRDS(plant_cutoff_TD_100km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_100km.rds")))
plant_cutoff_TD_100km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_100km.rds")))

# filter data as mentioned on line 49
plant_cutoff_TD_100km <- plant_cutoff_TD_100km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(plant_cutoff_TD_100km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_100km_Chao.rds")))
plant_cutoff_TD_100km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_100km_Chao.rds")))

# mapping

# set limits for all plant maps based off of 100 km
lims <- c(0, max(plant_cutoff_TD_100km$richness))
mpt <- max(plant_cutoff_TD_100km$richness)/2

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
plant_cutoff_TD_75km <- calculate_richness(plant_cutoff_sp_grid_75km)
saveRDS(plant_cutoff_TD_75km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_75km.rds")))
plant_cutoff_TD_75km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_75km.rds")))

# filter data as mentioned on line 49
plant_cutoff_TD_75km <- plant_cutoff_TD_75km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(plant_cutoff_TD_75km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_75km_Chao.rds")))
plant_cutoff_TD_75km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_75km_Chao.rds")))

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
plant_cutoff_TD_50km <- calculate_richness(plant_cutoff_sp_grid_50km)
saveRDS(plant_cutoff_TD_50km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_50km.rds")))
plant_cutoff_TD_50km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_50km.rds")))

# filter data as mentioned on line 49
plant_cutoff_TD_50km <- plant_cutoff_TD_50km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(plant_cutoff_TD_50km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_50km_Chao.rds")))
plant_cutoff_TD_50km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_50km_Chao.rds")))

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
plant_cutoff_TD_25km <- calculate_richness(plant_cutoff_sp_grid_25km)
saveRDS(plant_cutoff_TD_25km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_25km.rds")))
plant_cutoff_TD_25km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_25km.rds")))

# filter data as mentioned on line 49
plant_cutoff_TD_25km <- plant_cutoff_TD_25km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(plant_cutoff_TD_25km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_25km_Chao.rds")))
plant_cutoff_TD_25km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_25km_Chao.rds")))

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
plant_cutoff_TD_10km <- calculate_richness(plant_cutoff_sp_grid_10km)
saveRDS(plant_cutoff_TD_10km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_10km.rds")))
plant_cutoff_TD_10km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_10km.rds")))

# filter data as mentioned on line 49
plant_cutoff_TD_10km <- plant_cutoff_TD_10km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(plant_cutoff_TD_10km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_10km_Chao.rds")))
plant_cutoff_TD_10km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_10km_Chao.rds")))

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
plant_cutoff_TD_5km <- calculate_richness(plant_cutoff_sp_grid_5km)
saveRDS(plant_cutoff_TD_5km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_5km.rds")))
plant_cutoff_TD_5km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_5km.rds")))

# filter data as mentioned on line 49
plant_cutoff_TD_5km <- plant_cutoff_TD_5km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(plant_cutoff_TD_5km, file = file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_5km_Chao.rds")))
plant_cutoff_TD_5km <- readRDS(file.path(filtered_output_path_L2, paste0("plant_", cutoff_obs, "_TD_5km_Chao.rds")))

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
bird_cutoff_TD_100km <- calculate_richness(bird_cutoff_sp_grid_100km)
saveRDS(bird_cutoff_TD_100km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_100km.rds")))
bird_cutoff_TD_100km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_100km.rds")))

# filter data as mentioned on line 49
bird_cutoff_TD_100km <- bird_cutoff_TD_100km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(bird_cutoff_TD_100km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_100km_Chao.rds")))
bird_cutoff_TD_100km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_100km_Chao.rds")))

# mapping

# set limits for all bird maps based off of 100 km
lims <- c(0, max(bird_cutoff_TD_100km$richness))
mpt <- max(bird_cutoff_TD_100km$richness)/2

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
bird_cutoff_TD_75km <- calculate_richness(bird_cutoff_sp_grid_75km)
saveRDS(bird_cutoff_TD_75km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_75km.rds")))
bird_cutoff_TD_75km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_75km.rds")))

# filter data as mentioned on line 49
bird_cutoff_TD_75km <- bird_cutoff_TD_75km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(bird_cutoff_TD_75km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_75km_Chao.rds")))
bird_cutoff_TD_75km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_75km_Chao.rds")))

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
bird_cutoff_TD_50km <- calculate_richness(bird_cutoff_sp_grid_50km)
saveRDS(bird_cutoff_TD_50km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_50km.rds")))
bird_cutoff_TD_50km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_50km.rds")))

# filter data as mentioned on line 49
bird_cutoff_TD_50km <- bird_cutoff_TD_50km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(bird_cutoff_TD_50km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_50km_Chao.rds")))
bird_cutoff_TD_50km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_50km_Chao.rds")))

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
bird_cutoff_TD_25km <- calculate_richness(bird_cutoff_sp_grid_25km)
saveRDS(bird_cutoff_TD_25km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_25km.rds")))
bird_cutoff_TD_25km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_25km.rds")))

# filter data as mentioned on line 49
bird_cutoff_TD_25km <- bird_cutoff_TD_25km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(bird_cutoff_TD_25km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_25km_Chao.rds")))
bird_cutoff_TD_25km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_25km_Chao.rds")))

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
bird_cutoff_TD_10km <- calculate_richness(bird_cutoff_sp_grid_10km)
saveRDS(bird_cutoff_TD_10km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_10km.rds")))
bird_cutoff_TD_10km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_10km.rds")))

# filter data as mentioned on line 49
bird_cutoff_TD_10km <- bird_cutoff_TD_10km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(bird_cutoff_TD_10km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_10km_Chao.rds")))
bird_cutoff_TD_10km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_10km_Chao.rds")))

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
bird_cutoff_TD_5km <- calculate_richness(bird_cutoff_sp_grid_5km)
saveRDS(bird_cutoff_TD_5km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_5km.rds")))
bird_cutoff_TD_5km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_5km.rds")))

# filter data as mentioned on line 49
bird_cutoff_TD_5km <- bird_cutoff_TD_5km |> 
  filter(SC >= 0.3) |> 
  rename(richness = richness_Chao1, 
         cellid = Assemblage) |> 
  select(cellid, richness)
saveRDS(bird_cutoff_TD_5km, file = file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_5km_Chao.rds")))
bird_cutoff_TD_5km <- readRDS(file.path(filtered_output_path_L2, paste0("bird_", cutoff_obs, "_TD_5km_Chao.rds")))

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
