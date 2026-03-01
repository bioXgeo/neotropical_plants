#title: "Taxonomic diversity relationships for plants, mammals, and birds"
#author: "Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Hazel J. Anderson, Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "This script compares taxonomic diversity relationships for plants and frugivores for various spatial grains."
#data input: "TropicalAndes_plantRichness_5km.csv", "TropicalAndes_frugivoreRichness_5km.csv", "TropicalAndes_mammalRichness_5km.csv", "TropicalAndes_birdRichness_5km.csv", "TropicalAndes_plantRichness_10km.csv", "TropicalAndes_frugivoreRichness_10km.csv", "TropicalAndes_mammalRichness_10km.csv", "TropicalAndes_birdRichness_10km.csv", "TropicalAndes_plantRichness_25km.csv", "TropicalAndes_frugivoreRichness_25km.csv", "TropicalAndes_mammalRichness_25km.csv", "TropicalAndes_birdRichness_25km.csv", "TropicalAndes_plantRichness_50km.csv", "TropicalAndes_frugivoreRichness_50km.csv", "TropicalAndes_mammalRichness_50km.csv", "TropicalAndes_birdRichness_50km.csv", "TropicalAndes_plantRichness_75km.csv", "TropicalAndes_frugivoreRichness_75km.csv", "TropicalAndes_mammalRichness_75km.csv", "TropicalAndes_birdRichness_75km.csv", "TropicalAndes_plantRichness_100km.csv", "TropicalAndes_frugivoreRichness_100km.csv", "TropicalAndes_mammalRichness_100km.csv", "TropicalAndes_birdRichness_100km.csv"
#data output: "compare_BPrichness_100km.rds", "compare_MPrichness_100km.rds", "compare_BPrichness_75km.rds", "compare_MPrichness_75km.rds", "compare_BPrichness_50km.rds", "compare_MPrichness_50km.rds", "compare_BPrichness_25km.rds", "compare_MPrichness_25km.rds", "compare_BPrichness_10km.rds", "compare_MPrichness_10km.rds", "compare_BPrichness_5km.rds", "compare_MPrichness_5km.rds", "all_taxa_richness_plots.png"
#date: "2024-05-16; 2025-12-15"
#notes: JB used HPCC


# set file paths
data_path_L2 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')

# #HPCC
# data_path_L2 <- file.path('/mnt/research/nasabio/data_2025/plants/L2')
# output_path_L2 <- file.path('/mnt/research/nasabio/data_2025/plants/L2')
# figure_path <- file.path('/mnt/research/nasabio/data_2025/plants/figures')


# load required packages
library(dplyr); library(ggplot2); library(smoothr); library(purrr); library(ggtrendline); library(ggpubr); library(tidyr); library(patchwork)

# load functions
source("C:/GitHub_projects/plant-frugivore diversity/neotropical_plants/code/Functions.R")

# # HPCC
# source("/mnt/ffs24/home/baljunas/Documents/neotropical_plants/code/Functions.R")


# read in data

# 5km
plant_cellRichness_5km <- read.csv(file.path(data_path_L2,"TropicalAndes_plantRichness_5km.csv"))
frugivore_cellRichness_5km <- read.csv(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_5km.csv"))
mammal_cellRichness_5km <- read.csv(file.path(data_path_L2,"TropicalAndes_mammalRichness_5km.csv"))
bird_cellRichness_5km <- read.csv(file.path(data_path_L2,"TropicalAndes_birdRichness_5km.csv"))


# 10km 
plant_cellRichness_10km <- read.csv(file.path(data_path_L2,"TropicalAndes_plantRichness_10km.csv"))
frugivore_cellRichness_10km <- read.csv(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_10km.csv"))
mammal_cellRichness_10km <- read.csv(file.path(data_path_L2,"TropicalAndes_mammalRichness_10km.csv"))
bird_cellRichness_10km <- read.csv(file.path(data_path_L2,"TropicalAndes_birdRichness_10km.csv"))


# 25km
plant_cellRichness_25km <- read.csv(file.path(data_path_L2,"TropicalAndes_plantRichness_25km.csv"))
frugivore_cellRichness_25km <- read.csv(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_25km.csv"))
mammal_cellRichness_25km <- read.csv(file.path(data_path_L2,"TropicalAndes_mammalRichness_25km.csv"))
bird_cellRichness_25km <- read.csv(file.path(data_path_L2,"TropicalAndes_birdRichness_25km.csv"))


# 50km
plant_cellRichness_50km <- read.csv(file.path(data_path_L2,"TropicalAndes_plantRichness_50km.csv"))
frugivore_cellRichness_50km <- read.csv(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_50km.csv"))
mammal_cellRichness_50km <- read.csv(file.path(data_path_L2,"TropicalAndes_mammalRichness_50km.csv"))
bird_cellRichness_50km <- read.csv(file.path(data_path_L2,"TropicalAndes_birdRichness_50km.csv"))


# 75km
plant_cellRichness_75km <- read.csv(file.path(data_path_L2,"TropicalAndes_plantRichness_75km.csv"))
frugivore_cellRichness_75km <- read.csv(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_75km.csv"))
mammal_cellRichness_75km <- read.csv(file.path(data_path_L2,"TropicalAndes_mammalRichness_75km.csv"))
bird_cellRichness_75km <- read.csv(file.path(data_path_L2,"TropicalAndes_birdRichness_75km.csv"))


# 100km
plant_cellRichness_100km <- read.csv(file.path(data_path_L2,"TropicalAndes_plantRichness_100km.csv"))
frugivore_cellRichness_100km <- read.csv(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_100km.csv"))
mammal_cellRichness_100km <- read.csv(file.path(data_path_L2,"TropicalAndes_mammalRichness_100km.csv"))
bird_cellRichness_100km <- read.csv(file.path(data_path_L2,"TropicalAndes_birdRichness_100km.csv"))


#### comparison of plant-frugivore richness ####

# 100km
comparison_100km <- div_comparison(plant_cellRichness_100km, mammal_cellRichness_100km, bird_cellRichness_100km, 100)

comparison_100km_plot <- comparison_100km$plot
comparison_100km_plot <- comparison_100km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())

saveRDS(comparison_100km$bird, file = file.path(output_path_L2,"compare_BPrichness_100km.rds"))
saveRDS(comparison_100km$mammal, file = file.path(output_path_L2,"compare_MPrichness_100km.rds"))


# 75 km
comparison_75km <- div_comparison(plant_cellRichness_75km, mammal_cellRichness_75km, bird_cellRichness_75km, 75)

comparison_75km_plot <- comparison_75km$plot
comparison_75km_plot <- comparison_75km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())

saveRDS(comparison_75km$bird, file = file.path(output_path_L2,"compare_BPrichness_75km.rds"))
saveRDS(comparison_75km$mammal, file = file.path(output_path_L2,"compare_MPrichness_75km.rds"))


# 50 km
comparison_50km <- div_comparison(plant_cellRichness_50km, mammal_cellRichness_50km, bird_cellRichness_50km, 50)

comparison_50km_plot <- comparison_50km$plot
comparison_50km_plot <- comparison_50km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell')

saveRDS(comparison_50km$bird, file = file.path(output_path_L2,"compare_BPrichness_50km.rds"))
saveRDS(comparison_50km$mammal, file = file.path(output_path_L2,"compare_MPrichness_50km.rds"))


# 25 km
comparison_25km <- div_comparison(plant_cellRichness_25km, mammal_cellRichness_25km, bird_cellRichness_25km, 25)

comparison_25km_plot <- comparison_25km$plot
comparison_25km_plot <- comparison_25km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())

saveRDS(comparison_25km$bird, file = file.path(output_path_L2,"compare_BPrichness_25km.rds"))
saveRDS(comparison_25km$mammal, file = file.path(output_path_L2,"compare_MPrichness_25km.rds"))


# 10 km
comparison_10km <- div_comparison(plant_cellRichness_10km, mammal_cellRichness_10km, bird_cellRichness_10km, 10)

comparison_10km_plot <- comparison_10km$plot
comparison_10km_plot <- comparison_10km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())

saveRDS(comparison_10km$bird, file = file.path(output_path_L2,"compare_BPrichness_10km.rds"))
saveRDS(comparison_10km$mammal, file = file.path(output_path_L2,"compare_MPrichness_10km.rds"))


# 5 km
comparison_5km <- div_comparison(plant_cellRichness_5km, mammal_cellRichness_5km, bird_cellRichness_5km, 5)

comparison_5km_plot <- comparison_5km$plot
comparison_5km_plot <- comparison_5km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank()) 

saveRDS(comparison_5km$bird, file = file.path(output_path_L2,"compare_BPrichness_5km.rds"))
saveRDS(comparison_5km$mammal, file = file.path(output_path_L2,"compare_MPrichness_5km.rds"))


# combined plot
all_taxa_richness_plots <- wrap_plots(comparison_5km_plot, comparison_10km_plot, comparison_25km_plot, comparison_50km_plot, comparison_75km_plot, comparison_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 20, 20, 20),legend.position='bottom')

all_taxa_richness_plots

ggsave('all_taxa_richness_plots.png', all_taxa_richness_plots, path = figure_path, width = 14, height = 8, units = "in", dpi=1000)
