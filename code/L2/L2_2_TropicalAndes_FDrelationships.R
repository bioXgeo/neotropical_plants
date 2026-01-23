#title: "Functional diversity relationships for plants, mammals, and birds"
#author: "Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Hazel J. Anderson, Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "This script compares functional diversity relationships for plants and frugivores for various spatial grains."
#date: "2025-12-15"
#output: html_document


# set file paths
data_path_L2 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')

#HPCC
data_path_L2 <- file.path('/mnt/research/nasabio/data_2025/plants/L2')
output_path_L2 <- file.path('/mnt/research/nasabio/data_2025/plants/L2')
figure_path <- file.path('/mnt/research/nasabio/data_2025/plants/figures')


# load required packages
library(dplyr); library(ggplot2); library(smoothr); library(purrr); library(ggtrendline); library(ggpubr); library(tidyr); library(patchwork)

# load functions
setwd("/mnt/ffs24/home/baljunas/Documents/neotropical_plants/code")
source("Functions.R")


# read in data

#### 5km ####
fdis_frugivore_5km <- readRDS(file = file.path(output_path_L2,"fdis_frugivore_5km.rds"))
fdis_mammal_5km <- readRDS(file = file.path(output_path_L2,"fdis_mammal_5km.rds"))
fdis_bird_5km <- readRDS(file = file.path(output_path_L2,"fdis_bird_5km.rds"))
fdis_plant_5km <- readRDS(file = file.path(output_path_L2,"fdis_plant_5km.rds"))


#### 10km ####
fdis_frugivore_10km <- readRDS(file = file.path(output_path_L2,"fdis_frugivore_10km.rds"))
fdis_mammal_10km <- readRDS(file = file.path(output_path_L2,"fdis_mammal_10km.rds"))
fdis_bird_10km <- readRDS(file = file.path(output_path_L2,"fdis_bird_10km.rds"))
fdis_plant_10km <- readRDS(file = file.path(output_path_L2,"fdis_plant_10km.rds"))


#### 25km ####
fdis_frugivore_25km <- readRDS(file = file.path(output_path_L2,"fdis_frugivore_25km.rds"))
fdis_mammal_25km <- readRDS(file = file.path(output_path_L2,"fdis_mammal_25km.rds"))
fdis_bird_25km <- readRDS(file = file.path(output_path_L2,"fdis_bird_25km.rds"))
fdis_plant_25km <- readRDS(file = file.path(output_path_L2,"fdis_plant_25km.rds"))


#### 50km ####
fdis_frugivore_50km <- readRDS(file = file.path(output_path_L2,"fdis_frugivore_50km.rds"))
fdis_mammal_50km <- readRDS(file = file.path(output_path_L2,"fdis_mammal_50km.rds"))
fdis_bird_50km <- readRDS(file = file.path(output_path_L2,"fdis_bird_50km.rds"))
fdis_plant_50km <- readRDS(file = file.path(output_path_L2,"fdis_plant_50km.rds"))


#### 75km ####
fdis_frugivore_75km <- readRDS(file = file.path(output_path_L2,"fdis_frugivore_75km.rds"))
fdis_mammal_75km <- readRDS(file = file.path(output_path_L2,"fdis_mammal_75km.rds"))
fdis_bird_75km <- readRDS(file = file.path(output_path_L2,"fdis_bird_75km.rds"))
fdis_plant_75km <- readRDS(file = file.path(output_path_L2,"fdis_plant_75km.rds"))


#### 100km ####
fdis_frugivore_100km <- readRDS(file = file.path(output_path_L2,"fdis_frugivore_100km.rds"))
fdis_mammal_100km <- readRDS(file = file.path(output_path_L2,"fdis_mammal_100km.rds"))
fdis_bird_100km <- readRDS(file = file.path(output_path_L2,"fdis_bird_100km.rds"))
fdis_plant_100km <- readRDS(file = file.path(output_path_L2,"fdis_plant_100km.rds"))


#### fdis cleaning ####

frugivore_cellFDis_100km <- clean_fdis(fdis_frugivore_100km,100)
mammal_cellFDis_100km <- clean_fdis(fdis_mammal_100km,100)
bird_cellFDis_100km <- clean_fdis(fdis_bird_100km,100)
plant_cellFDis_100km <- clean_fdis(fdis_plant_100km,100)

frugivore_cellFDis_75km <- clean_fdis(fdis_frugivore_75km,75)
mammal_cellFDis_75km <- clean_fdis(fdis_mammal_75km,75)
bird_cellFDis_75km <- clean_fdis(fdis_bird_75km,75)
plant_cellFDis_75km <- clean_fdis(fdis_plant_75km,75)

frugivore_cellFDis_50km <- clean_fdis(fdis_frugivore_50km,50)
mammal_cellFDis_50km <- clean_fdis(fdis_mammal_50km,50)
bird_cellFDis_50km <- clean_fdis(fdis_bird_50km,50)
plant_cellFDis_50km <- clean_fdis(fdis_plant_50km,50)

frugivore_cellFDis_25km <- clean_fdis(fdis_frugivore_25km,25)
mammal_cellFDis_25km <- clean_fdis(fdis_mammal_25km,25)
bird_cellFDis_25km <- clean_fdis(fdis_bird_25km,25)
plant_cellFDis_25km <- clean_fdis(fdis_plant_25km,25)

frugivore_cellFDis_10km <- clean_fdis(fdis_frugivore_10km,10)
mammal_cellFDis_10km <- clean_fdis(fdis_mammal_10km,10)
bird_cellFDis_10km <- clean_fdis(fdis_bird_10km,10)
plant_cellFDis_10km <- clean_fdis(fdis_plant_10km,10)

frugivore_cellFDis_5km <- clean_fdis(fdis_frugivore_5km,5)
mammal_cellFDis_5km <- clean_fdis(fdis_mammal_5km,5)
bird_cellFDis_5km <- clean_fdis(fdis_bird_5km,5)
plant_cellFDis_5km <- clean_fdis(fdis_plant_5km,5)


#### comparison of plant-frugivore richness ####

# 100km
comparison_100km <- div_comparison(plant_cellFDis_100km, mammal_cellFDis_100km, bird_cellFDis_100km, 100)

comparison_100km_plot <- comparison_100km$plot
comparison_100km_plot <- comparison_100km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())

saveRDS(comparison_100km$bird, file = file.path(output_path_L2,"compare_BPFDis_100km.rds"))
saveRDS(comparison_100km$mammal, file = file.path(output_path_L2,"compare_MPFDis_100km.rds"))


# 75 km
comparison_75km <- div_comparison(plant_cellFDis_75km, mammal_cellFDis_75km, bird_cellFDis_75km, 75)

comparison_75km_plot <- comparison_75km$plot
comparison_75km_plot <- comparison_75km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())

saveRDS(comparison_75km$bird, file = file.path(output_path_L2,"compare_BPFDis_75km.rds"))
saveRDS(comparison_75km$mammal, file = file.path(output_path_L2,"compare_MPFDis_75km.rds"))


# 50 km
comparison_50km <- div_comparison(plant_cellFDis_50km, mammal_cellFDis_50km, bird_cellFDis_50km, 50)

comparison_50km_plot <- comparison_50km$plot
comparison_50km_plot <- comparison_50km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell')

saveRDS(comparison_50km$bird, file = file.path(output_path_L2,"compare_BPFDis_50km.rds"))
saveRDS(comparison_50km$mammal, file = file.path(output_path_L2,"compare_MPFDis_50km.rds"))


# 25 km
comparison_25km <- div_comparison(plant_cellFDis_25km, mammal_cellFDis_25km, bird_cellFDis_25km, 25)

comparison_25km_plot <- comparison_25km$plot
comparison_25km_plot <- comparison_25km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())

saveRDS(comparison_25km$bird, file = file.path(output_path_L2,"compare_BPFDis_25km.rds"))
saveRDS(comparison_25km$mammal, file = file.path(output_path_L2,"compare_MPFDis_25km.rds"))


# 10 km
comparison_10km <- div_comparison(plant_cellFDis_10km, mammal_cellFDis_10km, bird_cellFDis_10km, 10)

comparison_10km_plot <- comparison_10km$plot
comparison_10km_plot <- comparison_10km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())

saveRDS(comparison_10km$bird, file = file.path(output_path_L2,"compare_BPFDis_10km.rds"))
saveRDS(comparison_10km$mammal, file = file.path(output_path_L2,"compare_MPFDis_10km.rds"))


# 5 km
comparison_5km <- div_comparison(plant_cellFDis_5km, mammal_cellFDis_5km, bird_cellFDis_5km, 5)

comparison_5km_plot <- comparison_5km$plot
comparison_5km_plot <- comparison_5km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank()) 

saveRDS(comparison_5km$bird, file = file.path(output_path_L2,"compare_BPFDis_5km.rds"))
saveRDS(comparison_5km$mammal, file = file.path(output_path_L2,"compare_MPFDis_5km.rds"))


# combined plot
All_taxa_FDis_plots <- wrap_plots(comparison_5km_plot, comparison_10km_plot, comparison_25km_plot, comparison_50km_plot, comparison_75km_plot, comparison_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 20, 20, 20),legend.position='bottom')
ggsave('All_taxa_FDis_plots.png', All_taxa_FDis_plots, path = figure_path, width = 14, height = 8, units = "in", dpi=1000)
