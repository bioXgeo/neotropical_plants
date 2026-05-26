#title: "Taxonomic diversity relationships for plants, mammals, and birds"
#author: "Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Hazel J. Anderson, Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "This script compares taxonomic diversity relationships for plants and frugivores for various spatial grains."
#data input: "TropicalAndes_plantRichness_5km.csv", "TropicalAndes_frugivoreRichness_5km.csv", "TropicalAndes_mammalRichness_5km.csv", "TropicalAndes_birdRichness_5km.csv", "TropicalAndes_plantRichness_10km.csv", "TropicalAndes_frugivoreRichness_10km.csv", "TropicalAndes_mammalRichness_10km.csv", "TropicalAndes_birdRichness_10km.csv", "TropicalAndes_plantRichness_25km.csv", "TropicalAndes_frugivoreRichness_25km.csv", "TropicalAndes_mammalRichness_25km.csv", "TropicalAndes_birdRichness_25km.csv", "TropicalAndes_plantRichness_50km.csv", "TropicalAndes_frugivoreRichness_50km.csv", "TropicalAndes_mammalRichness_50km.csv", "TropicalAndes_birdRichness_50km.csv", "TropicalAndes_plantRichness_75km.csv", "TropicalAndes_frugivoreRichness_75km.csv", "TropicalAndes_mammalRichness_75km.csv", "TropicalAndes_birdRichness_75km.csv", "TropicalAndes_plantRichness_100km.csv", "TropicalAndes_frugivoreRichness_100km.csv", "TropicalAndes_mammalRichness_100km.csv", "TropicalAndes_birdRichness_100km.csv"
#data output: "compare_BPrichness_100km.rds", "compare_MPrichness_100km.rds", "compare_BPrichness_75km.rds", "compare_MPrichness_75km.rds", "compare_BPrichness_50km.rds", "compare_MPrichness_50km.rds", "compare_BPrichness_25km.rds", "compare_MPrichness_25km.rds", "compare_BPrichness_10km.rds", "compare_MPrichness_10km.rds", "compare_BPrichness_5km.rds", "compare_MPrichness_5km.rds", "all_taxa_richness_plots.png"
#date: "2024-05-16; 2025-12-15"


# load required packages
library(dplyr); library(ggplot2); library(smoothr); library(purrr); library(ggtrendline); library(ggpubr); library(tidyr); library(patchwork); library(mgcv); library(sf)


# set file paths
data_path_L2 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')


# load functions
source("C:/GitHub_projects/neotropical_plants/code/Functions.R")


# read in data

# 5km
plant_cellRichness_5km <- readRDS(file.path(data_path_L2,"TropicalAndes_plantRichness_5km.rds"))
frugivore_cellRichness_5km <- readRDS(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_5km.rds"))
mammal_cellRichness_5km <- readRDS(file.path(data_path_L2,"TropicalAndes_mammalRichness_5km.rds"))
bird_cellRichness_5km <- readRDS(file.path(data_path_L2,"TropicalAndes_birdRichness_5km.rds"))


# 10km 
plant_cellRichness_10km <- readRDS(file.path(data_path_L2,"TropicalAndes_plantRichness_10km.rds"))
frugivore_cellRichness_10km <- readRDS(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_10km.rds"))
mammal_cellRichness_10km <- readRDS(file.path(data_path_L2,"TropicalAndes_mammalRichness_10km.rds"))
bird_cellRichness_10km <- readRDS(file.path(data_path_L2,"TropicalAndes_birdRichness_10km.rds"))


# 25km
plant_cellRichness_25km <- readRDS(file.path(data_path_L2,"TropicalAndes_plantRichness_25km.rds"))
frugivore_cellRichness_25km <- readRDS(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_25km.rds"))
mammal_cellRichness_25km <- readRDS(file.path(data_path_L2,"TropicalAndes_mammalRichness_25km.rds"))
bird_cellRichness_25km <- readRDS(file.path(data_path_L2,"TropicalAndes_birdRichness_25km.rds"))


# 50km
plant_cellRichness_50km <- readRDS(file.path(data_path_L2,"TropicalAndes_plantRichness_50km.rds"))
frugivore_cellRichness_50km <- readRDS(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_50km.rds"))
mammal_cellRichness_50km <- readRDS(file.path(data_path_L2,"TropicalAndes_mammalRichness_50km.rds"))
bird_cellRichness_50km <- readRDS(file.path(data_path_L2,"TropicalAndes_birdRichness_50km.rds"))


# 75km
plant_cellRichness_75km <- readRDS(file.path(data_path_L2,"TropicalAndes_plantRichness_75km.rds"))
frugivore_cellRichness_75km <- readRDS(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_75km.rds"))
mammal_cellRichness_75km <- readRDS(file.path(data_path_L2,"TropicalAndes_mammalRichness_75km.rds"))
bird_cellRichness_75km <- readRDS(file.path(data_path_L2,"TropicalAndes_birdRichness_75km.rds"))


# 100km
plant_cellRichness_100km <- readRDS(file.path(data_path_L2,"TropicalAndes_plantRichness_100km.rds"))
frugivore_cellRichness_100km <- readRDS(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_100km.rds"))
mammal_cellRichness_100km <- readRDS(file.path(data_path_L2,"TropicalAndes_mammalRichness_100km.rds"))
bird_cellRichness_100km <- readRDS(file.path(data_path_L2,"TropicalAndes_birdRichness_100km.rds"))


#### comparison of plant-frugivore richness ####

# 100km
comparison_100km <- div_comparison(plant_cellRichness_100km, mammal_cellRichness_100km, bird_cellRichness_100km, 100)

comparison_100km_plot <- comparison_100km$plot
(comparison_100km_plot <- comparison_100km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 1200, y = 380, label = paste('Bird R² =', round(comparison_100km$r2_df$r2[2], 3)), size=5) + annotate("text", x = 1200, y = 120, label = paste('Mammal R² =', round(comparison_100km$r2_df$r2[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_100km$m1, plot=T)
simulateResiduals(comparison_100km$m2, plot=T)


# 75 km
comparison_75km <- div_comparison(plant_cellRichness_75km, mammal_cellRichness_75km, bird_cellRichness_75km, 75)

comparison_75km_plot <- comparison_75km$plot
(comparison_75km_plot <- comparison_75km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 1200, y = 380, label = paste('Bird R² =', round(comparison_75km$r2_df$r2[2], 3)), size=5) + annotate("text", x = 1200, y = 120, label = paste('Mammal R² =', round(comparison_75km$r2_df$r2[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_75km$m1, plot=T)
simulateResiduals(comparison_75km$m2, plot=T)


# 50 km
comparison_50km <- div_comparison(plant_cellRichness_50km, mammal_cellRichness_50km, bird_cellRichness_50km, 50)

comparison_50km_plot <- comparison_50km$plot
(comparison_50km_plot <- comparison_50km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + annotate("text", x = 1200, y = 380, label = paste('Bird R² =', round(comparison_50km$r2_df$r2[2], 3)), size=5) + annotate("text", x = 1200, y = 120, label = paste('Mammal R² =', round(comparison_50km$r2_df$r2[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_50km$m1, plot=T)
simulateResiduals(comparison_50km$m2, plot=T)


# 25 km
comparison_25km <- div_comparison(plant_cellRichness_25km, mammal_cellRichness_25km, bird_cellRichness_25km, 25)

comparison_25km_plot <- comparison_25km$plot
(comparison_25km_plot <- comparison_25km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) + annotate("text", x = 900, y = 350, label = paste('Bird R² =', round(comparison_25km$r2_df$r2[2], 3)), size=5) + annotate("text", x = 900, y = 80, label = paste('Mammal R² =', round(comparison_25km$r2_df$r2[1], 3)), size=5)+ theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_25km$m1, plot=T)
simulateResiduals(comparison_25km$m2, plot=T)


# 10 km
comparison_10km <- div_comparison(plant_cellRichness_10km, mammal_cellRichness_10km, bird_cellRichness_10km, 10)

comparison_10km_plot <- comparison_10km$plot
(comparison_10km_plot <- comparison_10km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) + annotate("text", x = 660, y = 300, label = paste('Bird R² =', round(comparison_10km$r2_df$r2[2], 3)), size=5) + annotate("text", x = 700, y = 50, label = paste('Mammal R² =', round(comparison_10km$r2_df$r2[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_10km$m1, plot=T)
simulateResiduals(comparison_10km$m2, plot=T)


# 5 km
comparison_5km <- div_comparison(plant_cellRichness_5km, mammal_cellRichness_5km, bird_cellRichness_5km, 5)

comparison_5km_plot <- comparison_5km$plot
((comparison_5km_plot <- comparison_5km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank()) + annotate("text", x = 570, y = 280, label = paste('Bird R² =', round(comparison_5km$r2_df$r2[2], 3)), size=5) + annotate("text", x = 570, y = 50, label = paste('Mammal R² =', round(comparison_5km$r2_df$r2[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))))

simulateResiduals(comparison_5km$m1, plot=T)
simulateResiduals(comparison_5km$m2, plot=T)


# combined plot
(all_taxa_richness_plots <- wrap_plots(comparison_5km_plot, comparison_10km_plot, comparison_25km_plot, comparison_50km_plot, comparison_75km_plot, comparison_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 15, 15, 15),legend.position='bottom'))

ggsave('all_taxa_richness_plots.png', all_taxa_richness_plots, path = figure_path, width = 13, height = 8, units = "in", dpi=1000)


#### GAMs ####

# 100km
comparison_100km_gam <- div_comparison_gam(plant_cellRichness_100km, mammal_cellRichness_100km, bird_cellRichness_100km, 100)

comparison_100km_gam_plot <- comparison_100km_gam$plot
(comparison_100km_gam_plot <- comparison_100km_gam_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 1200, y = 360, label = paste('Bird D² =', round(comparison_100km_gam$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 1200, y = 120, label = paste('Mammal D² =', round(comparison_100km_gam$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_100km_gam$m1, plot=T)
simulateResiduals(comparison_100km_gam$m2, plot=T)


# 75 km
comparison_75km_gam <- div_comparison_gam(plant_cellRichness_75km, mammal_cellRichness_75km, bird_cellRichness_75km, 75)

comparison_75km_gam_plot <- comparison_75km_gam$plot
(comparison_75km_gam_plot <- comparison_75km_gam_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 1200, y = 380, label = paste('Bird D² =', round(comparison_75km_gam$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 1200, y = 120, label = paste('Mammal D² =', round(comparison_75km_gam$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_75km_gam$m1, plot=T)
simulateResiduals(comparison_75km_gam$m2, plot=T)


# 50 km
comparison_50km_gam <- div_comparison_gam(plant_cellRichness_50km, mammal_cellRichness_50km, bird_cellRichness_50km, 50)

comparison_50km_gam_plot <- comparison_50km_gam$plot
(comparison_50km_gam_plot <- comparison_50km_gam_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + annotate("text", x = 1200, y = 350, label = paste('Bird D² =', round(comparison_50km_gam$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 1200, y = 100, label = paste('Mammal D² =', round(comparison_50km_gam$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_50km_gam$m1, plot=T)
simulateResiduals(comparison_50km_gam$m2, plot=T)


# 25 km
comparison_25km_gam <- div_comparison_gam(plant_cellRichness_25km, mammal_cellRichness_25km, bird_cellRichness_25km, 25)

comparison_25km_gam_plot <- comparison_25km_gam$plot
(comparison_25km_gam_plot <- comparison_25km_gam_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) + annotate("text", x = 900, y = 300, label = paste('Bird D² =', round(comparison_25km_gam$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 900, y = 80, label = paste('Mammal D² =', round(comparison_25km_gam$r2_df$dev_expl[1], 3)), size=5)+ theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_25km_gam$m1, plot=T)
simulateResiduals(comparison_25km_gam$m2, plot=T)


# 10 km
comparison_10km_gam <- div_comparison_gam(plant_cellRichness_10km, mammal_cellRichness_10km, bird_cellRichness_10km, 10)

comparison_10km_gam_plot <- comparison_10km_gam$plot
(comparison_10km_gam_plot <- comparison_10km_gam_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) + annotate("text", x = 660, y = 270, label = paste('Bird D² =', round(comparison_10km_gam$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 700, y = 50, label = paste('Mammal R² =', round(comparison_10km_gam$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_10km_gam$m1, plot=T)
simulateResiduals(comparison_10km_gam$m2, plot=T)


# 5 km
comparison_5km_gam <- div_comparison_gam(plant_cellRichness_5km, mammal_cellRichness_5km, bird_cellRichness_5km, 5)

comparison_5km_gam_plot <- comparison_5km_gam$plot
((comparison_5km_gam_plot <- comparison_5km_gam_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank()) + annotate("text", x = 570, y = 230, label = paste('Bird D² =', round(comparison_5km_gam$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 570, y = 50, label = paste('Mammal D² =', round(comparison_5km_gam$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))))

simulateResiduals(comparison_5km_gam$m1, plot=T)
simulateResiduals(comparison_5km_gam$m2, plot=T)


# combined plot
(all_taxa_richness_gam_plots <- wrap_plots(comparison_5km_gam_plot, comparison_10km_gam_plot, comparison_25km_gam_plot, comparison_50km_gam_plot, comparison_75km_gam_plot, comparison_100km_gam_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 15, 15, 15),legend.position='bottom'))

ggsave('all_taxa_richness_gam_plots.png', all_taxa_richness_gam_plots, path = figure_path, width = 13, height = 8, units = "in", dpi=1000)
