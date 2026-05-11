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
source("C:/GitHub_projects/plant-frugivore diversity/neotropical_plants/code/Functions.R")


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
comparison_100km_plot <- comparison_100km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 1200, y = 380, label = paste('Bird D² =', round(comparison_100km$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 1200, y = 120, label = paste('Mammal D² =', round(comparison_100km$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))


# 75 km
comparison_75km <- div_comparison(plant_cellRichness_75km, mammal_cellRichness_75km, bird_cellRichness_75km, 75)

comparison_75km_plot <- comparison_75km$plot
comparison_75km_plot <- comparison_75km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 1200, y = 380, label = paste('Bird D² =', round(comparison_75km$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 1200, y = 120, label = paste('Mammal D² =', round(comparison_75km$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))


# 50 km
comparison_50km <- div_comparison(plant_cellRichness_50km, mammal_cellRichness_50km, bird_cellRichness_50km, 50)

comparison_50km_plot <- comparison_50km$plot
comparison_50km_plot <- comparison_50km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + annotate("text", x = 1200, y = 380, label = paste('Bird D² =', round(comparison_50km$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 1200, y = 120, label = paste('Mammal D² =', round(comparison_50km$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))



# 25 km
comparison_25km <- div_comparison(plant_cellRichness_25km, mammal_cellRichness_25km, bird_cellRichness_25km, 25)

comparison_25km_plot <- comparison_25km$plot
comparison_25km_plot <- comparison_25km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) + annotate("text", x = 900, y = 350, label = paste('Bird D² =', round(comparison_25km$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 900, y = 80, label = paste('Mammal D² =', round(comparison_25km$r2_df$dev_expl[1], 3)), size=5)+ theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))


# 10 km
comparison_10km <- div_comparison(plant_cellRichness_10km, mammal_cellRichness_10km, bird_cellRichness_10km, 10)

comparison_10km_plot <- comparison_10km$plot
comparison_10km_plot <- comparison_10km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) + annotate("text", x = 700, y = 300, label = paste('Bird D² =', round(comparison_10km$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 700, y = 50, label = paste('Mammal D² =', round(comparison_10km$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))


# 5 km
comparison_5km <- div_comparison(plant_cellRichness_5km, mammal_cellRichness_5km, bird_cellRichness_5km, 5)

comparison_5km_plot <- comparison_5km$plot
comparison_5km_plot <- comparison_5km_plot + ylab('Frugivore richness by cell') + xlab('Plant richness by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank()) + annotate("text", x = 570, y = 280, label = paste('Bird D² =', round(comparison_5km$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 570, y = 50, label = paste('Mammal D² =', round(comparison_5km$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))


# combined plot
all_taxa_richness_plots <- wrap_plots(comparison_5km_plot, comparison_10km_plot, comparison_25km_plot, comparison_50km_plot, comparison_75km_plot, comparison_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 15, 15, 15),legend.position='bottom')

all_taxa_richness_plots

ggsave('all_taxa_richness_plots2.png', all_taxa_richness_plots, path = figure_path, width = 13, height = 8, units = "in", dpi=1000)


#### poster plots ####
library(glmmTMB)
library(performance)

# mammals - 100km
coords <- as.data.frame(st_coordinates(st_centroid(plant_cellRichness_100km)))

mammal_plant <- data.frame(cell_id=plant_cellRichness_100km$cellid, x=coords$X, y=coords$Y, plant_div = plant_cellRichness_100km$num_species, frug_div=mammal_cellRichness_100km$num_species, taxa=c(rep('Mammal', nrow(mammal_cellRichness_100km)))) %>% 
  dplyr::filter(plant_div > 0 & frug_div > 0)

m1 <- lm(
  frug_div ~ plant_div,
  data = mammal_plant)

rng <- range(plant_cellRichness_100km$num_species, na.rm = TRUE)

newdata <- data.frame(
  plant_div = seq(rng[1], rng[2], length.out = 100),
  x = mean(coords$X, na.rm = TRUE),
  y = mean(coords$Y, na.rm = TRUE)
)

newdata$frug_div <- predict(m1, newdata = newdata, type = "response")

(mammal_richness_plot <- ggplot(data = mammal_plant, aes(x = plant_div, y = frug_div)) +
    geom_point(alpha = 0.8, size = 3, color = 'burlywood3') +
    geom_line(data = newdata, aes(x = plant_div, y = frug_div), size = 2, color = 'burlywood3') +
    scale_x_continuous(expand=c(0,0), limits=c(0,1600))+
    scale_y_continuous(expand=c(0,0), limits=c(0,100))+
    annotate("text", x = Inf, y = Inf, hjust = "inward", vjust = "inward", label = paste('R² =', round(summary(m1)$adj.r.squared, 3)), size=5) +
    labs(x='Plant richness by cell', y='Mammal richness by cell', title='[100km]')+
    theme_classic()+
    theme(axis.title = element_text(size = 18), axis.text = element_text(size = 12), legend.title = element_text(size = 18), legend.text = element_text(size = 16), plot.margin = margin(4, 15, 4, 4, "pt")))

ggsave('mammal_richness_plot.png', mammal_richness_plot, path = figure_path, width = 5, height = 4, units = "in", dpi=1000)


# birds - 100km
coords <- as.data.frame(st_coordinates(st_centroid(plant_cellRichness_100km)))

bird_plant <- data.frame(cell_id=plant_cellRichness_100km$cellid, x=coords$X, y=coords$Y, plant_div = plant_cellRichness_100km$num_species, frug_div=bird_cellRichness_100km$num_species, taxa=c(rep('Bird', nrow(bird_cellRichness_100km)))) %>% 
  dplyr::filter(plant_div > 0 & frug_div > 0)

m2 <- lm(
  frug_div ~ plant_div,
  data = bird_plant)

rng <- range(plant_cellRichness_100km$num_species, na.rm = TRUE)

newdata <- data.frame(
  plant_div = seq(rng[1], rng[2], length.out = 100),
  x = mean(coords$X, na.rm = TRUE),
  y = mean(coords$Y, na.rm = TRUE)
)

newdata$frug_div <- predict(m2, newdata = newdata, type = "response")

(bird_richness_plot <- ggplot(data = bird_plant, aes(x = plant_div, y = frug_div)) +
    geom_point(alpha = 0.8, size = 3, color = 'lightsteelblue2') +
    geom_line(data = newdata, aes(x = plant_div, y = frug_div), size = 2, color = 'lightsteelblue2') +
    scale_x_continuous(expand=c(0,0), limits=c(0,1600))+
    scale_y_continuous(expand=c(0,0), limits=c(0,400))+
    labs(x='Plant richness by cell', y='Bird richness by cell', title='[100km]')+
  annotate("text", x = Inf, y = Inf, hjust = "inward", vjust = "inward", label = paste('R² =', round(summary(m1)$adj.r.squared, 3)), size=5) +
  theme_classic()+
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 12), legend.title = element_text(size = 18), legend.text = element_text(size = 16), plot.margin = margin(4, 15, 4, 4, "pt")))

ggsave('bird_richness_plot.png', bird_richness_plot, path = figure_path, width = 5, height = 4, units = "in", dpi=1000)
