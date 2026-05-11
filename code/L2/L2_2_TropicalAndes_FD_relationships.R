#title: "Functional diversity relationships for plants, mammals, and birds"
#author: "Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Hazel J. Anderson, Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "This script compares functional diversity relationships for plants and frugivores for various spatial grains."
#data input: "fdis_frugivore_5km.rds", "fdis_mammal_5km.rds", "fdis_bird_5km.rds", "fdis_plant_5km.rds", "fdis_frugivore_10km.rds", "fdis_mammal_10km.rds", "fdis_bird_10km.rds", "fdis_plant_10km.rds", "fdis_frugivore_25km.rds", "fdis_mammal_25km.rds", "fdis_bird_25km.rds", "fdis_plant_25km.rds", "fdis_frugivore_50km.rds", "fdis_mammal_50km.rds", "fdis_bird_50km.rds", "fdis_plant_50km.rds", "fdis_frugivore_75km.rds", "fdis_mammal_75km.rds", "fdis_bird_75km.rds", "fdis_plant_75km.rds", "fdis_frugivore_100km.rds", "fdis_mammal_100km.rds", "fdis_bird_100km.rds", "fdis_plant_100km.rds"
#data ouput: "compare_BPFDis_100km.rds", "compare_MPFDis_100km.rds", "compare_BPFDis_75km.rds", "compare_MPFDis_75km.rds", "compare_BPFDis_50km.rds", "compare_MPFDis_50km.rds", "compare_BPFDis_25km.rds", "compare_MPFDis_25km.rds", "compare_BPFDis_10km.rds", "compare_MPFDis_10km.rds", "compare_BPFDis_5km.rds", "compare_MPFDis_5km.rds", "all_taxa_FDis_plots.png"
#date: "2024-05-16; 2025-12-15"
#notes: JB used HPCC


# load required packages
library(dplyr); library(ggplot2); library(smoothr); library(purrr); library(ggtrendline); library(ggpubr); library(tidyr); library(patchwork); library(mgcv); library(DHARMa); library(sf)


# set file paths
data_path_L2 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')

# #HPCC
# data_path_L2 <- file.path('/mnt/research/nasabio/data_2025/plants/L2')
# output_path_L2 <- file.path('/mnt/research/nasabio/data_2025/plants/L2')
# figure_path <- file.path('/mnt/research/nasabio/data_2025/plants/figures')


# load functions
source("C:/GitHub_projects/plant-frugivore diversity/neotropical_plants/code/Functions.R")

## HPCC
# source("/mnt/ffs24/home/baljunas/Documents/neotropical_plants/code/Functions.R")


# read in data

# 5km
FcellFDis_5km <- readRDS(file = file.path(data_path_L2,"FcellFDis_5km.rds"))
McellFDis_5km <- readRDS(file = file.path(data_path_L2,"McellFDis_5km.rds"))
BcellFDis_5km <- readRDS(file = file.path(data_path_L2,"BcellFDis_5km.rds"))
PcellFDis_5km <- readRDS(file = file.path(data_path_L2,"PcellFDis_5km.rds"))


# 10km
FcellFDis_10km <- readRDS(file = file.path(data_path_L2,"FcellFDis_10km.rds"))
McellFDis_10km <- readRDS(file = file.path(data_path_L2,"McellFDis_10km.rds"))
BcellFDis_10km <- readRDS(file = file.path(data_path_L2,"BcellFDis_10km.rds"))
PcellFDis_10km <- readRDS(file = file.path(data_path_L2,"PcellFDis_10km.rds"))


# 25km
FcellFDis_25km <- readRDS(file = file.path(data_path_L2,"FcellFDis_25km.rds"))
McellFDis_25km <- readRDS(file = file.path(data_path_L2,"McellFDis_25km.rds"))
BcellFDis_25km <- readRDS(file = file.path(data_path_L2,"BcellFDis_25km.rds"))
PcellFDis_25km <- readRDS(file = file.path(data_path_L2,"PcellFDis_25km.rds"))


# 50km
FcellFDis_50km <- readRDS(file = file.path(data_path_L2,"FcellFDis_50km.rds"))
McellFDis_50km <- readRDS(file = file.path(data_path_L2,"McellFDis_50km.rds"))
BcellFDis_50km <- readRDS(file = file.path(data_path_L2,"BcellFDis_50km.rds"))
PcellFDis_50km <- readRDS(file = file.path(data_path_L2,"PcellFDis_50km.rds"))

# 75km
FcellFDis_75km <- readRDS(file = file.path(data_path_L2,"FcellFDis_75km.rds"))
McellFDis_75km <- readRDS(file = file.path(data_path_L2,"McellFDis_75km.rds"))
BcellFDis_75km <- readRDS(file = file.path(data_path_L2,"BcellFDis_75km.rds"))
PcellFDis_75km <- readRDS(file = file.path(data_path_L2,"PcellFDis_75km.rds"))


# 100km
FcellFDis_100km <- readRDS(file = file.path(data_path_L2,"FcellFDis_100km.rds"))
McellFDis_100km <- readRDS(file = file.path(data_path_L2,"McellFDis_100km.rds"))
BcellFDis_100km <- readRDS(file = file.path(data_path_L2,"BcellFDis_100km.rds"))
PcellFDis_100km <- readRDS(file = file.path(data_path_L2,"PcellFDis_100km.rds"))


# #### fdis cleaning ####
# 
# frugivore_cellFDis_100km <- clean_fdis(fdis_frugivore_100km, 100)
# mammal_cellFDis_100km <- clean_fdis(fdis_mammal_100km, 100)
# bird_cellFDis_100km <- clean_fdis(fdis_bird_100km, 100)
# plant_cellFDis_100km <- clean_fdis(fdis_plant_100km, 100)
# 
# frugivore_cellFDis_75km <- clean_fdis(fdis_frugivore_75km, 75)
# mammal_cellFDis_75km <- clean_fdis(fdis_mammal_75km, 75)
# bird_cellFDis_75km <- clean_fdis(fdis_bird_75km, 75)
# plant_cellFDis_75km <- clean_fdis(fdis_plant_75km, 75)
# 
# frugivore_cellFDis_50km <- clean_fdis(fdis_frugivore_50km, 50)
# mammal_cellFDis_50km <- clean_fdis(fdis_mammal_50km, 50)
# bird_cellFDis_50km <- clean_fdis(fdis_bird_50km, 50)
# plant_cellFDis_50km <- clean_fdis(fdis_plant_50km, 50)
# 
# frugivore_cellFDis_25km <- clean_fdis(fdis_frugivore_25km, 25)
# mammal_cellFDis_25km <- clean_fdis(fdis_mammal_25km, 25)
# bird_cellFDis_25km <- clean_fdis(fdis_bird_25km, 25)
# plant_cellFDis_25km <- clean_fdis(fdis_plant_25km, 25)
# 
# frugivore_cellFDis_10km <- clean_fdis(fdis_frugivore_10km, 10)
# mammal_cellFDis_10km <- clean_fdis(fdis_mammal_10km, 10)
# bird_cellFDis_10km <- clean_fdis(fdis_bird_10km, 10)
# plant_cellFDis_10km <- clean_fdis(fdis_plant_10km, 10)
# 
# frugivore_cellFDis_5km <- clean_fdis(fdis_frugivore_5km, 5)
# mammal_cellFDis_5km <- clean_fdis(fdis_mammal_5km, 5)
# bird_cellFDis_5km <- clean_fdis(fdis_bird_5km, 5)
# plant_cellFDis_5km <- clean_fdis(fdis_plant_5km, 5)


#### comparison of plant-frugivore richness ####

# 100km
comparison_100km <- div_comparison(PcellFDis_100km, McellFDis_100km, BcellFDis_100km, 100)

comparison_100km_plot <- comparison_100km$plot
comparison_100km_plot <- comparison_100km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank())+ annotate("text", x = 0.4, y = 0.7, label = paste('Bird D² =', round(comparison_100km$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 0.4, y = 0.5, label = paste('Mammal D² =', round(comparison_100km$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))


# 75 km
comparison_75km <- div_comparison(PcellFDis_75km, McellFDis_75km, BcellFDis_75km, 75)

comparison_75km_plot <- comparison_75km$plot
comparison_75km_plot <- comparison_75km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) + annotate("text", x = 0.3, y = 0.7, label = paste('Bird D² =', round(comparison_75km$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 0.3, y = 0.5, label = paste('Mammal D² =', round(comparison_75km$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))


# 50 km
comparison_50km <- div_comparison(PcellFDis_50km, McellFDis_50km, BcellFDis_50km, 50)

comparison_50km_plot <- comparison_50km$plot
comparison_50km_plot <- comparison_50km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + annotate("text", x = 0.3, y = 0.7, label = paste('Bird D² =', round(comparison_50km$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 0.3, y = 0.5, label = paste('Mammal D² =', round(comparison_50km$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))


# 25 km
comparison_25km <- div_comparison(PcellFDis_25km, McellFDis_25km, BcellFDis_25km, 25)

comparison_25km_plot <- comparison_25km$plot
comparison_25km_plot <- comparison_25km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) + annotate("text", x = 0.2, y = 0.7, label = paste('Bird D² =', round(comparison_25km$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 0.2, y = 0.4, label = paste('Mammal D² =', round(comparison_25km$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))


# 10 km
comparison_10km <- div_comparison(PcellFDis_10km, McellFDis_10km, BcellFDis_10km, 10)

comparison_10km_plot <- comparison_10km$plot
comparison_10km_plot <- comparison_10km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) + annotate("text", x = 0.2, y = 0.6, label = paste('Bird D² =', round(comparison_10km$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 0.2, y = 0.4, label = paste('Mammal D² =', round(comparison_10km$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))



# 5 km
comparison_5km <- div_comparison(PcellFDis_5km, McellFDis_5km, BcellFDis_5km, 5)

comparison_5km_plot <- comparison_5km$plot
comparison_5km_plot <- comparison_5km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.x = element_blank(),axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.line.x = element_blank()) + annotate("text", x = 0.2, y = 0.6, label = paste('Mammal D² =', round(comparison_5km$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 0.2, y = 0.4, label = paste('Bird D² =', round(comparison_5km$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16))


# combined plot
all_taxa_FDis_plots <- wrap_plots(comparison_5km_plot, comparison_10km_plot, comparison_25km_plot, comparison_50km_plot, comparison_75km_plot, comparison_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 15, 15, 15),legend.position='bottom')

all_taxa_FDis_plots

ggsave('all_taxa_FDis_plots2.png', all_taxa_FDis_plots, path = figure_path, width = 13, height = 8, units = "in", dpi=1000)


#### poster figures ####

# Mammals - 100km

coords <- as.data.frame(st_coordinates(st_centroid(PcellFDis_100km)))

mammal_plant <- data.frame(cell_id=PcellFDis_100km$cellid, x=coords$X, y=coords$Y, plant_div = PcellFDis_100km$fdis_value, frug_div=McellFDis_100km$fdis_value, taxa=c(rep('Mammal', nrow(McellFDis_100km)))) %>% 
  dplyr::filter(plant_div > 0 & frug_div > 0)

m1 <- lm(
  frug_div ~ plant_div,
  data = mammal_plant)

rng <- range(PcellFDis_100km$fdis_value, na.rm = TRUE)

newdata <- data.frame(
  plant_div = seq(rng[1], rng[2], length.out = 100),
  x = mean(coords$X, na.rm = TRUE),
  y = mean(coords$Y, na.rm = TRUE)
)

newdata$frug_div <- predict(m1, newdata = newdata, type = "response")

(mammal_FDis_plot <- ggplot(data = mammal_plant, aes(x = plant_div, y = frug_div)) +
    geom_point(alpha = 0.8, size = 3, color = 'burlywood3') +
    geom_line(data = newdata, aes(x = plant_div, y = frug_div), size = 2, color = 'burlywood3') +
    scale_x_continuous(expand=c(0,0), limits=c(0.4,0.7))+
    scale_y_continuous(expand=c(0,0), limits=c(0.15,0.85))+
    annotate("text", x = Inf, y = Inf, hjust = "inward", vjust = "inward", label = paste('R² =', round(summary(m1)$adj.r.squared, 3)), size=5) +
    labs(x='Plant FDis by cell', y='Mammal FDis by cell', title='[100km]')+
    theme_classic()+
    theme(axis.title = element_text(size = 18), axis.text = element_text(size = 12), legend.title = element_text(size = 18), legend.text = element_text(size = 16), plot.margin = margin(4, 15, 4, 4, "pt")))

ggsave('mammal_FDis_plot.png', mammal_FDis_plot, path = figure_path, width = 5, height = 4, units = "in", dpi=1000)


# Birds - 100km

coords <- as.data.frame(st_coordinates(st_centroid(PcellFDis_100km)))

bird_plant <- data.frame(cell_id=PcellFDis_100km$cellid, x=coords$X, y=coords$Y, plant_div = PcellFDis_100km$fdis_value, frug_div=BcellFDis_100km$fdis_value, taxa=c(rep('Bird', nrow(BcellFDis_100km)))) %>% 
  dplyr::filter(plant_div > 0 & frug_div > 0)

m2 <- lm(
  frug_div ~ plant_div,
  data = bird_plant)

rng <- range(PcellFDis_100km$fdis_value, na.rm = TRUE)

newdata <- data.frame(
  plant_div = seq(rng[1], rng[2], length.out = 100),
  x = mean(coords$X, na.rm = TRUE),
  y = mean(coords$Y, na.rm = TRUE)
)

newdata$frug_div <- predict(m2, newdata = newdata, type = "response")

(bird_FDis_plot <- ggplot(data = bird_plant, aes(x = plant_div, y = frug_div)) +
    geom_point(alpha = 0.8, size = 3, color = 'lightsteelblue2') +
    geom_line(data = newdata, aes(x = plant_div, y = frug_div), size = 2, color = 'lightsteelblue2') +
    scale_x_continuous(expand=c(0,0), limits=c(0.4,0.7))+
    scale_y_continuous(expand=c(0,0), limits=c(0.15,0.85))+
    annotate("text", x = Inf, y = Inf, hjust = "inward", vjust = "inward", label = paste('R² =', round(summary(m2)$adj.r.squared, 3)), size=5) +
    labs(x='Plant FDis by cell', y='Bird FDis by cell', title='[100km]')+
    theme_classic()+
    theme(axis.title = element_text(size = 18), axis.text = element_text(size = 12), legend.title = element_text(size = 18), legend.text = element_text(size = 16), plot.margin = margin(4, 15, 4, 4, "pt")))

ggsave('bird_FDis_plot.png', bird_FDis_plot, path = figure_path, width = 5, height = 4, units = "in", dpi=1000)




