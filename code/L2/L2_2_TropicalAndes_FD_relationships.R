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
source("C:/GitHub_projects/neotropical_plants/code/Functions.R")

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


#### comparison of plant-frugivore FDis ####

# 100km
comparison_100km <- div_comparison(PcellFDis_100km, McellFDis_100km, BcellFDis_100km, 100)

comparison_100km_plot <- comparison_100km$plot
(comparison_100km_plot <- comparison_100km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 0.4, y = 0.55, label = paste('Bird R² =', round(comparison_100km$r2_df$r2[2], 3)), size=5) + annotate("text", x = 0.4, y = 0.65, label = paste('Mammal R² =', round(comparison_100km$r2_df$r2[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_100km$m1, plot=T)
simulateResiduals(comparison_100km$m2, plot=T)



# 75 km
comparison_75km <- div_comparison(PcellFDis_75km, McellFDis_75km, BcellFDis_75km, 75)

comparison_75km_plot <- comparison_75km$plot
(comparison_75km_plot <- comparison_75km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 0.4, y = 0.55, label = paste('Bird R² =', round(comparison_75km$r2_df$r2[2], 3)), size=5) + annotate("text", x = 0.4, y = 0.65, label = paste('Mammal R² =', round(comparison_75km$r2_df$r2[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_75km$m1, plot=T)
simulateResiduals(comparison_75km$m2, plot=T)


# 50 km
comparison_50km <- div_comparison(PcellFDis_50km, McellFDis_50km, BcellFDis_50km, 50)

comparison_50km_plot <- comparison_50km$plot
(comparison_50km_plot <- comparison_50km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 0.4, y = 0.52, label = paste('Bird R² =', round(comparison_50km$r2_df$r2[2], 3)), size=5) + annotate("text", x = 0.4, y = 0.7, label = paste('Mammal R² =', round(comparison_50km$r2_df$r2[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_50km$m1, plot=T)
simulateResiduals(comparison_50km$m2, plot=T)


# 25 km
comparison_25km <- div_comparison(PcellFDis_25km, McellFDis_25km, BcellFDis_25km, 25)

comparison_25km_plot <- comparison_25km$plot
(comparison_25km_plot <- comparison_25km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 0.3, y = 0.52, label = paste('Bird R² =', round(comparison_25km$r2_df$r2[2], 3)), size=5) + annotate("text", x = 0.3, y = 0.65, label = paste('Mammal R² =', round(comparison_25km$r2_df$r2[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_25km$m1, plot=T)
simulateResiduals(comparison_25km$m2, plot=T)


# 10 km
comparison_10km <- div_comparison(PcellFDis_10km, McellFDis_10km, BcellFDis_10km, 10)

comparison_10km_plot <- comparison_10km$plot
(comparison_10km_plot <- comparison_10km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 0.2, y = 0.43, label = paste('Bird R² =', round(comparison_10km$r2_df$r2[2], 3)), size=5) + annotate("text", x = 0.2, y = 0.52, label = paste('Mammal R² =', round(comparison_10km$r2_df$r2[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_10km$m1, plot=T)
simulateResiduals(comparison_10km$m2, plot=T)


# 5 km
comparison_5km <- div_comparison(PcellFDis_5km, McellFDis_5km, BcellFDis_5km, 5)

comparison_5km_plot <- comparison_5km$plot
(comparison_5km_plot <- comparison_5km_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 0.2, y = 0.42, label = paste('Bird R² =', round(comparison_5km$r2_df$r2[2], 3)), size=5) + annotate("text", x = 0.2, y = 0.52, label = paste('Mammal R² =', round(comparison_5km$r2_df$r2[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_5km$m1, plot=T)
simulateResiduals(comparison_5km$m2, plot=T)


# combined plot
(all_taxa_FDis_plots <- wrap_plots(comparison_5km_plot, comparison_10km_plot, comparison_25km_plot, comparison_50km_plot, comparison_75km_plot, comparison_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 15, 15, 15),legend.position='bottom'))

ggsave('all_taxa_FDis_plots.png', all_taxa_FDis_plots, path = figure_path, width = 13, height = 8, units = "in", dpi=1000)


#### GAMs ####

# 100km
comparison_100km_gam <- div_comparison_gam(PcellFDis_100km, McellFDis_100km, BcellFDis_100km, 100)

comparison_100km_gam_plot <- comparison_100km_gam$plot
(comparison_100km_gam_plot <- comparison_100km_gam_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 0.4, y = 0.55, label = paste('Bird D² =', round(comparison_100km_gam$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 0.4, y = 0.65, label = paste('Mammal D² =', round(comparison_100km_gam$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_100km_gam$m1, plot=T)
simulateResiduals(comparison_100km_gam$m2, plot=T)


# 75 km
comparison_75km_gam <- div_comparison_gam(PcellFDis_75km, McellFDis_75km, BcellFDis_75km, 75)

comparison_75km_gam_plot <- comparison_75km_gam$plot
(comparison_75km_gam_plot <- comparison_75km_gam_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 0.4, y = 0.47, label = paste('Bird D² =', round(comparison_75km_gam$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 0.4, y = 0.65, label = paste('Mammal D² =', round(comparison_75km_gam$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_75km_gam$m1, plot=T)
simulateResiduals(comparison_75km_gam$m2, plot=T)


# 50 km
comparison_50km_gam <- div_comparison_gam(PcellFDis_50km, McellFDis_50km, BcellFDis_50km, 50)

comparison_50km_gam_plot <- comparison_50km_gam$plot
(comparison_50km_gam_plot <- comparison_50km_gam_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) +   annotate("text", x = 0.38, y = 0.51, label = paste('Bird D² =', round(comparison_50km_gam$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 0.4, y = 0.7, label = paste('Mammal D² =', round(comparison_50km_gam$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_50km_gam$m1, plot=T)
simulateResiduals(comparison_50km_gam$m2, plot=T)


# 25 km
comparison_25km_gam <- div_comparison_gam(PcellFDis_25km, McellFDis_25km, BcellFDis_25km, 25)

comparison_25km_gam_plot <- comparison_25km_gam$plot
(comparison_25km_gam_plot <- comparison_25km_gam_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) + annotate("text", x = 0.25, y = 0.49, label = paste('Bird D² =', round(comparison_25km_gam$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 0.25, y = 0.63, label = paste('Mammal D² =', round(comparison_25km_gam$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_25km_gam$m1, plot=T)
simulateResiduals(comparison_25km_gam$m2, plot=T)


# 10 km
comparison_10km_gam <- div_comparison_gam(PcellFDis_10km, McellFDis_10km, BcellFDis_10km, 10)

comparison_10km_gam_plot <- comparison_10km_gam$plot
(comparison_10km_gam_plot <- comparison_10km_gam_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) + annotate("text", x = 0.15, y = 0.43, label = paste('Bird D² =', round(comparison_10km_gam$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 0.15, y = 0.6, label = paste('Mammal D² =', round(comparison_10km_gam$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_10km_gam$m1, plot=T)
simulateResiduals(comparison_10km_gam$m2, plot=T)


# 5 km
comparison_5km_gam <- div_comparison_gam(PcellFDis_5km, McellFDis_5km, BcellFDis_5km, 5)

comparison_5km_gam_plot <- comparison_5km_gam$plot
(comparison_5km_gam_plot <- comparison_5km_gam_plot + ylab('Frugivore FDis by cell') + xlab('Plant FDis by cell') + theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.line.y = element_blank()) + annotate("text", x = 0.15, y = 0.49, label = paste('Bird D² =', round(comparison_5km_gam$r2_df$dev_expl[2], 3)), size=5) + annotate("text", x = 0.2, y = 0.3, label = paste('Mammal D² =', round(comparison_5km_gam$r2_df$dev_expl[1], 3)), size=5) + theme(plot.title = element_text(face = "bold", hjust=0.5, size=16)))

simulateResiduals(comparison_5km_gam$m1, plot=T)
simulateResiduals(comparison_5km_gam$m2, plot=T)


# combined plot
(all_taxa_FDis_gam_plots <- wrap_plots(comparison_5km_gam_plot, comparison_10km_gam_plot, comparison_25km_gam_plot, comparison_50km_gam_plot, comparison_75km_gam_plot, comparison_100km_gam_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 15, 15, 15),legend.position='bottom'))

ggsave('all_taxa_FDis_gam_plots.png', all_taxa_FDis_plots, path = figure_path, width = 13, height = 8, units = "in", dpi=1000)

