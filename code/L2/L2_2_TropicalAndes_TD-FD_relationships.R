#title: "Relationships between taxonomic and functional diversity for plants, mammals, and birds"
#author: "Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Hazel J. Anderson, Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "This script compares taxonomic and functional diversity relationships for plants and frugivores for various spatial grains."
#data input: "fdis_frugivore_5km.rds", "fdis_mammal_5km.rds", "fdis_bird_5km.rds", "fdis_plant_5km.rds", "fdis_frugivore_10km.rds", "fdis_mammal_10km.rds", "fdis_bird_10km.rds", "fdis_plant_10km.rds", "fdis_frugivore_25km.rds", "fdis_mammal_25km.rds", "fdis_bird_25km.rds", "fdis_plant_25km.rds", "fdis_frugivore_50km.rds", "fdis_mammal_50km.rds", "fdis_bird_50km.rds", "fdis_plant_50km.rds", "fdis_frugivore_75km.rds", "fdis_mammal_75km.rds", "fdis_bird_75km.rds", "fdis_plant_75km.rds", "fdis_frugivore_100km.rds", "fdis_mammal_100km.rds", "fdis_bird_100km.rds", "fdis_plant_100km.rds"
#data ouput: "compare_BPFDis_100km.rds", "compare_MPFDis_100km.rds", "compare_BPFDis_75km.rds", "compare_MPFDis_75km.rds", "compare_BPFDis_50km.rds", "compare_MPFDis_50km.rds", "compare_BPFDis_25km.rds", "compare_MPFDis_25km.rds", "compare_BPFDis_10km.rds", "compare_MPFDis_10km.rds", "compare_BPFDis_5km.rds", "compare_MPFDis_5km.rds", "all_taxa_FDis_plots.png"
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
library(dplyr); library(ggplot2); library(smoothr); library(purrr); library(ggtrendline); library(ggpubr); library(tidyr); library(patchwork); library(cowplot)

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

fdis_frugivore_5km <- readRDS(file = file.path(output_path_L2,"fdis_frugivore_5km.rds"))
fdis_mammal_5km <- readRDS(file = file.path(output_path_L2,"fdis_mammal_5km.rds"))
fdis_bird_5km <- readRDS(file = file.path(output_path_L2,"fdis_bird_5km.rds"))
fdis_plant_5km <- readRDS(file = file.path(output_path_L2,"fdis_plant_5km.rds"))


# 10km
plant_cellRichness_10km <- read.csv(file.path(data_path_L2,"TropicalAndes_plantRichness_10km.csv"))
frugivore_cellRichness_10km <- read.csv(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_10km.csv"))
mammal_cellRichness_10km <- read.csv(file.path(data_path_L2,"TropicalAndes_mammalRichness_10km.csv"))
bird_cellRichness_10km <- read.csv(file.path(data_path_L2,"TropicalAndes_birdRichness_10km.csv"))

fdis_frugivore_10km <- readRDS(file = file.path(output_path_L2,"fdis_frugivore_10km.rds"))
fdis_mammal_10km <- readRDS(file = file.path(output_path_L2,"fdis_mammal_10km.rds"))
fdis_bird_10km <- readRDS(file = file.path(output_path_L2,"fdis_bird_10km.rds"))
fdis_plant_10km <- readRDS(file = file.path(output_path_L2,"fdis_plant_10km.rds"))


# 25km
plant_cellRichness_25km <- read.csv(file.path(data_path_L2,"TropicalAndes_plantRichness_25km.csv"))
frugivore_cellRichness_25km <- read.csv(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_25km.csv"))
mammal_cellRichness_25km <- read.csv(file.path(data_path_L2,"TropicalAndes_mammalRichness_25km.csv"))
bird_cellRichness_25km <- read.csv(file.path(data_path_L2,"TropicalAndes_birdRichness_25km.csv"))

fdis_frugivore_25km <- readRDS(file = file.path(output_path_L2,"fdis_frugivore_25km.rds"))
fdis_mammal_25km <- readRDS(file = file.path(output_path_L2,"fdis_mammal_25km.rds"))
fdis_bird_25km <- readRDS(file = file.path(output_path_L2,"fdis_bird_25km.rds"))
fdis_plant_25km <- readRDS(file = file.path(output_path_L2,"fdis_plant_25km.rds"))


# 50km
plant_cellRichness_50km <- read.csv(file.path(data_path_L2,"TropicalAndes_plantRichness_50km.csv"))
frugivore_cellRichness_50km <- read.csv(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_50km.csv"))
mammal_cellRichness_50km <- read.csv(file.path(data_path_L2,"TropicalAndes_mammalRichness_50km.csv"))
bird_cellRichness_50km <- read.csv(file.path(data_path_L2,"TropicalAndes_birdRichness_50km.csv"))

fdis_frugivore_50km <- readRDS(file = file.path(output_path_L2,"fdis_frugivore_50km.rds"))
fdis_mammal_50km <- readRDS(file = file.path(output_path_L2,"fdis_mammal_50km.rds"))
fdis_bird_50km <- readRDS(file = file.path(output_path_L2,"fdis_bird_50km.rds"))
fdis_plant_50km <- readRDS(file = file.path(output_path_L2,"fdis_plant_50km.rds"))


# 75km
plant_cellRichness_75km <- read.csv(file.path(data_path_L2,"TropicalAndes_plantRichness_75km.csv"))
frugivore_cellRichness_75km <- read.csv(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_75km.csv"))
mammal_cellRichness_75km <- read.csv(file.path(data_path_L2,"TropicalAndes_mammalRichness_75km.csv"))
bird_cellRichness_75km <- read.csv(file.path(data_path_L2,"TropicalAndes_birdRichness_75km.csv"))

fdis_frugivore_75km <- readRDS(file = file.path(output_path_L2,"fdis_frugivore_75km.rds"))
fdis_mammal_75km <- readRDS(file = file.path(output_path_L2,"fdis_mammal_75km.rds"))
fdis_bird_75km <- readRDS(file = file.path(output_path_L2,"fdis_bird_75km.rds"))
fdis_plant_75km <- readRDS(file = file.path(output_path_L2,"fdis_plant_75km.rds"))


# 100km
plant_cellRichness_100km <- read.csv(file.path(data_path_L2,"TropicalAndes_plantRichness_100km.csv"))
frugivore_cellRichness_100km <- read.csv(file.path(data_path_L2,"TropicalAndes_frugivoreRichness_100km.csv"))
mammal_cellRichness_100km <- read.csv(file.path(data_path_L2,"TropicalAndes_mammalRichness_100km.csv"))
bird_cellRichness_100km <- read.csv(file.path(data_path_L2,"TropicalAndes_birdRichness_100km.csv"))

fdis_frugivore_100km <- readRDS(file = file.path(output_path_L2,"fdis_frugivore_100km.rds"))
fdis_mammal_100km <- readRDS(file = file.path(output_path_L2,"fdis_mammal_100km.rds"))
fdis_bird_100km <- readRDS(file = file.path(output_path_L2,"fdis_bird_100km.rds"))
fdis_plant_100km <- readRDS(file = file.path(output_path_L2,"fdis_plant_100km.rds"))


#### fdis cleaning ####

frugivore_cellFDis_100km <- clean_fdis(fdis_frugivore_100km, 100)
mammal_cellFDis_100km <- clean_fdis(fdis_mammal_100km, 100)
bird_cellFDis_100km <- clean_fdis(fdis_bird_100km, 100)
plant_cellFDis_100km <- clean_fdis(fdis_plant_100km, 100)

frugivore_cellFDis_75km <- clean_fdis(fdis_frugivore_75km, 75)
mammal_cellFDis_75km <- clean_fdis(fdis_mammal_75km, 75)
bird_cellFDis_75km <- clean_fdis(fdis_bird_75km, 75)
plant_cellFDis_75km <- clean_fdis(fdis_plant_75km, 75)

frugivore_cellFDis_50km <- clean_fdis(fdis_frugivore_50km, 50)
mammal_cellFDis_50km <- clean_fdis(fdis_mammal_50km, 50)
bird_cellFDis_50km <- clean_fdis(fdis_bird_50km, 50)
plant_cellFDis_50km <- clean_fdis(fdis_plant_50km, 50)

frugivore_cellFDis_25km <- clean_fdis(fdis_frugivore_25km, 25)
mammal_cellFDis_25km <- clean_fdis(fdis_mammal_25km, 25)
bird_cellFDis_25km <- clean_fdis(fdis_bird_25km, 25)
plant_cellFDis_25km <- clean_fdis(fdis_plant_25km, 25)

frugivore_cellFDis_10km <- clean_fdis(fdis_frugivore_10km, 10)
mammal_cellFDis_10km <- clean_fdis(fdis_mammal_10km, 10)
bird_cellFDis_10km <- clean_fdis(fdis_bird_10km, 10)
plant_cellFDis_10km <- clean_fdis(fdis_plant_10km, 10)

frugivore_cellFDis_5km <- clean_fdis(fdis_frugivore_5km, 5)
mammal_cellFDis_5km <- clean_fdis(fdis_mammal_5km, 5)
bird_cellFDis_5km <- clean_fdis(fdis_bird_5km, 5)
plant_cellFDis_5km <- clean_fdis(fdis_plant_5km, 5)


#### within-guild TD-FD comparison ####

# plants

# 100km
plants_100km <- div_comparison2(plant_cellRichness_100km, plant_cellFDis_100km, 'plant', 100)
plants_100km_plot <- plants_100km$plot
plants_100km_plot <- plants_100km_plot + xlab('Plant richness by cell') + ylab('Plant FDis by cell') + labs(subtitle = paste('r² =', plants_100km$trend$R.squared))


# 75km
plants_75km <- div_comparison2(plant_cellRichness_75km, plant_cellFDis_75km, 'plant', 75)
plants_75km_plot <- plants_75km$plot
plants_75km_plot <- plants_75km_plot + xlab('Plant richness by cell') + ylab('Plant FDis by cell') + labs(subtitle = paste('r² =', plants_75km$trend$R.squared))  


# 50km
plants_50km <- div_comparison2(plant_cellRichness_50km, plant_cellFDis_50km, 'plant', 50)
plants_50km_plot <- plants_50km$plot
plants_50km_plot <- plants_50km_plot + xlab('Plant richness by cell') + ylab('Plant FDis by cell') + labs(subtitle = paste('r² =', plants_50km$trend$R.squared))


# 25km
plants_25km <- div_comparison2(plant_cellRichness_25km, plant_cellFDis_25km, 'plant', 25)
plants_25km_plot <- plants_25km$plot
plants_25km_plot <- plants_25km_plot + xlab('Plant richness by cell') + ylab('Plant FDis by cell') + labs(subtitle = paste('r² =', plants_25km$trend$R.squared))


# 10km
plants_10km <- div_comparison2(plant_cellRichness_10km, plant_cellFDis_10km, 'plant', 10)
plants_10km_plot <- plants_10km$plot
plants_10km_plot <- plants_10km_plot + xlab('Plant richness by cell') + ylab('Plant FDis by cell') + labs(subtitle = paste('r² =', plants_10km$trend$R.squared)) 


# 5km
plants_5km <- div_comparison2(plant_cellRichness_5km, plant_cellFDis_5km, 'plant', 5)
plants_5km_plot <- plants_5km$plot
plants_5km_plot <- plants_5km_plot + xlab('Plant richness by cell') + ylab('Plant FDis by cell') + labs(subtitle = paste('r² =', plants_5km$trend$R.squared))


# combined plot
all_plant_R_FD_plots <- wrap_plots(plants_5km_plot, plants_10km_plot, plants_25km_plot, plants_50km_plot, plants_75km_plot, plants_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 20, 20, 20),legend.position='bottom')

all_plant_R_FD_plots

ggsave('plantRichness-FDis_plots.png', all_plant_R_FD_plots, path = figure_path, width = 14, height = 8, units = "in", dpi=1000)


# mammals

# 100km
mammals_100km <- div_comparison2(mammal_cellRichness_100km, mammal_cellFDis_100km, 'mammal', 100)
mammals_100km_plot <- mammals_100km$plot
mammals_100km_plot <- mammals_100km_plot + xlab('Mammal richness by cell') + ylab('Mammal FDis by cell') + labs(subtitle = paste('r² =', mammals_100km$trend$R.squared))


# 75km
mammals_75km <- div_comparison2(mammal_cellRichness_75km, mammal_cellFDis_75km, 'mammal', 75)
mammals_75km_plot <- mammals_75km$plot
mammals_75km_plot <- mammals_75km_plot + xlab('Mammal richness by cell') + ylab('Mammal FDis by cell') + labs(subtitle = paste('r² =', mammals_75km$trend$R.squared))


# 50km
mammals_50km <- div_comparison2(mammal_cellRichness_50km, mammal_cellFDis_50km, 'mammal', 50)
mammals_50km_plot <- mammals_50km$plot
mammals_50km_plot <- mammals_50km_plot + xlab('Mammal richness by cell') + ylab('Mammal FDis by cell') + labs(subtitle = paste('r² =', mammals_50km$trend$R.squared))


# 25km
mammals_25km <- div_comparison2(mammal_cellRichness_25km, mammal_cellFDis_25km, 'mammal', 25)
mammals_25km_plot <- mammals_25km$plot
mammals_25km_plot <- mammals_25km_plot + xlab('Mammal richness by cell') + ylab('Mammal FDis by cell') + labs(subtitle = paste('r² =', mammals_25km$trend$R.squared))


# 10km
mammals_10km <- div_comparison2(mammal_cellRichness_10km, mammal_cellFDis_10km, 'mammal', 10)
mammals_10km_plot <- mammals_10km$plot
mammals_10km_plot <- mammals_10km_plot + xlab('Mammal richness by cell') + ylab('Mammal FDis by cell') + labs(subtitle = paste('r² =', mammals_10km$trend$R.squared)) 


# 5km
mammals_5km <- div_comparison2(mammal_cellRichness_5km, mammal_cellFDis_5km, 'mammal', 5)
mammals_5km_plot <- mammals_5km$plot
mammals_5km_plot <- mammals_5km_plot + xlab('Mammal richness by cell') + ylab('Mammal FDis by cell') + labs(subtitle = paste('r² =', mammals_5km$trend$R.squared)) 


# combined plot
all_mammal_R_FD_plots <- wrap_plots(mammals_5km_plot, mammals_10km_plot, mammals_25km_plot, mammals_50km_plot, mammals_75km_plot, mammals_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 20, 20, 20),legend.position='bottom')

all_mammal_R_FD_plots

ggsave('mammalRichness-FDis_plots.png', all_mammal_R_FD_plots, path = figure_path, width = 14, height = 8, units = "in", dpi=1000)


# birds

# 100km
birds_100km <- div_comparison2(bird_cellRichness_100km, bird_cellFDis_100km, 'bird', 100)
birds_100km_plot <- birds_100km$plot
birds_100km_plot <- birds_100km_plot + xlab('Bird richness by cell') + ylab('Bird FDis by cell') + labs(subtitle = paste('r² =', birds_100km$trend$R.squared))


# 75km
birds_75km <- div_comparison2(bird_cellRichness_75km, bird_cellFDis_75km, 'bird', 75)
birds_75km_plot <- birds_75km$plot
birds_75km_plot <- birds_75km_plot + xlab('Bird richness by cell') + ylab('Bird FDis by cell') + labs(subtitle = paste('r² =', birds_75km$trend$R.squared)) 


# 50km
birds_50km <- div_comparison2(bird_cellRichness_50km, bird_cellFDis_50km, 'bird', 50)
birds_50km_plot <- birds_50km$plot
birds_50km_plot <- birds_50km_plot + xlab('Bird richness by cell') + ylab('Bird FDis by cell') + labs(subtitle = paste('r² =', birds_50km$trend$R.squared))


# 25km
birds_25km <- div_comparison2(bird_cellRichness_25km, bird_cellFDis_25km, 'bird', 25)
birds_25km_plot <- birds_25km$plot
birds_25km_plot <- birds_25km_plot + xlab('Bird richness by cell') + ylab('Bird FDis by cell') + labs(subtitle = paste('r² =', birds_25km$trend$R.squared)) 


# 10km
birds_10km <- div_comparison2(bird_cellRichness_10km, bird_cellFDis_10km, 'bird', 10)
birds_10km_plot <- birds_10km$plot
birds_10km_plot <- birds_10km_plot + xlab('Bird richness by cell') + ylab('Bird FDis by cell') + labs(subtitle = paste('r² =', birds_10km$trend$R.squared))


# 5km
birds_5km <- div_comparison2(bird_cellRichness_5km, bird_cellFDis_5km, 'bird', 5)
birds_5km_plot <- birds_5km$plot
birds_5km_plot <- birds_5km_plot + xlab('Bird richness by cell') + ylab('Bird FDis by cell') + labs(subtitle = paste('r² =', birds_5km$trend$R.squared))


# combined plot
all_bird_R_FD_plots <- wrap_plots(birds_5km_plot, birds_10km_plot, birds_25km_plot, birds_50km_plot, birds_75km_plot, birds_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 20, 20, 20),legend.position='bottom')

all_bird_R_FD_plots

ggsave('birdRichness-FDis_plots.png', all_bird_R_FD_plots, path = figure_path, width = 14, height = 8, units = "in", dpi=1000)


#### comparison of plant richness-frugivore FDis ####

# 100km
R_FD_comparison_100km <- div_comparison3(plant_cellRichness_100km, mammal_cellFDis_100km, bird_cellFDis_100km, 100)

# mammals
PR_MFD_comparison_100km_plot <- R_FD_comparison_100km$plot1
PR_MFD_comparison_100km_plot <- PR_MFD_comparison_100km_plot + ylab('Mammal FDis by cell') + xlab('Plant richness by cell') + labs(subtitle = paste('r² =', R_FD_comparison_100km$mammal$R.squared))

saveRDS(R_FD_comparison_100km$mammal, file = file.path(output_path_L2,"compare_Prichness-MFDis_100km.rds"))

# birds
PR_BFD_comparison_100km_plot <- R_FD_comparison_100km$plot2
PR_BFD_comparison_100km_plot <- PR_BFD_comparison_100km_plot + ylab('Bird FDis by cell') + xlab('Plant richness by cell') + labs(subtitle = paste('r² =', R_FD_comparison_100km$bird$R.squared))

saveRDS(R_FD_comparison_100km$bird, file = file.path(output_path_L2,"compare_Prichness-BFDis_100km.rds"))


# 75 km
R_FD_comparison_75km <- div_comparison3(plant_cellRichness_75km, mammal_cellFDis_75km, bird_cellFDis_75km, 75)

# mammals
PR_MFD_comparison_75km_plot <- R_FD_comparison_75km$plot1
PR_MFD_comparison_75km_plot <- PR_MFD_comparison_75km_plot + ylab('Mammal FDis by cell') + xlab('Plant richness by cell') + labs(subtitle = paste('r² =', R_FD_comparison_75km$mammal$R.squared))

saveRDS(R_FD_comparison_75km$mammal, file = file.path(output_path_L2,"compare_Prichness-MFDis_75km.rds"))

# birds
PR_BFD_comparison_75km_plot <- R_FD_comparison_75km$plot2
PR_BFD_comparison_75km_plot <- PR_BFD_comparison_75km_plot + ylab('Bird FDis by cell') + xlab('Plant richness by cell') + labs(subtitle = paste('r² =', R_FD_comparison_75km$bird$R.squared))

saveRDS(R_FD_comparison_75km$bird, file = file.path(output_path_L2,"compare_Prichness-BFDis_75km.rds"))


# 50 km
R_FD_comparison_50km <- div_comparison3(plant_cellRichness_50km, mammal_cellFDis_50km, bird_cellFDis_50km, 50)

# mammals
PR_MFD_comparison_50km_plot <- R_FD_comparison_50km$plot1
PR_MFD_comparison_50km_plot <- PR_MFD_comparison_50km_plot + ylab('Mammal FDis by cell') + xlab('Plant richness by cell') + labs(subtitle = paste('r² =', R_FD_comparison_50km$mammal$R.squared))

saveRDS(R_FD_comparison_50km$mammal, file = file.path(output_path_L2,"compare_Prichness-MFDis_50km.rds"))

# birds
PR_BFD_comparison_50km_plot <- R_FD_comparison_50km$plot2
PR_BFD_comparison_50km_plot <- PR_BFD_comparison_50km_plot + ylab('Bird FDis by cell') + xlab('Plant richness by cell') + labs(subtitle = paste('r² =', R_FD_comparison_50km$bird$R.squared))

saveRDS(R_FD_comparison_50km$bird, file = file.path(output_path_L2,"compare_Prichness-BFDis_50km.rds"))


# 25 km
R_FD_comparison_25km <- div_comparison3(plant_cellRichness_25km, mammal_cellFDis_25km, bird_cellFDis_25km, 25)

# mammals
PR_MFD_comparison_25km_plot <- R_FD_comparison_25km$plot1
PR_MFD_comparison_25km_plot <- PR_MFD_comparison_25km_plot + ylab('Mammal FDis by cell') + xlab('Plant richness by cell') + labs(subtitle = paste('r² =', R_FD_comparison_25km$mammal$R.squared))

saveRDS(R_FD_comparison_25km$mammal, file = file.path(output_path_L2,"compare_Prichness-MFDis_25km.rds"))

# birds
PR_BFD_comparison_25km_plot <- R_FD_comparison_25km$plot2
PR_BFD_comparison_25km_plot <- PR_BFD_comparison_25km_plot + ylab('Bird FDis by cell') + xlab('Plant richness by cell') + labs(subtitle = paste('r² =', R_FD_comparison_25km$bird$R.squared))

saveRDS(R_FD_comparison_25km$bird, file = file.path(output_path_L2,"compare_Prichness-BFDis_25km.rds"))


# 10 km
R_FD_comparison_10km <- div_comparison3(plant_cellRichness_10km, mammal_cellFDis_10km, bird_cellFDis_10km, 10)

# mammals
PR_MFD_comparison_10km_plot <- R_FD_comparison_10km$plot1
PR_MFD_comparison_10km_plot <- PR_MFD_comparison_10km_plot + ylab('Mammal FDis by cell') + xlab('Plant richness by cell') + labs(subtitle = paste('r² =', R_FD_comparison_10km$mammal$R.squared))

saveRDS(R_FD_comparison_10km$mammal, file = file.path(output_path_L2,"compare_Prichness-MFDis_10km.rds"))

# birds
PR_BFD_comparison_10km_plot <- R_FD_comparison_10km$plot2
PR_BFD_comparison_10km_plot <- PR_BFD_comparison_10km_plot + ylab('Bird FDis by cell') + xlab('Plant richness by cell') + labs(subtitle = paste('r² =', R_FD_comparison_10km$bird$R.squared))

saveRDS(R_FD_comparison_10km$bird, file = file.path(output_path_L2,"compare_Prichness-BFDis_10km.rds"))


# 5 km
R_FD_comparison_5km <- div_comparison3(plant_cellRichness_5km, mammal_cellFDis_5km, bird_cellFDis_5km, 5)

# mammals
PR_MFD_comparison_5km_plot <- R_FD_comparison_5km$plot1
PR_MFD_comparison_5km_plot <- PR_MFD_comparison_5km_plot + ylab('Mammal FDis by cell') + xlab('Plant richness by cell') + labs(subtitle = paste('r² =', R_FD_comparison_5km$mammal$R.squared))

saveRDS(R_FD_comparison_5km$mammal, file = file.path(output_path_L2,"compare_Prichness-MFDis_5km.rds"))

# birds
PR_BFD_comparison_5km_plot <- R_FD_comparison_5km$plot2
PR_BFD_comparison_5km_plot <- PR_BFD_comparison_5km_plot + ylab('Bird FDis by cell') + xlab('Plant richness by cell') + labs(subtitle = paste('r² =', R_FD_comparison_5km$bird$R.squared))

saveRDS(R_FD_comparison_5km$bird, file = file.path(output_path_L2,"compare_Prichness-BFDis_5km.rds"))


# combined plots

# mammals
all_mammal_R_FD_plots <- wrap_plots(PR_MFD_comparison_5km_plot, PR_MFD_comparison_10km_plot, PR_MFD_comparison_25km_plot, PR_MFD_comparison_50km_plot, PR_MFD_comparison_75km_plot, PR_MFD_comparison_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 20, 20, 20),legend.position='bottom')

all_mammal_R_FD_plots

ggsave('plantRichness-mammalFDis_plots.png', all_mammal_R_FD_plots, path = figure_path, width = 14, height = 8, units = "in", dpi=1000)

# birds
all_bird_R_FD_plots <- wrap_plots(PR_BFD_comparison_5km_plot, PR_BFD_comparison_10km_plot, PR_BFD_comparison_25km_plot, PR_BFD_comparison_50km_plot, PR_BFD_comparison_75km_plot, PR_BFD_comparison_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 20, 20, 20),legend.position='bottom')

all_bird_R_FD_plots

ggsave('plantRichness-birdFDis_plots.png', all_bird_R_FD_plots, path = figure_path, width = 14, height = 8, units = "in", dpi=1000)

# both together
plantRichness_frugFDis_plots <- plot_grid(all_mammal_R_FD_plots, all_bird_R_FD_plots, labels = c('A', 'B'), nrow=2)
plantRichness_frugFDis_plots
ggsave('plantRichness-frugFDis_plots.png', plantRichness_frugFDis_plots, path = figure_path, width = 14, height = 12, units = "in", dpi=1000)


#### comparison of plant FDis-frugivore richness ####

# 100km
FD_R_comparison_100km <- div_comparison3(plant_cellFDis_100km, mammal_cellRichness_100km, bird_cellRichness_100km, 100)

# mammals
PFD_MR_100km <- FD_R_comparison_100km$plot1
PFD_MR_100km <- PFD_MR_100km + ylab('Plant FDis by cell') + xlab('Mammal richness by cell') + labs(subtitle = paste('r² =', FD_R_comparison_100km$mammal$R.squared))

saveRDS(FD_R_comparison_100km$mammal, file = file.path(output_path_L2,"compare_PFDis-Mrichness_100km.rds"))

# birds
PFD_BR_100km <- FD_R_comparison_100km$plot2
PFD_BR_100km <- PFD_BR_100km + ylab('Plant FDis by cell') + xlab('Bird richness by cell') + labs(subtitle = paste('r² =', FD_R_comparison_100km$bird$R.squared))

saveRDS(FD_R_comparison_100km$bird, file = file.path(output_path_L2,"compare_PFDis-Brichness_100km.rds"))


# 75 km
FD_R_comparison_75km <- div_comparison3(plant_cellFDis_75km, mammal_cellRichness_75km, bird_cellRichness_75km, 75)

# mammals
PFD_MR_75km <- FD_R_comparison_75km$plot1
PFD_MR_75km <- PFD_MR_75km + ylab('Plant FDis by cell') + xlab('Mammal richness by cell') + labs(subtitle = paste('r² =', FD_R_comparison_75km$mammal$R.squared))

saveRDS(FD_R_comparison_75km$mammal, file = file.path(output_path_L2,"compare_PFDis-Mrichness_75km.rds"))

# birds
PFD_BR_75km <- FD_R_comparison_75km$plot2
PFD_BR_75km <- PFD_BR_75km + ylab('Plant FDis by cell') + xlab('Bird richness by cell') + labs(subtitle = paste('r² =', FD_R_comparison_75km$bird$R.squared)) 

saveRDS(FD_R_comparison_75km$bird, file = file.path(output_path_L2,"compare_PFDis-BRichness_75km.rds"))


# 50 km
FD_R_comparison_50km <- div_comparison3(plant_cellFDis_50km, mammal_cellRichness_50km, bird_cellRichness_50km, 50)

# mammals
PFD_MR_50km <- FD_R_comparison_50km$plot1
PFD_MR_50km <- PFD_MR_50km + ylab('Plant FDis by cell') + xlab('Mammal richness by cell') + labs(subtitle = paste('r² =', FD_R_comparison_50km$mammal$R.squared))

saveRDS(FD_R_comparison_50km$mammal, file = file.path(output_path_L2,"compare_PFDis-Mrichness_50km.rds"))

# birds
PFD_BR_50km <- FD_R_comparison_50km$plot2
PFD_BR_50km <- PFD_BR_50km + ylab('Plant FDis by cell') + xlab('Bird richness by cell') + labs(subtitle = paste('r² =', FD_R_comparison_50km$bird$R.squared))

saveRDS(FD_R_comparison_50km$bird, file = file.path(output_path_L2,"compare_PFDis-Brichness_50km.rds"))


# 25 km
FD_R_comparison_25km <- div_comparison3(plant_cellFDis_25km, mammal_cellRichness_25km, bird_cellRichness_25km, 25)

# mammals
PFD_MR_25km <- FD_R_comparison_25km$plot1
PFD_MR_25km <- PFD_MR_25km + ylab('Plant FDis by cell') + xlab('Mammal richness by cell') + labs(subtitle = paste('r² =', FD_R_comparison_25km$mammal$R.squared))

saveRDS(FD_R_comparison_25km$mammal, file = file.path(output_path_L2,"compare_PFDis-Mrichness_25km.rds"))

# birds
PFD_BR_25km <- FD_R_comparison_25km$plot2
PFD_BR_25km <- PFD_BR_25km + ylab('Plant FDis by cell') + xlab('Bird richness by cell') + labs(subtitle = paste('r² =', FD_R_comparison_25km$bird$R.squared)) 

saveRDS(FD_R_comparison_25km$bird, file = file.path(output_path_L2,"compare_PFDis-Brichness_25km.rds"))


# 10 km
FD_R_comparison_10km <- div_comparison3(plant_cellFDis_10km, mammal_cellRichness_10km, bird_cellRichness_10km, 10)

# mammals
PFD_MR_10km <- FD_R_comparison_10km$plot1
PFD_MR_10km <- PFD_MR_10km + ylab('Plant FDis by cell') + xlab('Mammal richness by cell') + labs(subtitle = paste('r² =', FD_R_comparison_10km$mammal$R.squared))

saveRDS(FD_R_comparison_10km$mammal, file = file.path(output_path_L2,"compare_PFDis-Mrichness_10km.rds"))

# birds
PFD_BR_10km <- FD_R_comparison_10km$plot2
PFD_BR_10km <- PFD_BR_10km + ylab('Plant FDis by cell') + xlab('Bird richness by cell') + labs(subtitle = paste('r² =', FD_R_comparison_10km$bird$R.squared))

saveRDS(FD_R_comparison_10km$bird, file = file.path(output_path_L2,"compare_PFDis-Brichness_10km.rds"))


# 5 km
FD_R_comparison_5km <- div_comparison3(plant_cellFDis_5km, mammal_cellRichness_5km, bird_cellRichness_5km, 5)

# mammals
PFD_MR_5km <- FD_R_comparison_5km$plot1
PFD_MR_5km <- PFD_MR_5km + ylab('Plant FDis by cell') + xlab('Mammal richness by cell') + labs(subtitle = paste('r² =', FD_R_comparison_5km$mammal$R.squared)) 

saveRDS(FD_R_comparison_5km$mammal, file = file.path(output_path_L2,"compare_PFDis-Mrichness_5km.rds"))

# birds
PFD_BR_5km <- FD_R_comparison_5km$plot2
PFD_BR_5km <- PFD_BR_5km + ylab('Plant FDis by cell') + xlab('Bird richness by cell') + labs(subtitle = paste('r² =', FD_R_comparison_5km$bird$R.squared))

saveRDS(FD_R_comparison_5km$bird, file = file.path(output_path_L2,"compare_PFDis-Brichness5km.rds"))


# combined plots

# mammals
all_mammal_FD_R_plots <- wrap_plots(PFD_MR_5km, PFD_MR_10km, PFD_MR_25km, PFD_MR_50km, PFD_MR_75km, PFD_MR_100km, ncol=3, nrow=2) + plot_layout( axis_titles = 'collect') & theme(plot.margin = margin(5, 20, 20, 20), legend.position='bottom')

all_mammal_FD_R_plots

ggsave('mammalRichness_plantFDis_plots.png', all_mammal_FD_R_plots, path = figure_path, width = 14, height = 8, units = "in", dpi=1000)

# birds
all_bird_FD_R_plots <- wrap_plots(PFD_BR_5km, PFD_BR_10km, PFD_BR_25km, PFD_BR_50km, PFD_BR_75km, PFD_BR_100km, ncol=3, nrow=2) + plot_layout( axis_titles = 'collect') & theme(plot.margin = margin(5, 20, 20, 20), legend.position='bottom')

all_bird_FD_R_plots

ggsave('birdRichness_plantFDis_plots.png', all_bird_FD_R_plots, path = figure_path, width = 14, height = 8, units = "in", dpi=1000)

# both together
frugRichness_plantFDis_plots <- plot_grid(all_mammal_FD_R_plots, all_bird_FD_R_plots, labels = c('A', 'B'), nrow=2)
frugRichness_plantFDis_plots
ggsave('frugRichness_plantFDis_plots.png', frugRichness_plantFDis_plots, path = figure_path, width = 14, height = 12, units = "in", dpi=1000)
