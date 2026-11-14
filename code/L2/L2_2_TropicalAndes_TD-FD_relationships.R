#title: "Relationships between taxonomic and functional diversity for plants, mammals, and birds"
#author: "Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Hazel J. Anderson, Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "This script compares taxonomic and functional diversity relationships for plants and frugivores for various spatial grains."
#data input: "fdis_frugivore_5km.rds", "fdis_mammal_5km.rds", "fdis_bird_5km.rds", "fdis_plant_5km.rds", "fdis_frugivore_10km.rds", "fdis_mammal_10km.rds", "fdis_bird_10km.rds", "fdis_plant_10km.rds", "fdis_frugivore_25km.rds", "fdis_mammal_25km.rds", "fdis_bird_25km.rds", "fdis_plant_25km.rds", "fdis_frugivore_50km.rds", "fdis_mammal_50km.rds", "fdis_bird_50km.rds", "fdis_plant_50km.rds", "fdis_frugivore_75km.rds", "fdis_mammal_75km.rds", "fdis_bird_75km.rds", "fdis_plant_75km.rds", "fdis_frugivore_100km.rds", "fdis_mammal_100km.rds", "fdis_bird_100km.rds", "fdis_plant_100km.rds"
#data ouput: "compare_BPFDis_100km.rds", "compare_MPFDis_100km.rds", "compare_BPFDis_75km.rds", "compare_MPFDis_75km.rds", "compare_BPFDis_50km.rds", "compare_MPFDis_50km.rds", "compare_BPFDis_25km.rds", "compare_MPFDis_25km.rds", "compare_BPFDis_10km.rds", "compare_MPFDis_10km.rds", "compare_BPFDis_5km.rds", "compare_MPFDis_5km.rds", "all_taxa_FDis_plots.png"
#date: "2024-05-16; 2025-12-15"


# load required packages
library(dplyr)
library(ggplot2)
library(smoothr)
library(purrr)
library(ggtrendline)
library(ggpubr)
library(tidyr)
library(patchwork)
library(mgcv)
library(DHARMa)
library(sf)
library(gstat)
library(sp)
library(cowplot)


# load functions
source("C:/GitHub_projects/neotropical_plants/code/Functions.R")

#### read in data (with obs cutoff) ####

# set file paths
filtered_data_path_L2 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2/filtered_data')
filtered_output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2/filtered_data')
filtered_data_figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures/filtered_data')


# set cutoff
cutoff_obs <- 20


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


# FD data

# 5km
mammal_cutoff_cell_FD_5km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("mammal_", cutoff_obs, "_cell_FD_5km.rds")
  )
)
plant_cutoff_cell_FD_5km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("plant_", cutoff_obs, "_cell_FD_5km.rds")
  )
)
bird_cutoff_cell_FD_5km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("bird_", cutoff_obs, "_cell_FD_5km.rds")
  )
)


# 10km
mammal_cutoff_cell_FD_10km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("mammal_", cutoff_obs, "_cell_FD_10km.rds")
  )
)
plant_cutoff_cell_FD_10km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("plant_", cutoff_obs, "_cell_FD_10km.rds")
  )
)
bird_cutoff_cell_FD_10km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("bird_", cutoff_obs, "_cell_FD_10km.rds")
  )
)


# 25km
mammal_cutoff_cell_FD_25km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("mammal_", cutoff_obs, "_cell_FD_25km.rds")
  )
)
plant_cutoff_cell_FD_25km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("plant_", cutoff_obs, "_cell_FD_25km.rds")
  )
)
bird_cutoff_cell_FD_25km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("bird_", cutoff_obs, "_cell_FD_25km.rds")
  )
)


# 50km
mammal_cutoff_cell_FD_50km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("mammal_", cutoff_obs, "_cell_FD_50km.rds")
  )
)
plant_cutoff_cell_FD_50km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("plant_", cutoff_obs, "_cell_FD_50km.rds")
  )
)
bird_cutoff_cell_FD_50km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("bird_", cutoff_obs, "_cell_FD_50km.rds")
  )
)


# 75km
mammal_cutoff_cell_FD_75km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("mammal_", cutoff_obs, "_cell_FD_75km.rds")
  )
)
plant_cutoff_cell_FD_75km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("plant_", cutoff_obs, "_cell_FD_75km.rds")
  )
)
bird_cutoff_cell_FD_75km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("bird_", cutoff_obs, "_cell_FD_75km.rds")
  )
)


# 100km
mammal_cutoff_cell_FD_100km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("mammal_", cutoff_obs, "_cell_FD_100km.rds")
  )
)
plant_cutoff_cell_FD_100km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("plant_", cutoff_obs, "_cell_FD_100km.rds")
  )
)
bird_cutoff_cell_FD_100km <- readRDS(
  file = file.path(
    filtered_data_path_L2,
    paste0("bird_", cutoff_obs, "_cell_FD_100km.rds")
  )
)

#### within-guild TD-FD comparison ####

# plants

# 100km
plants_100km <- div_comparison2(plant_cutoff_cell_TD_100km, plant_cutoff_cell_FD_100km, 'plant', 100)
plants_100km_plot <- plants_100km$plot
(plants_100km_plot <- plants_100km_plot + 
  xlab('Plant richness by cell') + 
  ylab('Plant FDis by cell') + 
  labs(subtitle = paste('D² =', round(plants_100km$r2_df$dev_expl, 3))) + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
  theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 75km
plants_75km <- div_comparison2(plant_cutoff_cell_TD_75km, plant_cutoff_cell_FD_75km, 'plant', 75)
plants_75km_plot <- plants_75km$plot
(plants_75km_plot <- plants_75km_plot + 
    xlab('Plant richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(plants_75km$r2_df$dev_expl, 3))) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    ) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 50km
plants_50km <- div_comparison2(plant_cutoff_cell_TD_50km, plant_cutoff_cell_FD_50km, 'plant', 50)
plants_50km_plot <- plants_50km$plot
(plants_50km_plot <- plants_50km_plot + 
    xlab('Plant richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(plants_50km$r2_df$dev_expl, 3))) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 25km
plants_25km <- div_comparison2(plant_cutoff_cell_TD_25km, plant_cutoff_cell_FD_25km, 'plant', 25)
plants_25km_plot <- plants_25km$plot
(plants_25km_plot <- plants_25km_plot + 
    xlab('Plant richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(plants_25km$r2_df$dev_expl, 3))) + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
    ) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 10km
plants_10km <- div_comparison2(plant_cutoff_cell_TD_10km, plant_cutoff_cell_FD_10km, 'plant', 10)
plants_10km_plot <- plants_10km$plot
(plants_10km_plot <- plants_10km_plot + 
    xlab('Plant richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(plants_10km$r2_df$dev_expl, 3))) + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
    ) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 5km
plants_5km <- div_comparison2(plant_cutoff_cell_TD_5km, plant_cutoff_cell_FD_5km, 'plant', 5)
plants_5km_plot <- plants_5km$plot
(plants_5km_plot <- plants_5km_plot + 
    xlab('Plant richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(plants_5km$r2_df$dev_expl, 3))) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
    ) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# combined plot
all_plant_R_FD_plots <- wrap_plots(plants_5km_plot, plants_10km_plot, plants_25km_plot, plants_50km_plot, plants_75km_plot, plants_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 15, 15, 15),legend.position='bottom')

all_plant_R_FD_plots

ggsave('plantRichness-FDis_plots.png', all_plant_R_FD_plots, path = filtered_data_figure_path, width = 14, height = 8, units = "in", dpi=1000)


# mammals

# 100km
mammals_100km <- div_comparison2(mammal_cutoff_cell_TD_100km, mammal_cutoff_cell_FD_100km, 'mammal', 100)
mammals_100km_plot <- mammals_100km$plot
(mammals_100km_plot <- mammals_100km_plot + 
    xlab('Mammal richness by cell') + 
    ylab('Mammal FDis by cell') + 
    labs(subtitle = paste('D² =', round(mammals_100km$r2_df$dev_expl, 3))) + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 75km
mammals_75km <- div_comparison2(mammal_cutoff_cell_TD_75km, mammal_cutoff_cell_FD_75km, 'mammal', 75)
mammals_75km_plot <- mammals_75km$plot
(mammals_75km_plot <- mammals_75km_plot + 
    xlab('Mammal richness by cell') + 
    ylab('Mammal FDis by cell') + 
    labs(subtitle = paste('D² =', round(mammals_75km$r2_df$dev_expl, 3))) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    ) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 50km
mammals_50km <- div_comparison2(mammal_cutoff_cell_TD_50km, mammal_cutoff_cell_FD_50km, 'mammal', 50)
mammals_50km_plot <- mammals_50km$plot
(mammals_50km_plot <- mammals_50km_plot + 
    xlab('Mammal richness by cell') + 
    ylab('Mammal FDis by cell') + 
    labs(subtitle = paste('D² =', round(mammals_50km$r2_df$dev_expl, 3))) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 25km
mammals_25km <- div_comparison2(mammal_cutoff_cell_TD_25km, mammal_cutoff_cell_FD_25km, 'mammal', 25)
mammals_25km_plot <- mammals_25km$plot
(mammals_25km_plot <- mammals_25km_plot + 
    xlab('Mammal richness by cell') + 
    ylab('Mammal FDis by cell') + 
    labs(subtitle = paste('D² =', round(mammals_25km$r2_df$dev_expl, 3))) + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
    ) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 10km
mammals_10km <- div_comparison2(mammal_cutoff_cell_TD_10km, mammal_cutoff_cell_FD_10km, 'mammal', 10)
mammals_10km_plot <- mammals_10km$plot
(mammals_10km_plot <- mammals_10km_plot + 
    xlab('Mammal richness by cell') + 
    ylab('Mammal FDis by cell') + 
    labs(subtitle = paste('D² =', round(mammals_10km$r2_df$dev_expl, 3))) + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
    ) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 5km
mammals_5km <- div_comparison2(mammal_cutoff_cell_TD_5km, mammal_cutoff_cell_FD_5km, 'mammal', 5)
mammals_5km_plot <- mammals_5km$plot
(mammals_5km_plot <- mammals_5km_plot + 
    xlab('Mammal richness by cell') + 
    ylab('Mammal FDis by cell') + 
    labs(subtitle = paste('D² =', round(mammals_5km$r2_df$dev_expl, 3))) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
    ) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# combined plot
all_mammal_R_FD_plots <- wrap_plots(mammals_5km_plot, mammals_10km_plot, mammals_25km_plot, mammals_50km_plot, mammals_75km_plot, mammals_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 15, 15, 15),legend.position='bottom')

all_mammal_R_FD_plots

ggsave('mammalRichness-FDis_plots.png', all_mammal_R_FD_plots, path = filtered_data_figure_path, width = 14, height = 8, units = "in", dpi=1000)


# birds

# 100km
birds_100km <- div_comparison2(bird_cutoff_cell_TD_100km, bird_cutoff_cell_FD_100km, 'bird', 100)
birds_100km_plot <- birds_100km$plot
(birds_100km_plot <- birds_100km_plot + 
    xlab('Bird richness by cell') + 
    ylab('Bird FDis by cell') + 
    labs(subtitle = paste('D² =', round(birds_100km$r2_df$dev_expl, 3))) + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 75km
birds_75km <- div_comparison2(bird_cutoff_cell_TD_75km, bird_cutoff_cell_FD_75km, 'bird', 75)
birds_75km_plot <- birds_75km$plot
(birds_75km_plot <- birds_75km_plot + 
    xlab('Bird richness by cell') + 
    ylab('Bird FDis by cell') + 
    labs(subtitle = paste('D² =', round(birds_75km$r2_df$dev_expl, 3))) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    ) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 50km
birds_50km <- div_comparison2(bird_cutoff_cell_TD_50km, bird_cutoff_cell_FD_50km, 'bird', 50)
birds_50km_plot <- birds_50km$plot
(birds_50km_plot <- birds_50km_plot + 
    xlab('Bird richness by cell') + 
    ylab('Bird FDis by cell') + 
    labs(subtitle = paste('D² =', round(birds_50km$r2_df$dev_expl, 3))) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 25km
birds_25km <- div_comparison2(bird_cutoff_cell_TD_25km, bird_cutoff_cell_FD_25km, 'bird', 25)
birds_25km_plot <- birds_25km$plot
(birds_25km_plot <- birds_25km_plot + 
    xlab('Bird richness by cell') + 
    ylab('Bird FDis by cell') + 
    labs(subtitle = paste('D² =', round(birds_25km$r2_df$dev_expl, 3))) + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
    ) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 10km
birds_10km <- div_comparison2(bird_cutoff_cell_TD_10km, bird_cutoff_cell_FD_10km, 'bird', 10)
birds_10km_plot <- birds_10km$plot
(birds_10km_plot <- birds_10km_plot + 
    xlab('Bird richness by cell') + 
    ylab('Bird FDis by cell') + 
    labs(subtitle = paste('D² =', round(birds_10km$r2_df$dev_expl, 3))) + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
    ) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 5km
birds_5km <- div_comparison2(bird_cutoff_cell_TD_5km, bird_cutoff_cell_FD_5km, 'bird', 5)
birds_5km_plot <- birds_5km$plot
(birds_5km_plot <- birds_5km_plot + 
    xlab('Bird richness by cell') + 
    ylab('Bird FDis by cell') + 
    labs(subtitle = paste('D² =', round(birds_5km$r2_df$dev_expl, 3))) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank()
    ) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# combined plot
all_bird_R_FD_plots <- wrap_plots(birds_5km_plot, birds_10km_plot, birds_25km_plot, birds_50km_plot, birds_75km_plot, birds_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 15, 15, 15),legend.position='bottom')

all_bird_R_FD_plots

ggsave('birdRichness-FDis_plots.png', all_bird_R_FD_plots, path = filtered_data_figure_path, width = 14, height = 8, units = "in", dpi=1000)


#### comparison of plant richness-frugivore FDis ####

# 100km
R_FD_comparison_100km <- div_comparison3(plant_cutoff_cell_TD_100km, mammal_cutoff_cell_FD_100km, bird_cutoff_cell_FD_100km, 100)

# mammals
PR_MFD_comparison_100km_plot <- R_FD_comparison_100km$plot1
(PR_MFD_comparison_100km_plot <- PR_MFD_comparison_100km_plot + 
    ylab('Mammal FDis by cell') + 
    xlab('Plant richness by cell') + 
    labs(subtitle = paste('D² =', round(R_FD_comparison_100km$r2_df$dev_expl[1], 3))) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))

# birds
PR_BFD_comparison_100km_plot <- R_FD_comparison_100km$plot2
(PR_BFD_comparison_100km_plot <- PR_BFD_comparison_100km_plot + 
    ylab('Bird FDis by cell') + 
    xlab('Plant richness by cell') + 
    labs(subtitle = paste('D² =', round(R_FD_comparison_100km$r2_df$dev_expl[2], 3))) + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 75 km
R_FD_comparison_75km <- div_comparison3(plant_cutoff_cell_TD_75km, mammal_cutoff_cell_FD_75km, bird_cutoff_cell_FD_75km, 75)

# mammals
PR_MFD_comparison_75km_plot <- R_FD_comparison_75km$plot1
(PR_MFD_comparison_75km_plot <- PR_MFD_comparison_75km_plot + 
    ylab('Mammal FDis by cell') + 
    xlab('Plant richness by cell') + 
    labs(subtitle = paste('D² =', round(R_FD_comparison_75km$r2_df$dev_expl[1], 3))) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank()
    ) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))

# birds
PR_BFD_comparison_75km_plot <- R_FD_comparison_75km$plot2
(PR_BFD_comparison_75km_plot <- PR_BFD_comparison_75km_plot + 
    ylab('Bird FDis by cell') + 
    xlab('Plant richness by cell') + 
    labs(subtitle = paste('D² =', round(R_FD_comparison_75km$r2_df$dev_expl[2], 3))) + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 50 km
R_FD_comparison_50km <- div_comparison3(plant_cutoff_cell_TD_50km, mammal_cutoff_cell_FD_50km, bird_cutoff_cell_FD_50km, 50)

# mammals
PR_MFD_comparison_50km_plot <- R_FD_comparison_50km$plot1
(PR_MFD_comparison_50km_plot <- PR_MFD_comparison_50km_plot + 
    ylab('Mammal FDis by cell') + 
    xlab('Plant richness by cell') + 
    labs(subtitle = paste('D² =', round(R_FD_comparison_50km$r2_df$dev_expl[1], 3))) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))

# birds
PR_BFD_comparison_50km_plot <- R_FD_comparison_50km$plot2
(PR_BFD_comparison_50km_plot <- PR_BFD_comparison_50km_plot + 
    ylab('Bird FDis by cell') + 
    xlab('Plant richness by cell') + 
    labs(subtitle = paste('D² =', round(R_FD_comparison_50km$r2_df$dev_expl[2], 3))) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 25 km
R_FD_comparison_25km <- div_comparison3(plant_cutoff_cell_TD_25km, mammal_cutoff_cell_FD_25km, bird_cutoff_cell_FD_25km, 25)

# mammals
PR_MFD_comparison_25km_plot <- R_FD_comparison_25km$plot1
(PR_MFD_comparison_25km_plot <- PR_MFD_comparison_25km_plot + 
    ylab('Mammal FDis by cell') + 
    xlab('Plant richness by cell') + 
    labs(subtitle = paste('D² =', round(R_FD_comparison_25km$r2_df$dev_expl[1], 3))) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.line.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))

# birds
PR_BFD_comparison_25km_plot <- R_FD_comparison_25km$plot2
(PR_BFD_comparison_25km_plot <- PR_BFD_comparison_25km_plot + 
    ylab('Bird FDis by cell') + 
    xlab('Plant richness by cell') + 
    labs(subtitle = paste('D² =', round(R_FD_comparison_25km$r2_df$dev_expl[2], 3))) + 
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.line.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 10 km
R_FD_comparison_10km <- div_comparison3(plant_cutoff_cell_TD_10km, mammal_cutoff_cell_FD_10km, bird_cutoff_cell_FD_10km, 10)

# mammals
PR_MFD_comparison_10km_plot <- R_FD_comparison_10km$plot1
(PR_MFD_comparison_10km_plot <- PR_MFD_comparison_10km_plot + 
    ylab('Mammal FDis by cell') + 
    xlab('Plant richness by cell') + 
    labs(subtitle = paste('D² =', round(R_FD_comparison_10km$r2_df$dev_expl[1], 3))) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.line.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))

# birds
PR_BFD_comparison_10km_plot <- R_FD_comparison_10km$plot2
(PR_BFD_comparison_10km_plot <- PR_BFD_comparison_10km_plot + 
    ylab('Bird FDis by cell') + 
    xlab('Plant richness by cell') + 
    labs(subtitle = paste('D² =', round(R_FD_comparison_10km$r2_df$dev_expl[2], 3))) + 
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.line.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 5 km
R_FD_comparison_5km <- div_comparison3(plant_cutoff_cell_TD_5km, mammal_cutoff_cell_FD_5km, bird_cutoff_cell_FD_5km, 5)

# mammals
PR_MFD_comparison_5km_plot <- R_FD_comparison_5km$plot1
(PR_MFD_comparison_5km_plot <- PR_MFD_comparison_5km_plot + 
    ylab('Mammal FDis by cell') + 
    xlab('Plant richness by cell') + 
    labs(subtitle = paste('D² =', round(R_FD_comparison_5km$r2_df$dev_expl[1], 3))) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.line.x = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))

# birds
PR_BFD_comparison_5km_plot <- R_FD_comparison_5km$plot2
(PR_BFD_comparison_5km_plot <- PR_BFD_comparison_5km_plot + 
    ylab('Bird FDis by cell') + 
    xlab('Plant richness by cell') + 
    labs(subtitle = paste('D² =', round(R_FD_comparison_5km$r2_df$dev_expl[2], 3))) + 
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.line.x = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# combined plots

# mammals
all_mammal_R_FD_plots <- wrap_plots(PR_MFD_comparison_5km_plot, PR_MFD_comparison_10km_plot, PR_MFD_comparison_25km_plot, PR_MFD_comparison_50km_plot, PR_MFD_comparison_75km_plot, PR_MFD_comparison_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 15, 15, 15),legend.position='bottom')

all_mammal_R_FD_plots

ggsave('plantRichness-mammalFDis_plots.png', all_mammal_R_FD_plots, path = filtered_data_figure_path, width = 14, height = 8, units = "in", dpi=1000)

# birds
all_bird_R_FD_plots <- wrap_plots(PR_BFD_comparison_5km_plot, PR_BFD_comparison_10km_plot, PR_BFD_comparison_25km_plot, PR_BFD_comparison_50km_plot, PR_BFD_comparison_75km_plot, PR_BFD_comparison_100km_plot, ncol=3, nrow=2) + plot_layout(guides='collect', axis_titles = 'collect') & theme(plot.margin = margin(5, 15, 15, 15),legend.position='bottom')

all_bird_R_FD_plots

ggsave('plantRichness-birdFDis_plots.png', all_bird_R_FD_plots, path = filtered_data_figure_path, width = 14, height = 8, units = "in", dpi=1000)

# both together
plantRichness_frugFDis_plots <- plot_grid(all_mammal_R_FD_plots, all_bird_R_FD_plots, labels = c('(a)', '(b)'), label_fontface='plain', nrow=2)

plantRichness_frugFDis_plots

ggsave('plantRichness-frugFDis_plots.png', plantRichness_frugFDis_plots, path = filtered_data_figure_path, width = 11, height = 10, units = "in", dpi=1000)


#### comparison of plant FDis-frugivore richness ####

# 100km
FD_R_comparison_100km <- div_comparison3(plant_cutoff_cell_FD_100km, mammal_cutoff_cell_TD_100km, bird_cutoff_cell_TD_100km, 100)

# mammals
PFD_MR_comparison_100km_plot <- FD_R_comparison_100km$plot1
(PFD_MR_comparison_100km_plot <- PFD_MR_comparison_100km_plot + 
    xlab('Mammal richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(FD_R_comparison_100km$r2_df$dev_expl[1], 3))) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))

# birds
PFD_BR_comparison_100km_plot <- FD_R_comparison_100km$plot2
(PFD_BR_comparison_100km_plot <- PFD_BR_comparison_100km_plot + 
    xlab('Bird richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(FD_R_comparison_100km$r2_df$dev_expl[2], 3))) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 75 km
FD_R_comparison_75km <- div_comparison3(plant_cutoff_cell_FD_75km, mammal_cutoff_cell_TD_75km, bird_cutoff_cell_TD_75km, 75)

# mammals
PFD_MR_comparison_75km_plot <- FD_R_comparison_75km$plot1
(PFD_MR_comparison_75km_plot <- PFD_MR_comparison_75km_plot + 
    xlab('Mammal richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(FD_R_comparison_75km$r2_df$dev_expl[1], 3))) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))

# birds
PFD_BR_comparison_75km_plot <- FD_R_comparison_75km$plot2
(PFD_BR_comparison_75km_plot <- PFD_BR_comparison_75km_plot + 
    xlab('Bird richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(FD_R_comparison_75km$r2_df$dev_expl[2], 3))) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 50 km
FD_R_comparison_50km <- div_comparison3(plant_cutoff_cell_FD_50km, mammal_cutoff_cell_TD_50km, bird_cutoff_cell_TD_50km, 50)

# mammals
PFD_MR_comparison_50km_plot <- FD_R_comparison_50km$plot1
(PFD_MR_comparison_50km_plot <- PFD_MR_comparison_50km_plot + 
    xlab('Mammal richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(FD_R_comparison_50km$r2_df$dev_expl[1], 3))) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))

# birds
PFD_BR_comparison_50km_plot <- FD_R_comparison_50km$plot2
(PFD_BR_comparison_50km_plot <- PFD_BR_comparison_50km_plot + 
    xlab('Bird richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(FD_R_comparison_50km$r2_df$dev_expl[2], 3))) +
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 25 km
FD_R_comparison_25km <- div_comparison3(plant_cutoff_cell_FD_25km, mammal_cutoff_cell_TD_25km, bird_cutoff_cell_TD_25km, 25)

# mammals
PFD_MR_comparison_25km_plot <- FD_R_comparison_25km$plot1
(PFD_MR_comparison_25km_plot <- PFD_MR_comparison_25km_plot + 
    xlab('Mammal richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(FD_R_comparison_25km$r2_df$dev_expl[1], 3))) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.line.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))

# birds
PFD_BR_comparison_25km_plot <- FD_R_comparison_25km$plot2
(PFD_BR_comparison_25km_plot <- PFD_BR_comparison_25km_plot + 
    xlab('Bird richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(FD_R_comparison_25km$r2_df$dev_expl[2], 3))) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.line.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 10 km
FD_R_comparison_10km <- div_comparison3(plant_cutoff_cell_FD_10km, mammal_cutoff_cell_TD_10km, bird_cutoff_cell_TD_10km, 10)

# mammals
PFD_MR_comparison_10km_plot <- FD_R_comparison_10km$plot1
(PFD_MR_comparison_10km_plot <- PFD_MR_comparison_10km_plot + 
    xlab('Mammal richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(FD_R_comparison_10km$r2_df$dev_expl[1], 3))) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.line.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))

# birds
PFD_BR_comparison_10km_plot <- FD_R_comparison_10km$plot2
(PFD_BR_comparison_10km_plot <- PFD_BR_comparison_10km_plot + 
    xlab('Bird richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(FD_R_comparison_10km$r2_df$dev_expl[2], 3))) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.line.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(), 
      axis.ticks.y = element_blank(), 
      axis.line.y = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# 5 km
FD_R_comparison_5km <- div_comparison3(plant_cutoff_cell_FD_5km, mammal_cutoff_cell_TD_5km, bird_cutoff_cell_TD_5km, 5)

# mammals
PFD_MR_comparison_5km_plot <- FD_R_comparison_5km$plot1
(PFD_MR_comparison_5km_plot <- PFD_MR_comparison_5km_plot + 
    xlab('Mammal richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(FD_R_comparison_5km$r2_df$dev_expl[1], 3))) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.line.x = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))

# birds
PFD_BR_comparison_5km_plot <- FD_R_comparison_5km$plot2
(PFD_BR_comparison_5km_plot <- PFD_BR_comparison_5km_plot + 
    xlab('Bird richness by cell') + 
    ylab('Plant FDis by cell') + 
    labs(subtitle = paste('D² =', round(FD_R_comparison_5km$r2_df$dev_expl[2], 3))) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(), 
      axis.ticks.x = element_blank(), 
      axis.line.x = element_blank()
    ) + 
    theme(plot.title = element_text(face = "bold", hjust=0.5, size=16), plot.subtitle = element_text(size=14, hjust=0.5)))


# combined plots

# mammals
all_mammal_FD_R_plots <- wrap_plots(PFD_MR_comparison_5km_plot, PFD_MR_comparison_10km_plot, PFD_MR_comparison_25km_plot, PFD_MR_comparison_50km_plot, PFD_MR_comparison_75km_plot, PFD_MR_comparison_100km_plot, ncol=3, nrow=2) + plot_layout( axis_titles = 'collect') & theme(plot.margin = margin(5, 15, 15, 15), legend.position='bottom')

all_mammal_FD_R_plots

ggsave('mammalRichness_plantFDis_plots.png', all_mammal_FD_R_plots, path = filtered_data_figure_path, width = 14, height = 8, units = "in", dpi=1000)


# birds
all_bird_FD_R_plots <- wrap_plots(PFD_BR_comparison_5km_plot, PFD_BR_comparison_10km_plot, PFD_BR_comparison_25km_plot, PFD_BR_comparison_50km_plot, PFD_BR_comparison_75km_plot, PFD_BR_comparison_100km_plot, ncol=3, nrow=2) + plot_layout( axis_titles = 'collect') & theme(plot.margin = margin(5, 15, 15, 15), legend.position='bottom')

all_bird_FD_R_plots

ggsave('birdRichness_plantFDis_plots.png', all_bird_FD_R_plots, path = filtered_data_figure_path, width = 14, height = 8, units = "in", dpi=1000)


# both together
frugRichness_plantFDis_plots <- plot_grid(all_mammal_FD_R_plots, all_bird_FD_R_plots, labels = c('(a)', '(b)'), label_fontface='plain', nrow=2)

frugRichness_plantFDis_plots

ggsave('frugRichness_plantFDis_plots.png', frugRichness_plantFDis_plots, path = filtered_data_figure_path, width = 11, height = 10, units = "in", dpi=1000)

