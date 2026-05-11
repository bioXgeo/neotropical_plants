#title: "Tropical Andes Taxonomic Diversity of Plants and Frugivores"
#author: "Hazel J. Anderson, Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "Calculating and mapping taxonomic diversity as species richness using occurrence data for plants and frugivores in the Tropical Andes Moist Lowland and Montane forests. Note: some code is adapted from https://luisdva.github.io/rstats/richness/."
#data input: "plants_sf_species.rds", "frugivores_sf_species.rds", "mammals_sf_species.rds", "birds_sf_species.rds", "Americas.rds", "TApoly.rds", "TropicalAndes_IUCNHabitat_Forest.rds"
#data output: "richness_5km2.rds", "richness_10km2.rds", "richness_25km2.rds", "richness_50km2.rds", "richness_75km2.rds", "richness_100km2.rds", "all_plant_richness_plots.png", "all_frugivore_richness_plots.png", "all_mammal_richness_plots.png", "all_bird_richness_plots.png", "all_richness_plots.png", "TropicalAndes_plantRichness_5km.csv", "TropicalAndes_frugivoreRichness_5km.csv", "TropicalAndes_mammalRichness_5km.csv", "TropicalAndes_birdRichness_5km.csv", "TropicalAndes_plantRichness_10km.csv", "TropicalAndes_frugivoreRichness_10km.csv", "TropicalAndes_mammalRichness_10km.csv", "TropicalAndes_birdRichness_10km.csv", "TropicalAndes_plantRichness_25km.csv", "TropicalAndes_frugivoreRichness_25km.csv", "TropicalAndes_mammalRichness_25km.csv", "TropicalAndes_birdRichness_25km.csv", "TropicalAndes_plantRichness_50km.csv", "TropicalAndes_frugivoreRichness_50km.csv", "TropicalAndes_mammalRichness_50km.csv", "TropicalAndes_birdRichness_50km.csv", "TropicalAndes_plantRichness_75km.csv", "TropicalAndes_frugivoreRichness_75km.csv", "TropicalAndes_mammalRichness_75km.csv", "TropicalAndes_birdRichness_75km.csv", "TropicalAndes_plantRichness_100km.csv", "TropicalAndes_frugivoreRichness_100km.csv", "TropicalAndes_mammalRichness_100km.csv", "TropicalAndes_birdRichness_100km.csv"
#date: "2023-08-01; 2025-10-27"
#notes: JB used HPCC


# load required packages
library(sf); library(dplyr); library(ggplot2); library(parallel); library(foreach); library(doParallel); library(ggspatial); library(ggpubr); library(patchwork); library(rphylopic)


# set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')

# #HPCC
# data_path_L0 <- file.path('/mnt/research/nasabio/data_2025/plants/L0')
# data_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')
# output_path_L2 <- file.path('/mnt/research/nasabio/data_2025/plants/L2')
# figure_path <- file.path('/mnt/research/nasabio/data_2025/plants/figures')


# load functions
source("C:/GitHub_projects/plant-frugivore diversity/neotropical_plants/code/Functions.R")

# # HPCC
# source("/mnt/ffs24/home/baljunas/Documents/neotropical_plants/code/Functions.R")


# read in data

# projected sf objects
plants_sf_species <- readRDS(file = file.path(data_path_L1,"plants_sf_species.rds"))
frugivores_sf_species <- readRDS(file = file.path(data_path_L1,"frugivores_sf_species.rds"))
mammals_sf_species <- readRDS(file = file.path(data_path_L1,"mammals_sf_species.rds"))
birds_sf_species <- readRDS(file = file.path(data_path_L1,"birds_sf_species.rds"))
Americas <- readRDS(file = file.path(data_path_L1, "Americas.rds"))
TApoly <- readRDS(file = file.path(data_path_L1,"TApoly.rds"))
TropicalAndes_IUCNHabitat_Forest <- readRDS(file = file.path(data_path_L1,"TropicalAndes_IUCNHabitat_Forest.rds"))


# 100 km 
richness_100km <- create_rich_plots(100000)

print(richness_100km)

# save plots
plantgridRichTA_100km <- richness_100km$plantgridRichTA
frugivoregridRichTA_100km <- richness_100km$frugivoregridRichTA
mammalgridRichTA_100km <- richness_100km$mammalgridRichTA
birdgridRichTA_100km <- richness_100km$birdgridRichTA

# extract cell values
plant_cellRichness_100km <- richness_100km$plant_cellRichness
frugivore_cellRichness_100km <- richness_100km$frugivore_cellRichness
mammal_cellRichness_100km <- richness_100km$mammal_cellRichness
bird_cellRichness_100km <- richness_100km$bird_cellRichness


# 75 km 
richness_75km <- create_rich_plots(75000)

print(richness_75km)

# save plots
plantgridRichTA_75km <- richness_75km$plantgridRichTA
frugivoregridRichTA_75km <- richness_75km$frugivoregridRichTA
mammalgridRichTA_75km <- richness_75km$mammalgridRichTA
birdgridRichTA_75km <- richness_75km$birdgridRichTA

# extract cell values
plant_cellRichness_75km <- richness_75km$plant_cellRichness
frugivore_cellRichness_75km <- richness_75km$frugivore_cellRichness
mammal_cellRichness_75km <- richness_75km$mammal_cellRichness
bird_cellRichness_75km <- richness_75km$bird_cellRichness


# 50 km
richness_50km <- create_rich_plots(50000)

print(richness_50km)

# save plots
plantgridRichTA_50km <- richness_50km$plantgridRichTA
frugivoregridRichTA_50km <- richness_50km$frugivoregridRichTA
mammalgridRichTA_50km <- richness_50km$mammalgridRichTA
birdgridRichTA_50km <- richness_50km$birdgridRichTA

# extract cell values
plant_cellRichness_50km <- richness_50km$plant_cellRichness
frugivore_cellRichness_50km <- richness_50km$frugivore_cellRichness
mammal_cellRichness_50km <- richness_50km$mammal_cellRichness
bird_cellRichness_50km <- richness_50km$bird_cellRichness


# 25 km
richness_25km <- create_rich_plots(25000)

print(richness_25km)

# save plots
plantgridRichTA_25km <- richness_25km$plantgridRichTA
frugivoregridRichTA_25km <- richness_25km$frugivoregridRichTA
mammalgridRichTA_25km <- richness_25km$mammalgridRichTA
birdgridRichTA_25km <- richness_25km$birdgridRichTA

# extract cell values
plant_cellRichness_25km <- richness_25km$plant_cellRichness
frugivore_cellRichness_25km <- richness_25km$frugivore_cellRichness
mammal_cellRichness_25km <- richness_25km$mammal_cellRichness
bird_cellRichness_25km <- richness_25km$bird_cellRichness


# 10 km
richness_10km <- create_rich_plots(10000)

print(richness_10km)

# save plots
plantgridRichTA_10km <- richness_10km$plantgridRichTA
frugivoregridRichTA_10km <- richness_10km$frugivoregridRichTA
mammalgridRichTA_10km <- richness_10km$mammalgridRichTA
birdgridRichTA_10km <- richness_10km$birdgridRichTA

# extract cell values
plant_cellRichness_10km <- richness_10km$plant_cellRichness
frugivore_cellRichness_10km <- richness_10km$frugivore_cellRichness
mammal_cellRichness_10km <- richness_10km$mammal_cellRichness
bird_cellRichness_10km <- richness_10km$bird_cellRichness


# 5 km resolution
richness_5km <- create_rich_plots(5000)

print(richness_5km)

# save plots
plantgridRichTA_5km <- richness_5km$plantgridRichTA
frugivoregridRichTA_5km <- richness_5km$frugivoregridRichTA
mammalgridRichTA_5km <- richness_5km$mammalgridRichTA
birdgridRichTA_5km <- richness_5km$birdgridRichTA

# extract cell values
plant_cellRichness_5km <- richness_5km$plant_cellRichness
frugivore_cellRichness_5km <- richness_5km$frugivore_cellRichness
mammal_cellRichness_5km <- richness_5km$mammal_cellRichness
bird_cellRichness_5km <- richness_5km$bird_cellRichness


# save richness objects to file
saveRDS(richness_5km, file = file.path(output_path_L2,"richness_5km.rds"))
saveRDS(richness_10km, file = file.path(output_path_L2,"richness_10km.rds"))
saveRDS(richness_25km, file = file.path(output_path_L2,"richness_25km.rds"))
saveRDS(richness_50km, file = file.path(output_path_L2,"richness_50km.rds"))
saveRDS(richness_75km, file = file.path(output_path_L2,"richness_75km.rds"))
saveRDS(richness_100km, file = file.path(output_path_L2,"richness_100km.rds"))

# load saved data
richness_5km <- readRDS(file = file.path(output_path_L2,"richness_5km.rds"))
richness_10km <- readRDS(file = file.path(output_path_L2,"richness_10km.rds"))
richness_25km <- readRDS(file = file.path(output_path_L2,"richness_25km.rds"))
richness_50km <- readRDS(file = file.path(output_path_L2,"richness_50km.rds"))
richness_75km <- readRDS(file = file.path(output_path_L2,"richness_75km.rds"))
richness_100km <- readRDS(file = file.path(output_path_L2,"richness_100km.rds"))

# combine all plots

# individual plot edits
plantgridRichTA_100km <- plantgridRichTA_100km + labs(title='[100km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammalgridRichTA_100km <- mammalgridRichTA_100km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

birdgridRichTA_100km <- birdgridRichTA_100km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


plantgridRichTA_75km <- plantgridRichTA_75km + labs(title='[75km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammalgridRichTA_75km <- mammalgridRichTA_75km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

birdgridRichTA_75km <- birdgridRichTA_75km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), axis.text = element_text(size = 16)) + theme(plot.margin = margin(0,0,0,0))


plantgridRichTA_50km <- plantgridRichTA_50km + labs(title='[50km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammalgridRichTA_50km <- mammalgridRichTA_50km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

birdgridRichTA_50km <- birdgridRichTA_50km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + theme(plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


plantgridRichTA_25km <- plantgridRichTA_25km + labs(title='[25km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammalgridRichTA_25km <- mammalgridRichTA_25km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

birdgridRichTA_25km <- birdgridRichTA_25km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))+ labs(x = "Longitude")


plantgridRichTA_10km <- plantgridRichTA_10km + labs(title='[10km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammalgridRichTA_10km <- mammalgridRichTA_10km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

birdgridRichTA_10km <- birdgridRichTA_10km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


# plant picture
plant <- pick_phylopic(name='Coffea alleizettei')

plantgridRichTA_5km <- plantgridRichTA_5km + labs(title='[5km]') + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20)) + add_phylopic(img=plant, x=-79.5, y=13, height=8)

# mammal picture
mammal <- pick_phylopic(name='Potos flavus', n=2, auto=2)

mammalgridRichTA_5km <- mammalgridRichTA_5km + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16)) + labs(y = "Latitude") + add_phylopic(img=mammal, x=-79, y=12, height=8)

# bird picture
bird <- pick_phylopic(name='Ramphastos sulfuratus', n=2, auto=1)

birdgridRichTA_5km <- birdgridRichTA_5km + annotation_scale(location = "bl",width_hint = 0.4, style = "bar") + annotation_north_arrow(location = "bl", which_north = "true", height = unit(0.5, "in"), width = unit(0.5, "in"), pad_x = unit(0.05, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) + add_phylopic(img=bird, x=-80, y=12.5, height=8) + theme(plot.margin = margin(0,0,0,0), axis.text = element_text(size = 16))


# arrange
all_richness_plots <- plantgridRichTA_5km + plantgridRichTA_10km + plantgridRichTA_25km + plantgridRichTA_50km + plantgridRichTA_75km + plantgridRichTA_100km + mammalgridRichTA_5km + mammalgridRichTA_10km + mammalgridRichTA_25km + mammalgridRichTA_50km + mammalgridRichTA_75km + mammalgridRichTA_100km + birdgridRichTA_5km + birdgridRichTA_10km + birdgridRichTA_25km + birdgridRichTA_50km + birdgridRichTA_75km + birdgridRichTA_100km + plot_layout(ncol = 6, nrow = 3, guides = 'collect', axis_titles = 'collect', heights = c(1, 1, 1)) & theme(legend.position = 'left', axis.title = element_text(size = 20)) & plot_annotation(title='Species richness', theme = theme(plot.title = element_text(hjust = 0.5, size=30, face='bold')))

all_richness_plots

ggsave('all_richness_plots.png', all_richness_plots, path = figure_path, width = 14, height = 12, units = "in", dpi=1000)


# save richness data

saveRDS(plant_cellRichness_5km, file.path(output_path_L2,"TropicalAndes_plantRichness_5km.rds"))
saveRDS(frugivore_cellRichness_5km, file.path(output_path_L2,"TropicalAndes_frugivoreRichness_5km.rds"))
saveRDS(mammal_cellRichness_5km, file.path(output_path_L2,"TropicalAndes_mammalRichness_5km.rds"))
saveRDS(bird_cellRichness_5km, file.path(output_path_L2,"TropicalAndes_birdRichness_5km.rds"))

saveRDS(plant_cellRichness_10km, file.path(output_path_L2,"TropicalAndes_plantRichness_10km.rds"))
saveRDS(frugivore_cellRichness_10km, file.path(output_path_L2,"TropicalAndes_frugivoreRichness_10km.rds"))
saveRDS(mammal_cellRichness_10km, file.path(output_path_L2,"TropicalAndes_mammalRichness_10km.rds"))
saveRDS(bird_cellRichness_10km, file.path(output_path_L2,"TropicalAndes_birdRichness_10km.rds"))

saveRDS(plant_cellRichness_25km, file.path(output_path_L2,"TropicalAndes_plantRichness_25km.rds"))
saveRDS(frugivore_cellRichness_25km, file.path(output_path_L2,"TropicalAndes_frugivoreRichness_25km.rds"))
saveRDS(mammal_cellRichness_25km, file.path(output_path_L2,"TropicalAndes_mammalRichness_25km.rds"))
saveRDS(bird_cellRichness_25km, file.path(output_path_L2,"TropicalAndes_birdRichness_25km.rds"))

saveRDS(plant_cellRichness_50km, file.path(output_path_L2,"TropicalAndes_plantRichness_50km.rds"))
saveRDS(frugivore_cellRichness_50km, file.path(output_path_L2,"TropicalAndes_frugivoreRichness_50km.rds"))
saveRDS(mammal_cellRichness_50km, file.path(output_path_L2,"TropicalAndes_mammalRichness_50km.rds"))
saveRDS(bird_cellRichness_50km, file.path(output_path_L2,"TropicalAndes_birdRichness_50km.rds"))

saveRDS(plant_cellRichness_75km, file.path(output_path_L2,"TropicalAndes_plantRichness_75km.rds"))
saveRDS(frugivore_cellRichness_75km, file.path(output_path_L2,"TropicalAndes_frugivoreRichness_75km.rds"))
saveRDS(mammal_cellRichness_75km, file.path(output_path_L2,"TropicalAndes_mammalRichness_75km.rds"))
saveRDS(bird_cellRichness_75km, file.path(output_path_L2,"TropicalAndes_birdRichness_75km.rds"))

saveRDS(plant_cellRichness_100km, file.path(output_path_L2,"TropicalAndes_plantRichness_100km.rds"))
saveRDS(frugivore_cellRichness_100km, file.path(output_path_L2,"TropicalAndes_frugivoreRichness_100km.rds"))
saveRDS(mammal_cellRichness_100km, file.path(output_path_L2,"TropicalAndes_mammalRichness_100km.rds"))
saveRDS(bird_cellRichness_100km, file.path(output_path_L2,"TropicalAndes_birdRichness_100km.rds"))
