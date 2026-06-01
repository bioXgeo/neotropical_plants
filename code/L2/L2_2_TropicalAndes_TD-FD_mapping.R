#title: "Tropical Andes functional and taxonomic diversity spatial patterns for plants and Frugivores"
#author: "Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Hazel J. Anderson, Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "Calculating and mapping the difference between taxonomic and functional diversity as functional dispersion for plants and frugivores."
#data input: 
#data output:
#date: "2026-04-22"
#notes: JB used HPCC


# load required packages
library(mFD); library(sf); library(dplyr); library(ggplot2); library(rnaturalearth); library(ggspatial); library(rlang); library(doParallel); library(foreach); library(purrr); library(ggpubr); library(patchwork); library(stringr); library(rphylopic); library(scales); library(viridis)


# set file paths
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
data_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')

# #HPCC
#data_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')
#output_path_L2 <- file.path('/mnt/research/nasabio/data_2025/plants/L2')
#figure_path <- file.path('/mnt/research/nasabio/data_2025/plants/figures')


# load functions
source("C:/GitHub_projects/neotropical_plants/code/Functions.R")

## HPCC
#source("/mnt/ffs24/home/baljunas/Documents/neotropical_plants/code/Functions.R")


# read in Data

# projected sf objects
plants_sf_species <- readRDS(file = file.path(data_path_L1,"plants_sf_species.rds"))
frugivores_sf_species <- readRDS(file = file.path(data_path_L1,"frugivores_sf_species.rds"))
Americas <- readRDS(file = file.path(data_path_L1, "Americas.rds"))
TApoly <- readRDS(file = file.path(data_path_L1,"TApoly.rds"))
TropicalAndes_IUCNHabitat_Forest <- readRDS(file = file.path(data_path_L1,"TropicalAndes_IUCNHabitat_Forest.rds"))


# FD data
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


# TD data
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

div_diff_map <- function(cell_FD, cell_TD, guild){
  
  df <- left_join(st_drop_geometry(cell_FD), cell_TD, by = "cellid")
  
  df$fdis_value[is.nan(df$fdis_value)] <- NA
  
  df$num_sp_scaled <- rescale(df$num_species)
  
  df$div_diff <- df$num_sp_scaled - df$fdis_value
  
  sf <- df %>%
    select(cellid, geometry, div_diff)
  
  sf <- st_as_sf(sf)
  
  cols <- viridis(3)
  
  if(guild == 'Mammals'){
    lims = c(-0.65, 0.25)
  } else(if (guild == 'Birds'){
    lims = c(-0.50, 0.50)
  } else(
    lims = c(-0.60, 0.30)
  ))
  
  plot <- ggplot() +
    geom_sf(data = Americas, fill = "white")+
    geom_sf(data = TApoly, fill = "lightgrey", size = 0.1) +
    geom_sf(data = sf, aes(fill = div_diff), color = 'NA') +
    labs(fill = paste(guild)) +
    scale_fill_gradient2(limits = lims, low = cols[1], mid = cols[2], high = cols[3], midpoint = 0, na.value = 'gray53') + 
    coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
    scale_x_continuous(breaks = seq(-85, -54, by = 10)) + 
    scale_y_continuous(breaks = seq(-24, 14, by = 10)) +
    theme(panel.background = element_rect(fill = "lightblue"), axis.title = element_text(size = 16), axis.text = element_text(size = 12), legend.title = element_text(size = 16), legend.text = element_text(size = 12), plot.title = element_text(hjust = 0.5, size=12))
  
  return(plot)
}


# Plant maps
plant_div_map_100km <- div_diff_map(plant_cellRichness_100km, PcellFDis_100km, 'Plants')
plant_div_map_75km <- div_diff_map(plant_cellRichness_75km, PcellFDis_75km, 'Plants')
plant_div_map_50km <- div_diff_map(plant_cellRichness_50km, PcellFDis_50km, 'Plants')
plant_div_map_25km <- div_diff_map(plant_cellRichness_25km, PcellFDis_25km, 'Plants')
plant_div_map_10km <- div_diff_map(plant_cellRichness_10km, PcellFDis_10km, 'Plants')
plant_div_map_5km <- div_diff_map(plant_cellRichness_5km, PcellFDis_5km, 'Plants')


# Mammal maps
mammal_div_map_100km <- div_diff_map(mammal_cellRichness_100km, McellFDis_100km, 'Mammals')
mammal_div_map_75km <- div_diff_map(mammal_cellRichness_75km, McellFDis_75km, 'Mammals')
mammal_div_map_50km <- div_diff_map(mammal_cellRichness_50km, McellFDis_50km, 'Mammals')
mammal_div_map_25km <- div_diff_map(mammal_cellRichness_25km, McellFDis_25km, 'Mammals')
mammal_div_map_10km <- div_diff_map(mammal_cellRichness_10km, McellFDis_10km, 'Mammals')
mammal_div_map_5km <- div_diff_map(mammal_cellRichness_5km, McellFDis_5km, 'Mammals')


# Bird maps
bird_div_map_100km <- div_diff_map(bird_cellRichness_100km, BcellFDis_100km, 'Birds')
bird_div_map_75km <- div_diff_map(bird_cellRichness_75km, BcellFDis_75km, 'Birds')
bird_div_map_50km <- div_diff_map(bird_cellRichness_50km, BcellFDis_50km, 'Birds')
bird_div_map_25km <- div_diff_map(bird_cellRichness_25km, BcellFDis_25km, 'Birds')
bird_div_map_10km <- div_diff_map(bird_cellRichness_10km, BcellFDis_10km, 'Birds')
bird_div_map_5km <- div_diff_map(bird_cellRichness_5km, BcellFDis_5km, 'Birds')


# combine all plots 

# individual plot edits
mammal_div_map_100km <- mammal_div_map_100km + labs(title='[100km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20)) 

plant_div_map_100km <- plant_div_map_100km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_100km <- bird_div_map_100km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


mammal_div_map_75km <- mammal_div_map_75km + labs(title='[75km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

plant_div_map_75km <- plant_div_map_75km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_75km <- bird_div_map_75km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


mammal_div_map_50km <- mammal_div_map_50km + labs(title='[50km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

plant_div_map_50km <- plant_div_map_50km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_50km <- bird_div_map_50km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


mammal_div_map_25km <- mammal_div_map_25km + labs(title='[25km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

plant_div_map_25km <- plant_div_map_25km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_25km <- bird_div_map_25km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + labs(x = "Longitude")


mammal_div_map_10km <- mammal_div_map_10km + labs(title='[10km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

plant_div_map_10km <- plant_div_map_10km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_10km <- bird_div_map_10km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


# mammal picture
mammal <- pick_phylopic(name='Potos flavus', n=2, auto=2)

mammal_div_map_5km <- mammal_div_map_5km + labs(title='[5km]') + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20), axis.text = element_text(size=16)) + add_phylopic(img=mammal, x=-79, y=12, height=8)

# plant picture
plant <- pick_phylopic(name='Coffea alleizettei')

plant_div_map_5km <- plant_div_map_5km + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + labs(y = "Latitude") + add_phylopic(img=plant, x=-79, y=12, height=8)

# bird picture
bird <- pick_phylopic(name='Ramphastos sulfuratus', n=2, auto=1)

bird_div_map_5km <- bird_div_map_5km + annotation_scale(location = "bl",width_hint = 0.4, style = "bar") + annotation_north_arrow(location = "bl", which_north = "true", height = unit(0.5, "in"), width = unit(0.5, "in"), pad_x = unit(0.05, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) + theme(plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + add_phylopic(img=bird, x=-79, y=12, height=8)


# arrange
all_div_plots <-  mammal_div_map_5km + mammal_div_map_10km + mammal_div_map_25km + mammal_div_map_50km + mammal_div_map_75km + mammal_div_map_100km + plant_div_map_5km + plant_div_map_10km + plant_div_map_25km + plant_div_map_50km + plant_div_map_75km + plant_div_map_100km + bird_div_map_5km + bird_div_map_10km + bird_div_map_25km + bird_div_map_50km + bird_div_map_75km + bird_div_map_100km + plot_layout(ncol = 6, nrow = 3, guides = 'collect', axis_titles = 'collect', heights = c(1, 1, 1)) & theme(legend.position = 'left', axis.title = element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=16), legend.background = element_rect(fill='transparent'), panel.background = element_rect(fill='transparent'), plot.background = element_rect(fill='transparent', color=NA)) & plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size=30, face='bold')))

all_div_plots

ggsave('div_comparison_maps.png', all_div_plots, path = figure_path, width = 14, height = 11.8, units = "in", dpi=1000)



# individual plot edits
plant_div_map_100km <- plant_div_map_100km + labs(title='[100km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20)) 

mammal_div_map_100km <- mammal_div_map_100km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_100km <- bird_div_map_100km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


plant_div_map_75km <- plant_div_map_75km + labs(title='[75km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_div_map_75km <- mammal_div_map_75km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_75km <- bird_div_map_75km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


plant_div_map_50km <- plant_div_map_50km + labs(title='[50km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_div_map_50km <- mammal_div_map_50km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_50km <- bird_div_map_50km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


plant_div_map_25km <- plant_div_map_25km + labs(title='[25km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_div_map_25km <- mammal_div_map_25km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_25km <- bird_div_map_25km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + labs(x = "Longitude")


plant_div_map_10km <- plant_div_map_10km + labs(title='[10km]') + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20))

mammal_div_map_10km <- mammal_div_map_10km + theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.margin = margin(0,0,0,0))

bird_div_map_10km <- bird_div_map_10km + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16))


# plant picture
plant <- pick_phylopic(name='Coffea alleizettei')

plant_div_map_5km <- plant_div_map_5km + labs(title='[5km]') + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), plot.title = element_text(size=20), axis.text = element_text(size=16)) + add_phylopic(img=plant, x=-79, y=12, height=8)

# mammal picture
mammal <- pick_phylopic(name='Potos flavus', n=2, auto=2)

mammal_div_map_5km <- mammal_div_map_5km + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + labs(y = "Latitude") + add_phylopic(img=mammal, x=-79, y=12, height=8)

# bird picture
bird <- pick_phylopic(name='Ramphastos sulfuratus', n=2, auto=1)

bird_div_map_5km <- bird_div_map_5km + annotation_scale(location = "bl",width_hint = 0.4, style = "bar") + annotation_north_arrow(location = "bl", which_north = "true", height = unit(0.5, "in"), width = unit(0.5, "in"), pad_x = unit(0.05, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering) + theme(plot.margin = margin(0,0,0,0), axis.text = element_text(size=16)) + add_phylopic(img=bird, x=-79, y=12, height=8)


# arrange
all_div_plots <- plant_div_map_5km + plant_div_map_10km + plant_div_map_25km + plant_div_map_50km + plant_div_map_75km + plant_div_map_100km + mammal_div_map_5km + mammal_div_map_10km + mammal_div_map_25km + mammal_div_map_50km + mammal_div_map_75km + mammal_div_map_100km + bird_div_map_5km + bird_div_map_10km + bird_div_map_25km + bird_div_map_50km + bird_div_map_75km + bird_div_map_100km + plot_layout(ncol = 6, nrow = 3, guides = 'collect', axis_titles = 'collect', heights = c(1, 1, 1)) & theme(legend.position = 'left', axis.title = element_text(size=20), legend.title = element_text(size=20), legend.text = element_text(size=16), legend.background = element_rect(fill='transparent'), panel.background = element_rect(fill='transparent'), plot.background = element_rect(fill='transparent', color=NA)) & plot_annotation(theme = theme(plot.title = element_text(hjust = 0.5, size=30, face='bold')))

all_div_plots

ggsave('div_comparison_maps.png', all_div_plots, path = figure_path, width = 14, height = 11.8, units = "in", dpi=1000)

