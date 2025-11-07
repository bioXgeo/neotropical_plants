#title: "Tropical Andes Taxonomic Diversity of Plants and Frugivores"
#author: "Hazel J. Anderson, Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "Calculating and mapping taxonomic diversity as species richness using occurrence data for plants and frugivores in the Tropical Andes Moist Lowland and Montane forests. Note: some code is adapted from https://luisdva.github.io/rstats/richness/."
#date: "2025-10-27"
#output: 
#  html_document:
#  highlight_downlit = TRUE
#df_print: tibble
#notes: JB used HPCC


# load required packages
library(sf); library(dplyr); library(ggplot2); library(parallel); library(foreach); library(doParallel); library(ggspatial); library(ggpubr)


# set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')

#HPCC
data_path_L0 <- file.path('/mnt/research/nasabio/data_2025/plants/L0')
data_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')
output_path <- file.path('/mnt/research/nasabio/data_2025/plants/L2')
figure_path <- file.path('/mnt/research/nasabio/data_2025/plants/figures')


# read in data

# projected sf objects
plants_sf_species <- readRDS(file = file.path(data_path_L1,"plants_sf_species.rds"))
frugivores_sf_species <- readRDS(file = file.path(data_path_L1,"frugivores_sf_species.rds"))
mammals_sf_species <- readRDS(file = file.path(data_path_L1,"mammals_sf_species.rds"))
birds_sf_species <- readRDS(file = file.path(data_path_L1,"birds_sf_species.rds"))
Americas <- readRDS(file = file.path(data_path_L1, "Americas.rds"))
TApoly <- readRDS(file = file.path(data_path_L1,"TApoly.rds"))
TropicalAndes_IUCNHabitat_Forest <- readRDS(file = file.path(data_path_L1,"TropicalAndes_IUCNHabitat_Forest.rds"))


# plot TD/Richness

# function for richness plots using parallel processing

# define the calculate_richness function outside
calculate_richness <- function(data_sf, species_sf) {
  result <- data_sf %>%
    st_join(species_sf) %>%
    mutate(overlap = ifelse(!is.na(species), 1, 0)) %>%
    group_by(cellid) %>%
    summarize(num_species = sum(overlap)) %>%
    ungroup()
  return(result)
}


create_rich_plots <- function(resolution_meters) {
  # Make Grid
  TAGrid <- TApoly %>%
    st_make_grid(cellsize = c(resolution_meters)) %>%
    st_intersection(TropicalAndes_IUCNHabitat_Forest) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(cellid = row_number())
  
  # Determine the chunk size
  chunk_size <- 1000
  
  # Split the TAGrid into chunks
  TAGrid_chunks <- split(TAGrid, ceiling(seq_along(TAGrid$cellid) / chunk_size))
  
  # Set up parallel processing
  # no_cores <- detectCores() - 1
  # cl <- makeCluster(no_cores)
  
  # HPCC
  no_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
  cl <- parallel::makeCluster(no_cores)
  registerDoParallel(cl)
  
  # Export necessary objects to the cluster
  clusterExport(cl, varlist = c("plants_sf_species", "frugivores_sf_species", "mammals_sf_species", "birds_sf_species", "calculate_richness"))
  
  # Process chunks in parallel
  plant_richness_grid <- foreach(chunk = TAGrid_chunks, .combine = bind_rows, .packages = c("dplyr", "sf")) %dopar% {
    calculate_richness(chunk, plants_sf_species)
  }
  
  frugivore_richness_grid <- foreach(chunk = TAGrid_chunks, .combine = bind_rows, .packages = c("dplyr", "sf")) %dopar% {
    calculate_richness(chunk, frugivores_sf_species)
  }
  
  mammal_richness_grid <- foreach(chunk = TAGrid_chunks, .combine = bind_rows, .packages = c("dplyr", "sf")) %dopar% {
    calculate_richness(chunk, mammals_sf_species)
  }
  
  bird_richness_grid <- foreach(chunk = TAGrid_chunks, .combine = bind_rows, .packages = c("dplyr", "sf")) %dopar% {
    calculate_richness(chunk, birds_sf_species)
  }
  
  # Stop the parallel cluster
  stopCluster(cl)
  
  # Helper functions to generate plots
  generate_plant_plot <- function(data) {
    ggplot(data) +
      geom_sf(data = Americas, fill = "white") +
      geom_sf(data = TApoly, fill = "grey") +
      geom_sf(aes(fill = num_species), color = NA) +
      scale_fill_distiller(palette = "Greens", direction = 1, limits = c(0,2000)) +
      labs(x = "Longitude", y = "Latitude", fill = "S") +
      coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
      annotation_scale(location = "bl", width_hint = 0.5) +
      theme(panel.background = element_rect(fill = "lightblue"))
  }
  
  generate_frugivore_plot <- function(data) {
    ggplot(data) +
      geom_sf(data = Americas, fill = "white") +
      geom_sf(data = TApoly, fill = "grey") +
      geom_sf(aes(fill = num_species), color = NA) +
      scale_fill_distiller(palette = "YlOrBr", direction = 1, limits = c(0,500)) +
      labs(x = "Longitude", y = "Latitude", fill = "S") +
      coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
      annotation_scale(location = "bl", width_hint = 0.5) +
      theme(panel.background = element_rect(fill = "lightblue"))
  }
  
  generate_mammal_plot <- function(data) {
    ggplot(data) +
      geom_sf(data = Americas, fill = "white") +
      geom_sf(data = TApoly, fill = "grey") +
      geom_sf(aes(fill = num_species), color = NA) +
      scale_fill_distiller(palette = "YlOrBr", direction = 1, limits = c(0,500)) +
      labs(x = "Longitude", y = "Latitude", fill = "S") +
      coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
      annotation_scale(location = "bl", width_hint = 0.5) +
      theme(panel.background = element_rect(fill = "lightblue"))
  }
  
  generate_bird_plot <- function(data) {
    ggplot(data) +
      geom_sf(data = Americas, fill = "white") +
      geom_sf(data = TApoly, fill = "grey") +
      geom_sf(aes(fill = num_species), color = NA) +
      scale_fill_distiller(palette = "YlOrBr", direction = 1, limits = c(0,500)) +
      labs(x = "Longitude", y = "Latitude", fill = "S") +
      coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
      annotation_scale(location = "bl", width_hint = 0.5) +
      theme(panel.background = element_rect(fill = "lightblue"))
  }
  
  # Generate plots
  plant_plot <- generate_plant_plot(plant_richness_grid)
  frugivore_plot <- generate_frugivore_plot(frugivore_richness_grid)
  mammal_plot <- generate_mammal_plot(mammal_richness_grid)
  bird_plot <- generate_bird_plot(bird_richness_grid)
  
  
  
  # Create histograms
  plant_richness_hist <- hist(plant_richness_grid$num_species,
                              main = paste("Histogram of Plant Richness [", resolution_meters / 1000, " km] by cell", sep = ""),
                              xlab = "Plant Richness by cell")
  frugivore_richness_hist <- hist(frugivore_richness_grid$num_species,
                                  main = paste("Histogram of Frugivore Richness [", resolution_meters / 1000, " km] by cell", sep = ""),
                                  xlab = "Frugivore Richness by cell")
  
  mammal_richness_hist <- hist(mammal_richness_grid$num_species,
                               main = paste("Histogram of Mammal Richness [", resolution_meters / 1000, " km] by cell", sep = ""),
                               xlab = "Mammal Richness by cell")
  
  bird_richness_hist <- hist(bird_richness_grid$num_species,
                             main = paste("Histogram of Bird Richness [", resolution_meters / 1000, " km] by cell", sep = ""),
                             xlab = "Bird Richness by cell")
  
  # Return a list of data
  list(plantgridRichTA = plant_plot,
       frugivoregridRichTA = frugivore_plot,
       mammalgridRichTA = mammal_plot,
       birdgridRichTA = bird_plot,
       
       plant_cellRichness = plant_richness_grid,
       frugivore_cellRichness = frugivore_richness_grid,
       mammal_cellRichness = mammal_richness_grid,
       bird_cellRichness = bird_richness_grid,
       
       plant_richness_hist = plant_richness_hist,
       frugivore_richness_hist = frugivore_richness_hist,
       mammal_richness_hist = mammal_richness_hist,
       bird_richness_hist = bird_richness_hist)
}


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

# save richness objects to file
saveRDS(richness_5km, file = file.path(output_path,"richness_5km.rds"))
saveRDS(richness_10km, file = file.path(output_path,"richness_10km.rds"))
saveRDS(richness_25km, file = file.path(output_path,"richness_25km.rds"))
saveRDS(richness_50km, file = file.path(output_path,"richness_50km.rds"))
saveRDS(richness_75km, file = file.path(output_path,"richness_75km.rds"))
saveRDS(richness_100km, file = file.path(output_path,"richness_100km.rds"))


# save plots to file
plot_list <- list(plantgridRichTA_5km = plantgridRichTA_5km, plantgridRichTA_10km = plantgridRichTA_10km,
                  plantgridRichTA_25km = plantgridRichTA_25km, plantgridRichTA_50km = plantgridRichTA_50km,
                  plantgridRichTA_75km = plantgridRichTA_75km, plantgridRichTA_100km = plantgridRichTA_100km,
                  
                  frugivoregridRichTA_5km = frugivoregridRichTA_5km, frugivoregridRichTA_10km = frugivoregridRichTA_10km,
                  frugivoregridRichTA_25km = frugivoregridRichTA_25km, frugivoregridRichTA_50km = frugivoregridRichTA_50km,
                  frugivoregridRichTA_75km = frugivoregridRichTA_75km, frugivoregridRichTA_100km = frugivoregridRichTA_100km,
                  
                  mammalgridRichTA_5km = mammalgridRichTA_5km, mammalgridRichTA_10km = mammalgridRichTA_10km,
                  mammalgridRichTA_25km = mammalgridRichTA_25km, mammalgridRichTA_50km = mammalgridRichTA_50km,
                  mammalgridRichTA_75km = mammalgridRichTA_75km, mammalgridRichTA_100km = mammalgridRichTA_100km,
                  
                  birdgridRichTA_5km = birdgridRichTA_5km, birdgridRichTA_10km = birdgridRichTA_10km,
                  birdgridRichTA_25km = birdgridRichTA_25km, birdgridRichTA_50km = birdgridRichTA_50km,
                  birdgridRichTA_75km = birdgridRichTA_75km, birdgridRichTA_100km = birdgridRichTA_100km)
#histogram_list <- list(plant_richness_hist_5km = richness_5km$plant_richness_hist, 
#                       frugivore_richness_hist_5km = richness_5km$frugivore_richness_hist,
#                       plant_richness_hist_10km = richness_10km$plant_richness_hist,
#                       frugivore_richness_hist_10km = richness_10km$frugivore_richness_hist,
#                       plant_richness_hist_25km = richness_25km$plant_richness_hist,
#                       frugivore_richness_hist_55km = richness_25km$frugivore_richness_hist,
#                       plant_richness_hist_50km = richness_50km$plant_richness_hist,
#                       frugivore_richness_hist_50km = richness_50km$frugivore_richness_hist,
#                       plant_richness_hist_75km = richness_75km$plant_richness_hist,
#                       frugivore_richness_hist_75km = richness_75km$frugivore_richness_hist,
#                       plant_richness_hist_100km = richness_100km$plant_richness_hist,
#                       frugivore_richness_hist_100km = richness_100km$frugivore_richness_hist)
#histograms would have to be converted to ggplot to save

save_plots <- function(plot_list, figure_path) {
  # Iterate over the list and save each plot
  for (plot_name in names(plot_list)) {
    plot_path <- file.path(figure_path, paste0(plot_name, ".png"))
    
    # Try to save the plot and handle any potential errors
    tryCatch({
      ggsave(filename = plot_path, 
             plot = plot_list[[plot_name]])
      cat("Saved plot:", plot_path, "\n")
    }, error = function(e) {
      cat("Failed to save plot:", plot_name, "\nError message:", e$message, "\n")
    })
  }
}

# Save the plots
save_plots(plot_list, figure_path)


# multi-panel plots of richness maps

#### plants ####

# add labels and remove axis labels from all plots
plantgridRichTA_5km_labeled <- plantgridRichTA_5km + ggtitle("5 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
plantgridRichTA_10km_labeled <- plantgridRichTA_10km + ggtitle("10 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
plantgridRichTA_25km_labeled <- plantgridRichTA_25km + ggtitle("25 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
plantgridRichTA_50km_labeled <- plantgridRichTA_50km + ggtitle("50 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
plantgridRichTA_75km_labeled <- plantgridRichTA_75km + ggtitle("75 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
plantgridRichTA_100km_labeled <- plantgridRichTA_100km + ggtitle("100 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
plantgridRichTA_100km_labeled

# arrange the plots with common legend
all_plant_richness_plots <- ggarrange(plantgridRichTA_5km_labeled, plantgridRichTA_10km_labeled,
                                      plantgridRichTA_25km_labeled, plantgridRichTA_50km_labeled,
                                      plantgridRichTA_75km_labeled, plantgridRichTA_100km_labeled,
                                      ncol = 6, nrow = 1,
                                      common.legend = TRUE, legend = "left")

all_plant_richness_plots

all_plant_richness_plots_labeled <- ggpubr::annotate_figure(all_plant_richness_plots,
                                                            left = ggpubr::text_grob("Plants", face = "bold", size = 20, 
                                                                                     rot = 90))
all_plant_richness_plots_labeled
ggsave("plant_richness_plots.png", all_plant_richness_plots_labeled, path = figure_path, width = 16, height = 5, units = "in")


#### frugivores ####

# add labels and remove axis labels from all plots
frugivoregridRichTA_5km_labeled <- frugivoregridRichTA_5km + ggtitle("5 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
frugivoregridRichTA_10km_labeled <- frugivoregridRichTA_10km + ggtitle("10 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
frugivoregridRichTA_25km_labeled <- frugivoregridRichTA_25km + ggtitle("25 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
frugivoregridRichTA_50km_labeled <- frugivoregridRichTA_50km + ggtitle("50 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
frugivoregridRichTA_75km_labeled <- frugivoregridRichTA_75km + ggtitle("75 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
frugivoregridRichTA_100km_labeled <- frugivoregridRichTA_100km + ggtitle("100 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
frugivoregridRichTA_100km_labeled

all_frugivore_richness_plots <- ggpubr::ggarrange(frugivoregridRichTA_5km_labeled,frugivoregridRichTA_10km_labeled,
                                                  frugivoregridRichTA_25km_labeled, frugivoregridRichTA_50km_labeled,
                                                  frugivoregridRichTA_75km_labeled, frugivoregridRichTA_100km_labeled,
                                                  ncol = 6, nrow = 1,
                                                  common.legend = TRUE, legend = "left")
all_frugivore_richness_plots

all_frugivore_richness_plots_labeled <- ggpubr::annotate_figure(all_frugivore_richness_plots,
                                                                left = ggpubr::text_grob("Frugivores", face = "bold", size = 20,
                                                                                         rot = 90))
all_frugivore_richness_plots_labeled
ggsave("frugivore_richness_plots.png", all_frugivore_richness_plots_labeled, path = figure_path, width = 16, height = 5, units = "in")


#### mammals ####

# add labels and remove axis labels from all plots
mammalgridRichTA_5km_labeled <- mammalgridRichTA_5km + ggtitle("5 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
mammalgridRichTA_10km_labeled <- mammalgridRichTA_10km + ggtitle("10 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
mammalgridRichTA_25km_labeled <- mammalgridRichTA_25km + ggtitle("25 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
mammalgridRichTA_50km_labeled <- mammalgridRichTA_50km + ggtitle("50 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
mammalgridRichTA_75km_labeled <- mammalgridRichTA_75km + ggtitle("75 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
mammalgridRichTA_100km_labeled <- mammalgridRichTA_100km + ggtitle("100 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
mammalgridRichTA_100km_labeled

all_mammal_richness_plots <- ggpubr::ggarrange(mammalgridRichTA_5km_labeled,mammalgridRichTA_10km_labeled,
                                               mammalgridRichTA_25km_labeled, mammalgridRichTA_50km_labeled,
                                               mammalgridRichTA_75km_labeled, mammalgridRichTA_100km_labeled,
                                               ncol = 6, nrow = 1,
                                               common.legend = TRUE, legend = "left")
all_mammal_richness_plots

all_mammal_richness_plots_labeled <- ggpubr::annotate_figure(all_mammal_richness_plots,
                                                             left = ggpubr::text_grob("Mammals", face = "bold", size = 20,
                                                                                      rot = 90))
all_mammal_richness_plots_labeled
ggsave("mammal_richness_plots.png", all_mammal_richness_plots_labeled, path = figure_path, width = 16, height = 5, units = "in")


#### birds ####

# add labels and remove axis labels from all plots
birdgridRichTA_5km_labeled <- birdgridRichTA_5km + ggtitle("5 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
birdgridRichTA_10km_labeled <- birdgridRichTA_10km + ggtitle("10 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
birdgridRichTA_25km_labeled <- birdgridRichTA_25km + ggtitle("25 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
birdgridRichTA_50km_labeled <- birdgridRichTA_50km + ggtitle("50 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
birdgridRichTA_75km_labeled <- birdgridRichTA_75km + ggtitle("75 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))
birdgridRichTA_100km_labeled <- birdgridRichTA_100km + ggtitle("100 km") + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1), legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10))

all_bird_richness_plots <- ggpubr::ggarrange(birdgridRichTA_5km_labeled,birdgridRichTA_10km_labeled,
                                             birdgridRichTA_25km_labeled, birdgridRichTA_50km_labeled,
                                             birdgridRichTA_75km_labeled, birdgridRichTA_100km_labeled,
                                             ncol = 6, nrow = 1,
                                             common.legend = TRUE, legend = "left")
all_bird_richness_plots

all_bird_richness_plots_labeled <- ggpubr::annotate_figure(all_bird_richness_plots,
                                                           left = ggpubr::text_grob("Birds", face = "bold", size = 20,
                                                                                    rot = 90))
all_bird_richness_plots_labeled
ggsave("bird_richness_plots.png", all_frugivore_richness_plots_labeled, path = figure_path, width = 16, height = 5, units = "in")


# combine plants & frugivores
all_richness_plots <- ggpubr::ggarrange(all_plant_richness_plots_labeled,
                                        all_frugivore_richness_plots_labeled,
                                        all_mammal_richness_plots_labeled,
                                        all_bird_richness_plots_labeled,
                                        ncol = 2, nrow = 2)
# add a white background to the plot
all_richness_plots <- all_richness_plots + theme(plot.background = element_rect(fill = "white", color = NA))
all_richness_plots

ggsave("all_richness_plots.png", all_richness_plots, path = figure_path, width = 16, height = 10, units = "in")


# write data to csv

# have to remove geometry before saving to csv
plant_cellRichness_5km_df <- data.frame(cellid = plant_cellRichness_5km$cellid,
                                        num_species = plant_cellRichness_5km$num_species)
frugivore_cellRichness_5km_df <- data.frame(cellid = frugivore_cellRichness_5km$cellid,
                                            num_species = frugivore_cellRichness_5km$num_species)
mammal_cellRichness_5km_df <- data.frame(cellid = mammal_cellRichness_5km$cellid,
                                         num_species = mammal_cellRichness_5km$num_species)
bird_cellRichness_5km_df <- data.frame(cellid = bird_cellRichness_5km$cellid,
                                       num_species = bird_cellRichness_5km$num_species)


plant_cellRichness_10km_df <- data.frame(cellid = plant_cellRichness_10km$cellid,
                                         num_species = plant_cellRichness_10km$num_species)
frugivore_cellRichness_10km_df <- data.frame(cellid = frugivore_cellRichness_10km$cellid,
                                             num_species = frugivore_cellRichness_10km$num_species)
mammal_cellRichness_10km_df <- data.frame(cellid = mammal_cellRichness_10km$cellid,
                                          num_species = mammal_cellRichness_10km$num_species)
bird_cellRichness_10km_df <- data.frame(cellid = bird_cellRichness_10km$cellid,
                                        num_species = bird_cellRichness_10km$num_species)


plant_cellRichness_25km_df <- data.frame(cellid = plant_cellRichness_25km$cellid,
                                         num_species = plant_cellRichness_25km$num_species)
frugivore_cellRichness_25km_df <- data.frame(cellid = frugivore_cellRichness_25km$cellid,
                                             num_species = frugivore_cellRichness_25km$num_species)
mammal_cellRichness_25km_df <- data.frame(cellid = mammal_cellRichness_25km$cellid,
                                          num_species = mammal_cellRichness_25km$num_species)
bird_cellRichness_25km_df <- data.frame(cellid = bird_cellRichness_25km$cellid,
                                        num_species = bird_cellRichness_25km$num_species)


plant_cellRichness_50km_df <- data.frame(cellid = plant_cellRichness_50km$cellid,
                                         num_species = plant_cellRichness_50km$num_species)
frugivore_cellRichness_50km_df <- data.frame(cellid = frugivore_cellRichness_50km$cellid,
                                             num_species = frugivore_cellRichness_50km$num_species)
mammal_cellRichness_50km_df <- data.frame(cellid = mammal_cellRichness_50km$cellid,
                                          num_species = mammal_cellRichness_50km$num_species)
bird_cellRichness_50km_df <- data.frame(cellid = bird_cellRichness_50km$cellid,
                                        num_species = bird_cellRichness_50km$num_species)


plant_cellRichness_75km_df <- data.frame(cellid = plant_cellRichness_75km$cellid,
                                         num_species = plant_cellRichness_75km$num_species)
frugivore_cellRichness_75km_df <- data.frame(cellid = frugivore_cellRichness_75km$cellid,
                                             num_species = frugivore_cellRichness_75km$num_species)
mammal_cellRichness_75km_df <- data.frame(cellid = mammal_cellRichness_75km$cellid,
                                          num_species = mammal_cellRichness_75km$num_species)
bird_cellRichness_75km_df <- data.frame(cellid = bird_cellRichness_75km$cellid,
                                        num_species = bird_cellRichness_75km$num_species)


plant_cellRichness_100km_df <- data.frame(cellid = plant_cellRichness_100km$cellid,
                                          num_species = plant_cellRichness_100km$num_species)
frugivore_cellRichness_100km_df <- data.frame(cellid = frugivore_cellRichness_100km$cellid,
                                              num_species = frugivore_cellRichness_100km$num_species)
mammal_cellRichness_100km_df <- data.frame(cellid = mammal_cellRichness_100km$cellid,
                                           num_species = mammal_cellRichness_100km$num_species)
bird_cellRichness_100km_df <- data.frame(cellid = bird_cellRichness_100km$cellid,
                                         num_species = bird_cellRichness_100km$num_species)

write.csv(plant_cellRichness_5km_df, file.path(output_path,"TropicalAndes_plantRichness_5km.csv"), row.names = FALSE)
write.csv(frugivore_cellRichness_5km_df, file.path(output_path,"TropicalAndes_frugivoreRichness_5km.csv"), row.names = FALSE)
write.csv(mammal_cellRichness_5km_df, file.path(output_path,"TropicalAndes_mammalRichness_5km.csv"), row.names = FALSE)
write.csv(bird_cellRichness_5km_df, file.path(output_path,"TropicalAndes_birdRichness_5km.csv"), row.names = FALSE)

write.csv(plant_cellRichness_10km_df, file.path(output_path,"TropicalAndes_plantRichness_10km.csv"), row.names = FALSE)
write.csv(frugivore_cellRichness_10km_df, file.path(output_path,"TropicalAndes_frugivoreRichness_10km.csv"), row.names = FALSE)
write.csv(mammal_cellRichness_10km_df, file.path(output_path,"TropicalAndes_mammalRichness_10km.csv"), row.names = FALSE)
write.csv(bird_cellRichness_10km_df, file.path(output_path,"TropicalAndes_birdRichness_10km.csv"), row.names = FALSE)

write.csv(plant_cellRichness_25km_df, file.path(output_path,"TropicalAndes_plantRichness_25km.csv"), row.names = FALSE)
write.csv(frugivore_cellRichness_25km_df, file.path(output_path,"TropicalAndes_frugivoreRichness_25km.csv"), row.names = FALSE)
write.csv(mammal_cellRichness_25km_df, file.path(output_path,"TropicalAndes_mammalRichness_25km.csv"), row.names = FALSE)
write.csv(bird_cellRichness_25km_df, file.path(output_path,"TropicalAndes_birdRichness_25km.csv"), row.names = FALSE)

write.csv(plant_cellRichness_50km_df, file.path(output_path,"TropicalAndes_plantRichness_50km.csv"), row.names = FALSE)
write.csv(frugivore_cellRichness_50km_df, file.path(output_path,"TropicalAndes_frugivoreRichness_50km.csv"), row.names = FALSE)
write.csv(mammal_cellRichness_50km_df, file.path(output_path,"TropicalAndes_mammalRichness_50km.csv"), row.names = FALSE)
write.csv(bird_cellRichness_50km_df, file.path(output_path,"TropicalAndes_birdRichness_50km.csv"), row.names = FALSE)

write.csv(plant_cellRichness_75km_df, file.path(output_path,"TropicalAndes_plantRichness_75km.csv"), row.names = FALSE)
write.csv(frugivore_cellRichness_75km_df, file.path(output_path,"TropicalAndes_frugivoreRichness_75km.csv"), row.names = FALSE)
write.csv(mammal_cellRichness_75km_df, file.path(output_path,"TropicalAndes_mammalRichness_75km.csv"), row.names = FALSE)
write.csv(bird_cellRichness_75km_df, file.path(output_path,"TropicalAndes_birdRichness_75km.csv"), row.names = FALSE)

write.csv(plant_cellRichness_100km_df, file.path(output_path,"TropicalAndes_plantRichness_100km.csv"), row.names = FALSE)
write.csv(frugivore_cellRichness_100km_df, file.path(output_path,"TropicalAndes_frugivoreRichness_100km.csv"), row.names = FALSE)
write.csv(mammal_cellRichness_100km_df, file.path(output_path,"TropicalAndes_mammalRichness_100km.csv"), row.names = FALSE)
write.csv(bird_cellRichness_100km_df, file.path(output_path,"TropicalAndes_birdRichness_100km.csv"), row.names = FALSE)