#### Data Summary ####
data_summary <- function(records, species, genera, families){
  num_records <- nrow(records)
  num_species <- length(unique(species))
  num_genera <- length(unique(genera))
  num_families <- length(unique(families))
  return(cat("The number of records is", num_records, "\n", "The number of species is", num_species, "\n","The number of genera is", num_genera, "\n", "The number of families is", num_families))
}

#### Taxonomic Diversity Calculation & Mapping ####

# Function for richness plots using parallel processing
calculate_richness <- function(data_sf, species_sf) {
  result <- data_sf %>%
    st_join(species_sf) %>%
    mutate(overlap = ifelse(!is.na(species), 1, 0)) %>%
    group_by(cellid) %>%
    summarize(num_species = sum(overlap)) %>%
    ungroup()
  return(result)
}

# Plot richness
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
  Pmpt=1000
  Plims=c(0,2000)
  
  frugivore_richness_grid <- foreach(chunk = TAGrid_chunks, .combine = bind_rows, .packages = c("dplyr", "sf")) %dopar% {
    calculate_richness(chunk, frugivores_sf_species)
  }
  Fmpt=250
  Flims=c(0,500)
  
  mammal_richness_grid <- foreach(chunk = TAGrid_chunks, .combine = bind_rows, .packages = c("dplyr", "sf")) %dopar% {
    calculate_richness(chunk, mammals_sf_species)
  }
  Mmpt=60
  Mlims=c(0,120)
  
  bird_richness_grid <- foreach(chunk = TAGrid_chunks, .combine = bind_rows, .packages = c("dplyr", "sf")) %dopar% {
    calculate_richness(chunk, birds_sf_species)
  }
  Bmpt=200
  Blims=c(0,400)
  
  # Stop the parallel cluster
  stopCluster(cl)
  
  # Helper functions to generate plots
  generate_plant_plot <- function(data) {
    ggplot(data) +
      geom_sf(data = Americas, fill = "white") +
      geom_sf(data = TApoly, fill = "grey") +
      geom_sf(aes(fill = num_species), color = NA) +
      scale_fill_viridis_c(limits=Plims, na.value = 'gray53', option='magma') +
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
      scale_fill_viridis_c(limits=Flims, na.value = 'gray53') +
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
      scale_fill_viridis_c(limits=Mlims, na.value = 'gray53') +
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
      scale_fill_viridis_c(limits=Blims, na.value = 'gray53') +
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


#### Functional Diversity Calculation & Mapping ####
# Quality of Functional Spaces
fspaces_quality <- function(PAM, traits, guild){
  
  if(guild %in% c('frugivore', 'mammal', 'bird')){
    # create trait type table
    trait_name <- c("body_mass_e",  "diet_cat", "diet_breadth", "habitat_breadth", "generation_time")
    trait_type <- c("Q", "N", "Q", "Q","Q")
    trait_cat <- as.data.frame(cbind(trait_name, trait_type))
    
    # fix nominal traits as factor
    traits$diet_cat <- as.factor(traits$diet_cat)
    
  } else {
    trait_name <- c("PlantHeight_m", "FruitType", "PlantLifespan_years", "SeedMass_g", "FruitLength_mm", "GrowthForm", "SeedLength_mm", "DispersalSyndrome")
    trait_type <- c("Q", "N", "Q", "Q", "Q", "N", "Q", "N") 
    trait_cat <- as.data.frame(cbind(trait_name, trait_type))
    
    traits$GrowthForm <- as.factor(traits$GrowthForm)
    traits$FruitType <- as.factor(traits$FruitType)
    traits$DispersalSyndrome <- as.factor(traits$DispersalSyndrome) 
  }
  
  # summary of the assemblages * species dataframe
  asb_sp_summ <- asb.sp.summary(asb_sp_w = PAM)
  
  # species traits summary
  traits_summ <- sp.tr.summary(tr_cat = trait_cat, sp_tr = traits)
  
  # estimate functional trait-based distances between species
  sp_dist <- funct.dist(sp_tr = traits, tr_cat = trait_cat, metric = "gower", scale_euclid = "scale_center", ordinal_var = "classic", weight_type = "equal", stop_if_NA = TRUE)
  
  # generate a multidimensional space
  fspaces_quality <- quality.fspaces(sp_dist = sp_dist, maxdim_pcoa = 10, deviation_weighting = "absolute", fdist_scaling = FALSE, fdendro = "average")
  
  assign(x=paste0('fspaces_quality_',guild), value=fspaces_quality, envir=.GlobalEnv)
}


fspace_quality_plot <- function(fspaces_quality){
  
  # look at the quality spaces only (MAD index looks at the mean absolute deviation from the dissimilarity matrix; want the deviation to be low meaning that the true distances have been retained in the PCA)
  MAD <- as.data.frame(round(fspaces_quality$"quality_fspaces", 3))
  
  low_MAD <- MAD |>
    slice_min(mad, n = 3)
  
  # generate a multidimensional space
  fspaces <- quality.fspaces.plot(fspaces_quality = fspaces_quality, quality_metric = "mad", fspaces_plot = rownames(low_MAD))
  
  print(c(fspaces))
  return(MAD)
}

pc_coords <- function(fspaces_quality, traits, guild){
  
  # testing correlation between functional axes and traits
  sp_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"
  
  # computes linear model for continuous traits and Kruskall-Wallis tests for other types. 
  tr_faxes <- traits.faxes.cor(sp_tr = traits, sp_faxes_coord = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")], plot = TRUE)
  
  # print traits with significant effect:
  tr_faxes$"tr_faxes_stat"[which(tr_faxes$"tr_faxes_stat"$"p.value" < 0.05), ]
  
  sp_faxes_coord <- fspaces_quality$"details_fspaces"$"sp_pc_coord"
  
  assign(x=paste0('tr_faxes_',guild), value=tr_faxes, envir=.GlobalEnv)
  assign(x=paste0('sp_faxes_coord_',guild), value=sp_faxes_coord, envir=.GlobalEnv)
}


# Correlation between functional axes and traits
fspace_corr_plots <- function(sp_faxes_coord, tr_faxes){
  
  # plotting functional space
  big_plot <- funct.space.plot(sp_faxes_coord = sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")], faxes = c("PC1", "PC2", "PC3", "PC4"), alpha_ch = 0.5, shape_vert = 6)
  
  print(c(big_plot, tr_faxes$"tr_faxes_plot"))
}


# FDis calculation
FDis <- function(PAM, sp_faxes_coord){
  
  # need to remove parts of the PAM that have values less than or equal to the number of dimensions (4)
  
  # calculate row sums
  row_sums <- rowSums(PAM)
  subset_matrix <- PAM[row_sums >= 4, ]
  
  # match frugivore names
  sp_faxes_coord_sub <- sp_faxes_coord[ , c("PC1", "PC2", "PC3", "PC4")]
  summary(sp_faxes_coord_sub)
  
  # extract names
  subset_matrix_names <- colnames(subset_matrix)
  sp_faxes_coord_sub_names <- row.names(sp_faxes_coord_sub)
  
  # create df
  sp_faxes_coord_sub <- as.data.frame(sp_faxes_coord_sub)
  row.names(sp_faxes_coord_sub) <- sp_faxes_coord_sub_names
  
  # remove names removed from matrix
  names <- intersect(sp_faxes_coord_sub_names, subset_matrix_names)
  names <- na.omit(names)
  
  sp_faxes_coord_sub <- sp_faxes_coord_sub[ which((row.names(sp_faxes_coord_sub) %in% names)==TRUE), ]
  
  subset_matrix <- as.data.frame(subset_matrix)
  subset_matrix <- subset_matrix[ ,which((colnames(subset_matrix) %in% names)==TRUE)]
  
  sp_faxes_coord_sub <- na.omit(sp_faxes_coord_sub)
  subset_matrix <- na.omit(subset_matrix)
  
  message(paste0('nrows subset:', nrow(sp_faxes_coord_sub), ', ncols matrix:', ncol(subset_matrix)))
  
  if (nrow(sp_faxes_coord_sub) != ncol(subset_matrix)) {
    stop("Number of rows in sp_faxes_coord_sub does not match number of columns in subset_matrix")
  }
  
  sp_faxes_coord_sub <- as.matrix(sp_faxes_coord_sub)
  subset_matrix <- as.matrix(subset_matrix)
  
  
  # computing FDis
  
  # the number of species per assemblage has to be higher or equal to the number of traits
  
  # Use three fewer cores than available
  #num_cores <- parallel::detectCores() - 3
  
  # HPCC
  num_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
  
  # Set up parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  tryCatch({
    # Define chunk size
    chunk_size <- ceiling(nrow(subset_matrix) / num_cores)
    
    # Split data into chunks for parallel processing
    chunks <- split(seq_len(nrow(subset_matrix)), ceiling(seq_len(nrow(subset_matrix)) / chunk_size))
    
    # Process chunks in parallel
    results_list <- foreach(chunk_indices = chunks, .combine = rbind, .packages = "mFD") %dopar% {
      subset_matrix_chunk <- subset_matrix[chunk_indices, , drop = FALSE]
      
      alpha_fd_indices <- alpha.fd.multidim(sp_faxes_coord = sp_faxes_coord_sub, asb_sp_w = subset_matrix, 
                                            ind_vect = "fdis", details_returned = TRUE)
      
      details_list <- alpha_fd_indices$"details" # see if this is needed
      
      # get functional dispersion
      fdis_values <- alpha_fd_indices$functional_diversity_indices$fdis
      
      # match with corresponding cell numbers
      cell_numbers <- rownames(subset_matrix)[chunk_indices]
      
      # Create a data frame for this chunk
      chunk_results <- data.frame(Cell_Number = cell_numbers, fdis = fdis_values)
      
      chunk_results
    }
    # # Combine all chunk results into a single data frame
    # all_results <- do.call(rbind, results_list)
    
    # Stop the parallel backend
    stopCluster(cl)
    
    return(results_list)
    
  }, error = function(e) {
    stop("Error in parallel processing: ", conditionMessage(e))
  })
  
}


# Mapping FDis
FD_map <- function(loc_key, PAM, resolution_meters, fdis, guild){
  
  if(guild=='plant'){
    mpt=0.4
    lims=c(0.2,0.8)
  } else {
    if (guild=='mammal'){
      mpt=0.3
      lims=c(0,0.8)
    } else {
      mpt=0.4
      lims=c(0.3,0.5)
    }
  }
  
  # generate coordinates
  subset_coords <- loc_key[rowSums(PAM) >= 4,]
  subset_coords_sp <-subset_coords[,1:2]
  
  subset_coords_sp <- as.data.frame(subset_coords_sp)
  
  subset_coords_sp$Cell_Number <- rownames(subset_coords_sp)
  
  common_ids <- intersect(subset_coords_sp$Cell_Number, fdis$Cell_Number)
  
  if (length(common_ids) == 0) stop("No matching Cell_Number values found.")
  
  # Align both tables using match()
  subset_coords_sp <- subset_coords_sp[match(common_ids, subset_coords_sp$Cell_Number), ]
  fdis <- fdis[match(common_ids, fdis$Cell_Number), ]
  
  # Drop duplicate ID column before merging
  fdis$Cell_Number <- NULL
  
  # --- 4. Combine coordinates + FDis ---
  fd_sp <- cbind(subset_coords_sp, fdis)
  
  fd_sp <- data.frame(subset_coords_sp, fdis)
  
  # convert the dataframe to sf format
  spatial_fdis <- st_as_sf(fd_sp, coords = c("Longitude", "Latitude"))
  
  # set crs of sf objects
  spatial_fdis <- spatial_fdis %>% st_set_crs(5389)
  
  # extract coordinates 
  spatial_fdis_coords <- st_coordinates(spatial_fdis)
  
  # add coordinates as separate columns
  spatial_fdis <- spatial_fdis %>%
    mutate(x = spatial_fdis_coords[, 1], y = spatial_fdis_coords[, 2])
  
  TAGrid<- TApoly %>%
    st_make_grid(cellsize = c(resolution_meters)) %>%
    st_intersection(TropicalAndes_IUCNHabitat_Forest) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(cellid = row_number())
  
  spatial_fdis$cellid <- st_intersects(spatial_fdis, TAGrid)$nn
  
  spatial_fdis_grid <- TAGrid %>%
    st_join(spatial_fdis, join = st_contains) %>%
    group_by(cellid) %>%
    summarize(fdis_value = mean(fdis, na.rm = TRUE)) %>%
    ungroup()
  
  if(guild=='plant'){
    gridFDisTA <-
      ggplot() +
      geom_sf(data = Americas, fill = "white")+
      geom_sf(data = TApoly, fill = "lightgrey", size = 0.1) +
      geom_sf(data = spatial_fdis_grid, aes(fill = fdis_value), color = 'NA') +
      labs(fill = "FDis") +
      ggtitle(paste0(resolution_meters/1000,"km")) +
      scale_fill_viridis_c(limits=lims, na.value = 'gray53', option='magma') +
      coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
      theme(panel.background = element_rect(fill = "lightblue"),
            text = element_text(size = 12), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
    
  }else{
    gridFDisTA <-
      ggplot() +
      geom_sf(data = Americas, fill = "white")+
      geom_sf(data = TApoly, fill = "lightgrey", size = 0.1) +
      geom_sf(data = spatial_fdis_grid, aes(fill = fdis_value), color = 'NA') +
      labs(fill = "FDis") +
      ggtitle(paste0(resolution_meters/1000,"km")) +
      scale_fill_viridis_c(limits=lims, na.value = 'gray53') +
      coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
      theme(panel.background = element_rect(fill = "lightblue"),
            text = element_text(size = 12), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  list(gridFDisTA = gridFDisTA, spatial_fdis_grid = spatial_fdis_grid)
}

