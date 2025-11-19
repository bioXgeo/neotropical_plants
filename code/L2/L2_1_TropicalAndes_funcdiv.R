#title: "Tropical Andes Functional diversity for plants and Frugivores"
#author: "Hazel J. Anderson, Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "This script calculates functional diversity as functional dispersion for plants and frugivores."
#date: "2025-11-03"
#output: html_document
#notes: JB used HPCC


# load required packages
library(mFD); library(sf); library(dplyr); library(ggplot2); library(rnaturalearth); library(ggspatial); library(rlang); library(doParallel); library(foreach); library(purrr); library(ggpubr); library(patchwork)


# set file paths
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')

#HPCC
data_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')
output_path_L2 <- file.path('/mnt/research/nasabio/data_2025/plants/L2')
figure_path <- file.path('/mnt/research/nasabio/data_2025/plants/figures')


# read in Data

# projected sf objects
plants_sf_species <- readRDS(file = file.path(data_path_L1,"plants_sf_species.rds"))
frugivores_sf_species <- readRDS(file = file.path(data_path_L1,"frugivores_sf_species.rds"))
Americas <- readRDS(file = file.path(data_path_L1, "Americas.rds"))
TApoly <- readRDS(file = file.path(data_path_L1,"TApoly.rds"))
TropicalAndes_IUCNHabitat_Forest <- readRDS(file = file.path(data_path_L1,"TropicalAndes_IUCNHabitat_Forest.rds"))

# trait data
plant_traits_df_final <- readRDS(file = file.path(data_path_L1,"plant_traits_df_final.rds"))
frugivore_traits_df_final <- readRDS(file = file.path(data_path_L1,"frugivore_traits_df_final.rds"))
mammal_traits_df_final <- readRDS(file=file.path(data_path_L1, 'mammal_traits_df_final.rds'))
bird_traits_df_final <- readRDS(file=file.path(data_path_L1, 'bird_traits_df_final.rds'))

# 100km
site_loc_key_plant_100km <- readRDS(file = file.path(data_path_L1,"site_loc_key_plant_100km.rds"))
site_loc_key_frugivore_100km <- readRDS(file = file.path(data_path_L1,"site_loc_key_frugivore_100km.rds"))
site_loc_key_mammal_100km <- readRDS(file = file.path(data_path_L1,"site_loc_key_mammal_100km.rds"))
site_loc_key_bird_100km <- readRDS(file = file.path(data_path_L1,"site_loc_key_bird_100km.rds"))
PAM_plant_site_final_100km <- readRDS(file = file.path(data_path_L1,"PAM_plant_site_final_100km.rds"))
PAM_frugivore_site_final_100km <- readRDS(file = file.path(data_path_L1,"PAM_frugivore_site_final_100km.rds"))
PAM_mammal_site_final_100km <- readRDS(file = file.path(data_path_L1,"PAM_mammal_site_final_100km.rds"))
PAM_bird_site_final_100km <- readRDS(file = file.path(data_path_L1,"PAM_bird_site_final_100km.rds"))

# 75km
site_loc_key_plant_75km <- readRDS(file = file.path(data_path_L1,"site_loc_key_plant_75km.rds"))
site_loc_key_frugivore_75km <- readRDS(file = file.path(data_path_L1,"site_loc_key_frugivore_75km.rds"))
site_loc_key_mammal_75km <- readRDS(file = file.path(data_path_L1,"site_loc_key_mammal_75km.rds"))
site_loc_key_bird_75km <- readRDS(file = file.path(data_path_L1,"site_loc_key_bird_75km.rds"))
PAM_plant_site_final_75km <- readRDS(file = file.path(data_path_L1,"PAM_plant_site_final_75km.rds"))
PAM_frugivore_site_final_75km <- readRDS(file = file.path(data_path_L1,"PAM_frugivore_site_final_75km.rds"))
PAM_mammal_site_final_75km <- readRDS(file = file.path(data_path_L1,"PAM_mammal_site_final_75km.rds"))
PAM_bird_site_final_75km <- readRDS(file = file.path(data_path_L1,"PAM_bird_site_final_75km.rds"))

# 50km
site_loc_key_plant_50km <- readRDS(file = file.path(data_path_L1,"site_loc_key_plant_50km.rds"))
site_loc_key_mammal_50km <- readRDS(file = file.path(data_path_L1,"site_loc_key_mammal_50km.rds"))
site_loc_key_bird_50km <- readRDS(file = file.path(data_path_L1,"site_loc_key_bird_50km.rds"))
site_loc_key_frugivore_50km <- readRDS(file = file.path(data_path_L1,"site_loc_key_frugivore_50km.rds"))
PAM_plant_site_final_50km <- readRDS(file = file.path(data_path_L1,"PAM_plant_site_final_50km.rds"))
PAM_frugivore_site_final_50km <- readRDS(file = file.path(data_path_L1,"PAM_frugivore_site_final_50km.rds"))
PAM_mammal_site_final_50km <- readRDS(file = file.path(data_path_L1,"PAM_mammal_site_final_50km.rds"))
PAM_bird_site_final_50km <- readRDS(file = file.path(data_path_L1,"PAM_bird_site_final_50km.rds"))

# 25km
site_loc_key_plant_25km <- readRDS(file = file.path(data_path_L1,"site_loc_key_plant_25km.rds"))
site_loc_key_frugivore_25km <- readRDS(file = file.path(data_path_L1,"site_loc_key_frugivore_25km.rds"))
site_loc_key_mammal_25km <- readRDS(file = file.path(data_path_L1,"site_loc_key_mammal_25km.rds"))
site_loc_key_bird_25km <- readRDS(file = file.path(data_path_L1,"site_loc_key_bird_25km.rds"))
PAM_plant_site_final_25km <- readRDS(file = file.path(data_path_L1,"PAM_plant_site_final_25km.rds"))
PAM_frugivore_site_final_25km <- readRDS(file = file.path(data_path_L1,"PAM_frugivore_site_final_25km.rds"))
PAM_mammal_site_final_25km <- readRDS(file = file.path(data_path_L1,"PAM_mammal_site_final_25km.rds"))
PAM_bird_site_final_25km <- readRDS(file = file.path(data_path_L1,"PAM_bird_site_final_25km.rds"))

# 10km
site_loc_key_plant_10km <- readRDS(file = file.path(data_path_L1,"site_loc_key_plant_10km.rds"))
site_loc_key_frugivore_10km <- readRDS(file = file.path(data_path_L1,"site_loc_key_frugivore_10km.rds"))
site_loc_key_mammal_10km <- readRDS(file = file.path(data_path_L1,"site_loc_key_mammal_10km.rds"))
site_loc_key_bird_10km <- readRDS(file = file.path(data_path_L1,"site_loc_key_bird_10km.rds"))
PAM_plant_site_final_10km <- readRDS(file = file.path(data_path_L1,"PAM_plant_site_final_10km.rds"))
PAM_frugivore_site_final_10km <- readRDS(file = file.path(data_path_L1,"PAM_frugivore_site_final_10km.rds"))
PAM_mammal_site_final_10km <- readRDS(file = file.path(data_path_L1,"PAM_mammal_site_final_10km.rds"))
PAM_bird_site_final_10km <- readRDS(file = file.path(data_path_L1,"PAM_bird_site_final_10km.rds"))

# 5km
site_loc_key_plant_5km <- readRDS(file = file.path(data_path_L1,"site_loc_key_plant_5km.rds"))
site_loc_key_frugivore_5km <- readRDS(file = file.path(data_path_L1,"site_loc_key_frugivore_5km.rds"))
site_loc_key_mammal_5km <- readRDS(file = file.path(data_path_L1,"site_loc_key_mammal_5km.rds"))
site_loc_key_bird_5km <- readRDS(file = file.path(data_path_L1,"site_loc_key_bird_5km.rds"))
PAM_plant_site_final_5km <- readRDS(file = file.path(data_path_L1,"PAM_plant_site_final_5km.rds"))
PAM_frugivore_site_final_5km <- readRDS(file = file.path(data_path_L1,"PAM_frugivore_site_final_5km.rds"))
PAM_mammal_site_final_5km <- readRDS(file = file.path(data_path_L1,"PAM_mammal_site_final_5km.rds"))
PAM_bird_site_final_5km <- readRDS(file = file.path(data_path_L1,"PAM_bird_site_final_5km.rds"))


# create trait type tables
trait_name <- c("diet_cat", "body_mass_e", "body_size_mm", "generation_time")
trait_type <- c("N", "Q", "Q", "Q")
frug_trait_cat <- as.data.frame(cbind(trait_name, trait_type))

trait_name <- c("PlantLifespan_years","GrowthForm", "SeedMass_g", "PlantHeight_m", "FruitType", "FruitLength_mm", "SeedLength_mm", "DispersalSyndrome")
trait_type <- c("Q", "N", "Q", "Q", "N", "Q", "Q", "N") 
plant_trait_cat <- as.data.frame(cbind(trait_name, trait_type))

# fix Nominal traits as factor
frugivore_traits_df_final$diet_cat <- as.factor(frugivore_traits_df_final$diet_cat)
mammal_traits_df_final$diet_cat <- as.factor(mammal_traits_df_final$diet_cat)
bird_traits_df_final$diet_cat <- as.factor(bird_traits_df_final$diet_cat)

plant_traits_df_final$GrowthForm <- as.factor(plant_traits_df_final$GrowthForm)
plant_traits_df_final$FruitType <- as.factor(plant_traits_df_final$FruitType)
plant_traits_df_final$DispersalSyndrome <- as.factor(plant_traits_df_final$DispersalSyndrome)


# frugivores

FDis <- function(PAM, traits, matrix_name, df_name){
  # summary of the assemblages * species dataframe
  asb_sp_frugivore_summ <- asb.sp.summary(asb_sp_w = PAM)
  
  # species traits summary
  frugivore_traits_summ <- sp.tr.summary(tr_cat = frug_trait_cat, sp_tr = traits)
  
  # estimate functional trait-based distances between species
  sp_dist_frugivore <- funct.dist( sp_tr = traits, tr_cat = frug_trait_cat, metric = "gower", scale_euclid = "scale_center", ordinal_var = "classic", weight_type = "equal", stop_if_NA = TRUE)
  
  # generate a multidimensional space
  fspaces_quality_frugivore <- quality.fspaces(sp_dist = sp_dist_frugivore, maxdim_pcoa = 10, deviation_weighting = "absolute", fdist_scaling = FALSE, fdendro = "average")
  
  # look at the quality spaces only (MAD index looks at the mean absolute deviation from the dissimilarity matrix; want the deviation to be low meaning that the true distances have been retained in the PCA)
  MAD <- as.data.frame(round(fspaces_quality_frugivore$quality_fspaces, 3))
  
  low_MAD <- MAD |>
    slice_min(mad, n = 3)
  
  # generate a multidimensional space
  fspaces <- quality.fspaces.plot(fspaces_quality = fspaces_quality_frugivore, quality_metric = "mad",
                                  fspaces_plot = rownames(low_MAD))
  
  # testing correlation between functional axes and traits
  # maybe just make function that creates this and utilize the one down below for everything???
  sp_faxes_coord_frugivore <- fspaces_quality_frugivore$"details_fspaces"$"sp_pc_coord"
  
  # computes linear model for continuous traits and Kruskall-Wallis tests for other types. 
  frugivore_tr_faxes <- mFD::traits.faxes.cor(sp_tr = traits, sp_faxes_coord = sp_faxes_coord_frugivore[ , c("PC1", "PC2", "PC3", "PC4")],plot = TRUE)
  
  # print traits with significant effect:
  sig_traits <- as.data.frame(frugivore_tr_faxes$"tr_faxes_stat"[which(frugivore_tr_faxes$"tr_faxes_stat"$"p.value" < 0.05), ])
  
  trait_PCs <- frugivore_tr_faxes$"tr_faxes_plot"
  
  # plotting functional space
  sp_faxes_coord_frugivore <- fspaces_quality_frugivore$"details_fspaces"$"sp_pc_coord"
  
  big_plot_frugivore <- mFD::funct.space.plot(sp_faxes_coord = sp_faxes_coord_frugivore[ , c("PC1", "PC2", "PC3", "PC4")], faxes = c("PC1", "PC2", "PC3", "PC4"), alpha_ch = 0.5, shape_vert= 6)
  
  # need to remove parts of the PAM that have values less than or equal to the number of dimensions (3)
  
  # calculate row sums
  row_sums_frugivore <- rowSums(PAM)
  subset_matrix_frugivore <- PAM[row_sums_frugivore >= 3, ]
  
  # match frugivore names
  sp_faxes_coord_frugivore_sub <- as.data.frame(sp_faxes_coord_frugivore[ , c("PC1", "PC2", "PC3", "PC4")])
  
  assign(x=paste(matrix_name), value=subset_matrix_frugivore, envir=.GlobalEnv)
  assign(x=paste(df_name), value=sp_faxes_coord_frugivore_sub, envir=.GlobalEnv)
  print(c(fspaces, trait_PCs, big_plot_frugivore))
  return(list(MAD,sig_traits))
}

# 100km
FDis(PAM_frugivore_site_final_100km, frugivore_traits_df_final,'subset_matrix_frugivore','sp_faxes_coord_frugivore_sub')
FDis(PAM_mammal_site_final_100km, mammal_traits_df_final, 'subset_matrix_mammal', 'sp_faxes_coord_mammal_sub')
FDis(PAM_bird_site_final_100km, bird_traits_df_final, 'subset_matrix_bird', 'sp_faxes_coord_bird_sub')

summary(sp_faxes_coord_frugivore_sub)
summary(sp_faxes_coord_mammal_sub)
summary(sp_faxes_coord_bird_sub)

# check number of species names
nrow(sp_faxes_coord_frugivore_sub)
ncol(subset_matrix_frugivore) 

nrow(sp_faxes_coord_mammal_sub)
ncol(subset_matrix_mammal) 

nrow(sp_faxes_coord_bird_sub)
ncol(subset_matrix_bird) 

sp_faxes_coord_frugivore_sub_names <- row.names(sp_faxes_coord_frugivore_sub)
sp_faxes_coord_mammal_sub_names <- row.names(sp_faxes_coord_mammal_sub)
sp_faxes_coord_bird_sub_names <- row.names(sp_faxes_coord_bird_sub)

subset_matrix_frugivore_names <- colnames(subset_matrix_frugivore)
subset_matrix_mammal_names <- colnames(subset_matrix_mammal)
subset_matrix_bird_names <- colnames(subset_matrix_bird)

# name cleaning
# frugivores
frugivore_names <- intersect(sp_faxes_coord_frugivore_sub_names, subset_matrix_frugivore_names)
frugivore_names <- na.omit(frugivore_names)

sp_faxes_coord_frugivore_sub <- sp_faxes_coord_frugivore_sub[which((row.names(sp_faxes_coord_frugivore_sub) %in% frugivore_names)==TRUE), ]

subset_matrix_frugivore  <- as.data.frame(subset_matrix_frugivore)
subset_matrix_frugivore <- subset_matrix_frugivore[ ,which((colnames(subset_matrix_frugivore) %in% frugivore_names)==TRUE)]

sp_faxes_coord_frugivore_sub <- na.omit(sp_faxes_coord_frugivore_sub)
subset_matrix_frugivore <- na.omit(subset_matrix_frugivore)

nrow(sp_faxes_coord_frugivore_sub)
ncol(subset_matrix_frugivore)

sp_faxes_coord_frugivore_sub <- as.matrix(sp_faxes_coord_frugivore_sub)
subset_matrix_frugivore <- as.matrix(subset_matrix_frugivore)

# mammals
mammal_names <- intersect(sp_faxes_coord_mammal_sub_names, subset_matrix_mammal_names)
mammal_names <- na.omit(mammal_names)

sp_faxes_coord_mammal_sub <- sp_faxes_coord_mammal_sub[which((row.names(sp_faxes_coord_mammal_sub) %in% mammal_names)==TRUE), ]

subset_matrix_mammal  <- as.data.frame(subset_matrix_mammal)
subset_matrix_mammal <- subset_matrix_mammal[ ,which((colnames(subset_matrix_mammal) %in% mammal_names)==TRUE)]

sp_faxes_coord_mammal_sub <- na.omit(sp_faxes_coord_mammal_sub)
subset_matrix_mammal <- na.omit(subset_matrix_mammal)

nrow(sp_faxes_coord_mammal_sub)
ncol(subset_matrix_mammal)

sp_faxes_coord_mammal_sub <- as.matrix(sp_faxes_coord_mammal_sub)
subset_matrix_mammal <- as.matrix(subset_matrix_mammal)

# birds
bird_names <- intersect(sp_faxes_coord_bird_sub_names, subset_matrix_bird_names)
bird_names <- na.omit(bird_names)

sp_faxes_coord_bird_sub <- sp_faxes_coord_bird_sub[which((row.names(sp_faxes_coord_bird_sub) %in% bird_names)==TRUE), ]

subset_matrix_bird <- as.data.frame(subset_matrix_bird)
subset_matrix_bird <- subset_matrix_bird[ ,which((colnames(subset_matrix_bird) %in% bird_names)==TRUE)]

sp_faxes_coord_bird_sub <- na.omit(sp_faxes_coord_bird_sub)
subset_matrix_bird <- na.omit(subset_matrix_bird)

nrow(sp_faxes_coord_bird_sub)
ncol(subset_matrix_bird)

sp_faxes_coord_bird_sub <- as.matrix(sp_faxes_coord_bird_sub)
subset_matrix_bird <- as.matrix(subset_matrix_bird)


# computing FD

# the number of species per assemblage has to be higher or equal to the number of traits

alpha_fd_indices_frugivore <- mFD::alpha.fd.multidim(sp_faxes_coord = sp_faxes_coord_frugivore_sub, asb_sp_w = subset_matrix_frugivore,
                                                     ind_vect = "fdis", details_returned = TRUE)

details_list_frugivore <- alpha_fd_indices_frugivore$"details"

# get functional dispersion
fdis_frugivore <- alpha_fd_indices_frugivore$functional_diversity_indices$fdis


# mapping FD

# generate coordinates
subset_coords_frugivore <-site_loc_key_frugivore_100km[rowSums(PAM_frugivore_site_final_100km) >= 3, ]
subset_coords_frugivore_sp <-subset_coords_frugivore[,1:2 ]

frugivore_fd_sp <- data.frame(subset_coords_frugivore_sp, fdis_frugivore)

# convert the dataframe to sf format
spatial_fdis_frugivore <- st_as_sf(frugivore_fd_sp, coords = c("Longitude", "Latitude"))

# set crs of sf objects
spatial_fdis_frugivore <- spatial_fdis_frugivore %>% st_set_crs(5389)

# extract coordinates 
spatial_fdis_frugivore_coords <- st_coordinates(spatial_fdis_frugivore)

# add coordinates as separate columns
spatial_fdis_frugivore <- spatial_fdis_frugivore %>%
  mutate(x = spatial_fdis_frugivore_coords[, 1], y = spatial_fdis_frugivore_coords[, 2])

summary(spatial_fdis_frugivore)

TAGrid_100km <- TApoly %>%
  st_make_grid(cellsize = c(100000)) %>%
  st_intersection(TropicalAndes_IUCNHabitat_Forest) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number())

spatial_fdis_frugivore$cellid <- st_intersects(spatial_fdis_frugivore, TAGrid_100km)$nn

spatial_fdis_frugivore_grid <- TAGrid_100km %>%
  st_join(spatial_fdis_frugivore, join = st_contains) %>%
  group_by(cellid) %>%
  summarize(fdis_value = mean(fdis_frugivore, na.rm = TRUE)) %>%
  ungroup()

summary(spatial_fdis_frugivore_grid)

frugivoregridFDisTA_100km <-
  ggplot() +
  geom_sf(data = Americas, fill = "white")+
  geom_sf(data = TApoly, fill = "lightgrey", size = 0.1) +
  geom_sf(data = spatial_fdis_frugivore_grid, aes(fill = fdis_value), color = NA) +
  labs(fill = "FDis") +
  ggtitle("100 km") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1, limits = c(0,0.8)) +
  coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
  theme(panel.background = element_rect(fill = "lightblue"),
        text = element_text(size = 12), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
frugivoregridFDisTA_100km

#ggsave("frugivoregridFDisTA_100km.png", frugivoregridFDisTA_100km, path = figure_path)

# extract cell values
frugivore_cellFDis_100km <- spatial_fdis_frugivore_grid
hist(frugivore_cellFDis_100km$fdis_value)


# create function to calculate FD as fdis for frugivores for other spatial grains

# uses inputs from trait objects created above to avoid redundancies 

calculate_fd_as_fdis_frugivores <- function(PAM_site_final) {
  # Calculate row sums and subset matrix early to avoid unnecessary calculations
  row_sums_frugivore <- rowSums(PAM_site_final)
  subset_matrix_frugivore <- PAM_site_final[row_sums_frugivore >= 4, ]
  
  # Select the necessary columns from sp_faxes_coord_frugivore
  sp_faxes_coord_frugivore_sub <- sp_faxes_coord_frugivore_sub[, c("PC1", "PC2", "PC3", "PC4")]
  
  ## check number of species names
  print(nrow(sp_faxes_coord_frugivore_sub))
  print(ncol(subset_matrix_frugivore)) 
  
  sp_faxes_coord_frugivore_sub_names <- row.names(sp_faxes_coord_frugivore_sub)
  subset_matrix_frugivore_names <- colnames(subset_matrix_frugivore)
  
  sp_faxes_coord_frugivore_sub <- as.data.frame(sp_faxes_coord_frugivore_sub)
  row.names(sp_faxes_coord_frugivore_sub) <- sp_faxes_coord_frugivore_sub_names
  
  sp_faxes_coord_frugivore_sub_names <- row.names(sp_faxes_coord_frugivore_sub)
  subset_matrix_frugivore_names <- colnames(subset_matrix_frugivore)
  
  frugivore_names <- intersect(sp_faxes_coord_frugivore_sub_names, subset_matrix_frugivore_names)
  frugivore_names <- na.omit(frugivore_names)
  
  sp_faxes_coord_frugivore_sub <- sp_faxes_coord_frugivore_sub[ which((row.names(sp_faxes_coord_frugivore_sub) %in%
                                                                         frugivore_names)==TRUE), ]
  
  subset_matrix_frugivore  <- as.data.frame(subset_matrix_frugivore)
  subset_matrix_frugivore <- subset_matrix_frugivore[ ,which((colnames(subset_matrix_frugivore) %in% frugivore_names)==TRUE)]
  
  #remove NAs
  sp_faxes_coord_frugivore_sub <- na.omit(sp_faxes_coord_frugivore_sub)
  subset_matrix_frugivore <- na.omit(subset_matrix_frugivore)
  
  print(nrow(sp_faxes_coord_frugivore_sub))
  print(ncol(subset_matrix_frugivore)) 
  
  
  if (nrow(sp_faxes_coord_frugivore_sub) != ncol(subset_matrix_frugivore)) {
    stop("Number of rows in sp_faxes_coord_frugivore_sub does not match number of columns in subset_matrix_frugivore")
  }
  
  sp_faxes_coord_frugivore_sub <- as.matrix(sp_faxes_coord_frugivore_sub)
  subset_matrix_frugivore <- as.matrix(subset_matrix_frugivore)
  
  # HPCC
  num_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
  
  # Use three fewer cores than available
  #num_cores <- parallel::detectCores() - 3
  
  # Set up parallel backend
  cl <- makeCluster(num_cores)
  registerDoParallel(cl)
  
  tryCatch({
    # Define chunk size
    chunk_size <- ceiling(nrow(subset_matrix_frugivore) / num_cores)
    
    # Split data into chunks for parallel processing
    chunks <- split(seq_len(nrow(subset_matrix_frugivore)), ceiling(seq_len(nrow(subset_matrix_frugivore)) / chunk_size))
    
    # Process chunks in parallel
    results_list <- foreach(chunk_indices = chunks, .combine = rbind, .packages = "mFD") %dopar% {
      subset_matrix_frugivore_chunk <- subset_matrix_frugivore[chunk_indices, , drop = FALSE]
      
      alpha_fd_indices_frugivore <- mFD::alpha.fd.multidim(
        sp_faxes_coord = sp_faxes_coord_frugivore_sub, 
        asb_sp_w = subset_matrix_frugivore_chunk, 
        ind_vect = "fdis", 
        details_returned = TRUE
      )
      # Extract fdis values and match with corresponding cell numbers
      fdis_values <- alpha_fd_indices_frugivore$functional_diversity_indices$fdis
      cell_numbers <- rownames(subset_matrix_frugivore)[chunk_indices]
      
      # Create a data frame for this chunk
      chunk_results <- data.frame(Cell_Number = cell_numbers, fdis = fdis_values)
      chunk_results
    }
    
    # Combine all chunk results into a single data frame
    all_results <- do.call(rbind, results_list)
    
    # Stop the parallel backend
    stopCluster(cl)
    
    return(results_list)
  }, error = function(e) {
    stop("Error in parallel processing: ", conditionMessage(e))
  })
}

fdis_frugivore_75km <- calculate_fd_as_fdis_frugivores(PAM_frugivore_site_final_75km)

fdis_frugivore_50km <- calculate_fd_as_fdis_frugivores(PAM_frugivore_site_final_50km)

fdis_frugivore_25km <- calculate_fd_as_fdis_frugivores(PAM_frugivore_site_final_25km)

fdis_frugivore_10km <- calculate_fd_as_fdis_frugivores(PAM_frugivore_site_final_10km)

fdis_frugivore_5km <- calculate_fd_as_fdis_frugivores(PAM_frugivore_site_final_5km)


# map FD for spatial grains other than 100 km
mapping_fd_as_fdis_frugivores <- function(PAM_frugivore_site, site_loc_key_frugivore, fdis_frugivore, resolution_meters) {
  # Generate coordinates
  subset_coords_frugivore <- site_loc_key_frugivore[rowSums(PAM_frugivore_site) >= 4, ]
  subset_coords_frugivore_sp <-subset_coords_frugivore[,1:2 ]
  
  frugivore_fd_sp <- data.frame(subset_coords_frugivore_sp, fdis_frugivore)
  
  # Convert the dataframe to sf format
  spatial_fdis_frugivore <- st_as_sf(frugivore_fd_sp, coords = c("Longitude", "Latitude"))
  
  # set crs of sf objects
  spatial_fdis_frugivore <- spatial_fdis_frugivore %>% st_set_crs(5389)
  
  # Extract coordinates 
  spatial_fdis_frugivore_coords <- st_coordinates(spatial_fdis_frugivore)
  
  # Add coordinates as separate columns
  spatial_fdis_frugivore <- spatial_fdis_frugivore %>%
    mutate(x = spatial_fdis_frugivore_coords[, 1], y = spatial_fdis_frugivore_coords[, 2])
  
  TAGrid <- TApoly %>%
    st_make_grid(cellsize = c(resolution_meters)) %>%
    st_intersection(TropicalAndes_IUCNHabitat_Forest) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(cellid = row_number())
  
  spatial_fdis_frugivore$cellid <- st_intersects(spatial_fdis_frugivore, TAGrid)$nn
  
  spatial_fdis_frugivore_grid <- TAGrid %>%
    st_join(spatial_fdis_frugivore) %>%
    group_by(cellid) %>%
    summarize(fdis_value = mean(fdis_frugivore, na.rm = TRUE)) %>%
    ungroup()
  
  frugivoregridFDisTA <-
    ggplot() +
    geom_sf(data = Americas, fill = "white")+
    geom_sf(data = TApoly, fill = "lightgrey") +
    geom_sf(data = spatial_fdis_frugivore_grid, aes(fill = fdis_value), color = NA) +
    labs(fill = "FDis") +
    scale_fill_distiller(palette = "YlOrBr", direction = 1, limits = c(0,0.8)) +
    coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
    theme(panel.background = element_rect(fill = "lightblue"),
          text = element_text(size = 12), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
  frugivoregridFDisTA
  
  list(frugivoregridFDisTA = frugivoregridFDisTA,
       spatial_fdis_frugivore_grid = spatial_fdis_frugivore_grid)
}

mapping_75km <- mapping_fd_as_fdis_frugivores(PAM_frugivore_site_final_75km, site_loc_key_frugivore_75km, fdis_frugivore_75km$fdis, 75000)
frugivoregridFDisTA_75km <- mapping_75km$frugivoregridFDisTA
frugivore_cellFDis_75km <- mapping_75km$spatial_fdis_frugivore_grid

# add title to maps
(frugivoregridFDisTA_75km <- frugivoregridFDisTA_75km +
    ggtitle("75 km"))
#ggsave("frugivoregridFDisTA_75km.png", frugivoregridFDisTA_75km, path = figure_path)

mapping_50km <- mapping_fd_as_fdis_frugivores(PAM_frugivore_site_final_50km, site_loc_key_frugivore_50km, fdis_frugivore_50km$fdis, 50000)
frugivoregridFDisTA_50km <- mapping_50km$frugivoregridFDisTA
frugivore_cellFDis_50km <- mapping_50km$spatial_fdis_frugivore_grid

# add title to maps
(frugivoregridFDisTA_50km <- frugivoregridFDisTA_50km +
    ggtitle("50 km")
)
#ggsave("frugivoregridFDisTA_50km.png", frugivoregridFDisTA_50km, path = figure_path)

mapping_25km <- mapping_fd_as_fdis_frugivores(PAM_frugivore_site_final_25km, site_loc_key_frugivore_25km, fdis_frugivore_25km$fdis, 25000)
frugivoregridFDisTA_25km <- mapping_25km$frugivoregridFDisTA
frugivore_cellFDis_25km <- mapping_25km$spatial_fdis_frugivore_grid

# add title to maps
(frugivoregridFDisTA_25km <- frugivoregridFDisTA_25km +
    ggtitle("25 km")
)
#ggsave("frugivoregridFDisTA_25km.png", frugivoregridFDisTA_25km, path = figure_path)

mapping_10km <- mapping_fd_as_fdis_frugivores(PAM_frugivore_site_final_10km, site_loc_key_frugivore_10km, fdis_frugivore_10km$fdis, 10000)
frugivoregridFDisTA_10km <- mapping_10km$frugivoregridFDisTA
frugivore_cellFDis_10km <- mapping_10km$spatial_fdis_frugivore_grid

# add title to maps
(frugivoregridFDisTA_10km <- frugivoregridFDisTA_10km +
    ggtitle("10 km")
)
#ggsave("frugivoregridFDisTA_10km.png", frugivoregridFDisTA_10km, path = figure_path)

hist(frugivore_cellFDis_75km$fdis_value)
hist(frugivore_cellFDis_50km$fdis_value)
hist(frugivore_cellFDis_25km$fdis_value)
hist(frugivore_cellFDis_10km$fdis_value)


# 5 km

# generate coordinates
subset_coords_frugivore_5km <-site_loc_key_frugivore_5km[rowSums(PAM_frugivore_site_final_5km) >= 4, ]
subset_coords_frugivore_sp_5km <-subset_coords_frugivore_5km[,1:2 ]

frugivore_fd_sp_5km <- data.frame(subset_coords_frugivore_sp_5km, fdis_frugivore_5km)

# convert the dataframe to sf format
spatial_fdis_frugivore_5km <- st_as_sf(frugivore_fd_sp_5km, coords = c("Longitude", "Latitude"))

# set crs of sf objects
spatial_fdis_frugivore_5km <- spatial_fdis_frugivore_5km %>% st_set_crs(5389)

# extract coordinates 
spatial_fdis_frugivore_coords_5km <- st_coordinates(spatial_fdis_frugivore_5km)

# add coordinates as separate columns
spatial_fdis_frugivore_5km <- spatial_fdis_frugivore_5km %>%
  mutate(x = spatial_fdis_frugivore_coords_5km[, 1], y = spatial_fdis_frugivore_coords_5km[, 2])

TAGrid_5km <- TApoly %>%
  st_make_grid(cellsize = c(5000)) %>%
  st_intersection(TropicalAndes_IUCNHabitat_Forest) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number())

spatial_fdis_frugivore_5km$cellid <- st_intersects(spatial_fdis_frugivore_5km, TAGrid_5km)$nn

# determine number of cores to use
#num_cores <- detectCores() - 2

# HPCC
num_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))

# register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# ensure the cluster stops when done
on.exit(stopCluster(cl))

# split TAGrid_5km indices into chunks for parallel processing
chunk_indices <- split(seq_len(nrow(TAGrid_5km)), rep(1:num_cores, each = ceiling(nrow(TAGrid_5km) / num_cores), length.out = nrow(TAGrid_5km)))

# perform spatial join and aggregation in parallel
results_list <- foreach(indices = chunk_indices, .combine = 'c', .packages = c("sf", "dplyr")) %dopar% {
  grid_chunk <- TAGrid_5km[indices, ]
  
  # perform spatial join
  joined <- st_join(grid_chunk, spatial_fdis_frugivore_5km)
  
  # group by cellid and summarize fdis values
  summarized <- joined %>%
    group_by(cellid) %>%
    summarize(fdis_value = mean(fdis, na.rm = TRUE)) %>%
    ungroup()
  
  # ensure that summarized is a data frame and add debugging information
  summarized_df <- as.data.frame(summarized)
  
  # debugging: check the structure of the summarized data frame
  message("Processed chunk: ", indices[1], " to ", indices[length(indices)])
  str(summarized_df)
  
  list(summarized_df)  # Return as list to avoid unintended simplification
}

# flatten the results_list and ensure each element is a data frame
flattened_results <- map_dfr(results_list, ~ as.data.frame(.x))

# check the structure of the combined result
str(flattened_results)

# convert to sf object if applicable
if ("geometry" %in% colnames(flattened_results)) {
  spatial_fdis_frugivore_grid <- st_as_sf(flattened_results)
} else {
  spatial_fdis_frugivore_grid <- flattened_results
}

# check the structure of the final output
str(spatial_fdis_frugivore_grid)

frugivoregridFDisTA_5km <-
  ggplot() +
  geom_sf(data = Americas, fill = "white")+
  geom_sf(data = TApoly, fill = "lightgrey") +
  geom_sf(data = spatial_fdis_frugivore_grid, aes(fill = fdis_value), color = NA) +
  labs(fill = "FDis") +
  scale_fill_distiller(palette = "YlOrBr", direction = 1, limits = c(0,0.8)) +
  coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
  theme(panel.background = element_rect(fill = "lightblue"),
        text = element_text(size = 12), plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
frugivore_cellFDis_5km <- spatial_fdis_frugivore_grid

# add title to maps
(frugivoregridFDisTA_5km <- frugivoregridFDisTA_5km +
    ggtitle("5 km")
)
#ggsave("frugivoregridFDisTA_5km.png", frugivoregridFDisTA_5km, path = figure_path)
hist(frugivore_cellFDis_5km$fdis_value)


# plants

# 100 km

# summary of the assemblages * species dataframe
asb_sp_plant_summ <- asb.sp.summary(asb_sp_w = PAM_plant_site_final_100km)

# species traits summary
plant_traits_summ <- sp.tr.summary(tr_cat = plant_trait_cat, sp_tr = plant_traits_df_final)

# estimate functional trait-based distances between species
sp_dist_plant <- funct.dist( sp_tr = plant_traits_df_final, tr_cat = plant_trait_cat, metric = "gower",
                             scale_euclid = "scale_center", ordinal_var = "classic", weight_type = "equal", stop_if_NA = TRUE)

# generate a multidimensional space
fspaces_quality_plant <- quality.fspaces(sp_dist = sp_dist_plant, maxdim_pcoa = 10, deviation_weighting = "absolute",
                                         fdist_scaling = FALSE, fdendro = "average")

# look at the quality spaces only (MAD index looks at the mean absolute deviation from the dissimilarity matrix; want the deviation to be low meaning that the true distances have been retained in the PCA)
round(fspaces_quality_plant$"quality_fspaces", 3)

# plot the quality spaces (chose to look at )
quality.fspaces.plot(fspaces_quality = fspaces_quality_plant, quality_metric = "mad",
                     fspaces_plot = c("pcoa_6d", "pcoa_7d", "pcoa_8d"))

# testing correlation between functional axes and traits
sp_faxes_coord_plant <- fspaces_quality_plant$"details_fspaces"$"sp_pc_coord"

# computes linear model for continuous traits and Kruskall-Wallis tests for other types. 
plant_tr_faxes <- mFD::traits.faxes.cor(sp_tr = plant_traits_df_final, 
                                        sp_faxes_coord = sp_faxes_coord_plant[ , c("PC1", "PC2", "PC3", "PC4")],
                                        plot = TRUE)

# print traits with significant effect:
plant_tr_faxes$"tr_faxes_stat"[which(plant_tr_faxes$"tr_faxes_stat"$"p.value" < 0.05), ]
# return plots:
plant_tr_faxes$"tr_faxes_plot"

# plotting functional space
sp_faxes_coord_plant <- fspaces_quality_plant$"details_fspaces"$"sp_pc_coord"

big_plot_plant <- mFD::funct.space.plot(sp_faxes_coord = sp_faxes_coord_plant[ , c("PC1", "PC2", "PC3", "PC4")],
                                        faxes = c("PC1", "PC2", "PC3", "PC4"), alpha_ch = 0.5, shape_vert = 6)
big_plot_plant

# need to remove parts of the PAM that have values less than or equal to the number of dimensions (4)
# calculate row sums
row_sums_plant <- rowSums(PAM_plant_site_final_100km)
subset_matrix_plant <- PAM_plant_site_final_100km[row_sums_plant >= 4, ]

# match plant names
sp_faxes_coord_plant_sub <- sp_faxes_coord_plant[ , c("PC1", "PC2", "PC3", "PC4")]
summary(sp_faxes_coord_plant_sub)

# check number of species names
nrow(sp_faxes_coord_plant_sub)
ncol(subset_matrix_plant) 

sp_faxes_coord_plant_sub_names <- row.names(sp_faxes_coord_plant_sub)
subset_matrix_plant_names <- colnames(subset_matrix_plant)

sp_faxes_coord_plant_sub <- as.data.frame(sp_faxes_coord_plant_sub)
row.names(sp_faxes_coord_plant_sub) <- sp_faxes_coord_plant_sub_names

sp_faxes_coord_plant_sub_names <- row.names(sp_faxes_coord_plant_sub)
subset_matrix_plant_names <- colnames(subset_matrix_plant)

plant_names <- intersect(sp_faxes_coord_plant_sub_names, subset_matrix_plant_names)
plant_names <- na.omit(plant_names)

sp_faxes_coord_plant_sub <- sp_faxes_coord_plant_sub[ which((row.names(sp_faxes_coord_plant_sub) %in% plant_names)==TRUE), ]

subset_matrix_plant  <- as.data.frame(subset_matrix_plant)
subset_matrix_plant <- subset_matrix_plant[ ,which((colnames(subset_matrix_plant) %in% plant_names)==TRUE)]

# remove NAs
sp_faxes_coord_plant_sub <- na.omit(sp_faxes_coord_plant_sub)
subset_matrix_plant <- na.omit(subset_matrix_plant)

nrow(sp_faxes_coord_plant_sub)
ncol(subset_matrix_plant)

sp_faxes_coord_plant_sub <- as.matrix(sp_faxes_coord_plant_sub)
subset_matrix_plant <- as.matrix(subset_matrix_plant)

# computing FD

# the number of species per assemblage has to be higher or equal to the number of traits

alpha_fd_indices_plant <- mFD::alpha.fd.multidim(sp_faxes_coord = sp_faxes_coord_plant_sub, asb_sp_w = subset_matrix_plant,
                                                 ind_vect = "fdis", details_returned = TRUE)

details_list_plant <- alpha_fd_indices_plant$"details"

# get functional dispersion
fdis_plant <- alpha_fd_indices_plant$functional_diversity_indices$fdis


# mapping FD

# generate coordinates
subset_coords_plant <-site_loc_key_plant_100km[rowSums(PAM_plant_site_final_100km) >= 4, ]
subset_coords_plant_sp <-subset_coords_plant[,1:2 ]

plant_fd_sp <- data.frame(subset_coords_plant_sp, fdis_plant)

# convert the dataframe to sf format
spatial_fdis_plant <- st_as_sf(plant_fd_sp, coords = c("Longitude", "Latitude"))

# set crs of sf objects
spatial_fdis_plant <- spatial_fdis_plant %>% st_set_crs(5389)

# extract coordinates 
spatial_fdis_plant_coords <- st_coordinates(spatial_fdis_plant)

# add coordinates as separate columns
spatial_fdis_plant <- spatial_fdis_plant %>%
  mutate(x = spatial_fdis_plant_coords[, 1], y = spatial_fdis_plant_coords[, 2])

summary(spatial_fdis_plant)

TAGrid_100km <- TApoly %>%
  st_make_grid(cellsize = c(100000)) %>%
  st_intersection(TropicalAndes_IUCNHabitat_Forest) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number())

spatial_fdis_plant$cellid <- st_intersects(spatial_fdis_plant, TAGrid_100km)$nn

spatial_fdis_plant_grid <- TAGrid_100km %>%
  st_join(spatial_fdis_plant) %>%
  group_by(cellid) %>%
  summarize(fdis_value = mean(fdis_plant, na.rm = TRUE)) %>%
  ungroup()

summary(spatial_fdis_plant_grid)

plantgridFDisTA_100km <-
  ggplot() +
  geom_sf(data = Americas, fill = "white")+
  geom_sf(data = TApoly, fill = "lightgrey", size = 0.1) +
  geom_sf(data = spatial_fdis_plant_grid, aes(fill = fdis_value), color = NA) +
  labs(fill = "FDis") +
  ggtitle("100 km") +
  scale_fill_distiller(palette = "Greens", direction = 1, limits = c(0,0.8)) +
  coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
  theme(panel.background = element_rect(fill = "lightblue"),
        text = element_text(size = 12), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
plantgridFDisTA_100km

#ggsave("plantgridFDisTA_100km.png", plantgridFDisTA_100km, path = figure_path)

# extract cell values
plant_cellFDis_100km <- spatial_fdis_plant_grid
hist(plant_cellFDis_100km$fdis_value)

# create function to calculate FD as fdis for plants for other spatial grains

# uses inputs from trait objects created above to avoid redundancies 

calculate_fd_as_fdis_plants <- function(PAM_site_final) {
  # Calculate row sums and subset matrix early to avoid unnecessary calculations
  row_sums_plant <- rowSums(PAM_site_final)
  subset_matrix_plant <- PAM_site_final[row_sums_plant >= 4, ]
  
  # Select the necessary columns from sp_faxes_coord_plant
  sp_faxes_coord_plant_sub <- sp_faxes_coord_plant[, c("PC1", "PC2", "PC3", "PC4")]
  
  if (nrow(sp_faxes_coord_plant_sub) != ncol(subset_matrix_plant)) {
    stop("Number of rows in sp_faxes_coord_plant_sub does not match number of columns in subset_matrix_plant")
  }
  
  sp_faxes_coord_plant_sub <- as.matrix(sp_faxes_coord_plant_sub)
  subset_matrix_plant <- as.matrix(subset_matrix_plant)
  
  # Use three fewer cores than available
  num_cores <- parallel::detectCores() - 3
  
  # Set up parallel backend
  #cl <- makeCluster(num_cores)
  # HPCC
  num_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))
  registerDoParallel(cl)
  
  tryCatch({
    # Define chunk size
    chunk_size <- ceiling(nrow(subset_matrix_plant) / num_cores)
    
    # Split data into chunks for parallel processing
    chunks <- split(seq_len(nrow(subset_matrix_plant)), ceiling(seq_len(nrow(subset_matrix_plant)) / chunk_size))
    
    # Process chunks in parallel
    results_list <- foreach(chunk_indices = chunks, .combine = rbind, .packages = "mFD") %dopar% {
      subset_matrix_plant_chunk <- subset_matrix_plant[chunk_indices, , drop = FALSE]
      
      alpha_fd_indices_plant <- mFD::alpha.fd.multidim(
        sp_faxes_coord = sp_faxes_coord_plant_sub, 
        asb_sp_w = subset_matrix_plant_chunk, 
        ind_vect = "fdis", 
        details_returned = TRUE
      )
      # Extract fdis values and match with corresponding cell numbers
      fdis_values <- alpha_fd_indices_plant$functional_diversity_indices$fdis
      cell_numbers <- rownames(subset_matrix_plant)[chunk_indices]
      
      # Create a data frame for this chunk
      chunk_results <- data.frame(Cell_Number = cell_numbers, fdis = fdis_values)
      chunk_results
    }
    
    # Combine all chunk results into a single data frame
    all_results <- do.call(rbind, results_list)
    
    # Stop the parallel backend
    stopCluster(cl)
    
    return(results_list)
  }, error = function(e) {
    stop("Error in parallel processing: ", conditionMessage(e))
  })
}


fdis_plant_75km <- calculate_fd_as_fdis_plants(PAM_site_final = PAM_plant_site_final_75km)

fdis_plant_50km <- calculate_fd_as_fdis_plants(PAM_site_final = PAM_plant_site_final_50km)

fdis_plant_25km <- calculate_fd_as_fdis_plants(PAM_site_final = PAM_plant_site_final_25km)

fdis_plant_10km <- calculate_fd_as_fdis_plants(PAM_site_final = PAM_plant_site_final_10km)

fdis_plant_5km <- calculate_fd_as_fdis_plants(PAM_site_final = PAM_plant_site_final_5km)


# map FD for spatial grains other than 100 km
mapping_fd_as_fdis_plants <- function(PAM_plant_site, site_loc_key_plant, fdis_plant, resolution_meters) {
  # Generate coordinates
  subset_coords_plant <- site_loc_key_plant[rowSums(PAM_plant_site) >= 4, ]
  subset_coords_plant_sp <- subset_coords_plant[, 1:2]
  
  plant_fd_sp <- data.frame(subset_coords_plant_sp, fdis_plant)
  
  # Convert the dataframe to sf format
  spatial_fdis_plant <- st_as_sf(plant_fd_sp, coords = c("Longitude", "Latitude"))
  
  # set crs of sf objects
  spatial_fdis_plant <- spatial_fdis_plant %>% st_set_crs(5389)
  
  # Extract coordinates 
  spatial_fdis_plant_coords <- st_coordinates(spatial_fdis_plant)
  
  # Add coordinates as separate columns
  spatial_fdis_plant <- spatial_fdis_plant %>%
    mutate(x = spatial_fdis_plant_coords[, 1], y = spatial_fdis_plant_coords[, 2])
  
  # Generate the grid
  TAGrid <- TApoly %>%
    st_make_grid(cellsize = resolution_meters) %>%
    st_intersection(TropicalAndes_IUCNHabitat_Forest) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(cellid = row_number())
  
  spatial_fdis_plant$cellid <- st_intersects(spatial_fdis_plant, TAGrid)$nn
  
  spatial_fdis_plant_grid <- TAGrid %>%
    st_join(spatial_fdis_plant) %>%
    group_by(cellid) %>%
    summarize(fdis_value = mean(fdis_plant, na.rm = TRUE)) %>%
    ungroup()
  
  # Create the plot
  plantgridFDisTA <- ggplot() +
    geom_sf(data = Americas, fill = "white") +
    geom_sf(data = TApoly, fill = "lightgrey") +
    geom_sf(data = spatial_fdis_plant_grid, aes(fill = fdis_value), color = NA) +
    labs(fill = "FDis") +
    scale_fill_distiller(palette = "Greens", direction = 1, limits = c(0,0.8)) +
    coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
    theme(panel.background = element_rect(fill = "lightblue"),
          text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  list(plantgridFDisTA = plantgridFDisTA, spatial_fdis_plant_grid = spatial_fdis_plant_grid)
}

mapping_75km <- mapping_fd_as_fdis_plants(PAM_plant_site_final_75km, site_loc_key_plant_75km, fdis_plant_75km$fdis, 75000)
plantgridFDisTA_75km <- mapping_75km$plantgridFDisTA
plant_cellFDis_75km <- mapping_75km$spatial_fdis_plant_grid

# add title to maps
(plantgridFDisTA_75km <- plantgridFDisTA_75km +
    ggtitle("75 km")
)

#ggsave("plantgridFDisTA_75km.png", plantgridFDisTA_75km, path = figure_path)

mapping_50km <- mapping_fd_as_fdis_plants(PAM_plant_site_final_50km, site_loc_key_plant_50km, fdis_plant_50km$fdis, 50000)
plantgridFDisTA_50km <- mapping_50km$plantgridFDisTA
plant_cellFDis_50km <- mapping_50km$spatial_fdis_plant_grid

# add title to maps
(plantgridFDisTA_50km <- plantgridFDisTA_50km +
    ggtitle("50 km")
)

#ggsave("plantgridFDisTA_50km.png", plantgridFDisTA_50km, path = figure_path)

mapping_25km <- mapping_fd_as_fdis_plants(PAM_plant_site_final_25km, site_loc_key_plant_25km, fdis_plant_25km$fdis, 25000)
plantgridFDisTA_25km <- mapping_25km$plantgridFDisTA
plant_cellFDis_25km <- mapping_25km$spatial_fdis_plant_grid

# add title to maps
(plantgridFDisTA_25km <- plantgridFDisTA_25km +
    ggtitle("25 km")
)

#ggsave("plantgridFDisTA_25km.png", plantgridFDisTA_25km, path = figure_path)

mapping_10km <- mapping_fd_as_fdis_plants(PAM_plant_site_final_10km, site_loc_key_plant_10km, fdis_plant_10km$fdis, 10000)
plantgridFDisTA_10km <- mapping_10km$plantgridFDisTA
plant_cellFDis_10km <- mapping_10km$spatial_fdis_plant_grid

# add title to maps
(plantgridFDisTA_10km <- plantgridFDisTA_10km +
    ggtitle("10 km")
)

#ggsave("plantgridFDisTA_10km.png", plantgridFDisTA_10km, path = figure_path)

hist(plant_cellFDis_75km$fdis_value)
hist(plant_cellFDis_50km$fdis_value)
hist(plant_cellFDis_25km$fdis_value)
hist(plant_cellFDis_10km$fdis_value)


# 5 km 

# generate coordinates
subset_coords_plant_5km <-site_loc_key_plant_5km[rowSums(PAM_plant_site_final_5km) >= 4, ]
subset_coords_plant_sp_5km <-subset_coords_plant_5km[,1:2 ]

plant_fd_sp_5km <- data.frame(subset_coords_plant_sp_5km, fdis_plant_5km)

# convert the dataframe to sf format
spatial_fdis_plant_5km <- st_as_sf(plant_fd_sp_5km, coords = c("Longitude", "Latitude"))

# set crs of sf objects
spatial_fdis_plant_5km <- spatial_fdis_plant_5km %>% st_set_crs(5389)

# extract coordinates 
spatial_fdis_plant_coords_5km <- st_coordinates(spatial_fdis_plant_5km)

# add coordinates as separate columns
spatial_fdis_plant_5km <- spatial_fdis_plant_5km %>%
  mutate(x = spatial_fdis_plant_coords_5km[, 1], y = spatial_fdis_plant_coords_5km[, 2])

summary(spatial_fdis_plant_5km)

TAGrid_5km <- TApoly %>%
  st_make_grid(cellsize = c(5000)) %>%
  st_intersection(TropicalAndes_IUCNHabitat_Forest) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(cellid = row_number())

spatial_fdis_plant_5km$cellid <- st_intersects(spatial_fdis_plant_5km, TAGrid_5km)$nn


# determine number of cores to use

#num_cores <- detectCores() - 3

# HPCC
num_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "1"))

# register parallel backend
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# ensure the cluster stops when done
on.exit(stopCluster(cl))

# split TAGrid_5km indices into chunks for parallel processing
chunk_indices <- split(seq_len(nrow(TAGrid_5km)), rep(1:num_cores, each = ceiling(nrow(TAGrid_5km) / num_cores), length.out = nrow(TAGrid_5km)))

# perform spatial join and aggregation in parallel
results_list <- foreach(indices = chunk_indices, .combine = 'c', .packages = c("sf", "dplyr")) %dopar% {
  grid_chunk <- TAGrid_5km[indices, ]
  
  # Perform spatial join
  joined <- st_join(grid_chunk, spatial_fdis_plant_5km)
  
  # Group by cellid and summarize fdis values
  summarized <- joined %>%
    group_by(cellid) %>%
    summarize(fdis_value = mean(fdis, na.rm = TRUE)) %>%
    ungroup()
  
  # Ensure that summarized is a data frame and add debugging information
  summarized_df <- as.data.frame(summarized)
  
  
  list(as.data.frame(summarized_df))
}

# flatten the results_list and ensure each element is a data frame
flattened_results <- map_dfr(results_list, ~ as.data.frame(.x))

# check the structure of the combined result
str(flattened_results)

# convert to sf object if applicable
if ("geometry" %in% colnames(flattened_results)) {
  spatial_fdis_plant_grid_5km <- st_as_sf(flattened_results)
} else {
  spatial_fdis_plant_grid_5km <- flattened_results
}

# check the structure of the final output
str(spatial_fdis_plant_grid_5km)

plantgridFDisTA_5km <-
  ggplot() +
  geom_sf(data = Americas, fill = "white")+
  geom_sf(data = TApoly, fill = "lightgrey") +
  geom_sf(data = spatial_fdis_plant_grid_5km, aes(fill = fdis_value), color = NA) +
  labs(fill = "FDis") +
  scale_fill_distiller(palette = "Greens", direction = 1, limits = c(0,0.8)) +
  coord_sf(xlim = c(-82, -60), ylim = c(-24, 14), expand = FALSE, crs = 4326) +
  theme(panel.background = element_rect(fill = "lightblue"),
        text = element_text(size = 12), plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

plant_cellFDis_5km <- spatial_fdis_plant_grid_5km

# add title to maps
(plantgridFDisTA_5km <- plantgridFDisTA_5km +
    ggtitle("5 km")
)

#ggsave("plantgridFDisTA_5km.png", plantgridFDisTA_5km, path = figure_path)

hist(plant_cellFDis_5km$fdis_value)


# multi panel plots of FD maps

# make sure all plots for the same taxa have the same legend scale
plant_plot_legend <- function(plot){
  plot + scale_fill_distiller(palette = "Greens", direction = 1, limits = c(0,0.8),
                              oob = scales::squish, breaks = seq(0, 0.8, by = 0.2)) +
    theme(legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10)) +
    annotation_scale(location = "bl", width_hint = 0.5)
}
frugivore_plot_legend <- function(plot){
  plot + scale_fill_distiller(palette = "YlOrBr", direction = 1, limits = c(0,0.8),
                              oob = scales::squish, breaks = seq(0, 0.8, by = 0.2)) +
    theme(legend.key.height = unit(0.5, "in"), legend.text = element_text(size = 10)) +
    annotation_scale(location = "bl", width_hint = 0.5)
}

(plantgridFDisTA_100km <- plant_plot_legend(plantgridFDisTA_100km))
(plantgridFDisTA_75km <- plant_plot_legend(plantgridFDisTA_75km))
(plantgridFDisTA_50km <- plant_plot_legend(plantgridFDisTA_50km))
(plantgridFDisTA_25km <- plant_plot_legend(plantgridFDisTA_25km))
(plantgridFDisTA_10km <- plant_plot_legend(plantgridFDisTA_10km))
(plantgridFDisTA_5km <- plant_plot_legend(plantgridFDisTA_5km))

(frugivoregridFDisTA_100km <- frugivore_plot_legend(frugivoregridFDisTA_100km))
(frugivoregridFDisTA_75km <- frugivore_plot_legend(frugivoregridFDisTA_75km))
(frugivoregridFDisTA_50km <- frugivore_plot_legend(frugivoregridFDisTA_50km))
(frugivoregridFDisTA_25km <- frugivore_plot_legend(frugivoregridFDisTA_25km))
(frugivoregridFDisTA_10km <- frugivore_plot_legend(frugivoregridFDisTA_10km))
(frugivoregridFDisTA_5km <- frugivore_plot_legend(frugivoregridFDisTA_5km))


# plants

# arrange the plots with common legend
all_plant_fdis_plots <- ggarrange(plantgridFDisTA_5km, plantgridFDisTA_10km,
                                  plantgridFDisTA_25km, plantgridFDisTA_50km,
                                  plantgridFDisTA_75km, plantgridFDisTA_100km,
                                  ncol = 6, nrow = 1,
                                  common.legend = TRUE, legend = "left")

all_plant_fdis_plots

all_plant_fdis_plots_labeled <- ggpubr::annotate_figure(all_plant_fdis_plots,
                                                        left = ggpubr::text_grob("Plants", face = "bold", size = 20, 
                                                                                 rot = 90))
all_plant_fdis_plots_labeled
ggsave("plant_fdis_plots.png", all_plant_fdis_plots_labeled, path = figure_path, width = 16, height = 5, units = "in")


# frugivores
all_frugivore_fdis_plots <- ggpubr::ggarrange(frugivoregridFDisTA_5km,frugivoregridFDisTA_10km,
                                              frugivoregridFDisTA_25km, frugivoregridFDisTA_50km,
                                              frugivoregridFDisTA_75km, frugivoregridFDisTA_100km,
                                              ncol = 6, nrow = 1,
                                              common.legend = TRUE, legend = "left")
all_frugivore_fdis_plots

all_frugivore_fdis_plots_labeled <- ggpubr::annotate_figure(all_frugivore_fdis_plots,
                                                            left = ggpubr::text_grob("Frugivores", face = "bold", size = 20,
                                                                                     rot = 90))
all_frugivore_fdis_plots_labeled
ggsave("frugivore_fdis_plots.png", all_frugivore_fdis_plots_labeled, path = figure_path, width = 16, height = 5, units = "in")


# combine plants & frugivores
all_fdis_plots <- ggpubr::ggarrange(all_plant_fdis_plots_labeled,
                                    all_frugivore_fdis_plots_labeled,
                                    ncol = 1, nrow = 2)
# add a white background to the plot
all_fdis_plots <- all_fdis_plots + theme(plot.background = element_rect(fill = "white", color = NA))
all_fdis_plots

ggsave("all_fdis_plots.png", all_fdis_plots, path = figure_path, width = 16, height = 10, units = "in")
```

# write data to csv

# have to remove geometry before saving to csv
plant_cellFDis_5km_df <- data.frame(cellid = plant_cellFDis_5km$fdis_value,
                                    fdis_value = plant_cellFDis_5km$fdis_value)
frugivore_cellFDis_5km_df <- data.frame(cellid = frugivore_cellFDis_5km$fdis_value,
                                        fdis_value = frugivore_cellFDis_5km$fdis_value)

plant_cellFDis_10km_df <- data.frame(cellid = plant_cellFDis_10km$fdis_value,
                                     fdis_value = plant_cellFDis_10km$fdis_value)
frugivore_cellFDis_10km_df <- data.frame(cellid = frugivore_cellFDis_10km$fdis_value,
                                         fdis_value = frugivore_cellFDis_10km$fdis_value)

plant_cellFDis_25km_df <- data.frame(cellid = plant_cellFDis_25km$fdis_value,
                                     fdis_value = plant_cellFDis_25km$fdis_value)
frugivore_cellFDis_25km_df <- data.frame(cellid = frugivore_cellFDis_25km$fdis_value,
                                         fdis_value = frugivore_cellFDis_25km$fdis_value)

plant_cellFDis_50km_df <- data.frame(cellid = plant_cellFDis_50km$fdis_value,
                                     fdis_value = plant_cellFDis_50km$fdis_value)
frugivore_cellFDis_50km_df <- data.frame(cellid = frugivore_cellFDis_50km$fdis_value,
                                         fdis_value = frugivore_cellFDis_50km$fdis_value)

plant_cellFDis_75km_df <- data.frame(cellid = plant_cellFDis_75km$fdis_value,
                                     fdis_value = plant_cellFDis_75km$fdis_value)
frugivore_cellFDis_75km_df <- data.frame(cellid = frugivore_cellFDis_75km$fdis_value,
                                         fdis_value = frugivore_cellFDis_75km$fdis_value)

plant_cellFDis_100km_df <- data.frame(cellid = plant_cellFDis_100km$fdis_value,
                                      fdis_value = plant_cellFDis_100km$fdis_value)
frugivore_cellFDis_100km_df <- data.frame(cellid = frugivore_cellFDis_100km$fdis_value,
                                          fdis_value = frugivore_cellFDis_100km$fdis_value)

write.csv(plant_cellFDis_5km_df, file.path(output_path,"TropicalAndes_plantFDis_5km.csv"), row.names = FALSE)
write.csv(frugivore_cellFDis_5km_df, file.path(output_path,"TropicalAndes_frugivoreFDis_5km.csv"), row.names = FALSE)

write.csv(plant_cellFDis_10km_df, file.path(output_path,"TropicalAndes_plantFDis_10km.csv"), row.names = FALSE)
write.csv(frugivore_cellFDis_10km_df, file.path(output_path,"TropicalAndes_frugivoreFDis_10km.csv"), row.names = FALSE)

write.csv(plant_cellFDis_25km_df, file.path(output_path,"TropicalAndes_plantFDis_25km.csv"), row.names = FALSE)
write.csv(frugivore_cellFDis_25km_df, file.path(output_path,"TropicalAndes_frugivoreFDis_25km.csv"), row.names = FALSE)

write.csv(plant_cellFDis_50km_df, file.path(output_path,"TropicalAndes_plantFDis_50km.csv"), row.names = FALSE)
write.csv(frugivore_cellFDis_50km_df, file.path(output_path,"TropicalAndes_frugivoreFDis_50km.csv"), row.names = FALSE)

write.csv(plant_cellFDis_75km_df, file.path(output_path,"TropicalAndes_plantFDis_75km.csv"), row.names = FALSE)
write.csv(frugivore_cellFDis_75km_df, file.path(output_path,"TropicalAndes_frugivoreFDis_75km.csv"), row.names = FALSE)

write.csv(plant_cellFDis_100km_df, file.path(output_path,"TropicalAndes_plantFDis_100km.csv"), row.names = FALSE)
write.csv(frugivore_cellFDis_100km_df, file.path(output_path,"TropicalAndes_frugivoreFDis_100km.csv"), row.names = FALSE)