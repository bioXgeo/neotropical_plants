#Func Div troubleshooting


# Load required packages
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(smoothr)
library(purrr)
library(raster)
library(scico)
library(ggspatial)
library(letsR)
library(mFD)

# Set file paths
data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
data_path_L0 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
output_path_L2 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L2')


# Read in Data

TropicalAndes_plant_occ_forest <- read.csv(file.path(data_path_L0,"TropicalAndes_GBIF_plant_occ.csv"))
TropicalAndes_frugivore_occ_forest <- read.csv(file.path(data_path_L0,"TropicalAndes_GBIF_frugivore_occ.csv"))
TropicalAndes_IUCNHabitat_Forest <- read_sf(file.path(data_path_L0, "Forest_sf.shp"), layer = "Forest_sf")
frugivore_traits <- read.csv(file.path(data_path_L1,"TropicalAndes_Frugivoria_traits_Forest.csv"))
plant_traits <- read.csv(file.path(data_path_L1,"TropicalAndes_plant_traits_forest.csv"))


# Create Presence-Absense matrix

x <- TropicalAndes_plant_occ_forest$decimalLongitude
y <- TropicalAndes_plant_occ_forest$decimalLatitude
plant_xy <- cbind(x, y)
plant_species <- TropicalAndes_plant_occ_forest$species
plants_PAM <- lets.presab.points(plant_xy, plant_species, xmn = -85, xmx = -54, ymn = -24, ymx = 14)
summary(plants_PAM)
plot(plants_PAM, xlab = "Longitude", ylab = "Latitude", main = "Plant richness map")
plants_PAM_matrix <- lets.presab.points(plant_xy, plant_species, xmn = -85, xmx = -54, ymn = -24, ymx = 14, show.matrix = TRUE, remove.cells = TRUE)


x <- TropicalAndes_frugivore_occ_forest$decimalLongitude
y <- TropicalAndes_frugivore_occ_forest$decimalLatitude
frugivore_xy <- cbind(x, y)
frugivore_species <- TropicalAndes_frugivore_occ_forest$species
frugivore_PAM <- lets.presab.points(frugivore_xy, frugivore_species, xmn = -85, xmx = -54, ymn = -24, ymx = 14)
summary(frugivore_PAM)
plot(frugivore_PAM, xlab = "Longitude", ylab = "Latitude", main = "Frugivore richness map")
frugivore_PAM_matrix <- lets.presab.points(frugivore_xy, frugivore_species, xmn = -85, xmx = -54, ymn = -24, ymx = 14, show.matrix = TRUE, remove.cells = TRUE)


# Prepping data for functional diversity calculations


plant_traits <- na.omit(plant_traits)
frugivore_traits <- na.omit(frugivore_traits)
frugivore_traits$body_mass_e <- as.numeric(frugivore_traits$body_mass_e)
frugivore_traits$body_size_mm <- as.numeric(frugivore_traits$body_size_mm)
plant_traits <- plant_traits[, c("species", "plant_height", "dispersal_syndrome", "plant_lifespan")]
frugivore_traits <- frugivore_traits[, c("IUCN_species_name", "diet_cat", "body_mass_e", "body_size_mm", "generation_time")]


coordinates_sf_plant <- st_as_sf(TropicalAndes_plant_occ_forest, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(TropicalAndes_IUCNHabitat_Forest))
coordinates_sf_frugivore <- st_as_sf(TropicalAndes_frugivore_occ_forest, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(TropicalAndes_IUCNHabitat_Forest))


# Perform the spatial intersection
intersections_plant_TA <- st_intersection(coordinates_sf_plant, TropicalAndes_IUCNHabitat_Forest)
intersections_frugivore_TA <- st_intersection(coordinates_sf_frugivore, TropicalAndes_IUCNHabitat_Forest)
# Convert the intersected sf object back to a dataframe
coordinates_df_subset_plant <- as.data.frame(intersections_plant_TA)
coordinates_df_subset_frugivore <- as.data.frame(intersections_frugivore_TA)



# Print the resulting subset of coordinates
print(coordinates_df_subset_plant)
print(coordinates_df_subset_frugivore)
# Convert the point geometry to a dataframe of longitude and latitude
subset_plant <- as.data.frame(st_coordinates(intersections_plant_TA))
subset_frugivore <- as.data.frame(st_coordinates(intersections_frugivore_TA))

# Match the coordinates to the original dataframe
#merge the subset and full dataframe together to get final TA dataset
plant_PAM_filter <- merge(plants_PAM_matrix, subset_plant, by.x = c("Longitude(x)", "Latitude(y)"), by.y = c("X", "Y"))
frugivore_PAM_filter <- merge(frugivore_PAM_matrix, subset_frugivore, by.x = c("Longitude(x)", "Latitude(y)"), by.y = c("X", "Y"))



# Turn PAM into matrix
plant_PAM <- as.matrix(plant_PAM_filter)
frugivore_PAM <- as.matrix(frugivore_PAM_filter)

# Add rownames
# Get the number of rows in the matrix
num_rows_plant <- nrow(plant_PAM)
num_rows_frugivore <- nrow(frugivore_PAM)
# Generate unique row names
row_names_plant <- paste0("cell", 1:num_rows_plant)
row_names_frugivore <- paste0("cell", 1:num_rows_frugivore)
# Assign row names to the matrix
rownames(plant_PAM) <- row_names_plant
rownames(frugivore_PAM) <- row_names_frugivore
# Print the matrix with row names
print(plant_PAM)
column_names_plant <- colnames(plant_PAM)
clean_column_names_plant <- gsub("_", " ", column_names_plant)
print(frugivore_PAM)
column_names_frugivore <- colnames(frugivore_PAM)
clean_column_names_frugivore <- gsub("_", " ", column_names_frugivore)
#Insert clean column names
colnames(plant_PAM) <- clean_column_names_plant
colnames(frugivore_PAM) <- clean_column_names_frugivore

#turn data into correct data types for inputs into the trait categories dataframe
frugivore_traits$diet_cat <- as.factor(frugivore_traits$diet_cat)
frugivore_traits$body_mass_e <- as.numeric(frugivore_traits$body_mass_e)
frugivore_traits$body_size_mm <- as.numeric(frugivore_traits$body_size_mm)
frugivore_traits$generation_time <- as.numeric(frugivore_traits$generation_time)

plant_traits$plant_height <- as.numeric(plant_traits$plant_height)
plant_traits$plant_lifespan<- as.numeric(plant_traits$plant_lifespan)
plant_traits$dispersal_syndrome <- as.factor(plant_traits$dispersal_syndrome)

# Remove the species from PAM that have no occurrences anymore after subsetting to TA
# Remove columns with sum equal to zero
PAM_plant_site_final <- plant_PAM[, colSums(plant_PAM) != 0]
PAM_frugivore_site_final <- frugivore_PAM[, colSums(frugivore_PAM) != 0]
#Save coordinates for later
site_loc_key_plant <- PAM_plant_site_final[,1:2]
site_loc_key_frugivore <- PAM_frugivore_site_final[,1:2]


columns_to_remove <- c(1,2)
PAM_plant_site_final <- PAM_plant_site_final[,-columns_to_remove]
PAM_frugivore_site_final <- PAM_frugivore_site_final[,-columns_to_remove]
colnames_plant <- colnames(PAM_plant_site_final)
colnames_frugivore <- colnames(PAM_frugivore_site_final)



# Remove species names from trait matrix not in the PAM
plant_traits_df_subset <- plant_traits %>% filter(species %in% colnames_plant)
frugivore_traits_df_subset <- frugivore_traits %>% filter(IUCN_species_name %in% colnames_frugivore)

# Turn trait dataframe into a matrix
class(plant_traits)
class(frugivore_traits)
plant_traits_matrix <- as.matrix(plant_traits_df_subset)
frugivore_traits_matrix <- as.matrix(frugivore_traits_df_subset)

# Define row names as species names
row_names_plant <- plant_traits_matrix[,1]
row_names_frugivore <- frugivore_traits_matrix[,1]

# Assign row names to the matrix
rownames(plant_traits_matrix) <- row_names_plant
rownames(frugivore_traits_matrix) <- row_names_frugivore
# Turn back into dataframe
plant_traits_df_final <-as.data.frame(plant_traits_matrix)
frugivore_traits_df_final <-as.data.frame(frugivore_traits_matrix)
plant_traits_df_final$X <-NULL
frugivore_traits_df_final$X <-NULL



#fix types
frugivore_traits_df_final$diet_cat <- as.factor(frugivore_traits_df_final$diet_cat)
frugivore_traits_df_final$body_mass_e <- as.numeric(frugivore_traits_df_final$body_mass_e)
frugivore_traits_df_final$body_size_mm<- as.numeric(frugivore_traits_df_final$body_size_mm)
frugivore_traits_df_final$generation_time <- as.numeric(frugivore_traits_df_final$generation_time)

plant_traits_df_final$plant_height <- as.numeric(plant_traits_df_final$plant_height)
plant_traits_df_final$plant_lifespan<- as.numeric(plant_traits_df_final$plant_lifespan)
plant_traits_df_final$dispersal_syndrome <- as.factor(plant_traits_df_final$dispersal_syndrome)

# remove duplicate species name column
plant_traits_df_final <- plant_traits_df_final[, c("plant_height", "dispersal_syndrome", "plant_lifespan")]
frugivore_traits_df_final <- frugivore_traits_df_final[, c("diet_cat", "body_mass_e", "body_size_mm", "generation_time")]



# Create trait type table
trait_name <- c("diet_cat", "body_mass_e", "body_size_mm", "generation_time")
trait_type <- c("N", "Q", "Q", "Q")
frug_trait_cat <- as.data.frame(cbind(trait_name, trait_type))
trait_name <- c("dispersal_syndrome", "plant_height", "plant_lifespan")
trait_type <- c("N", "Q", "Q") 
plant_trait_cat <- as.data.frame(cbind(trait_name, trait_type))



# Summary of the assemblages * species dataframe:
asb_sp_plant_summ <- mFD::asb.sp.summary(asb_sp_w = PAM_plant_site_final)
asb_sp_frugivore_summ <- mFD::asb.sp.summary(asb_sp_w = PAM_frugivore_site_final)



# Species traits summary:
plant_traits_summ <- mFD::sp.tr.summary(
  tr_cat     = plant_trait_cat,   
  sp_tr      = plant_traits_df_final, 
  stop_if_NA = TRUE)
frugivore_traits_summ <- mFD::sp.tr.summary(
  tr_cat     = frug_trait_cat,   
  sp_tr      = frugivore_traits_df_final, 
  stop_if_NA = TRUE)



# Estimate functional trait-based distances between species


sp_dist_plants <- mFD::funct.dist(
  sp_tr         = plant_traits_df_final,
  tr_cat        = plant_trait_cat,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)


sp_dist_frugivore <- mFD::funct.dist(
  sp_tr         = frugivore_traits_df_final,
  tr_cat        = frug_trait_cat,
  metric        = "gower",
  scale_euclid  = "scale_center",
  ordinal_var   = "classic",
  weight_type   = "equal",
  stop_if_NA    = TRUE)


# Generate a multidimensional space

fspaces_quality_plants <- mFD::quality.fspaces(
  sp_dist             = sp_dist_plants,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")



fspaces_quality_frugivore <- mFD::quality.fspaces(
  sp_dist             = sp_dist_frugivore,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")




#testing correlation between functional axes and traits
sp_faxes_coord_plants <- fspaces_quality_plants$"details_fspaces"$"sp_pc_coord"
sp_faxes_coord_frugivore <- fspaces_quality_frugivore$"details_fspaces"$"sp_pc_coord"




#plotting functional space
sp_faxes_coord_plants <- fspaces_quality_plants$"details_fspaces"$"sp_pc_coord"
sp_faxes_coord_frugivore <- fspaces_quality_frugivore$"details_fspaces"$"sp_pc_coord"

#Need to remove parts of the PAM that have values less than or equal to the number of dimensions (4)
# Calculate row sums
row_sums_plant <- rowSums(PAM_plant_site_final)
subset_matrix_plant <- PAM_plant_site_final[row_sums_plant >= 4, ]
row_sums_frugivore <- rowSums(PAM_frugivore_site_final)
subset_matrix_frugivore <- PAM_frugivore_site_final[row_sums_frugivore >= 4, ]




# getting sp_faxes_coord_plants and subset_matrix_plants names to match
sp_faxes_coord_plants_sub <- sp_faxes_coord_plants[ , c("PC1", "PC2", "PC3", "PC4")]
## check number of species names
nrow(sp_faxes_coord_plants_sub) # 54
ncol(subset_matrix_plant) #82

sp_faxes_coord_plants_sub_names <- row.names(sp_faxes_coord_plants_sub)
subset_matrix_plant_names <- colnames(subset_matrix_plant)

sp_faxes_coord_plants_sub_df <- as.data.frame(sp_faxes_coord_plants_sub)
subset_matrix_plant_df <- as.data.frame(subset_matrix_plant)

subset_matrix_plant <- subset_matrix_plant_df[, which((names(subset_matrix_plant_df) %in% sp_faxes_coord_plants_sub_names)==TRUE)]

ncol(subset_matrix_plant)
subset_matrix_plant <- as.matrix(subset_matrix_plant)



# match frugivore names
sp_faxes_coord_frugivore_sub <- sp_faxes_coord_frugivore[ , c("PC1", "PC2", "PC3", "PC4")]

## check number of species names
nrow(sp_faxes_coord_frugivore_sub) #718
ncol(subset_matrix_frugivore) #712

sp_faxes_coord_frugivore_sub_names <- row.names(sp_faxes_coord_frugivore_sub)
subset_matrix_frugivore_names <- colnames(subset_matrix_frugivore)

sp_faxes_coord_frugivore_sub_names <- chartr(".", " ", sp_faxes_coord_frugivore_sub_names)
sp_faxes_coord_frugivore_sub <- as.data.frame(sp_faxes_coord_frugivore_sub)
row.names(sp_faxes_coord_frugivore_sub) <- sp_faxes_coord_frugivore_sub_names

sp_faxes_coord_frugivore_sub <- sp_faxes_coord_frugivore_sub[ which((row.names(sp_faxes_coord_frugivore_sub) %in% subset_matrix_frugivore_names)==TRUE), ]

nrow(sp_faxes_coord_frugivore_sub)
sp_faxes_coord_frugivore_sub <- as.matrix(sp_faxes_coord_frugivore_sub)


#computing FD
# The number of species per assemblage has to be higher or equal to the number of traits
alpha_fd_indices_plant <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_plants,
  asb_sp_w         = PAM_plant_site_final,
  ind_vect         = c("fide", "fdis", "fmpd", "fnnd", "feve", "fori", "fspe"), #did not run fdiv or fric (slows it down)
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)

alpha_fd_indices_frugivore <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_frugivore,
  asb_sp_w         = PAM_plant_site_final,
  ind_vect         = c("fide", "fdis", "fmpd", "fnnd", "feve", "fori", "fspe"), #did not run fdiv or fric (slows it down)
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)
