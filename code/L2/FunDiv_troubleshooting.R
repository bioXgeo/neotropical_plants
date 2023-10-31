#Func Div troubleshooting
## Focus on plants

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

TropicalAndes_plant_occ_forest <- read.csv(file.path(data_path_L1,"TropicalAndes_GBIF_plant_occ_cleaned.csv"))
TropicalAndes_IUCNHabitat_Forest <- read_sf(file.path(data_path_L0, "Forest_sf.shp"), layer = "Forest_sf")
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


# Prepping data for functional diversity calculations


plant_traits <- na.omit(plant_traits)
plant_traits <- plant_traits[, c("species", "plant_height", "dispersal_syndrome", "plant_lifespan")]

coordinates_sf_plant <- st_as_sf(TropicalAndes_plant_occ_forest, coords = c("decimalLongitude", "decimalLatitude"), crs = st_crs(TropicalAndes_IUCNHabitat_Forest))


# Perform the spatial intersection
intersections_plant_TA <- st_intersection(coordinates_sf_plant, TropicalAndes_IUCNHabitat_Forest)
# Convert the intersected sf object back to a dataframe
coordinates_df_subset_plant <- as.data.frame(intersections_plant_TA)


# Print the resulting subset of coordinates
print(coordinates_df_subset_plant)
# Convert the point geometry to a dataframe of longitude and latitude
subset_plant <- as.data.frame(st_coordinates(intersections_plant_TA))

# Match the coordinates to the original dataframe
#merge the subset and full dataframe together to get final TA dataset
plant_PAM_filter <- merge(plants_PAM_matrix, subset_plant, by.x = c("Longitude(x)", "Latitude(y)"), by.y = c("X", "Y"))



# Turn PAM into matrix
plant_PAM <- as.matrix(plant_PAM_filter)

# Add rownames
# Get the number of rows in the matrix
num_rows_plant <- nrow(plant_PAM)
# Generate unique row names
row_names_plant <- paste0("cell", 1:num_rows_plant)
# Assign row names to the matrix
rownames(plant_PAM) <- row_names_plant
# Print the matrix with row names
print(plant_PAM)
column_names_plant <- colnames(plant_PAM)
clean_column_names_plant <- gsub("_", " ", column_names_plant)
#Insert clean column names
colnames(plant_PAM) <- clean_column_names_plant

#turn data into correct data types for inputs into the trait categories dataframe
plant_traits$plant_height <- as.numeric(plant_traits$plant_height)
plant_traits$plant_lifespan<- as.numeric(plant_traits$plant_lifespan)
plant_traits$dispersal_syndrome <- as.factor(plant_traits$dispersal_syndrome)

# Remove the species from PAM that have no occurrences anymore after subsetting to TA
# Remove columns with sum equal to zero
PAM_plant_site_final <- plant_PAM[, colSums(plant_PAM) != 0]
#Save coordinates for later
site_loc_key_plant <- PAM_plant_site_final[,1:2]


columns_to_remove <- c(1,2)
PAM_plant_site_final <- PAM_plant_site_final[,-columns_to_remove]
colnames_plant <- colnames(PAM_plant_site_final)



# Remove species names from trait matrix not in the PAM
plant_traits_df_subset <- plant_traits %>% filter(species %in% colnames_plant)

# Turn trait dataframe into a matrix
class(plant_traits)

plant_traits_matrix <- as.matrix(plant_traits_df_subset)

# Define row names as species names
row_names_plant <- plant_traits_matrix[,1]

# Assign row names to the matrix
rownames(plant_traits_matrix) <- row_names_plant
# Turn back into dataframe
plant_traits_df_final <-as.data.frame(plant_traits_matrix)
plant_traits_df_final$X <-NULL



#fix types

plant_traits_df_final$plant_height <- as.numeric(plant_traits_df_final$plant_height)
plant_traits_df_final$plant_lifespan<- as.numeric(plant_traits_df_final$plant_lifespan)
plant_traits_df_final$dispersal_syndrome <- as.factor(plant_traits_df_final$dispersal_syndrome)

# remove duplicate species name column
plant_traits_df_final <- plant_traits_df_final[, c("plant_height", "dispersal_syndrome", "plant_lifespan")]

# Create trait type table
trait_name <- c("dispersal_syndrome", "plant_height", "plant_lifespan")
trait_type <- c("N", "Q", "Q") 
plant_trait_cat <- as.data.frame(cbind(trait_name, trait_type))

# Summary of the assemblages * species dataframe:
asb_sp_plant_summ <- mFD::asb.sp.summary(asb_sp_w = PAM_plant_site_final)

# Species traits summary:
plant_traits_summ <- mFD::sp.tr.summary(
  tr_cat     = plant_trait_cat,   
  sp_tr      = plant_traits_df_final, 
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


# Generate a multidimensional space
fspaces_quality_plants <- mFD::quality.fspaces(
  sp_dist             = sp_dist_plants,
  maxdim_pcoa         = 10,
  deviation_weighting = "absolute",
  fdist_scaling       = FALSE,
  fdendro             = "average")


#testing correlation between functional axes and traits
sp_faxes_coord_plants <- fspaces_quality_plants$"details_fspaces"$"sp_pc_coord"

#plotting functional space
sp_faxes_coord_plants <- fspaces_quality_plants$"details_fspaces"$"sp_pc_coord"

#Need to remove parts of the PAM that have values less than or equal to the number of dimensions (4)
# Calculate row sums
row_sums_plant <- rowSums(PAM_plant_site_final)
subset_matrix_plant <- PAM_plant_site_final[row_sums_plant >= 4, ]


# getting sp_faxes_coord_plants and subset_matrix_plants names to match
sp_faxes_coord_plants_sub <- sp_faxes_coord_plants[ , c("PC1", "PC2", "PC3", "PC4")]
## check number of species names
nrow(sp_faxes_coord_plants_sub) # 250
ncol(subset_matrix_plant) #39272

sp_faxes_coord_plants_sub_names <- row.names(sp_faxes_coord_plants_sub)
subset_matrix_plant_names <- colnames(subset_matrix_plant)

sp_faxes_coord_plants_sub_df <- as.data.frame(sp_faxes_coord_plants_sub)
subset_matrix_plant_df <- as.data.frame(subset_matrix_plant)

subset_matrix_plant <- subset_matrix_plant_df[, which((names(subset_matrix_plant_df) %in% sp_faxes_coord_plants_sub_names)==TRUE)]

ncol(subset_matrix_plant) #250
subset_matrix_plant <- as.matrix(subset_matrix_plant)





#computing FD
# The number of species per assemblage has to be higher or equal to the number of traits
alpha_fd_indices_plant <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = sp_faxes_coord_plants,
  asb_sp_w         = subset_matrix_plant,
  ind_vect         = c("fide", "fdis", "fmpd", "fnnd", "fori", "fspe"), #did not run fdiv or fric (slows it down); need more species for feve
  scaling          = TRUE,
  check_input      = TRUE,
  details_returned = TRUE)
