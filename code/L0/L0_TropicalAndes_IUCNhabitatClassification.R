# title: Tropical Andes IUCN Montane and Lowland Forest
# author: Hazel J. Anderson
# project: Plant-Frugivore Diversity
# collaborators: Beth E. Gerstner, Phoebe L. Zarnetske, Jenna B. Baljunas, Kelly Kaspar
# overview: Create raster of IUCN habitat classification Forest-Subtropical moist montane and Forest-Subtropical moist lowland for Tropical Andes
# data input: "iucn_habitatclassification_composite_lvl2_ver004/lvl2_frac_1km_ver004/iucn_habitatclassification_fraction_lvl2__109_Forest – Subtropical-tropical moist montane__ver004.tif", "iucn_habitatclassification_composite_lvl2_ver004/lvl2_frac_1km_ver004/iucn_habitatclassification_fraction_lvl2__106_Forest – Subtropical-tropical moist lowland__ver004.tif", TA_refined.shp"
# data output: "TropicalAndes_IUCNHabitat_Forest.GTiff", "TropicalAndes_IUCNHabitat_Forest_Montane.GTiff", "TropicalAndes_IUCNHabitat_Forest_Lowland.GTiff", TropicalAndes_IUCNHabitat_Forest0.1.tif", Forest_sf.shp", ""Tropical Andes Forest Map.png"
# date: "2023-07-18; 2025-09-22"


# Load required packages
library(raster);library(sf);library(rnaturalearth);library(dplyr);library(terra);library(ggplot2);library(ggspatial)


# Set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
output_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')


# Load in Ecosystem Functional Groups

# Data downloaded from https://zenodo.org/record/4058819
# Citation: Martin Jung, Prabhat Raj Dahal, Stuart H. M. Butchart, Paul F. Donald, Xavier De Lamo, Myroslava Lesiv, … Piero Visconti. (2020). A global map of terrestrial habitat types (Version 004) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.4058819

iucn_moist_montane_forest <- raster(file.path(data_path_L0, "iucn_habitatclassification_composite_lvl2_ver004/lvl2_frac_1km_ver004/iucn_habitatclassification_fraction_lvl2__109_Forest – Subtropical-tropical moist montane__ver004.tif"))
plot(iucn_moist_montane_forest)

iucn_moist_lowland_forest <- raster(file.path(data_path_L0, "iucn_habitatclassification_composite_lvl2_ver004/lvl2_frac_1km_ver004/iucn_habitatclassification_fraction_lvl2__106_Forest – Subtropical-tropical moist lowland__ver004.tif"))
plot(iucn_moist_lowland_forest)


# Crop to Tropical Andes hotspot

# Load in shape
TropicalAndes <- read_sf(file.path(data_path_L0, "TA_refined.shp"), layer = "TA_refined")
plot(TropicalAndes$geometry)


# Crop moist montane
iucn_moist_montane_forest_crop <- crop(iucn_moist_montane_forest, TropicalAndes)
TA_iucn_moist_montane_forest <- mask(iucn_moist_montane_forest_crop, TropicalAndes)
plot(TA_iucn_moist_montane_forest)


# Crop moist lowland
iucn_moist_lowland_forest_crop <- crop(iucn_moist_lowland_forest, TropicalAndes)
TA_iucn_moist_lowland_forest <- mask(iucn_moist_lowland_forest_crop, TropicalAndes)
plot(TA_iucn_moist_lowland_forest)


# Combine TA_iucn_moist_lowland_forest & TA_iucn_moist_montane_forest into one raster
TA_iucn_habitat_forest <- merge(TA_iucn_moist_lowland_forest, TA_iucn_moist_montane_forest)
plot(TA_iucn_habitat_forest)


# Reclassify forest raster with values above 500 to one value and less than 500 to NA
mat <- c(0, 500, NA, 
         500, 1000, 1)
rclmat <- matrix(mat, ncol = 3, byrow = TRUE)
TA_iucn_habitat_forest <- reclassify(TA_iucn_habitat_forest, rclmat)
plot(TA_iucn_habitat_forest)


# Save raster
writeRaster(TA_iucn_habitat_forest, filename = file.path(output_path_L0, "TropicalAndes_IUCNHabitat_Forest.GTiff"), format = "GTiff", overwrite=TRUE)

writeRaster(TA_iucn_moist_montane_forest, filename = file.path(output_path_L0, "TropicalAndes_IUCNHabitat_Forest_Montane.GTiff"), format = "GTiff", overwrite=TRUE)

writeRaster(TA_iucn_moist_lowland_forest, filename = file.path(output_path_L0, "TropicalAndes_IUCNHabitat_Forest_Lowland.GTiff"), format = "GTiff", overwrite=TRUE)


# Aggregate to 0.1
TropicalAndes_IUCNHabitat_Forest0.1 <- aggregate(TA_iucn_habitat_forest, 10, fun=mean, na.rm=TRUE)

writeRaster(TropicalAndes_IUCNHabitat_Forest0.1,file.path(output_path_L0, "TropicalAndes_IUCNHabitat_Forest0.1.tif"), overwrite = TRUE)


# Forest shape conversion
Forest_poly <- rasterToPolygons(TropicalAndes_IUCNHabitat_Forest0.1, na.rm = TRUE, dissolve = TRUE)
plot(Forest_poly)

Forest_sf <- st_as_sf(Forest_poly)
plot(Forest_sf)

st_write(Forest_sf, file.path(data_path_L0, "Forest_sf.shp"), append = TRUE)


# Map of Tropical Andes Forested region

worldMap <- ne_countries(scale = "medium", type = "countries", returnclass = "sf")

# country subset
ECpoly <- worldMap %>% filter(sovereignt == "Ecuador")
VEpoly <- worldMap %>% filter(sovereignt == "Venezuela")
COpoly <- worldMap %>% filter(sovereignt == "Colombia")
PEpoly <- worldMap %>% filter(sovereignt == "Peru")
BOpoly <- worldMap %>% filter(sovereignt == "Bolivia")

# polygon of Tropical Andes
TApoly <- worldMap %>% filter(sovereignt == "Bolivia" |sovereignt == "Ecuador" | sovereignt == "Venezuela" | sovereignt == "Colombia" | sovereignt == "Peru")

# set colors
Andean_states <- "grey90"
Tropical_Andes <- "peachpuff3"
Forest <- "forestgreen"

# plot
ggplot() + 
  geom_sf(data = worldMap, fill = "white") +
  geom_sf(data = ECpoly, aes(fill = "Andean states")) +
  geom_sf(data = VEpoly, aes(fill = "Andean states")) +
  geom_sf(data = COpoly, aes(fill = "Andean states")) +
  geom_sf(data = PEpoly, aes(fill = "Andean states")) +
  geom_sf(data = BOpoly, aes(fill = "Andean states")) +
  geom_sf(data = TropicalAndes, aes(fill = "Tropical Andes")) +
  geom_sf(data = Forest_sf, aes(fill = "Tropical Andes Forest")) +
  scale_fill_manual(
    name = "Region",
    values = c("Andean states" = Andean_states, "Tropical Andes" = Tropical_Andes, "Tropical Andes Forest" = Forest)
  ) +
  labs(x = "Latitude", y = "Longitude") +
  guides(fill = guide_legend(reverse = TRUE))+
  coord_sf(xlim = c(-85, -60), ylim = c(-24, 14), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5, height = unit(0.08, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.1, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
  theme(panel.background = element_rect(fill = "lightblue"), panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), legend.text = element_text(size = 14), legend.title = element_text(size = 16), axis.title = element_text(size = 16))

ggsave(filename = "Tropical Andes Forest Map.png", dpi = 300, width = 8, height = 6, device="png", path = output_path_L0)

# cite packages and print session info
library(report)
cite_packages()

devtools::session_info()
