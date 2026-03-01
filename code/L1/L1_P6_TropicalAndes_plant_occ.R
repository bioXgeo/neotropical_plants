# title: "Plant occurrence subset by species with complete trait records"
# author: "Hazel J. Anderson"
# project: "Plant-Frugivore Diversity"
# collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske, Jenna B. Baljunas"
# overview: "This script subsets plant occurrence data to species with complete trait coverage and summarizes the taxonomic coverage."
# data input: "TropicalAndes_imputed_plant_traits.csv", "TropicalAndes_GBIF_plant_occ_harmonized_subset.csv", "Forest_sf.shp"
# data output: "TropicalAndes_GBIF_plant_occ_harmonized_subset_final.csv"
# date: "2023-10-04; 2025-10-17"


# load required packages
library(GIFT); library(dplyr); library(sf); library(tidyr); library(ggplot2)

  
# set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')


# load functions
source("C:/GitHub_projects/plant-frugivore diversity/neotropical_plants/code/Functions.R")

## HPCC
# source("/mnt/ffs24/home/baljunas/Documents/neotropical_plants/code/Functions.R")


# read in data
plant_traits <- read.csv(file.path(data_path_L1, file = "TropicalAndes_imputed_plant_traits.csv"))
plant_occ <- read.csv(file.path(data_path_L1, file = "TropicalAndes_GBIF_plant_occ_harmonized_subset.csv"))
TropicalAndes_IUCNHabitat_Forest <- read_sf(file.path(data_path_L0, "Forest_sf.shp"), layer = "Forest_sf")


# create list of species with complete trait records
plant_species <- unique(plant_traits$species)
length(plant_species)


# subset plant occurrence data by species list
plant_occ_subset <- plant_occ %>%
  filter(species %in% plant_species)
nrow(plant_occ_subset)


# get a checklist of Angiosperms (flowering plants) in the Tropical Andes Forest shapefile
checklist <- GIFT_checklists(taxon_name = "Angiospermae", taxonomic_group = TRUE, floristic_group = "all", shp = TropicalAndes_IUCNHabitat_Forest, overlap = "extent_intersect")


# add genus based on work_ID
full <- GIFT_taxonomy()

genus_ID <- checklist$checklists$genus_ID
genus_list <- full %>% filter(taxon_ID %in% genus_ID) %>% select(taxon_ID, taxon_name)
names(genus_list) <- c("genus_ID", "genus")

checklist_tax <- dplyr::inner_join(checklist$checklists, genus_list)

reference_ID_list <- unique(checklist_tax$ref_ID)
ref <- GIFT_references()
checklist_ref <- ref[which(ref$ref_ID %in% reference_ID_list),
                     c("ref_ID", "ref_long", "geo_entity_ref")]
checklist_ref %>%
  mutate(ref_long = iconv(ref_long, from = "UTF-8", to = "ASCII//TRANSLIT"))


# summary
glimpse(plant_occ_subset)

data_summary(plant_occ_subset, plant_occ_subset$Accepted_species, plant_occ_subset$genus, plant_occ_subset$family)

data_summary(checklist_tax, checklist_tax$work_species, checklist_tax$genus, checklist_tax$family)


# visualize taxonomic coverage
GBIF_occ_summary <- plant_occ_subset %>%
  summarise(
    num_species = n_distinct(Accepted_species),
    num_families = n_distinct(family),
    num_genera = n_distinct(genus)
  )

GIFT_checklist_summary <- checklist_tax %>%
  summarise(
    num_species = n_distinct(work_species),
    num_families = n_distinct(family),
    num_genera = n_distinct(genus)
  )

# combine the summaries into one dataframe
combined_summary <- bind_rows(
  GBIF_occ_summary %>% mutate(source = "Data"),
  GIFT_checklist_summary %>% mutate(source = "Checklist")
)

# reshape to long format for easier plotting
combined_summary_long <- combined_summary %>%
  pivot_longer(cols = c(num_species, num_families, num_genera), 
               names_to = "category", 
               values_to = "count")

# reorder and rename the categories
combined_summary_long <- combined_summary_long %>%
  mutate(
    source = factor(source, levels = c("Data", "Checklist")),
    category = fct_recode(category,
                          "Species" = "num_species",
                          "Genera" = "num_genera",
                          "Families" = "num_families")
  )

# reorder the category variable
combined_summary_long <- combined_summary_long %>%
  mutate(
    category = factor(category, levels = c("Species", "Genera", "Families"))
  )

# custom colors for source
custom_colors <- c("Data" = "seagreen", "Checklist" = "darkgrey")

ggplot(combined_summary_long, aes(x = source, y = count, fill = source)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Source",
       y = "Count",
       fill = "Source") +
  scale_fill_manual(values = custom_colors) + 
  theme_minimal() +
  theme(
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
    legend.position = "none"
  ) +
  facet_wrap(~ category, scales = "free_y") +
  theme(strip.text.x = element_text(size = 12, face = "bold"))
ggsave("plant_taxonomic_coverage.png", plot = last_plot(), path = figure_path)


# write data to csv
write.csv(plant_occ_subset, file.path(output_path_L1,"TropicalAndes_GBIF_plant_occ_harmonized_subset_final.csv"))
