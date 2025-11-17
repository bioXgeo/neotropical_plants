#title: "Plant Trait Imputation"
#author: "Hazel J. Anderson, Jenna B. Baljunas"
#project: "Plant-Frugivore Diversity"
#collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
#overview: "This script fill plant trait gaps with imputation."
#data input: "TropicalAndes_all_plant_traits_standardized.csv"
#data output: "TropicalAndes_imputed_plant_traits.csv"
#date: "2025-10-15"
#output: html_document

  
# set file paths
data_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')


# load required packages
library(tidyr); library(BIEN); library(GIFT); library(purrr); library(mice); library(dplyr); library(funbiogeo); library(visdat); library(ggplot2); library(forcats); library(viridis)


# read in data
plant_traits <- read.csv(file.path(data_path_L1,"TropicalAndes_all_plant_traits_standardized.csv"))

# use BIEN and GIFT to fill in gaps by genus or family.
# 1. need a list of families and genus for each trait that needs requesting
# 2. add genus and family information to dataframe
# 3. list by trait of species and genus

# create a dataframe of species and TraitNames with TraitValue = NA

# remove X column
plant_traits <- plant_traits[, !colnames(plant_traits) %in% "X", drop = FALSE]

# convert the wide dataframe to long format
long_plant_traits <- plant_traits %>%
  gather(TraitName, TraitValue, -species)

# filter rows with NA values
na_traits <- long_plant_traits %>%
  filter(is.na(TraitValue))

dim(na_traits)

# get a list of species
na_species_list <- unique(plant_traits$species)
length(na_species_list)

# add genus and family information to dataframe
taxonomic_info <- BIEN_taxonomy_species(na_species_list)

# keep only columns with family, genus, species
subset_df <- taxonomic_info[, c("scrubbed_family", "scrubbed_genus", "scrubbed_species_binomial")]

# remove duplicates
na_species_taxonomy <- distinct(subset_df)

# rename column to family, genus, species
na_species_taxonomy <- na_species_taxonomy %>%
  rename(family = scrubbed_family,
         genus = scrubbed_genus,
         species = scrubbed_species_binomial)

# add to species dataframe
na_species_df <- as.data.frame(na_species_list)

names(na_species_df) <- "species"

na_species_df <- merge(na_species_df, na_species_taxonomy, by = "species", all.x = TRUE)

# retrieve list of species without family & genus information
species_no_family <- na_species_df %>%
  filter(is.na(family) | family == "") %>%
  select(species) %>%
  pull()

species_no_genus <- na_species_df %>%
  filter(is.na(genus) | genus == "") %>%
  select(species) %>%
  pull()


# check if all the species in no_family & no_genus lists are the same
identical(species_no_family, species_no_genus)

length(species_no_family)

#### use taxize for species with no family and genus info ####
library(taxize)

# function to retrieve taxonomic information for a chunk of species names
# use .Reviron to add NCBI ENTREZ API info
get_taxonomic_info_chunk <- function(chunk_species_names) {
  # Initialize an empty list to store taxonomic information for each chunk
  chunk_taxonomic_info <- list()
  
  # Loop through each species name in the chunk and retrieve taxonomic information
  for (species_name in chunk_species_names) {
    tryCatch({
      # Add a delay between consecutive API requests
      Sys.sleep(1)
      
      # Make API request to retrieve taxonomic information
      taxon_info <- tax_name(species_name, get = c("genus", "family"), db = "ncbi")
      
      # Store taxonomic information for the species in the list
      chunk_taxonomic_info[[species_name]] <- data.frame(Species = species_name, 
                                                         Genus = taxon_info$genus, 
                                                         Family = taxon_info$family)
    }, error = function(e) {
      # Print error message
      cat("Error retrieving taxonomic information for", species_name, ":", conditionMessage(e), "\n")
    })
  }
  
  # Return the list of taxonomic information for the chunk
  return(chunk_taxonomic_info)
}

# define the chunk size
chunk_size <- 300 # Adjust the chunk size as needed

# split the dataframe into chunks based on the chunk size
chunks <- split(species_no_family, ceiling(seq_along(species_no_family) / chunk_size))

# initialize an empty list to store taxonomic information for all chunks
all_taxonomic_info <- list()

# iterate over each chunk and retrieve taxonomic information
for (chunk_index in seq_along(chunks)) {
  # Get the species names in the current chunk
  chunk_species_names <- chunks[[chunk_index]]
  
  # Retrieve taxonomic information for the chunk
  chunk_taxonomic_info <- get_taxonomic_info_chunk(chunk_species_names)
  
  # Append taxonomic information for the chunk to the list
  all_taxonomic_info <- c(all_taxonomic_info, list(chunk_taxonomic_info))
  
  # Add a delay 
  Sys.sleep(5)
}

# combine taxonomic information from all chunks into a single list
all_taxonomic_info <- do.call(c, all_taxonomic_info)

# combine the list of dataframes into a single dataframe
taxonomic_df <- do.call(rbind, all_taxonomic_info)

# remove duplicates
taxonomic_df <- distinct(taxonomic_df)

# rename column to family, genus, species
taxonomic_df <- taxonomic_df %>%
  rename(family = Family,
         genus = Genus,
         species = Species)

# add to species dataframe
na_species_df$family <- ifelse(is.na(na_species_df$family), taxonomic_df$family[match(na_species_df$species, taxonomic_df$species)], na_species_df$family)

na_species_df$genus <- ifelse(is.na(na_species_df$genus), taxonomic_df$genus[match(na_species_df$species, taxonomic_df$species)], na_species_df$genus)


# retrieve list of species without family & genus information
species_no_family_2 <- na_species_df %>%
  filter(is.na(family) | family == "") %>%
  select(species) %>%
  pull()

length(species_no_family_2)

# get powo id
# when prompted to choose one out of a list, selected the one that matched the species exactly and has TRUE under accepted
species_no_family_2_powo <- lapply(species_no_family_2, function(sp) {
  Sys.sleep(1)  # wait 1 second between requests
  tryCatch(get_pow(sp, messages = TRUE), error = function(e) NULL)
})

# function to retrieve taxonomic information for a chunk of taxon IDs
get_taxonomic_info_chunk <- function(chunk_taxon_id) {
  # Initialize an empty list to store taxonomic information for each chunk
  chunk_taxonomic_info <- list()
  
  # Loop through each taxon ID in the chunk and retrieve taxonomic information
  for (taxon_id in chunk_taxon_id) {
    tryCatch({
      # Print taxon ID for debugging
      print(paste("Processing taxon ID:", taxon_id))
      
      # Add a delay between consecutive API requests
      Sys.sleep(1)
      
      # Query POWO database for taxonomic information
      taxon_info <- pow_lookup(id = taxon_id)
      
      # Check if taxon_info is NULL
      if (is.null(taxon_info)) {
        # If taxon_info is NULL, return NA values for species, family, and genus
        chunk_taxonomic_info[[taxon_id]] <- data.frame(taxon_id = taxon_id, species = NA, family = NA, genus = NA)
      } else {
        # Store taxonomic information for the taxon ID in the list
        chunk_taxonomic_info[[taxon_id]] <- data.frame(taxon_id = taxon_id,
                                                       species = taxon_info$meta$name,
                                                       genus = taxon_info$meta$genus, 
                                                       family = taxon_info$meta$family)
      }
    }, error = function(e) {
      # Print error message for debugging
      print(paste("Error processing taxon ID:", taxon_id))
      print(e)
    })
  }
  
  # Return the list of taxonomic information for the chunk
  return(chunk_taxonomic_info)
}

# Define the chunk size
chunk_size <- 300 # Adjust the chunk size as needed

# Split the vector of taxon IDs into chunks based on the chunk size
chunks <- split(species_no_family_2_powo, ceiling(seq_along(species_no_family_2_powo) / chunk_size))

# Initialize an empty list to store taxonomic information for all chunks
all_taxonomic_info <- list()

# Iterate over each chunk and retrieve taxonomic information
for (chunk_index in seq_along(chunks)) {
  # Get the taxon IDs in the current chunk
  chunk_taxon_id <- chunks[[chunk_index]]
  
  # Retrieve taxonomic information for the chunk
  chunk_taxonomic_info <- get_taxonomic_info_chunk(chunk_taxon_id)
  
  # Append taxonomic information for the chunk to the list
  all_taxonomic_info <- c(all_taxonomic_info, list(chunk_taxonomic_info))
  
  # Add a delay 
  Sys.sleep(5)
}


# combine taxonomic information from all chunks into a single list
all_taxonomic_info <- do.call(c, all_taxonomic_info)

# combine the list of dataframes into a single dataframe
taxonomic_df <- do.call(rbind, all_taxonomic_info)

# add to species dataframe
na_species_df$family <- ifelse(is.na(na_species_df$family), taxonomic_df$family[match(na_species_df$species, taxonomic_df$species)], na_species_df$family)

na_species_df$genus <- ifelse(is.na(na_species_df$genus), taxonomic_df$genus[match(na_species_df$species, taxonomic_df$species)], na_species_df$genus)


# retrieve list of species without family & genus information
species_no_family_3 <- na_species_df %>%
  filter(is.na(family) | family == "") %>%
  select(species) %>%
  pull()
length(species_no_family_3)


# add genus and family info to dataframe with species and traits
na_traits_family_genus <- merge(na_traits, na_species_df, by = "species", all.x = TRUE)

# get a list for each trait of species, genus, and family to fill in gaps

# get unique traits
traits <- unique(na_traits_family_genus$TraitName)

# create an empty list to store dataframes for each trait
trait_dfs <- list()

# split the dataframe by traitName and create a dataframe for each trait
for (trait in traits) {
  # Subset the dataframe for the current trait
  trait_df <- subset(na_traits_family_genus, TraitName == trait)
  
  # Remove the traitName column from the subsetted dataframe
  trait_df <- trait_df[, !(names(trait_df) %in% "traitName")]
  
  # Add the dataframe to the list
  trait_dfs[[trait]] <- trait_df
}

# separate dataframes from list
FruitDryness <- trait_dfs$FruitDryness
DispersalSyndrome <- trait_dfs$DispersalSyndrome
PlantLifespan_years <- trait_dfs$PlantLifespan_years
FruitConspicuousness <- trait_dfs$FruitConspicuousness
FruitType <- trait_dfs$FruitType
FruitColor <- trait_dfs$FruitColor
SeedLength_mm <- trait_dfs$SeedLength_mm
SeedWidth_mm <- trait_dfs$SeedWidth_mm
FruitMass_mg <- trait_dfs$FruitMass_mg
PlantHeight_m <- trait_dfs$PlantHeight_m
GrowthForm <- trait_dfs$GrowthForm
FruitLength_mm <- trait_dfs$FruitLength_mm
SeedMass_g <- trait_dfs$SeedMass_g


#### retrieve trait records for species using BIEN & GIFT ####

# for GIFT data: make sure Lvl3 IDs are correct
traits_meta <- GIFT_traits_meta()

GIFT_trait <- function(trait){
  subset <- traits_meta[traits_meta$Trait1==trait,]
  paste(subset$Lvl3)
}

# see all BIEN traits
all_bien_traits <- BIEN_trait_list()

# TRY only data: conpsicuousness and fruit mass


#### fruit dryness ####

# create a list of unique genus and families for trait
# FruitDryness_families <- unique(FruitDryness$family)
# FruitDryness_genus <- unique(FruitDryness$genus)
# length(FruitDryness_families)
# length(FruitDryness_genus)

# GIFT only
#FruitDryness_families_GIFT <- GIFT_traits_tax(trait_IDs = GIFT_trait('Fruit dryness'))
# no records


#### dispersal syndrome ####

# create a list of unique genus and families for trait
DispersalSyndrome_families <- unique(DispersalSyndrome$family)
DispersalSyndrome_genus <- unique(DispersalSyndrome$genus)
length(DispersalSyndrome_families)
length(DispersalSyndrome_genus)

# both BIEN and GIFT
# BIEN
DispersalSyndrome_families_BIEN <- BIEN_trait_traitbyfamily(family = DispersalSyndrome_families, trait = "whole plant dispersal syndrome")

DispersalSyndrome_genus_BIEN <- BIEN_trait_traitbygenus(genus = DispersalSyndrome_genus, trait = "whole plant dispersal syndrome")

nrow(DispersalSyndrome_families_BIEN)
length(unique(DispersalSyndrome_families_BIEN$scrubbed_family))
unique(DispersalSyndrome_families_BIEN$trait_value)

nrow(DispersalSyndrome_genus_BIEN)
length(unique(DispersalSyndrome_genus_BIEN$scrubbed_genus))
unique(DispersalSyndrome_genus_BIEN$trait_value)

# GIFT
DispersalSyndrome_families_GIFT <- GIFT_traits_tax(trait_IDs = GIFT_trait('Dispersal syndrome'))

DispersalSyndrome_families_GIFT <- DispersalSyndrome_families_GIFT[DispersalSyndrome_families_GIFT$taxon_name %in% DispersalSyndrome_families, ]

# harmonize trait values
DispersalSyndrome_families_GIFT$trait_value <- ifelse(is.na(DispersalSyndrome_families_GIFT$`3.3.2`), DispersalSyndrome_families_GIFT$`3.3.1`, DispersalSyndrome_families_GIFT$`3.3.1`)

DispersalSyndrome_families_GIFT$trait_value <- ifelse(is.na(DispersalSyndrome_families_GIFT$trait_value), DispersalSyndrome_families_GIFT$`3.3.2`, DispersalSyndrome_families_GIFT$trait_value)

unique(DispersalSyndrome_families_GIFT$trait_value)

DispersalSyndrome_families_GIFT <- DispersalSyndrome_families_GIFT %>%
  filter() %>%
  mutate(trait_value= case_when(
    grepl('endozoochorous|epizoochorous|zoochorous', trait_value, ignore.case = TRUE) ~ "zoochorous",
    grepl('hydrochorous', trait_value, ignore.case = TRUE) ~ "hydrochorous",
    grepl('anemochorous', trait_value, ignore.case = TRUE) ~ "anemochorous"))%>%
  filter(!is.na(trait_value))

unique(DispersalSyndrome_families_GIFT$trait_value)

nrow(DispersalSyndrome_families_GIFT)
length(unique(DispersalSyndrome_families_GIFT$taxon_name))
unique(DispersalSyndrome_families_GIFT$taxon_name)


#### plant lifespan ####

# create a list of unique genus and families for trait
PlantLifespan_years_families <- unique(PlantLifespan_years$family)
PlantLifespan_years_genus <- unique(PlantLifespan_years$genus)
length(PlantLifespan_years_families)
length(PlantLifespan_years_genus)

# both BIEN and GIFT
# BIEN
PlantLifespan_years_families_BIEN <- BIEN_trait_traitbyfamily(family = PlantLifespan_years_families, trait = c("maximum whole plant longevity","longest whole plant longevity"))

PlantLifespan_years_genus_BIEN <- BIEN_trait_traitbygenus(genus = PlantLifespan_years_genus, trait = c("maximum whole plant longevity","longest whole plant longevity"))

nrow(PlantLifespan_years_families_BIEN)
length(unique(PlantLifespan_years_families_BIEN$scrubbed_family))
unique(PlantLifespan_years_families_BIEN$trait_value)

nrow(PlantLifespan_years_genus_BIEN)
length(unique(PlantLifespan_years_genus_BIEN$scrubbed_genus))
unique(PlantLifespan_years_genus_BIEN$trait_value)

# GIFT
#Lifespan_families_GIFT <- GIFT_traits_tax(trait_IDs = GIFT_trait('Lifespan'))
# no records

# remove non-numeric trait values and check units
PlantLifespan_years_families_BIEN <- PlantLifespan_years_families_BIEN %>%
  filter(!is.na(as.numeric(trait_value))) %>%
  mutate(trait_value = as.numeric(trait_value))

nrow(PlantLifespan_years_families_BIEN)
unique(PlantLifespan_years_families_BIEN$unit)

PlantLifespan_years_genus_BIEN <- PlantLifespan_years_genus_BIEN %>%
  filter(!is.na(as.numeric(trait_value))) %>%
  mutate(trait_value = as.numeric(trait_value))

nrow(PlantLifespan_years_genus_BIEN)
unique(PlantLifespan_years_genus_BIEN$unit)


#### fruit type ####

# create a list of unique genus and families for trait
FruitType_families <- unique(FruitType$family)
FruitType_genus <- unique(FruitType$genus)
length(FruitType_families)
length(FruitType_genus)

# both BIEN and GIFT
# BIEN
FruitType_families_BIEN <- BIEN_trait_traitbyfamily(family = FruitType_families, trait = "fruit type")

FruitType_genus_BIEN <- BIEN_trait_traitbygenus(genus = FruitType_genus, trait = "fruit type")

nrow(FruitType_families_BIEN)
length(unique(FruitType_families_BIEN$scrubbed_family))
unique(FruitType_families_BIEN$trait_value)

nrow(FruitType_genus_BIEN)
length(unique(FruitType_genus_BIEN$scrubbed_genus))
unique(FruitType_genus_BIEN$trait_value)

# GIFT
#FruitType_families_GIFT <- GIFT_traits_tax(trait_IDs = GIFT_trait('Fruit type'))
# no records

# haromonize trait values
unique(FruitType_families_BIEN$trait_value)
unique(FruitType_genus_BIEN$trait_value)

FruitType_families_BIEN <- FruitType_families_BIEN %>%
  filter() %>%
  mutate(trait_value = case_when(
    grepl('Berry|berry|Berry ', trait_value, ignore.case = TRUE) ~ "berry",
    grepl('capsule|Capsule|Capsule ', trait_value, ignore.case = TRUE) ~ "capsule",
    grepl('drupe|Drupe|Drupe |Drupaceous', trait_value, ignore.case = TRUE) ~ "drupe",
    grepl('Squash', trait_value, ignore.case = TRUE) ~ "squash",
    grepl('pod|Pod|Pod ', trait_value, ignore.case = TRUE) ~ "pod",
    grepl('Samara|Samaroid', trait_value, ignore.case = TRUE) ~ "samara"
  )) %>%
  filter(!is.na(trait_value))

FruitType_genus_BIEN <- FruitType_genus_BIEN %>%
  filter() %>%
  mutate(trait_value = case_when(
    grepl('Berry|berry|Berry ', trait_value, ignore.case = TRUE) ~ "berry",
    grepl('capsule|Capsule|Capsule ', trait_value, ignore.case = TRUE) ~ "capsule",
    grepl('drupe|Drupe|Drupe |Drupaceous', trait_value, ignore.case = TRUE) ~ "drupe",
    grepl('Squash', trait_value, ignore.case = TRUE) ~ "squash",
    grepl('pod|Pod|Pod ', trait_value, ignore.case = TRUE) ~ "pod",
    grepl('Samara|Samaroid', trait_value, ignore.case = TRUE) ~ "samara"
  )) %>%
  filter(!is.na(trait_value))

unique(FruitType_families_BIEN$trait_value)
unique(FruitType_genus_BIEN$trait_value)


#### fruit color ####

# create a list of unique genus and families for trait
# FruitColor_families <- unique(FruitColor$family)
# FruitColor_genus <- unique(FruitColor$genus)
# length(FruitColor_families)
# length(FruitColor_genus)

# GIFT only
#FruitColor_families_GIFT <- GIFT_traits_tax(trait_IDs = GIFT_trait('Fruit colour'))
# no records


#### seed length ####

# create a list of unique genus and families for trait
SeedLength_mm_families <- unique(SeedLength_mm$family)
SeedLength_mm_genus <- unique(SeedLength_mm$genus)
length(SeedLength_mm_families)
length(SeedLength_mm_genus)

# both BIEN and GIFT
# BIEN
SeedLength_mm_families_BIEN <- BIEN_trait_traitbyfamily(family = SeedLength_mm_families, trait = "seed length")

SeedLength_mm_genus_BIEN <- BIEN_trait_traitbygenus(genus = SeedLength_mm_genus, trait = "seed length")

nrow(SeedLength_mm_families_BIEN)
length(unique(SeedLength_mm_families_BIEN$scrubbed_family))
unique(SeedLength_mm_families_BIEN$trait_value)

nrow(SeedLength_mm_genus_BIEN)
length(unique(SeedLength_mm_genus_BIEN$scrubbed_genus))
unique(SeedLength_mm_genus_BIEN$trait_value)

# GIFT
SeedLength_mm_families_GIFT <- GIFT_traits_tax(trait_IDs = GIFT_trait('Seed length'))

SeedLength_mm_families_GIFT <- SeedLength_mm_families_GIFT[SeedLength_mm_families_GIFT$taxon_name %in% SeedLength_mm_families, ]

nrow(SeedLength_mm_families_GIFT)

# remove non-numeric trait values and check units
SeedLength_mm_families_BIEN <- SeedLength_mm_families_BIEN %>%
  filter(!is.na(as.numeric(trait_value))) %>%
  mutate(trait_value = as.numeric(trait_value))

nrow(SeedLength_mm_families_BIEN)
unique(SeedLength_mm_families_BIEN$unit)

SeedLength_mm_genus_BIEN <- SeedLength_mm_genus_BIEN %>%
  filter(!is.na(as.numeric(trait_value))) %>%
  mutate(trait_value = as.numeric(trait_value))

nrow(SeedLength_mm_genus_BIEN)
unique(SeedLength_mm_genus_BIEN$unit)

SeedLength_mm_families_GIFT$trait_value <- SeedLength_mm_families_GIFT$`3.10.3`
SeedLength_mm_families_GIFT$unit <- 'mm'


#### seed width ####

# create a list of unique genus and families for trait
# SeedWidth_mm_families <- unique(SeedWidth_mm$family)
# SeedWidth_mm_genus <- unique(SeedWidth_mm$genus)
# length(SeedWidth_mm_families)
# length(SeedWidth_mm_genus)

# GIFT only
#SeedWidth_mm_families_GIFT <- GIFT_traits_tax(trait_IDs = GIFT_trait('Seed width'))
# no records


#### plant height ####

# create a list of unique genus and families for trait
PlantHeight_m_families <- unique(PlantHeight_m$family)
PlantHeight_m_genus <- unique(PlantHeight_m$genus)
length(PlantHeight_m_families)
length(PlantHeight_m_genus)

# both BIEN and GIFT
# BIEN
PlantHeight_m_families_BIEN <- BIEN_trait_traitbyfamily(family = PlantHeight_m_families, trait = c("whole plant height","minimum whole plant height","maximum whole plant height"))

PlantHeight_m_genus_BIEN <- BIEN_trait_traitbygenus(genus = PlantHeight_m_genus, trait = c("whole plant height","minimum whole plant height","maximum whole plant height"))

nrow(PlantHeight_m_families_BIEN)
length(unique(PlantHeight_m_families_BIEN$scrubbed_family))
unique(PlantHeight_m_families_BIEN$trait_value)

nrow(PlantHeight_m_genus_BIEN)
length(unique(PlantHeight_m_genus_BIEN$scrubbed_genus))
unique(PlantHeight_m_genus_BIEN$trait_value)

# GIFT
#PlantHeight_families_GIFT <- GIFT_traits_tax(trait_IDs = GIFT_trait('Plant height'))
# no records

# remove non-numeric trait values and check units
PlantHeight_m_families_BIEN <- PlantHeight_m_families_BIEN %>%
  filter(!is.na(as.numeric(trait_value))) %>%
  mutate(trait_value = as.numeric(trait_value))

nrow(PlantHeight_m_families_BIEN)
unique(PlantHeight_m_families_BIEN$unit)

PlantHeight_m_genus_BIEN <- PlantHeight_m_genus_BIEN %>%
  filter(!is.na(as.numeric(trait_value))) %>%
  mutate(trait_value = as.numeric(trait_value))

nrow(PlantHeight_m_genus_BIEN)
unique(PlantHeight_m_genus_BIEN$unit)


#### growth form ####

# create a list of unique genus and families for trait
GrowthForm_families <- unique(GrowthForm$family)
GrowthForm_genus <- unique(GrowthForm$genus)
length(GrowthForm_families)
length(GrowthForm_genus)

# both BIEN and GIFT
# BIEN
GrowthForm_families_BIEN <- BIEN_trait_traitbyfamily(family = GrowthForm_families, trait = "whole plant growth form")

GrowthForm_genus_BIEN <- BIEN_trait_traitbygenus(genus = GrowthForm_genus, trait = "whole plant growth form")

nrow(GrowthForm_families_BIEN)
length(unique(GrowthForm_families_BIEN$scrubbed_family))
unique(GrowthForm_families_BIEN$trait_value)

nrow(GrowthForm_genus_BIEN)
length(unique(GrowthForm_genus_BIEN$scrubbed_genus))
unique(GrowthForm_genus_BIEN$trait_value)

# GIFT
#Growth.Form_families_GIFT <- GIFT_traits_tax(trait_IDs = GIFT_trait('Growth form'))
# no records

# harmonize trait values and remove numeric values

# remove numeric rows
GrowthForm_families_BIEN <- GrowthForm_families_BIEN %>%
  filter(is.na(as.numeric(trait_value)))
nrow(GrowthForm_families_BIEN)

GrowthForm_genus_BIEN <- GrowthForm_genus_BIEN %>%
  filter(is.na(as.numeric(trait_value)))
nrow(GrowthForm_genus_BIEN)

# classify values as herb, shrub, tree, or other
GrowthForm_families_BIEN <- GrowthForm_families_BIEN %>%
  filter() %>%
  mutate(trait_value = case_when(
    grepl("herb|forb", trait_value, ignore.case = TRUE) ~ "herb",
    grepl("shrub|bush", trait_value, ignore.case = TRUE) ~ "shrub",
    grepl("tree", trait_value, ignore.case = TRUE) ~ "tree",
    TRUE ~ "other"
  ))

GrowthForm_genus_BIEN <- GrowthForm_genus_BIEN %>%
  filter() %>%
  mutate(trait_value = case_when(
    grepl("herb|forb", trait_value, ignore.case = TRUE) ~ "herb",
    grepl("shrub|bush", trait_value, ignore.case = TRUE) ~ "shrub",
    grepl("tree", trait_value, ignore.case = TRUE) ~ "tree",
    TRUE ~ "other"
  ))

unique(GrowthForm_families_BIEN$trait_value)
unique(GrowthForm_genus_BIEN$trait_value)


#### fruit length ####

# create a list of unique genus and families for trait
FruitLength_mm_families <- unique(FruitLength_mm$family)
FruitLength_mm_genus <- unique(FruitLength_mm$genus)
length(FruitLength_mm_families)
length(FruitLength_mm_genus)

# both BIEN and GIFT
# BIEN
FruitLength_mm_families_BIEN <- BIEN_trait_traitbyfamily(family = FruitLength_mm_families, trait = c("maximum fruit length", "minimum fruit length"))

FruitLength_mm_genus_BIEN <- BIEN_trait_traitbygenus(genus = FruitLength_mm_genus, trait = c("maximum fruit length", "minimum fruit length"))

nrow(FruitLength_mm_families_BIEN)
length(unique(FruitLength_mm_families_BIEN$scrubbed_family))
unique(FruitLength_mm_families_BIEN$trait_value)

nrow(FruitLength_mm_genus_BIEN)
length(unique(FruitLength_mm_genus_BIEN$scrubbed_genus))
unique(FruitLength_mm_genus_BIEN$trait_value)

# GIFT
#FruitLength_families_GIFT <- GIFT_traits_tax(trait_IDs = GIFT_trait('Fruit length'))
# no records

# remove non-numeric trait values and check units
FruitLength_mm_families_BIEN <- FruitLength_mm_families_BIEN %>%
  filter(!is.na(as.numeric(trait_value))) %>%
  mutate(trait_value = as.numeric(trait_value))

nrow(FruitLength_mm_families_BIEN)
unique(FruitLength_mm_families_BIEN$unit)

FruitLength_mm_genus_BIEN <- FruitLength_mm_genus_BIEN %>%
  filter(!is.na(as.numeric(trait_value))) %>%
  mutate(trait_value = as.numeric(trait_value))

nrow(FruitLength_mm_genus_BIEN)
unique(FruitLength_mm_genus_BIEN$unit)


#### seed mass ####

# create a list of unique genus and families for trait
SeedMass_g_families <- unique(SeedMass_g$family)
SeedMass_g_genus <- unique(SeedMass_g$genus)
length(SeedMass_g_families)
length(SeedMass_g_genus)

# both in BIEN and GIFT
# BIEN
SeedMass_g_families_BIEN <- BIEN_trait_traitbyfamily(family = SeedMass_g_families, trait = "seed mass")

SeedMass_g_genus_BIEN <- BIEN_trait_traitbygenus(genus = SeedMass_g_genus, trait = "seed mass")

nrow(SeedMass_g_families_BIEN)
length(unique(SeedMass_g_families_BIEN$scrubbed_family))
unique(SeedMass_g_families_BIEN$trait_value)

nrow(SeedMass_g_genus_BIEN)
length(unique(SeedMass_g_genus_BIEN$scrubbed_genus))
unique(SeedMass_g_genus_BIEN$trait_value)

# GIFT
#SeedMass_g_families_GIFT <- GIFT_traits_tax(trait_IDs = GIFT_trait('seed mass'))
# no records

# remove non-numeric trait values and check units
SeedMass_g_families_BIEN <- SeedMass_g_families_BIEN %>%
  filter(!is.na(as.numeric(trait_value))) %>%
  mutate(trait_value = as.numeric(trait_value))

nrow(SeedMass_g_families_BIEN)
unique(SeedMass_g_families_BIEN$unit)

SeedMass_g_genus_BIEN <- SeedMass_g_genus_BIEN %>%
  filter(!is.na(as.numeric(trait_value))) %>%
  mutate(trait_value = as.numeric(trait_value))

nrow(SeedMass_g_genus_BIEN)
unique(SeedMass_g_genus_BIEN$unit)


# combine new info with existing trait table

#### To fill in gaps of missing traits, use genus level data first then family level. Add column to trait dataframe of the level of trait measurement: species, genus, family. ####

# subset dataframe to only keep required columns

# for traits to family level from BIEN, keep columns of scrubbed_family, trait_name, trait_value
BIEN_family <- function(df){
  newdf <- data.frame(df[,c("scrubbed_family", "trait_name", "trait_value")])
  return(newdf)
}

DispersalSyndrome_families_BIEN_df <- BIEN_family(DispersalSyndrome_families_BIEN)
PlantLifespan_years_families_BIEN_df <- BIEN_family(PlantLifespan_years_families_BIEN)
FruitType_families_BIEN_df <- BIEN_family(FruitType_families_BIEN)
SeedLength_mm_families_BIEN_df <- BIEN_family(SeedLength_mm_families_BIEN)
PlantHeight_m_families_BIEN_df <- BIEN_family(PlantHeight_m_families_BIEN)
GrowthForm_families_BIEN_df <- BIEN_family(GrowthForm_families_BIEN)
FruitLength_mm_families_BIEN_df <- BIEN_family(FruitLength_mm_families_BIEN)
SeedMass_g_families_BIEN_df <- BIEN_family(SeedMass_g_families_BIEN)

# for traits with GIFT family data
DispersalSyndrome_families_GIFT_df <- DispersalSyndrome_families_GIFT[,c('taxon_name','trait_value')] 
DispersalSyndrome_families_GIFT_df$trait_name <- 'Dispersal syndrome'
colnames(DispersalSyndrome_families_GIFT_df)[1] <- 'scrubbed_family'

SeedLength_mm_families_GIFT_df <-SeedLength_mm_families_GIFT[,c('taxon_name','trait_value')] 
SeedLength_mm_families_GIFT_df$trait_name <- 'Seed length'
colnames(SeedLength_mm_families_GIFT_df)[1] <- 'scrubbed_family'


# for traits to genus level from BIEN, keep columns of scrubbed_genus, trait_name, trait_value
BIEN_genus <- function(df){
  newdf <- data.frame(df[,c("scrubbed_genus", "trait_name", "trait_value")])
  return(newdf)
}

DispersalSyndrome_genus_BIEN_df <- BIEN_genus(DispersalSyndrome_genus_BIEN)
PlantLifespan_years_genus_BIEN_df <- BIEN_genus(PlantLifespan_years_genus_BIEN)
FruitType_genus_BIEN_df <- BIEN_genus(FruitType_genus_BIEN)
SeedLength_mm_genus_BIEN_df <- BIEN_genus(SeedLength_mm_genus_BIEN)
PlantHeight_m_genus_BIEN_df <- BIEN_genus(PlantHeight_m_genus_BIEN)
GrowthForm_genus_BIEN_df <- BIEN_genus(GrowthForm_genus_BIEN)
FruitLength_mm_genus_BIEN_df <- BIEN_genus(FruitLength_mm_genus_BIEN)
SeedMass_g_genus_BIEN_df <- BIEN_genus(SeedMass_g_genus_BIEN)


# get a single trait value for each family/genus

# numeric traits
numeric_traits_combined <- function(df, level, traitname) {
  df$trait_value <- as.numeric(df$trait_value)
  if (level == "genus") {
    summary_data <- df %>%
      group_by(scrubbed_genus) %>%
      summarise(
        trait_value_mean = exp(mean(log(trait_value))),
        records_used = n(),
        variance = var(trait_value, na.rm = TRUE)
      )
    # Add trait name column
    summary_data$TraitName <- traitname
    
    # Rename the mean column to TraitValue
    summary_data <- summary_data %>%
      rename(TraitValue = trait_value_mean)
    
    return(summary_data)
  } else if (level == "family") {
    summary_data <- df %>%
      group_by(scrubbed_family) %>%
      summarise(
        trait_value_mean = exp(mean(log(trait_value))),
        records_used = n(),
        variance = var(trait_value, na.rm = TRUE)
      )
    # Add trait name column
    summary_data$TraitName <- traitname
    
    # Rename the mean column to TraitValue
    summary_data <- summary_data %>%
      rename(TraitValue = trait_value_mean)
    
    return(summary_data)
  }
}

# non-numeric traits
cat_traits_combined <- function(df, level, traitname){
  if (level == "genus") {
    summary_data <- df %>%
      group_by(scrubbed_genus) %>%
      summarise(
        ModeValue = names(sort(table(trait_value), decreasing = TRUE))[1],
        records_used = n(),
        mode_freq = max(table(trait_value)),
        variance = 1 - (max(table(trait_value)) / n())
      )
    
    # Add trait name column
    summary_data$TraitName <- traitname
    
    # Rename the mean column to TraitValue
    summary_data <- summary_data %>%
      rename(TraitValue = ModeValue)
    
    return(summary_data)
  } else if (level == "family") {
    summary_data <- df %>%
      group_by(scrubbed_family) %>%
      summarise(
        ModeValue = names(sort(table(trait_value), decreasing = TRUE))[1],
        records_used = n(),
        mode_freq = max(table(trait_value)),
        variance = 1 - (max(table(trait_value)) / n())
      )
    
    # Add trait name column
    summary_data$TraitName <- traitname
    
    # Rename the mean column to TraitValue
    summary_data <- summary_data %>%
      rename(TraitValue = ModeValue)
    
    return(summary_data)
  }
}


# dispersal syndrome
dim(DispersalSyndrome_families_GIFT_df)
dim(DispersalSyndrome_families_BIEN_df)

DispersalSyndrome_families_df <- rbind(DispersalSyndrome_families_BIEN_df,DispersalSyndrome_families_GIFT_df)

DispersalSyndrome_families_df_average <- cat_traits_combined(DispersalSyndrome_families_df, "family", "DispersalSyndrome")
dim(DispersalSyndrome_families_df_average)

dim(DispersalSyndrome_genus_BIEN_df)
DispersalSyndrome_genus_BIEN_df_average <- cat_traits_combined(DispersalSyndrome_genus_BIEN_df, "genus", "DispersalSyndrome")
dim(DispersalSyndrome_genus_BIEN_df_average)

# lifespan
dim(PlantLifespan_years_families_BIEN_df)
PlantLifespan_years_families_BIEN_df_average <- numeric_traits_combined(PlantLifespan_years_families_BIEN_df, "family", "PlantLifespan_years")
dim(PlantLifespan_years_families_BIEN_df_average)

dim(PlantLifespan_years_genus_BIEN_df)
PlantLifespan_years_genus_BIEN_df_average <- numeric_traits_combined(PlantLifespan_years_genus_BIEN_df, "genus", "PlantLifespan_years")
dim(PlantLifespan_years_genus_BIEN_df_average)

# fruit type
dim(FruitType_families_BIEN_df)
FruitType_families_BIEN_df_average <- cat_traits_combined(FruitType_families_BIEN_df, "family", "FruitType")
dim(FruitType_families_BIEN_df_average)

dim(FruitType_genus_BIEN_df)
FruitType_genus_BIEN_df_average <- cat_traits_combined(FruitType_genus_BIEN_df, "genus", "FruitType")
dim(FruitType_genus_BIEN_df_average)

# seed length
SeedLength_mm_families_df <- rbind(SeedLength_mm_families_BIEN_df,SeedLength_mm_families_GIFT_df)

dim(SeedLength_mm_families_df)
SeedLength_mm_families_df_average <- numeric_traits_combined(SeedLength_mm_families_df, "family", "SeedLength_mm")
dim(SeedLength_mm_families_df_average)

dim(SeedLength_mm_genus_BIEN_df)
SeedLength_mm_genus_BIEN_df_average <- numeric_traits_combined(SeedLength_mm_genus_BIEN_df, "genus", "SeedLength_mm")
dim(SeedLength_mm_genus_BIEN_df_average)

# height
dim(PlantHeight_m_families_BIEN_df)
PlantHeight_m_families_BIEN_df_average <- numeric_traits_combined(PlantHeight_m_families_BIEN_df, "family", "PlantHeight_m")
dim(PlantHeight_m_families_BIEN_df_average)

dim(PlantHeight_m_genus_BIEN_df)
PlantHeight_m_genus_BIEN_df_average <- numeric_traits_combined(PlantHeight_m_genus_BIEN_df, "genus", "PlantHeight_m")
dim(PlantHeight_m_genus_BIEN_df_average)

# growth form
dim(GrowthForm_families_BIEN_df)
GrowthForm_families_BIEN_df_average <- cat_traits_combined(GrowthForm_families_BIEN_df, "family", "GrowthForm")
dim(GrowthForm_families_BIEN_df_average)

dim(GrowthForm_genus_BIEN_df)
GrowthForm_genus_BIEN_df_average <- cat_traits_combined(GrowthForm_genus_BIEN_df, "genus", "GrowthForm")
dim(GrowthForm_genus_BIEN_df_average)

# fruit length
dim(FruitLength_mm_families_BIEN_df)
FruitLength_mm_families_BIEN_df_average <- numeric_traits_combined(FruitLength_mm_families_BIEN_df, "family", "FruitLength_mm")
dim(FruitLength_mm_families_BIEN_df_average)

dim(FruitLength_mm_genus_BIEN_df)
FruitLength_mm_genus_BIEN_df_average <- numeric_traits_combined(FruitLength_mm_genus_BIEN_df, "genus", "FruitLength_mm")
dim(FruitLength_mm_genus_BIEN_df_average)

# seed mass
dim(SeedMass_g_families_BIEN_df)
SeedMass_g_families_BIEN_df_average <- numeric_traits_combined(SeedMass_g_families_BIEN_df, "family", "SeedMass_g")
dim(PlantLifespan_years_families_BIEN_df_average)

dim(SeedMass_g_genus_BIEN_df)
SeedMass_g_genus_BIEN_df_average <- numeric_traits_combined(SeedMass_g_genus_BIEN_df, "genus", "SeedMass_g")
dim(SeedMass_g_genus_BIEN_df_average)


# add column of trait level (species, genus, family)

# add new column trait_level to long_plant_trait dataframe
long_plant_traits <- long_plant_traits %>%
  mutate(TraitLevel = ifelse(!is.na(TraitValue), "species", NA))

# add family and genus information to long_plant_trait dataframe
long_plant_traits_tax <- merge(long_plant_traits, na_species_df, by = "species", all.x = TRUE)


# combine all family level trait data
family_level_trait_data <- list(DispersalSyndrome_families_df_average,
                                PlantLifespan_years_families_BIEN_df_average,
                                FruitType_families_BIEN_df_average, 
                                SeedLength_mm_families_df_average,
                                PlantHeight_m_families_BIEN_df_average,
                                GrowthForm_families_BIEN_df_average, 
                                FruitLength_mm_families_BIEN_df_average, 
                                SeedMass_g_families_BIEN_df_average)

# function to select and convert columns to character
select_and_convert <- function(df) {
  df %>%
    dplyr::select(scrubbed_family, TraitName, TraitValue) %>%
    mutate(across(everything(), as.character))
}

# apply the function to each dataframe in the list and combine them
all_family_traits <- family_level_trait_data %>%
  map_df(select_and_convert) %>%
  bind_rows()

# add trait level column
all_family_traits$TraitLevel <- "family"


# combine all genus level trait data
genus_level_trait_data <- list(DispersalSyndrome_genus_BIEN_df_average, 
                               PlantLifespan_years_genus_BIEN_df_average,
                               FruitType_genus_BIEN_df_average,
                               SeedLength_mm_genus_BIEN_df_average,
                               PlantHeight_m_genus_BIEN_df_average,
                               GrowthForm_genus_BIEN_df_average, 
                               FruitLength_mm_genus_BIEN_df_average, 
                               SeedMass_g_genus_BIEN_df_average)

# function to select and convert columns to character
select_and_convert <- function(df) {
  df %>%
    dplyr::select(scrubbed_genus, TraitName, TraitValue) %>%
    mutate(across(everything(), as.character))
}

# apply the function to each dataframe in the list and combine them
all_genus_traits <- genus_level_trait_data %>%
  map_df(select_and_convert) %>%
  bind_rows()

# add trait level column
all_genus_traits$TraitLevel <- "genus"

# change family and genus columns
colnames(all_family_traits)[which(colnames(all_family_traits) == "scrubbed_family")] <- "family"

colnames(all_genus_traits)[which(colnames(all_genus_traits) == "scrubbed_genus")] <- "genus"

# use family & genus level traits to fill in gaps of na_traits_family_genus
# genus level preferred over family

# join the dataframes based on genus and TraitName
filled_na_traits_family_genus <- na_traits_family_genus %>%
  left_join(all_genus_traits, by = c("genus", "TraitName"), relationship = "many-to-many") %>%
  # Fill in TraitValue column with values from all_genus_traits
  mutate(TraitValue = coalesce(TraitValue.x, TraitValue.y)) %>%
  # Remove the columns
  select(-TraitValue.y, -TraitValue.x)

na_traits_after_genus <- filled_na_traits_family_genus %>%
  filter(is.na(TraitValue))

nrow(na_traits_after_genus)

filled_traits_with_genus <- filled_na_traits_family_genus %>%
  filter(!is.na(TraitValue))

nrow(filled_traits_with_genus)
unique(filled_traits_with_genus$TraitName)


# join the dataframes based on family and TraitName

# remove trait level column from na_traits_after_genus
na_traits_after_genus <- na_traits_after_genus %>%
  select(-TraitLevel)

filled_na_traits_family <- na_traits_after_genus %>%
  left_join(all_family_traits, by = c("family", "TraitName"), relationship = "many-to-many") %>%
  # Fill in TraitValue column with values from all_genus_traits
  mutate(TraitValue = coalesce(TraitValue.x, TraitValue.y)) %>%
  # Remove the columns
  select(-TraitValue.y, -TraitValue.x)

filled_traits_with_family <- filled_na_traits_family %>%
  filter(!is.na(TraitValue))

nrow(filled_traits_with_family)
unique(filled_traits_with_family$TraitName)

na_traits_after_family <- filled_na_traits_family %>%
  filter(is.na(TraitValue))

nrow(na_traits_after_family)

nonimputed_traits <- long_plant_traits_tax %>%
  filter(!is.na(TraitValue))


# combine filled_traits_with_genus, filled_traits_with_family, and nonimputed_traits
all_traits_with_tax <- rbind(filled_traits_with_genus, filled_traits_with_family, nonimputed_traits)

all_traits_with_tax %>%
  group_by(TraitName) %>%
  summarise(num_species = n_distinct(species))

na_traits_after_family %>%
  group_by(TraitName) %>%
  summarise(num_species = n_distinct(species))

# add NAs back to all traits
all_traits_with_NAs <- rbind(all_traits_with_tax, na_traits_after_family)

# count number of NA records
na_records_count <- sum(is.na(all_traits_with_NAs$TraitValue))

# count number of unique species with NA records
species_with_na_count <- all_traits_with_NAs %>%
  filter(is.na(TraitValue)) %>%
  distinct(species) %>%
  nrow()

# count number of unique traits with NA records
traits_with_na_count <- all_traits_with_NAs %>%
  filter(is.na(TraitValue)) %>%
  distinct(TraitName) %>%
  nrow()

# Print results
cat("Number of NA records:", na_records_count, "\n")
cat("Number of species with NA records:", species_with_na_count, "\n")
cat("Number of traits with NA records:", traits_with_na_count, "\n")


# save long dataframe
write.csv(all_traits_with_NAs, file.path(output_path_L1,"TropicalAndes_all_plant_traits_filled_with_family_genus_long.csv"))


# convert long dataframe to wide

#remove unused columns
all_traits_with_NAs_long <- all_traits_with_NAs %>%
  select(-TraitLevel, -family, -genus) %>%
  distinct()

wide_traits <- pivot_wider(all_traits_with_NAs_long, names_from = TraitName, values_from = TraitValue)

glimpse(wide_traits)

# convert column types from character to factor & numeric
names(wide_traits)

wide_traits$PlantHeight_m <- as.numeric(wide_traits$PlantHeight_m)
wide_traits$FruitType <- as.factor(wide_traits$FruitType)
wide_traits$SeedWidth_mm <- as.numeric(wide_traits$SeedWidth_mm)
wide_traits$FruitColor <- as.factor(wide_traits$FruitColor)
wide_traits$PlantLifespan_years <- as.numeric(wide_traits$PlantLifespan_years)
wide_traits$SeedMass_g <- as.numeric(wide_traits$SeedMass_g)
wide_traits$FruitLength_mm <- as.numeric(wide_traits$FruitLength_mm)
wide_traits$FruitMass_mg <- as.numeric(wide_traits$FruitMass_mg )
wide_traits$FruitDryness <- as.factor(wide_traits$FruitDryness)
wide_traits$GrowthForm <- as.factor(wide_traits$GrowthForm)
wide_traits$SeedLength_mm <- as.numeric(wide_traits$SeedLength_mm)
wide_traits$DispersalSyndrome <- as.factor(wide_traits$DispersalSyndrome)
wide_traits$FruitConspicuousness <- as.factor(wide_traits$FruitConspicuousness)

# summary
fb_table_trait_summary(wide_traits)

fb_plot_species_traits_completeness(wide_traits)
ggsave("plant_trait_completeness_familygenus.png", plot = last_plot(), path = figure_path)

fb_plot_number_species_by_trait(wide_traits)
ggsave("plant_number_species_trait_familygenus.png", plot = last_plot(), path = figure_path)

# impute Traits
# set the seed for reproducibility
set.seed(123)

# perform the imputation
imp_model <- mice(wide_traits, method = "cart", maxit = 20)

imputed_data <- complete(imp_model)

glimpse(imputed_data)

fb_table_trait_summary(imputed_data)

# Save imputed data
write.csv(imputed_data, file.path(output_path_L1,"TropicalAndes_imputed_plant_traits.csv"))


#### remove traits that no GIFT or BIEN data can be collected from ####

# exploratory analyses
# some traits cover a small % of plants: look for correlation

hist(log(wide_traits$SeedLength_mm))
hist(log(wide_traits$FruitMass_mg))
hist(log(wide_traits$FruitLength_mm))
hist(log(wide_traits$SeedWidth_mm))
hist(log(wide_traits$SeedMass_g))

# comparing fruit measurements
measurements <- subset(wide_traits, select=c('SeedLength_mm','FruitMass_mg','FruitLength_mm','SeedWidth_mm', 'SeedMass_g'))

measurements$SeedLength_mm <- log(measurements$SeedLength_mm+1)
measurements$FruitMass_mg <- log(measurements$FruitMass_mg)
measurements$FruitLength_mm <- log(measurements$FruitLength_mm+1)
measurements$SeedWidth_mm <- log(measurements$SeedWidth_mm+1)
measurements$SeedMass_g <- log(measurements$SeedMass_g+1)

mG <- lm(data=measurements, SeedLength_mm~.)
step(mG, direction='backward')

mL <- lm(data=measurements, SeedLength_mm~1)
step(mG, direction='forward')

m <- lm(formula = SeedLength_mm ~ FruitMass_mg + SeedWidth_mm + FruitLength_mm + SeedMass_g, data = measurements)
summary(m)

ggplot(measurements, aes(x=FruitMass_mg, y=SeedLength_mm))+
  geom_point()+
  theme_classic()

ggplot(measurements, aes(x=SeedWidth_mm, y=SeedLength_mm))+
  geom_point()+
  theme_classic()

ggplot(measurements, aes(x=FruitLength_mm, y=SeedLength_mm))+
  geom_point()+
  theme_classic()

ggplot(measurements, aes(x=SeedMass_g, y=SeedLength_mm))+
  geom_point()+
  theme_classic()

library(car)
dryness <- filter(wide_traits, !is.na(FruitDryness))
m2 <- lm(data=dryness, log(SeedMass_g+1)~FruitDryness)
summary(m2)
Anova(m2)

ggplot(dryness, aes(x=FruitDryness, y=log(SeedMass_g+1)))+
  geom_boxplot()+
  theme_classic()

conspicuousness <- filter(wide_traits, !is.na(FruitConspicuousness))
m2 <- lm(data=conspicuousness, log(SeedLength_mm+1)~FruitConspicuousness)
summary(m2)
Anova(m2)

ggplot(conspicuousness, aes(x=FruitConspicuousness, y=log(SeedLength_mm+1)))+
  geom_boxplot()+
  theme_classic()

# subset data
traits_kept <- c('PlantHeight_m','FruitType','PlantLifespan_years','SeedMass_g','FruitLength_mm','GrowthForm','SeedLength_mm','DispersalSyndrome')
all_traits_with_NAs2 <- filter(all_traits_with_NAs, TraitName==traits_kept)
unique(all_traits_with_NAs2$TraitName)

wide_traits2 <- subset(wide_traits, select=c('species','PlantHeight_m','FruitType','PlantLifespan_years','SeedMass_g','FruitLength_mm','GrowthForm','SeedLength_mm','DispersalSyndrome'))

# plot completeness
fb_plot_species_traits_completeness(wide_traits2)
ggsave("plant_trait_completeness_familygenus2.png", plot = last_plot(), path = figure_path)

fb_plot_number_species_by_trait(wide_traits2)
ggsave("plant_number_species_trait_familygenus2.png", plot = last_plot(), path = figure_path)

# impute Traits
# set the seed for reproducibility
set.seed(123)

# perform the imputation
imp_model2 <- mice(wide_traits2, method = "cart", maxit = 20)

imputed_data2 <- complete(imp_model2)

glimpse(imputed_data2)

# imputed data
fb_plot_species_traits_completeness(imputed_data2)
ggsave("plant_trait_completeness_imputed2.png", plot = last_plot(), path = figure_path)

fb_plot_number_species_by_trait(imputed_data2)
ggsave("plant_number_species_trait_imputed2.png", plot = last_plot(), path = figure_path)

fb_table_trait_summary(imputed_data2)

# reassign NA values in TraitLevel as "imputed"
all_traits_with_NAs2$TraitLevel[is.na(all_traits_with_NAs2$TraitLevel)] <- "imputed"

# count the number of traits per TraitLevel and TraitName
trait_counts <- all_traits_with_NAs2 %>%
  count(TraitLevel, TraitName) %>%
  mutate(TraitLevel = fct_relevel(TraitLevel, "species", "genus", "family", "imputed"))


# create the faceted bar plot with free y-axis scales
(trait_count_level_plot <- ggplot(trait_counts, aes(x = TraitLevel, y = n, fill = TraitLevel)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_viridis(discrete = TRUE) +
    labs(x = "Trait Level",
         y = "Trait Count",
         fill = "Level") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none") +  # Remove legend
    facet_wrap(~ TraitName, scales = "free_y", nrow = 2) +
    guides(fill = "none"))  # Remove legend
ggsave("plant_trait_counts_per_level2.png", plot = trait_count_level_plot, path = figure_path, height = 6, width = 8, units = "in")

# count the number of traits per TraitLevel
trait_counts_overall <- all_traits_with_NAs2 %>%
  count(TraitLevel) %>%
  mutate(TraitLevel = fct_relevel(TraitLevel, "species", "genus", "family", "imputed"))


# create the faceted bar plot with free y-axis scales
(all_trait_count_plot <- ggplot(trait_counts_overall, aes(x = TraitLevel, y = n, fill = TraitLevel)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_viridis(discrete = TRUE) +
    labs(title = "All traits",
         x = "Trait Level",
         y = "Trait Count",
         fill = "Level") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(fill = "none"))  # Remove legend
ggsave("plant_trait_counts_per_level_overall2.png", plot = all_trait_count_plot, path = figure_path)


# calculate the total sum of the values
total_sum <- sum(trait_counts_overall$n)


# calculate the percentage of each value
trait_counts_overall$percentage <- (trait_counts_overall$n / total_sum) * 100


# print the dataframe with the percentages
print(trait_counts_overall)


# Save imputed data
write.csv(imputed_data2, file.path(output_path_L1,"TropicalAndes_imputed_plant_traits2.csv"))
