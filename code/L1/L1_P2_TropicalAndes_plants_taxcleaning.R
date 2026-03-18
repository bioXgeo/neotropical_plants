# title: "Tropical Andes plant Lookup Table"
# author: "Hazel J. Anderson"
# project: "Plant-Frugivore Diversity"
# collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske, Jenna B. Baljunas"
# overview: "Taxonomic harmonization across trait databases"
# data input: "TropicalAndes_GBIF_plant_occ_cleaned.csv", "TropicalAndes_TRY_plant_traits.csv", "AllDesired_BIEN_plant_traits.csv", "TropicalAndes_GIFT_plant_traits.csv"
# data output: "all_species_harmonized_results.csv", "TropicalAndes_GBIF_plant_occ_harmonized.csv", "TropicalAndes_TRY_plant_traits_harmonized.csv", "TropicalAndes_BIEN_plant_traits_harmonized.csv", "TropicalAndes_GIFT_plant_traits_harmonized.csv"
# date: "2023-07-25; 2025-10-21"
# Notes: JB used HPCC (takes a long time (~12 hrs))

  
# load required packages
library(knitr); library(TNRS); library(bdc); library(dplyr)


# set file paths
data_path_L0<-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')

# #HPCC
# data_path_L0 <- file.path('/mnt/research/nasabio/data_2025/plants/L0')
# data_path_L1 <- file.path('/mnt/research/nasabio/data_2025/plants/L1')
# output_path <- file.path('/mnt/research/nasabio/data_2025/plants/L1')


# read in data
GBIF_occ <- read.csv(file.path(data_path_L1, "TropicalAndes_GBIF_plant_occ_cleaned.csv"))
TRY_traits <- read.csv(file.path(data_path_L0,"TropicalAndes_TRY_plant_traits.csv"))
BIEN_traits <- read.csv(file.path(data_path_L0,"AllDesired_BIEN_plant_traits.csv"))
GIFT_traits <- read.csv(file.path(data_path_L0,"TropicalAndes_GIFT_plant_traits.csv"))


# generate species lists
GBIF_species <- unique(GBIF_occ$species)
TRY_species <- unique(TRY_traits$SpeciesName)
BIEN_species <- unique(BIEN_traits$scrubbed_species_binomial)
GIFT_species <- unique(GIFT_traits$species)


length(GBIF_species)
length(TRY_species)
length(BIEN_species)
length(GIFT_species)


# combine species lists together
all_species <- c(GBIF_species, TRY_species, BIEN_species, GIFT_species)
all_species <- unique(all_species)
length(all_species)


# use bdc to check scientific names 
name_check_results <- bdc_clean_names(sci_names = all_species, save_outputs = FALSE)

all_species_cleaned <- name_check_results$names_clean
length(all_species_cleaned)


# remove NAs and duplicates
all_species_cleaned <- all_species[!is.na(all_species_cleaned)]
length(all_species_cleaned)

all_species_cleaned <- unique(all_species_cleaned)
length(all_species_cleaned)


# using TNRS to get harmonize taxonomy
all_species_harmonized_results <- TNRS(taxonomic_names = all_species_cleaned)


# subset TNRS results to species with matches, conflicts, and disparties
nrow(all_species_harmonized_results)
unique(all_species_harmonized_results$Taxonomic_status)

all_species_matches <- subset(all_species_harmonized_results, Taxonomic_status == "Accepted")
nrow(all_species_matches)

all_species_conflicts <- subset(all_species_harmonized_results, Taxonomic_status == "Synonym")
nrow(all_species_conflicts)

# disparities
all_species_no.opinion <- subset(all_species_harmonized_results, Taxonomic_status == "No opinion")
nrow(all_species_no.opinion)

all_species_illegitimate <- subset(all_species_harmonized_results, Taxonomic_status == "Illegitimate")
nrow(all_species_illegitimate)

all_species_invalid <- subset(all_species_harmonized_results, Taxonomic_status == "Invalid")
nrow(all_species_invalid)

all_species_other <- subset(all_species_harmonized_results, Taxonomic_status == "")
nrow(all_species_other)

all_species_NA <- subset(all_species_harmonized_results, is.na(all_species_harmonized_results$Taxonomic_status))
nrow(all_species_NA)


# create species list of names with disparities
all_species_disparities <- all_species_harmonized_results %>%
  filter(Taxonomic_status %in% c("No opinion", "Illegitimate", "Invalid", "", NA)) %>%
  dplyr::select(Name_submitted)
nrow(all_species_disparities)


# use bdc to harmonize disparities
# using COL database

all_species_disparities_col_results <- bdc_query_names_taxadb(sci_name = all_species_disparities$Name_submitted, replace_synonyms = TRUE,suggest_names = TRUE, db = "col", rank_name = "Plantae", rank = "kingdom")


# subset COL results to accepted, synonym, and NA
unique(all_species_disparities_col_results$taxonomicStatus)

all_species_col_accepted <- subset(all_species_disparities_col_results, taxonomicStatus == "accepted")
nrow(all_species_col_accepted)

all_species_col_synonym <- subset(all_species_disparities_col_results, taxonomicStatus == "synonym")
nrow(all_species_col_synonym)

all_species_col_NA <- subset(all_species_disparities_col_results, is.na(taxonomicStatus))
nrow(all_species_col_NA)


# create list of species still unharmonized
unresolved_species <- all_species_col_NA$original_search


# create lookup table

# from TNRS
species_matches <- all_species_matches[ , c("Name_submitted", "Accepted_species", "Source")]

species_corrected <- all_species_conflicts[ , c("Name_submitted", "Accepted_species", "Source")]

#combine
lookup_table_TNRS <- rbind(species_matches, species_corrected)
lookup_table_TNRS$harmonization_package <- c("TNRS")


# from bdc using COL
species_col_accepted <- all_species_col_accepted[ , c("original_search", "scientificName")]

species_col_accepted$source <- c("col")

species_col_synonmyn <- all_species_col_synonym[ , c("original_search", "scientificName")]

species_col_synonmyn$source <- c("col")


# update col names
colnames(species_col_accepted) <- c("Name_submitted", "Accepted_species", "Source")

colnames(species_col_synonmyn) <- c("Name_submitted", "Accepted_species", "Source")


# combine
lookup_table_bdc <- rbind(species_col_accepted, species_col_synonmyn)

lookup_table_bdc$harmonization_package <- c("bdc")


# combine to create lookup table
lookup_table <- rbind(lookup_table_TNRS, lookup_table_bdc)
nrow(lookup_table)


# check lookup table for duplicates
n_occur <- data.frame(table(lookup_table$Name_submitted))
nrow(n_occur)

duplicate_counts <- n_occur[n_occur$Freq >1,]
nrow(duplicate_counts)

duplicates <- lookup_table[lookup_table$Name_submitted %in% n_occur$Var1[n_occur$Freq > 1],]
nrow(duplicates)

duplicates_pairs <- duplicates[order(duplicates$Name_submitted),]
nrow(duplicates_pairs)


# remove duplicates and accepted names that are NA
lookup_table <- lookup_table[!duplicated(lookup_table), ]
nrow(lookup_table)

lookup_table <- subset(lookup_table, !is.na(Accepted_species))
nrow(lookup_table)


# save lookup table and unresolved species list as csv
write.csv(lookup_table, file.path(output_path,"plant_lookup_table.csv"), row.names = FALSE)

write.csv(unresolved_species, file.path(output_path,"plant_unresolved_species.csv"), row.names = FALSE)


# update data files species names using lookup table

# GBIF

# filter data to species in lookup_table$Names_submitted
GBIF_occ_subset <- GBIF_occ %>%
  filter(species %in% lookup_table$Name_submitted)
nrow(GBIF_occ_subset)

# filter lookup_table to species in data
lookup_table_subset <- lookup_table %>%
  filter(Name_submitted %in% GBIF_occ_subset$species)
nrow(lookup_table_subset)

# add accepted species column from lookup table based on species column
GBIF_occ_subset_harmonized <- right_join(lookup_table_subset, GBIF_occ_subset, join_by(Name_submitted == species), keep = TRUE, relationship = "one-to-many")
nrow(GBIF_occ_subset_harmonized)


# TRY

# filter data to species in lookup_table$Names_submitted
TRY_traits_subset <- TRY_traits %>%
  filter(SpeciesName %in% lookup_table$Name_submitted)
nrow(TRY_traits_subset)

# filter lookup_table to species in data
lookup_table_subset <- lookup_table %>%
  filter(Name_submitted %in% TRY_traits_subset$SpeciesName)
nrow(lookup_table_subset)

# add accepted species column from lookup table based on species column
TRY_traits_subset_harmonized <- right_join(lookup_table_subset, TRY_traits_subset, join_by(Name_submitted == SpeciesName), keep = TRUE, relationship = "one-to-many")
nrow(TRY_traits_subset_harmonized)


# BIEN

# filter data to species in lookup_table$Names_submitted
BIEN_traits_subset <- BIEN_traits %>%
  filter(scrubbed_species_binomial %in% lookup_table$Name_submitted)
nrow(BIEN_traits_subset)

# filter lookup_table to species in data
lookup_table_subset <- lookup_table %>%
  filter(Name_submitted %in% BIEN_traits_subset$scrubbed_species_binomial)
nrow(lookup_table_subset)

# add accepted species column from lookup table based on species column
BIEN_traits_subset_harmonized <- right_join(lookup_table_subset, BIEN_traits_subset, join_by(Name_submitted == scrubbed_species_binomial), keep = TRUE, relationship = "one-to-many")
nrow(BIEN_traits_subset_harmonized)


# GIFT

# filter data to species in lookup_table$Names_submitted
GIFT_traits_subset <- GIFT_traits %>%
  filter(species %in% lookup_table$Name_submitted)
nrow(GIFT_traits_subset)

# filter lookup_table to species in data
lookup_table_subset <- lookup_table %>%
  filter(Name_submitted %in% GIFT_traits_subset$species)
nrow(lookup_table_subset)

# add accepted species column from lookup table based on species column
GIFT_traits_subset_harmonized <- right_join(lookup_table_subset, GIFT_traits_subset, join_by(Name_submitted == species), keep = TRUE, relationship = "one-to-many")
nrow(GIFT_traits_subset_harmonized)


# summary

# GBIF
print("Number of records: ")
nrow(GBIF_occ_subset_harmonized)
print("Number of species: ")
length(unique(GBIF_occ_subset_harmonized$Accepted_species))

# TRY
print("Number of records: ")
nrow(TRY_traits_subset_harmonized)
print("Number of species: ")
length(unique(TRY_traits_subset_harmonized$Accepted_species))

# BIEN
print("Number of records: ")
nrow(BIEN_traits_subset_harmonized)
print("Number of species: ")
length(unique(BIEN_traits_subset_harmonized$Accepted_species))

# GIFT
print("Number of records: ")
nrow(GIFT_traits_subset_harmonized)
print("Number of species: ")
length(unique(GIFT_traits_subset_harmonized$Accepted_species))


# write data to csv
write.csv(all_species_harmonized_results, file.path(output_path,"all_species_harmonized_results.csv"), row.names = FALSE)
write.csv(GBIF_occ_subset_harmonized, file.path(output_path,"TropicalAndes_GBIF_plant_occ_harmonized.csv"), row.names = FALSE)
write.csv(TRY_traits_subset_harmonized, file.path(output_path,"TropicalAndes_TRY_plant_traits_harmonized.csv"), row.names = FALSE)
write.csv(BIEN_traits_subset_harmonized, file.path(output_path,"TropicalAndes_BIEN_plant_traits_harmonized.csv"), row.names = FALSE)
write.csv(GIFT_traits_subset_harmonized, file.path(output_path,"TropicalAndes_GIFT_plant_traits_harmonized.csv"), row.names = FALSE)
