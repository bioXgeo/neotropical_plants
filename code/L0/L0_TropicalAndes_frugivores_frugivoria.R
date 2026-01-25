# title: "Tropical Andes frugivore trait data Frugivoria"
# authors: "Hazel J. Anderson, Jenna B. Baljunas"
# project: "Plant-Frugivore Diversity"
# collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
# overview: "This script retrived the Frugivoria database from EDI."
# data input: "None"
# data output: "TropicalAndes_Frugivoria_frugivore_traits.csv, TropicalAndes_Frugivoria_frugivore_traits_species.csv, TropicalAndes_frugivore_LookupTable.csv, TropicalAndes_Frugivoria_mammal_traits.csv, TropicalAndes_Frugivoria_bird_traits.csv, TropicalAndes_mammal_LookupTable.csv, TropicalAndes_bird_LookupTable.csv"
# date: "2023-07-18; 2025-09-22"


# set file paths
data_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
output_path_L0 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
output_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')


# load required packages
library(dplyr); library(stringr)

# load functions
source("C:/GitHub_projects/plant-frugivore diversity/neotropical_plants/code/Functions.R")


# retrieve Frugivoria data

#Using r code provided at https://portal.edirepository.org/nis/codeGeneration?packageId=edi.1220.6&statisticalFileType=r
# Package ID: edi.1220.6 Cataloging System:https://pasta.edirepository.org.
# Data set title: Frugivoria: A trait database for birds and mammals exhibiting frugivory across contiguous Neotropical moist forests.
# Data set creator:  Beth Gerstner - Michigan State University 
# Data set creator:  Phoebe Zarnetske - Michigan State University 
# Data set creator:  Patrick Bills - Michigan State University 
# Contact:  Beth Gerstner -  Michigan State University  - gerstn11@msu.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 


# retrieve file Frugivoria: Mammal database simple
#file 3  
inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/1220/6/3c655f2ab1d525d1b1f05ee78153e875" 
infile3 <- tempfile()
download.file(inUrl3, infile3, method = "libcurl")
#try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")

Frug_mammal <-read.csv(infile3,header=F 
                       ,skip=1
                       ,sep=","  
                       ,quot='"' 
                       , col.names=c(
                         "IUCN_species_name",     
                         "common_name",     
                         "family",     
                         "genus",     
                         "species",     
                         "subspecies",     
                         "elton_species_name",     
                         "diet_cat",     
                         "diet_source_e",     
                         "diet_breadth",     
                         "diet_level",     
                         "activity_nocturnal_e",     
                         "activity_source_e",     
                         "activity_level",     
                         "for_strat_value_e",     
                         "for_strat_certainty_e",     
                         "body_mass_e",     
                         "body_mass_level_e",     
                         "body_mass_source_e",     
                         "body_size_mm",     
                         "body_size_notes",     
                         "body_size_level",     
                         "body_size_source",     
                         "sexual_dimorphism",     
                         "sexual_dimorphism_notes",     
                         "sexual_dimorphism_level",     
                         "sexual_dimorphism_source",     
                         "longevity",     
                         "longevity_notes",     
                         "longevity_level",     
                         "longevity_source",     
                         "home_range_size",     
                         "home_range_notes",     
                         "home_range_level",     
                         "home_range_source",     
                         "habitat_specialization",     
                         "habitat_specialization_source",     
                         "generation_time",     
                         "generation_time_notes",     
                         "generation_time_level",     
                         "generation_time_source",     
                         "IUCN_category",     
                         "habitat",     
                         "habitat_breadth",     
                         "habitat_breadth_source",     
                         "habitat_level",     
                         "mean_CHELSA_bio1_1981.2010_V.2.1",     
                         "mean_CHELSA_bio12_1981.2010_V.2.1",     
                         "mean_human_fp_range_2010",     
                         "mean_human_fp_range_2020",     
                         "percent_change_hf_2010_2020",     
                         "inferred_range_sqkm",     
                         "date_data_obtained",     
                         "filled_by"), check.names=TRUE)

unlink(infile3)

Frug_mammal2 <- col_class(Frug_mammal)


# retrieve file Frugivoria: Bird database simple
#file 4
inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/1220/6/5a86fde71322a1ff64d94ace0ed1982c" 
infile4 <- tempfile()
download.file(inUrl4, infile4, method = "libcurl")
#try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")

Frug_bird <-read.csv(infile4,header=F 
                     ,skip=1
                     ,sep=","  
                     ,quot='"' 
                     , col.names=c(
                       "IUCN_species_name",     
                       "common_name",     
                       "family_e",     
                       "genus",     
                       "species",     
                       "elton_species_name",     
                       "diet_cat_e",     
                       "diet_source_e",     
                       "diet_breadth",     
                       "diet_level",     
                       "activity_nocturnal_e",     
                       "for_strat_ground_e",     
                       "for_strat_understory_e",     
                       "for_strat_midhigh_e",     
                       "for_strat_canopy_e",     
                       "for_strat_aerial_e",     
                       "for_strat_spec_level",     
                       "for_strat_source_e",     
                       "body_mass_e",     
                       "body_mass_level_e",     
                       "body_mass_source_e",     
                       "body_size_mm",     
                       "body_size_notes",     
                       "body_size_level",     
                       "body_size_source",     
                       "sexual_dimorphism",     
                       "sexual_dimorphism_notes",     
                       "sexual_dimorphism_level",     
                       "sexual_dimorphism_source",     
                       "longevity",     
                       "longevity_notes",     
                       "longevity_level",     
                       "longevity_source",     
                       "home_range_size",     
                       "home_range_notes",     
                       "home_range_level",     
                       "home_range_source",     
                       "habitat_specialization",     
                       "habitat_specialization_source",     
                       "generation_time",     
                       "generation_time_notes",     
                       "generation_time_level",     
                       "generation_time_source",     
                       "IUCN_category",     
                       "habitat",     
                       "habitat_breadth",     
                       "habitat_breadth_source",     
                       "habitat_level",     
                       "mean_CHELSA_bio1_1981.2010_V.2.1",     
                       "mean_CHELSA_bio12_1981.2010_V.2.1",     
                       "mean_human_fp_range_2010",     
                       "mean_human_fp_range_2020",     
                       "percent_change_hf_2010_2020",     
                       "inferred_range_sqkm",     
                       "date_data_obtained",     
                       "filled_by"    ), check.names=TRUE)

unlink(infile4)

Frug_bird2 <- col_class(Frug_bird)


# retrieve file Lookup table: Mammals exhibiting frugivory
inUrl10  <- "https://pasta.lternet.edu/package/data/eml/edi/1220/6/f73cd1b16a4ed908dac4cf532690a12b" 
infile10 <- tempfile()
download.file(inUrl10, infile10, method = "libcurl")
#try(download.file(inUrl10,infile10,method="curl"))
if (is.na(file.size(infile10))) download.file(inUrl10,infile10,method="auto")


Frug_mammal_LookupTable <-read.csv(infile10,header=F 
                                   ,skip=1
                                   ,sep=","  
                                   ,quot='"' 
                                   , col.names=c(
                                     "IUCN_species_name",     
                                     "elton_species_name",     
                                     "PanTHERIA_species_name",     
                                     "habitat",     
                                     "new_species",     
                                     "reclassified",     
                                     "spelling_differences"    ), check.names=TRUE)

unlink(infile10)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
Frug_mammal_LookupTable2 <- col_class(Frug_mammal_LookupTable)


# retrieve file Lookup table: Birds exhibiting frugivory
inUrl11  <- "https://pasta.lternet.edu/package/data/eml/edi/1220/6/d1f32cc55b692cfb3d614cb400797299" 
infile11 <- tempfile()
download.file(inUrl11, infile11, method = "libcurl")
#try(download.file(inUrl11,infile11,method="curl"))
if (is.na(file.size(infile11))) download.file(inUrl11,infile11,method="auto")


Frug_bird_LookupTable <-read.csv(infile11,header=F 
                                 ,skip=1
                                 ,sep=","  
                                 ,quot='"' 
                                 , col.names=c(
                                   "IUCN_species_name",     
                                   "elton_species_name",     
                                   "habitat",     
                                   "new_species",     
                                   "reclassified",     
                                   "spelling_difference"    ), check.names=TRUE)

unlink(infile11)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
Frug_bird_LookupTable2 <- col_class(Frug_bird_LookupTable)


# combine bird and mammal databases

#rename columns to match
Frug_bird2 <- Frug_bird2 %>% rename(family = family_e, diet_cat = diet_cat_e)
#remove columns that don't match
Frug_bird2 <- Frug_bird2 %>% select(-one_of("for_strat_ground_e", "for_strat_understory_e", "for_strat_midhigh_e", "for_strat_canopy_e", "for_strat_aerial_e", "for_strat_spec_level", "for_strat_source_e")) 
Frug_mammal2 <- Frug_mammal2 %>% select(-one_of("activity_source_e", "activity_level","for_strat_value_e", "for_strat_certainty_e", "subspecies"))

Frugivoria <- rbind(Frug_mammal2, Frug_bird2)

Frug_bird_LookupTable2 <- Frug_bird_LookupTable2[,1:2]
Frug_mammal_LookupTable2 <- Frug_mammal_LookupTable2[,1:3]
Frugivore_LookupTable <- merge(Frug_bird_LookupTable2, Frug_mammal_LookupTable2, all = TRUE)


# summary
data_summary(Frugivoria, Frugivoria$IUCN_species_name, Frugivoria$genus, Frugivoria$family)
data_summary(Frug_mammal2, Frug_mammal2$IUCN_species_name, Frug_mammal2$genus, Frug_mammal2$family)
data_summary(Frug_bird2, Frug_bird2$IUCN_species_name, Frug_bird2$genus, Frug_bird2$family)

Frugivoria_SpeciesList <- unique(Frugivoria$IUCN_species_name)
length(Frugivoria_SpeciesList)


# write data to csv
write.csv(Frugivoria, file = file.path(output_path_L0,"TropicalAndes_Frugivoria_frugivore_traits.csv"))
write.csv(Frugivoria_SpeciesList, file = file.path(output_path_L0,"TropicalAndes_Frugivoria_frugivore_traits_species.csv"))
write.csv(Frugivore_LookupTable, file = file.path(output_path_L1,"TropicalAndes_frugivore_LookupTable.csv"))

# also save mammal and bird data separately
write.csv(Frug_mammal2, file = file.path(output_path_L0,"TropicalAndes_Frugivoria_mammal_traits.csv"))
write.csv(Frug_mammal_LookupTable2, file = file.path(output_path_L1,"TropicalAndes_mammal_LookupTable.csv"))

write.csv(Frug_bird2, file = file.path(output_path_L0,"TropicalAndes_Frugivoria_bird_traits.csv"))
write.csv(Frug_bird_LookupTable2, file = file.path(output_path_L1,"TropicalAndes_bird_LookupTable.csv"))

# package citations and session info
library(report)
report::cite_packages()

devtools::session_info()
