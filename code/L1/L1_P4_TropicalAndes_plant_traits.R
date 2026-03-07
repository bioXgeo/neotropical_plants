#title: "Plant trait cleaning and combining"
# author: "Hazel J. Anderson, Jenna B. Baljunas"
# project: "Plant-Frugivore Diversity"
# collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
# overview: "This script subsets plant trait data by synonyms species list and combine."
# data input: "TropicalAndes_all_plant_traits_harmonized_subset.csv"
# data output: "TropicalAndes_all_plant_traits_standardized.csv", "TropicalAndes_all_plant_traits_standardized_with_recordCount.csv", "TropicalAndes_all_plant_traits_cleaned_unaveraged.csv"
# date: "2023-11-04; 2025-10-15"

  
# load required packages
library(dplyr); library(tidyr); library(purrr); library(rstatix); library(funbiogeo); library(visdat); library(ggplot2)


# set file paths
data_path_L1 <-file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
output_path_L1 <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L1')
figure_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/figures')


# load functions
source("C:/GitHub_projects/plant-frugivore diversity/neotropical_plants/code/Functions.R")


# read in subset fruiting species data
plant_traits <- read.csv(file.path(data_path_L1, "TropicalAndes_all_plant_traits_harmonized_subset.csv"))


# remove X column
plant_traits <- plant_traits[, !colnames(plant_traits) %in% "X", drop = FALSE]


# convert all character columns to UTF-8 encoding to handle encoding issues
plant_traits <- plant_traits %>%
  mutate(across(where(is.character), ~ iconv(., from = "", to = "UTF-8", sub = "byte")))


# look at trait numbers
plant_traits %>%
  count(TraitName)


plant_traits %>%
  distinct(Accepted_species, TraitName) %>%
  count(TraitName)


#### dispersal syndrome ####
traits_dispersal <- plant_traits %>%
  filter(grepl('Dispersal syndrome|Dispersal_syndrome_1|Dispersal_syndrome_2|whole plant dispersal syndrome', TraitName))

traits_dispersal$OrigTraitValue <- traits_dispersal$TraitValue

nrow(traits_dispersal)
length(unique(traits_dispersal$Accepted_species))

# remove rows containing weight or mass
traits_dispersal <- traits_dispersal[!grepl("weight|mass|air-dry|oven|mean|SE",traits_dispersal$TraitValue, ignore.case = TRUE),]
nrow(traits_dispersal)
length(unique(traits_dispersal$Accepted_species))


# remove rows where TraitValue is numeric
traits_dispersal <- traits_dispersal %>%
  filter(is.na(as.numeric(TraitValue)))
nrow(traits_dispersal)
length(unique(traits_dispersal$Accepted_species))


traits_dispersal %>%
  distinct(TraitValue)


# Reclassify to the following:
# anemochorous - wind dispersal
# anthropochorous - human dispersal
# autochorous - non-aided dispersal
# hydrochorous - water dispersal
# myrmecochorous - ant dispersal
# zoochorous - animal dispersal
# unspecialized - multiple


traits_dispersal <- traits_dispersal %>%
  filter() %>%
  mutate(TraitValue= case_when(
    grepl('Eaten|Animal|Bird|Mammal|animals|endozoochor|dysochor|epizoochor|zoochor|Vertebrates|Reptiles+birds|Endo-zoochory|mammals|exozoochory|birds|Zoochory|vertebrate|Dys-zoochory|Ornithochory|Epi-zoochory|endozoochorous|endozoochory|ectozoochorous|zoochorous', OrigTraitValue, ignore.case = TRUE) ~ "zoochorous",
    grepl('Ants', OrigTraitValue, ignore.case = TRUE) ~ "myrmecochorous",
    grepl('wind|Anemochory|anemochor|meteorochor|chamaechor|Anemochory|anemochorous', OrigTraitValue, ignore.case = TRUE) ~ "anemochorous",
    grepl('human|agochor|man|hemerochor|anthropochorous', OrigTraitValue, ignore.case = TRUE) ~ "anthropochorous",
    grepl('No|Unassisted|autochor|blastochor|ombrochor|Autochory|Barochory|ballochor|herpochor|autochorous', OrigTraitValue, ignore.case = TRUE) ~ "autochorous",
    grepl('water|nautochor|floating|hydrochor|hydrochorous', OrigTraitValue, ignore.case = TRUE) ~ "hydrochorous",
    grepl('boleochor|combination|non specialized|unspecialized', OrigTraitValue, ignore.case = TRUE) ~ "unspecialized"
  )) %>%
  filter(!is.na(TraitValue))
nrow(traits_dispersal)
length(unique(traits_dispersal$Accepted_species))


traits_dispersal %>%
  distinct(TraitValue)


# standardize trait names
traits_dispersal <- traits_dispersal %>%
  mutate(TraitName = "DispersalSyndrome")

summarize_species_records(traits_dispersal)

averagetraits_dispersal <- factor_data_merge(traits_dispersal, "DispersalSyndrome")

nrow(averagetraits_dispersal)
length(unique(averagetraits_dispersal$Accepted_species))

averagetraits_dispersal %>%
  count(TraitValue)

summary(averagetraits_dispersal)


#### fruit dry mass ####
traits_fruitMass <- plant_traits %>%
  filter(grepl('Fruit dry mass', TraitName))

traits_fruitMass$OrigTraitValue <- traits_fruitMass$TraitValue

nrow(traits_fruitMass)
length(unique(traits_fruitMass$Accepted_species))

# remove non-numeric rows
traits_fruitMass <- traits_fruitMass %>%
  filter(!is.na(as.numeric(TraitValue))) %>%
  mutate(TraitValue = as.numeric(TraitValue))

nrow(traits_fruitMass)
length(unique(traits_fruitMass$Accepted_species))

# check that units match
print(unique(traits_fruitMass$Unit))

# standardize trait names with units
traits_fruitMass <- traits_fruitMass %>%
  mutate(TraitName = "FruitMass_mg")

dotchart(traits_fruitMass$TraitValue)

# detect outliers
fruitMass_outliers <- detect_outliers(traits_fruitMass)
print(fruitMass_outliers)

fruitMass_outliers_species <- species_with_outliers(fruitMass_outliers)
print(fruitMass_outliers_species)
count(fruitMass_outliers_species)

summarize_species_records(traits_fruitMass)

averagetraits_fruitMass <- numeric_data_average(traits_fruitMass, "mg", "FruitMass_mg")

nrow(averagetraits_fruitMass)
length(unique(averagetraits_fruitMass$Accepted_species))

summary(averagetraits_fruitMass)


#### fruit length ####
traits_fruitLength <- plant_traits %>%
  filter(grepl('Fruit length|maximum fruit length|minimum fruit length|Fruit_length_min|Fruit_length_max|Fruit_length_mean', TraitName))
traits_fruitLength$OrigTraitValue <- traits_fruitLength$TraitValue

nrow(traits_fruitLength)
length(unique(traits_fruitLength$Accepted_species))

# remove rows if TraitValue is non-numeric
traits_fruitLength <- traits_fruitLength %>%
  filter(!is.na(as.numeric(TraitValue))) %>%
  mutate(TraitValue = as.numeric(TraitValue))

nrow(traits_fruitLength)
length(unique(traits_fruitLength$Accepted_species))

# check that units match
print(unique(traits_fruitLength$Unit))

# if units = cm, multiply trait value by 10 and change units to mm
traits_fruitLength <- traits_fruitLength %>%
  mutate(TraitValue = if_else(Unit == "cm", TraitValue * 10, TraitValue),
         Unit = if_else(Unit == "cm", "mm", Unit))

# standardize trait names with units
traits_fruitLength <- traits_fruitLength %>%
  mutate(TraitName = "FruitLength_mm")

dotchart(traits_fruitLength$TraitValue)

# detect outliers
fruitLength_outliers <- detect_outliers(traits_fruitLength)
print(fruitLength_outliers)

fruitLength_outliers_species <- species_with_outliers(fruitLength_outliers)
print(fruitLength_outliers_species)
count(fruitLength_outliers_species)

summarize_species_records(traits_fruitLength)

averagetraits_fruitLength <- numeric_data_average(traits_fruitLength, "mm", "FruitLength_mm")

nrow(averagetraits_fruitLength)
length(unique(averagetraits_fruitLength$Accepted_species))

summary(averagetraits_fruitLength)


#### fruit type ####
traits_fruitType <- plant_traits %>%
  filter(grepl('Fruit type|Fruit_type_1|fruit type', TraitName))
traits_fruitType$OrigTraitValue <- traits_fruitType$TraitValue

nrow(traits_fruitType)
length(unique(traits_fruitType$Accepted_species))

# remove numeric values
traits_fruitType <- traits_fruitType %>%
  filter(is.na(as.numeric(TraitValue)))

nrow(traits_fruitType)
length(unique(traits_fruitType$Accepted_species))

traits_fruitType %>%
  count(TraitValue)

# standardize trait values
traits_fruitType <- traits_fruitType %>%
  filter() %>%
  mutate(TraitValue= case_when(
    grepl('Berry|berry|Berry ', OrigTraitValue, ignore.case = TRUE) ~ "berry",
    grepl('capsule|Capsule|Capsule |Pixidium|Dehiscent capsule|Indehiscent capsule', OrigTraitValue, ignore.case = TRUE) ~ "capsule",
    grepl('drupe|Drupe|Drupe |Drupaceous', OrigTraitValue, ignore.case = TRUE) ~ "drupe",
    grepl('aggregate berries', OrigTraitValue, ignore.case = TRUE) ~ "aggregate berries",
    grepl('aggregate drupelets', OrigTraitValue, ignore.case = TRUE) ~ "aggregate drupelets",
    grepl('fleshy', OrigTraitValue, ignore.case = TRUE) ~ "fleshy",
    grepl('pome', OrigTraitValue, ignore.case = TRUE) ~ "pome",
    grepl('Squash', OrigTraitValue, ignore.case = TRUE) ~ "squash",
    grepl('aggregate follicles', OrigTraitValue, ignore.case = TRUE) ~ "aggregate follicles",
    grepl('aggregate nutlets', OrigTraitValue, ignore.case = TRUE) ~ "aggregate nutlets",
    grepl('achene', OrigTraitValue, ignore.case = TRUE) ~ "achene",
    grepl('follicle|Follicle ', OrigTraitValue, ignore.case = TRUE) ~ "follicle",
    grepl('legume|Legume ', OrigTraitValue, ignore.case = TRUE) ~ "legume",
    grepl('lomentum', OrigTraitValue, ignore.case = TRUE) ~ "lomentum",
    grepl('nut', OrigTraitValue, ignore.case = TRUE) ~ "nut",
    grepl('pod|Pod|Pod ', OrigTraitValue, ignore.case = TRUE) ~ "pod",
    grepl('Samara|Samaroid', OrigTraitValue, ignore.case = TRUE) ~ "samara",
    grepl('schizocarp', OrigTraitValue, ignore.case = TRUE) ~ "schizocarp",
    grepl('siliqua', OrigTraitValue, ignore.case = TRUE) ~ "siliqua",
    grepl('syncarpous', OrigTraitValue, ignore.case = TRUE) ~ "syncarpous",
    grepl('utricle', OrigTraitValue, ignore.case = TRUE) ~ "utricle",
    grepl('other|apocarpous|pericarp, aril, arillode|pseudosyncarpous|vegetative|dry|fleshy', OrigTraitValue, ignore.case = TRUE) ~ "unspecified",
  )) %>%
  filter(!is.na(TraitValue))

traits_fruitType %>%
  count(TraitValue)

# standardize trait name
traits_fruitType <- traits_fruitType %>%
  mutate(TraitName = "FruitType")

summarize_species_records(traits_fruitType)

averagetraits_fruitType <- factor_data_merge(traits_fruitType, "FruitType")

nrow(averagetraits_fruitType)
length(unique(averagetraits_fruitType$Accepted_species))

summary(averagetraits_fruitType)


#### fruit color ####
traits_fruitColor <- plant_traits %>%
  filter(grepl('Fruit/seed color|Fruit_colour', TraitName))
traits_fruitColor$OrigTraitValue <- traits_fruitColor$TraitValue

nrow(traits_fruitColor)
length(unique(traits_fruitColor$Accepted_species))

# remove numeric values
traits_fruitColor <- traits_fruitColor %>%
  filter(is.na(as.numeric(TraitValue)))

nrow(traits_fruitColor)
length(unique(traits_fruitColor$Accepted_species))

traits_fruitColor %>%
  count(TraitValue)

# standardize trait values
traits_fruitColor <- traits_fruitColor %>%
  filter() %>%
  mutate(TraitValue= case_when(
    grepl('yellow green|yellowish green|fruit green|dispersed by black lemur', OrigTraitValue, ignore.case = TRUE) ~ 'green',
    grepl('brown to black|maturing black|blueish black|ripening black|purplish black|shiny black|black at maturity|purple-black|pink to black', OrigTraitValue, ignore.case = TRUE) ~ 'black',
    grepl('reddish brown|reddish-brown|deep chestnut|yellow brown|yellowish brown', OrigTraitValue, ignore.case = TRUE) ~ 'brown',
    grepl('becoming dull orange|yellow to orange|yellowish-orange', OrigTraitValue, ignore.case = TRUE) ~ 'orange',
    grepl('crimson|red at maturity|bright red|bright red when ripe|scarlet|brown/red|dark red|green to red', OrigTraitValue, ignore.case = TRUE) ~ 'red',
    grepl('beige|yellowish white|dark yellow|green to yellow', OrigTraitValue, ignore.case = TRUE) ~ 'yellow',
    grepl('Grey', OrigTraitValue, ignore.case = TRUE) ~ 'grey',
    grepl('Black', OrigTraitValue, ignore.case = TRUE) ~ 'black',
    grepl('Blue', OrigTraitValue, ignore.case = TRUE) ~ 'blue',
    grepl('Brown', OrigTraitValue, ignore.case = TRUE) ~ 'brown',
    grepl('Green', OrigTraitValue, ignore.case = TRUE) ~ 'green',
    grepl('Orange', OrigTraitValue, ignore.case = TRUE) ~ 'orange',
    grepl('Purple', OrigTraitValue, ignore.case = TRUE) ~ 'purple',
    grepl('Red', OrigTraitValue, ignore.case = TRUE) ~ 'red',
    grepl('White', OrigTraitValue, ignore.case = TRUE) ~ 'white',
    grepl('Yellow', OrigTraitValue, ignore.case = TRUE) ~ 'yellow',
    ))%>%
  filter(!is.na(TraitValue))

traits_fruitColor %>%
  count(TraitValue)

# standardize trait name
traits_fruitColor <- traits_fruitColor %>%
  mutate(TraitName = "FruitColor")

summarize_species_records(traits_fruitColor)

averagetraits_fruitColor <- factor_data_merge(traits_fruitColor, "FruitColor")

nrow(averagetraits_fruitColor)
length(unique(averagetraits_fruitColor$Accepted_species))

summary(averagetraits_fruitColor)


#### fruit conspicuousness ####
traits_fruitConspicuousness <- plant_traits %>%
  filter(grepl('Fruit/seed conspicuous', TraitName))
traits_fruitConspicuousness$OrigTraitValue <- traits_fruitConspicuousness$TraitValue

nrow(traits_fruitConspicuousness)
length(unique(traits_fruitConspicuousness$Accepted_species))

# remove numeric values
traits_fruitConspicuousness <- traits_fruitConspicuousness %>%
  filter(is.na(as.numeric(TraitValue)))

nrow(traits_fruitConspicuousness)
length(unique(traits_fruitConspicuousness$Accepted_species))

traits_fruitConspicuousness %>%
  count(TraitValue)

# standardize trait values
traits_fruitConspicuousness <- traits_fruitConspicuousness %>%
  filter() %>%
mutate(TraitValue= case_when(
  grepl('Yes|conspicuous', OrigTraitValue, ignore.case = TRUE) ~ "conspicuous",
  grepl('No|cryptic', OrigTraitValue, ignore.case = TRUE) ~ "cryptic"))%>%
  filter(!is.na(TraitValue))

traits_fruitConspicuousness %>%
  count(TraitValue)

# standardize trait name
traits_fruitConspicuousness <- traits_fruitConspicuousness %>%
  mutate(TraitName = "FruitConspicuousness")

summarize_species_records(traits_fruitConspicuousness)

averagetraits_fruitConspicuousness <- factor_data_merge(traits_fruitConspicuousness, "FruitConspicuousness")

nrow(averagetraits_fruitConspicuousness)
length(unique(averagetraits_fruitConspicuousness$Accepted_species))

summary(averagetraits_fruitConspicuousness)


#### fruit dryness ####
traits_fruitDryness <- plant_traits %>%
  filter(grepl('Fruit_dryness_1', TraitName))
traits_fruitDryness$OrigTraitValue <- traits_fruitDryness$TraitValue

nrow(traits_fruitDryness)
length(unique(traits_fruitDryness$Accepted_species))

# remove numeric values
traits_fruitDryness <- traits_fruitDryness %>%
  filter(is.na(as.numeric(TraitValue)))

nrow(traits_fruitDryness)
length(unique(traits_fruitDryness$Accepted_species))

traits_fruitDryness %>%
  count(TraitValue)

# standardize trait name
traits_fruitDryness <- traits_fruitDryness %>%
  mutate(TraitName = "FruitDryness")

summarize_species_records(traits_fruitDryness)

averagetraits_fruitDryness <- factor_data_merge(traits_fruitDryness, "FruitDryness")

nrow(averagetraits_fruitDryness)
length(unique(averagetraits_fruitDryness$Accepted_species))

summary(averagetraits_fruitDryness)


#### fruiting duration ####
# a little more variable and complicated, with less obs, so might leave out
traits_fruitingDuration <- plant_traits %>%
  filter(grepl('Fruiting_end|Fruiting_start|plant fruiting duration', TraitName))
traits_fruitingDuration$OrigTraitValue <- traits_fruitingDuration$TraitValue

nrow(traits_fruitingDuration)
length(unique(traits_fruitingDuration$Accepted_species))


#### growth form ####
# Just using data from GIFT 
traits_growthForm <- plant_traits %>%
  filter(grepl('Growth_form_1', TraitName))
traits_growthForm$OrigTraitValue <- traits_growthForm$TraitValue

nrow(traits_growthForm)
length(unique(traits_growthForm$Accepted_species))

# remove numeric rows
traits_growthForm <- traits_growthForm %>%
  filter(is.na(as.numeric(TraitValue)))

nrow(traits_growthForm)
length(unique(traits_growthForm$Accepted_species))

# look at unique trait values
traits_growthForm %>%
  distinct(TraitValue)

# standardize trait names with units
traits_growthForm <- traits_growthForm %>%
  mutate(TraitName = "GrowthForm")

summarize_species_records(traits_growthForm)

averagetraits_growthForm <- factor_data_merge(traits_growthForm, "GrowthForm")

nrow(averagetraits_growthForm)
length(unique(averagetraits_growthForm$Accepted_species))

summary(averagetraits_growthForm)


#### plant lifespan/longevity ####
traits_plantLifespan <- plant_traits %>%
  filter(grepl('Lifespan_1|Plant lifespan \\(longevity\\)|longest whole plant longevity|maximum whole plant longevity', TraitName))
traits_plantLifespan$OrigTraitValue <- traits_plantLifespan$TraitValue

nrow(traits_plantLifespan)
length(unique(traits_plantLifespan$Accepted_species))

# remove rows if TraitValue is non-numeric
traits_plantLifespan <- traits_plantLifespan %>%
  filter(!is.na(as.numeric(TraitValue))) %>%
  mutate(TraitValue = as.numeric(TraitValue))

nrow(traits_plantLifespan)
length(unique(traits_plantLifespan$Accepted_species))

# check that units match
print(unique(traits_plantLifespan$Unit))
traits_plantLifespan$Unit <- "years"
print(unique(traits_plantLifespan$Unit))

# standardize trait names with units
traits_plantLifespan <- traits_plantLifespan %>%
  mutate(TraitName = "PlantLifespan_years")

dotchart(traits_plantLifespan$TraitValue)

# detect outliers
plantLifespan_outliers <- detect_outliers(traits_plantLifespan)
print(plantLifespan_outliers)

plantLifespan_outliers_species <- species_with_outliers(plantLifespan_outliers)
print(plantLifespan_outliers_species)
count(plantLifespan_outliers_species)

summarize_species_records(traits_plantLifespan)

averagetraits_plantLifespan <- numeric_data_average(traits_plantLifespan, "years", "PlantLifespan_years")

nrow(averagetraits_plantLifespan)
length(unique(averagetraits_plantLifespan$Accepted_species))

summary(averagetraits_plantLifespan)


#### plant height ####
traits_plantHeight <- plant_traits %>%
  filter(grepl('maximum whole plant height|minimum whole plant height|whole plant height|Plant height generative|Plant height vegetative|plant_height_mean|plant_height_max|plant_height_min', TraitName))

traits_plantHeight$OrigTraitValue <- traits_plantHeight$TraitValue

nrow(traits_plantHeight)
length(unique(traits_plantHeight$Accepted_species))

# remove non-numeric rows
traits_plantHeight <- traits_plantHeight %>%
  filter(!is.na(as.numeric(TraitValue))) %>%
  mutate(TraitValue = as.numeric(TraitValue))

nrow(traits_plantHeight)
length(unique(traits_plantHeight$Accepted_species))

print(unique(traits_plantHeight$Unit))

# standardize trait names with units
traits_plantHeight <- traits_plantHeight %>%
  mutate(TraitName = "PlantHeight_m")

# takes really long time:
dotchart(traits_plantHeight$TraitValue)

# detect outliers
plantHeight_outliers <- detect_outliers(traits_plantHeight)
print(plantHeight_outliers)

plantHeight_outliers_species <- species_with_outliers(plantHeight_outliers)
print(plantHeight_outliers_species)
count(plantHeight_outliers_species)

summarize_species_records(traits_plantHeight)

averagetraits_plantHeight <- numeric_data_average(traits_plantHeight, "m", "PlantHeight_m")

nrow(averagetraits_plantHeight)
length(unique(averagetraits_plantHeight$Accepted_species))

summary(averagetraits_plantHeight)


#### seed mass ####
traits_seedMass <- plant_traits %>%
  filter(grepl('Seed dry mass|seed_mass_max|seed_mass_mean|seed_mass_min|seed mass', TraitName))
traits_seedMass$OrigTraitValue <- traits_seedMass$TraitValue

nrow(traits_seedMass)
length(unique(traits_seedMass$Accepted_species))

# remove non-numeric rows
traits_seedMass <- traits_seedMass %>%
  filter(!is.na(as.numeric(TraitValue))) %>%
  mutate(TraitValue = as.numeric(TraitValue))

nrow(traits_seedMass)
length(unique(traits_seedMass$Accepted_species))

print(unique(traits_seedMass$Unit))

# convert rows with unit = g to mg; multiply by 1000
traits_seedMass <- traits_seedMass %>%
  mutate(TraitValue = if_else(Unit == "g", TraitValue * 1000, TraitValue),
         Unit = if_else(Unit == "g", "mg", Unit))

# standardize trait names with units
traits_seedMass <- traits_seedMass %>%
  mutate(TraitName = "SeedMass_g")

dotchart(traits_seedMass$TraitValue)

# detect outliers
seedMass_outliers <- detect_outliers(traits_seedMass)
print(seedMass_outliers)

seedMass_outliers_species <- species_with_outliers(seedMass_outliers)
print(seedMass_outliers_species)
count(seedMass_outliers_species)

summarize_species_records(traits_seedMass)

averagetraits_seedMass <- numeric_data_average(traits_seedMass, "mg", "SeedMass_g")

nrow(averagetraits_seedMass)
length(unique(averagetraits_seedMass$Accepted_species))

summary(averagetraits_seedMass)


#### seed length ####
traits_seedLength <- plant_traits %>%
  filter(grepl('Seed length|seed_length_max|seed_length_mean|seed_length_min', TraitName))
traits_seedLength$OrigTraitValue <- traits_seedLength$TraitValue

nrow(traits_seedLength)
length(unique(traits_seedLength$Accepted_species))

# remove non-numeric rows
traits_seedLength <- traits_seedLength %>%
  filter(!is.na(as.numeric(TraitValue))) %>%
  mutate(TraitValue = as.numeric(TraitValue))

nrow(traits_seedLength)
length(unique(traits_seedLength$Accepted_species))

print(unique(traits_seedLength$Unit))

# standardize trait names with units
traits_seedLength <- traits_seedLength %>%
  mutate(TraitName = "SeedLength_mm")

dotchart(traits_seedLength$TraitValue)

# detect outliers
seedLength_outliers <- detect_outliers(traits_seedLength)
print(seedLength_outliers)

seedLength_outliers_species <- species_with_outliers(seedLength_outliers)
print(seedLength_outliers_species)
count(seedLength_outliers_species)

summarize_species_records(traits_seedLength)

averagetraits_seedLength <- numeric_data_average(traits_seedLength, "mm", "SeedLength_mm")

nrow(averagetraits_seedLength)
length(unique(averagetraits_seedLength$Accepted_species))

summary(averagetraits_seedLength)


#### seed width ####
traits_seedWidth <- plant_traits %>%
  filter(grepl('Seed width|seed_width_max|seed_width_mean|seed_width_min'
               , TraitName))
traits_seedWidth$OrigTraitValue <- traits_seedWidth$TraitValue

nrow(traits_seedWidth)
length(unique(traits_seedWidth$Accepted_species))

# remove non-numeric rows
traits_seedWidth <- traits_seedWidth %>%
  filter(!is.na(as.numeric(TraitValue))) %>%
  mutate(TraitValue = as.numeric(TraitValue))

nrow(traits_seedWidth)
length(unique(traits_seedWidth$Accepted_species))

print(unique(traits_seedWidth$Unit))

# standardize trait names with units
traits_seedWidth <- traits_seedWidth %>%
  mutate(TraitName = "SeedWidth_mm")

dotchart(traits_seedWidth$TraitValue)

# detect outliers
seedWidth_outliers <- detect_outliers(traits_seedWidth)
print(seedWidth_outliers)

seedWidth_outliers_species <- species_with_outliers(seedWidth_outliers)
print(seedWidth_outliers_species)
count(seedWidth_outliers_species)

summarize_species_records(traits_seedWidth)

averagetraits_seedWidth <- numeric_data_average(traits_seedWidth, "mm", "SeedWidth_mm")

nrow(averagetraits_seedWidth)
length(unique(averagetraits_seedWidth$Accepted_species))

summary(averagetraits_seedWidth)


# combine all traits

# for summary
unaveraged_traits <- rbind(traits_dispersal, traits_fruitColor, traits_fruitConspicuousness, traits_fruitDryness, traits_fruitLength, traits_fruitMass, traits_fruitType, traits_growthForm, traits_plantHeight, traits_plantLifespan, traits_seedLength, traits_seedMass, traits_seedWidth)

trait_dfs <- list(averagetraits_dispersal, averagetraits_fruitColor, averagetraits_fruitConspicuousness, averagetraits_fruitDryness, averagetraits_fruitLength, averagetraits_fruitMass, averagetraits_fruitType, averagetraits_growthForm, averagetraits_plantHeight, averagetraits_plantLifespan, averagetraits_seedLength, averagetraits_seedMass, averagetraits_seedWidth)


# clean traits
clean_traits <- combine_matching_columns(trait_dfs)
nrow(clean_traits)


# transform plant dataframe from long to wide
wide_plant_traits <- clean_traits %>%
  select(Accepted_species, TraitName, TraitValue) %>%
  pivot_wider(names_from = TraitName, values_from = TraitValue)
nrow(wide_plant_traits)


# assign data types
wide_plant_traits$`DispersalSyndrome` <- as.factor(wide_plant_traits$`DispersalSyndrome`)
wide_plant_traits$`FruitColor` <- as.factor(wide_plant_traits$`FruitColor`)
wide_plant_traits$`FruitConspicuousness` <- as.factor(wide_plant_traits$`FruitConspicuousness`)
wide_plant_traits$`FruitDryness` <- as.factor(wide_plant_traits$`FruitDryness`)
wide_plant_traits$`FruitLength_mm` <- as.numeric(wide_plant_traits$`FruitLength_mm`)
wide_plant_traits$`FruitMass_mg` <- as.numeric(wide_plant_traits$`FruitMass_mg`)
wide_plant_traits$`FruitType` <- as.factor(wide_plant_traits$`FruitType`)
wide_plant_traits$`GrowthForm` <- as.factor(wide_plant_traits$`GrowthForm`)
wide_plant_traits$`PlantHeight_m` <- as.numeric(wide_plant_traits$`PlantHeight_m`)
wide_plant_traits$`PlantLifespan_years` <- as.numeric(wide_plant_traits$`PlantLifespan_years`)
wide_plant_traits$`SeedLength_mm` <- as.numeric(wide_plant_traits$`SeedLength_mm`)
wide_plant_traits$`SeedMass_g` <- as.numeric(wide_plant_traits$`SeedMass_g`)
wide_plant_traits$`SeedWidth_mm` <- as.numeric(wide_plant_traits$`SeedWidth_mm`)


# rename accepted_species column to species
wide_plant_traits <- wide_plant_traits %>%
  rename(species = Accepted_species)
write.csv(wide_plant_traits, file.path(output_path_L1,"TropicalAndes_wide_traits_after_standardization.csv"))


# summary
cat("Number of records:", nrow(clean_traits), "\n")
cat("Number of species:", length(unique(clean_traits$Accepted_species)), "\n")

fb_plot_species_traits_completeness(wide_plant_traits)
ggsave("plant_trait_completeness.png", plot = last_plot(), path = figure_path)

trait_props <- fb_plot_number_species_by_trait(wide_plant_traits)
ggsave("plant_number_species_trait.png", plot = last_plot(), path = figure_path)

fb_table_trait_summary(wide_plant_traits)

vis_dat(wide_plant_traits)
ggsave("plant_trait_visual_type.png", plot = last_plot(), path = figure_path)

vis_miss(wide_plant_traits)
ggsave("plant_trait_visual_missing.png", plot = last_plot(), path = figure_path)


# count the number of traits per DatabaseSource and TraitName
trait_counts <- unaveraged_traits %>%
  count(DatabaseSource, TraitName)


# customize fill color of database
custom_colors <- c("BIEN" = "#a6cf3c" , "GIFT" = "#009900", "TRY" = "#40a353")


# create the faceted bar plot
ggplot(trait_counts, aes(x = DatabaseSource, y = n, fill = DatabaseSource)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Trait counts per database",
       x = "Trait name",
       y = "Trait count",
       fill = "Database Source") +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size=20),
        axis.text = element_text(size=12),
        strip.text = element_text(size=14),
        axis.title = element_text(size=18)) +
  facet_wrap(~ TraitName, scales = "free_y", nrow = 2)
ggsave("plant_trait_counts_per_database.png", plot = last_plot(), path = figure_path, width = 18, height = 8)


# write data to csv
write.csv(wide_plant_traits, file.path(output_path_L1,"TropicalAndes_all_plant_traits_standardized.csv"))
write.csv(clean_traits, file.path(output_path_L1,"TropicalAndes_all_plant_traits_standardized_with_recordCount.csv"))
write.csv(unaveraged_traits, file.path(output_path_L1,"TropicalAndes_all_plant_traits_cleaned_unaveraged.csv"))


# package citations and session info
library(report)
report::cite_packages()

devtools::session_info()
