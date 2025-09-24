# title: "Tropical Andes plant trait data GIFT"
# author: "Hazel J. Anderson, Jenna B. Baljunas"
# project: "Plant-Frugivore Diversity"
# collaborators: "Beth E. Gerstner, Phoebe L. Zarnetske"
# overview: "This script retrieves plant trait data from the GIFT database for plant species list."
# data input: "none"
# data output: "TropicalAndes_GIFT_plant_traits.csv"
# date: "2025-09-22"


# set file paths
data_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')
output_path <- file.path('G:/Shared drives/SpaCE_Lab_FRUGIVORIA/data/plants/L0')


# load required packages
library(GIFT); library(dplyr)


# retrieve GIFT data

# data downloaded 2025-09-22, done by running this through
all_traits <- GIFT_traits_meta()

# list all the traits of interest here:
ID <- c('1.6.3','1.6.1','1.6.2','2.2.1','3.3.1','3.3.2','3.16.1','3.18.1','3.13.3','3.13.1','3.13.2','3.2.3','3.2.1','3.2.2','3.10.3','3.10.1','3.10.2','3.11.3','3.11.1','3.11.2','1.2.1','3.22.1','3.8.1','3.8.2')

traits <- lapply(ID,function(x){
  # extract row of each ID from entire list of traits 
  desired_trait <- all_traits[all_traits$Lvl3 == x,]
  
  # import GIFT data for each trait
  GIFT_data <- GIFT_traits(trait_IDs=x)
  
  # only keep species and trait value columns (consistently columns 2 and 4)
  GIFT_data <- GIFT_data[,c(2,4)]
  
  # rename these columns for combining later
  colnames(GIFT_data)<- c('species','trait_value')
  
  # characterize values
  GIFT_data$trait_value <- as.character(GIFT_data$trait_value)
  
  # add trait name column
  GIFT_data$trait_name <- desired_trait$Trait2
  
  # create separate df
  df <- data.frame(species=GIFT_data$species, trait_value=GIFT_data$trait_value, trait_name=GIFT_data$trait_name)
})

# data for all desired traits:  
GIFT_data <- bind_rows(traits)

paste("The number of records is", nrow(GIFT_data))
paste("The number of species is", length(unique(GIFT_data$species)))

GIFT_data %>% count(trait_name)

# write data to csv
write.csv(GIFT_data, file = file.path(output_path,"TropicalAndes_GIFT_plant_traits.csv"))
