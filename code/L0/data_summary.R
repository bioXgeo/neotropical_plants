data_summary <- function(records, species, genera, families){
  num_records <- nrow(records)
  num_species <- length(unique(species))
  num_genera <- length(unique(genera))
  num_families <- length(unique(families))
  return(cat("The number of records is", num_records, "\n", "The number of species is", num_species, "\n","The number of genera is", num_genera, "\n", "The number of families is", num_families))
}