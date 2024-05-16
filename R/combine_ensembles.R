combine_ensembles <- function(ensemble_path = "ensemble_output", combined_path = "output"){
  
  #This pulls together the results
  dir.create(combined_path,showWarnings = FALSE)
  arrow::open_dataset(ensemble_path) |> arrow::write_dataset(combined_path)
  unlink(ensemble_path, recursive = TRUE)
  
}