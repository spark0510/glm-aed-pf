clean_up_run <- function(working_directory){
  
  unlink(list.files(working_directory, pattern = "glm3-", full.names = TRUE))
  unlink(list.files(working_directory, pattern = "WQ-", full.names = TRUE))
  unlink(list.files(working_directory, pattern = "lake-", full.names = TRUE))
  unlink(list.files(working_directory, pattern = "aed2-", full.names = TRUE))
  unlink(list.files(working_directory, pattern = "aed_phyto_pars-", full.names = TRUE))
  unlink(list.files(working_directory, pattern = "met_", full.names = TRUE))
  
}
  
  