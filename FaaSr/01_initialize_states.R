faasr_initialize_states <- function(){
  working_directory <- here::here()
  setwd(working_directory)
  #purrr::walk(list.files("R", recursive = TRUE, full.names = TRUE), function(file){source(file)})
  library(tidyverse)

  conf_files <- c("aed2_zoop_pars.nml", "aed2.nml", "configuration.yaml", 
                  "fcre-targets-inflow.csv", "glm3_initial.nml", "aed_phyto_pars.csv",
                  "parameter_calibration_config.csv", "states_config.csv")

  for (conf_file in conf_files){
    FaaSr::faasr_get_file(server_name="My_Minio_Bucket",
                          remote_folder="configuration", 
                          remote_file=conf_file, 
                          local_file=conf_file)
  }

  config <- read_configuration(file = "configuration.yaml")

  initialize_states(config)

  FaaSr::faasr_put_file(server_name="My_Minio_Bucket",
                        remote_file="states_prior.json", 
                        local_file="states_prior.json")
}
