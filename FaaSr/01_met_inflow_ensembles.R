faasr_met_inflow_ensembles <- function(){

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

  met_inflow_ensembles <- generate_inputs(config) ## FaaSr - copy met and inflow csvs to S3

  write_rds(met_inflow_ensembles, "met_inflow_ensembles.RDS")

  FaaSr::faasr_put_file(server_name="My_Minio_Bucket",
                        remote_file="met_inflow_ensembles.RDS", 
                        local_file="met_inflow_ensembles.RDS")
}
