faasr_run_ensemble <- function(start, end){

  working_directory <- here::here()
  setwd(working_directory)
  #purrr::walk(list.files("R", recursive = TRUE, full.names = TRUE), function(file){source(file)})
  library(tidyverse)
  print(working_directory)

  conf_files <- c("aed2_zoop_pars.nml", "aed2.nml", "configuration.yaml", 
                  "fcre-targets-inflow.csv", "glm3_initial.nml", "aed_phyto_pars.csv",
                  "parameter_calibration_config.csv", "states_config.csv")

  for (conf_file in conf_files){
    FaaSr::faasr_get_file(server_name="My_Minio_Bucket",
                          remote_folder="configuration", 
                          remote_file=conf_file, 
                          local_file=conf_file)
  }

  step1_files <- c("met_inflow_ensembles.RDS", "states_prior.json", "pars_prior.json")
  
  for (step1_file in step1_files){
    FaaSr::faasr_get_file(server_name="My_Minio_Bucket",
                          remote_file=step1_file, 
                          local_file=step1_file)
  }

  config <- read_configuration(file = "configuration.yaml")

  met_inflow_ensembles <- read_rds("met_inflow_ensembles.RDS")

  ensemble_path <- "ensemble_output"
  dir.create(file.path(working_directory, ensemble_path), showWarnings = FALSE)
  # Note output_path could be an S3 file system
  # run_ensemble() can be run in parallel using FaaSr.  What to do about met_inflow_ensembles?
  future::plan("future::multisession", workers = parallel::detectCores())
  bench::bench_time(
    furrr::future_walk(start:end,
                      .f = function(m, met_inflow_ensembles, config, output_path)
                      {
                        run_ensemble(m, met_inflow_ensembles, config, output_path)
                      },
                      met_inflow_ensembles,
                      config,
                      output_path = file.path(working_directory, ensemble_path)
    )
  )

  parquet_files <- list.files(ensemble_path)

  for(parquet_folder in parquet_files){
    FaaSr::faasr_put_file(server_name="My_Minio_Bucket",
                          remote_folder=file.path(ensemble_path, parquet_folder),
                          remote_file="part-0.parquet", 
                          local_folder=file.path(ensemble_path, parquet_folder),
                          local_file="part-0.parquet")
  }
  
}
