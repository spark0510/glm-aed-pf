faasr_combine_ensembles <- function(){
  
  working_directory <- here::here()
  setwd(working_directory)
  #purrr::walk(list.files("R", recursive = TRUE, full.names = TRUE), function(file){source(file)})
  library(tidyverse)

  conf_files <- c("aed2_zoop_pars.nml", "aed2.nml", "configuration.yaml", 
                  "fcre-targets-inflow.csv", "glm3_initial.nml",
                  "parameter_calibration_config.csv", "states_config.csv")

  for (conf_file in conf_files){
    FaaSr::faasr_get_file(server_name="My_Minio_Bucket",
                          remote_folder="configuration", 
                          remote_file=conf_file, 
                          local_file=conf_file)
  }

  config <- read_configuration(file = "configuration.yaml")

  ensemble_path <- "ensemble_output"
  parquet_files <- FaaSr::faasr_get_folder_list(server_name="My_Minio_Bucket", faasr_prefix=ensemble_path)

  for(parquet_file in parquet_files){
    FaaSr::faasr_get_file(server_name="My_Minio_Bucket",
                          remote_folder=ensemble_path,
                          remote_file=as.character(parquet_file), 
                          local_folder=ensemble_path,
                          local_file=as.character(parquet_file))
  }

  #This pulls together the results into a single parquet
  combined_path <- "output"
  combine_ensembles(ensemble_path = ensemble_path, combined_path = combined_path)


  # Standardize obs and ensemble output
  states_config <- read_csv(config$states_config_file, show_col_types = FALSE)

  oxy <- states_config |> 
    filter(state_names == "OXY_oxy")

  fdom <- states_config |> 
    filter(state_names == "OGM_doc")

  prior <- arrow::open_dataset(combined_path) |>
    mutate(variable = ifelse(variable == "temp", "Temp_C_mean", variable),
          variable = ifelse(variable == "PHY_tchla", "Chla_ugL_mean", variable),
          variable = ifelse(variable == "OXY_oxy", "DO_mgL_mean", variable),
          variable = ifelse(variable == "secchi", "Secchi_m_sample", variable),
          variable = ifelse(variable == "CAR_ch4_atm", "ch4flux_umolm2s_mean", variable),
          variable = ifelse(variable == "co2_flux", "co2flux_umolm2s_mean", variable)) |>
    mutate(prediction = ifelse(variable == "DO_mgL_mean", prediction / oxy$conversion_slope,prediction),
          prediction = ifelse(variable == "ch4flux_umolm2s_mean", (prediction * 1000) / (60 * 60 * 24),prediction),
          prediction = ifelse(variable == "co2flux_umolm2s_mean", (prediction * 1000) / (60 * 60 * 24),prediction)) |>
    collect() |>
    pivot_wider(names_from = variable, values_from = prediction) |>
    mutate(fDOM_QSU_mean = (OGM_doc + OGM_docr - fdom$conversion_intercept) / fdom$conversion_slope)  |>
    select(-OGM_doc, -OGM_docr) |>
    pivot_longer(-c("depth_m", "datetime",  "ensemble"), names_to = "variable", values_to = "prediction") |>
    filter(!is.na(prediction))


  obs <- read_csv(config$historical_insitu, show_col_types = FALSE) |>
    mutate(datetime = as_date(datetime)) |>
    filter(site_id == config$site_id) |>
    mutate(depth_m = ifelse(is.na(depth_m), -1, depth_m))

  out <- calc_particle_weights(prior, obs, config)

  prior <- out$prior
  weights <- out$weights

  pars <- jsonlite::read_json("pars_prior.json")
  out <- generate_posteriors(prior, pars, weights)

  par_posterior <- out$par_posterior
  posterior <- out$posterior

  forecast_df <- standardize_forecast(posterior, config)
  #Note the posterior is the forecast

  arrow::write_parquet(posterior, "forecast.parquet")

  pars <- jsonlite::read_json("pars_prior.json")

  arrow::write_parquet(par_posterior, "parameters.parquet")

  FaaSr::faasr_put_file(server_name="My_Minio_Bucket",
                          remote_folder="output", 
                          remote_file="part-0.parquet", 
                          local_folder="output", 
                          local_file="part-0.parquet")

  FaaSr::faasr_put_file(server_name="My_Minio_Bucket",
                          remote_file="forecast.parquet", 
                          local_file="forecast.parquet")                        

  FaaSr::faasr_put_file(server_name="My_Minio_Bucket",
                          remote_file="parameters.parquet", 
                          local_file="parameters.parquet")
}
