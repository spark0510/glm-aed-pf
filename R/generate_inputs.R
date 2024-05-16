generate_inputs <- function(config){
  
  met_out <- generate_met_files(obs_met_file = NULL,
                                out_dir = working_directory,
                                start_datetime = config$start_datetime,
                                forecast_start_datetime = config$forecast_start_datetime,
                                forecast_horizon =  config$horizon,
                                site_id = config$site_id,
                                use_s3 = TRUE,
                                bucket = config$met_bucket ,
                                endpoint = config$met_endpoint,
                                use_hive_met = TRUE)
  
  met_ensemble_index <- sample(1:length(met_out$filenames), size = config$nmembers, replace = TRUE)
  
  met_ensembles <- tibble(
    ensemble = 1:config$nmembers,
    met_ensemble = met_out$filenames[met_ensemble_index]
  )
  
  inflow_outflow_files <- generate_inflow_outflow_files(inflow_forecast_dir = NULL,
                                                        inflow_obs = config$historical_inflow,
                                                        variables =  variables <- c("time", "FLOW", "TEMP", "SALT",
                                                                                    'OXY_oxy',
                                                                                    'CAR_dic',
                                                                                    'CAR_ch4',
                                                                                    'SIL_rsi',
                                                                                    'NIT_amm',
                                                                                    'NIT_nit',
                                                                                    'PHS_frp',
                                                                                    'OGM_doc',
                                                                                    'OGM_docr',
                                                                                    'OGM_poc',
                                                                                    'OGM_don',
                                                                                    'OGM_donr',
                                                                                    'OGM_pon',
                                                                                    'OGM_dop',
                                                                                    'OGM_dopr',
                                                                                    'OGM_pop',
                                                                                    'PHY_cyano',
                                                                                    'PHY_green',
                                                                                    'PHY_diatom'),
                                                        out_dir = working_directory,
                                                        start_datetime = config$start_datetime,
                                                        forecast_start_datetime = config$forecast_start_datetime,
                                                        forecast_horizon =  config$horizon,
                                                        site_id = config$site_id,
                                                        use_s3 = FALSE,
                                                        bucket = NA,
                                                        endpoint = NA,
                                                        local_directory = NA,
                                                        use_forecast = FALSE,
                                                        use_ler_vars = FALSE)
  
  met_inflow_ensembles <- tibble(
    ensemble = 1:config$nmembers,
    met_ensemble = normalizePath(met_out$filenames[met_ensemble_index]),
    inflow_ensemble = normalizePath(inflow_outflow_files$inflow_file_names),
    outflow_ensemble = normalizePath(inflow_outflow_files$outflow_file_names)
  )
  
  return(met_inflow_ensembles)
  
}