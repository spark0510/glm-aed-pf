run_ensemble <- function(m, met_inflow_ensembles, config, output_path){
  working_directory <- here::here()
  #Need to copy in from FaaSr???  Better to be a remote read
  init <- jsonlite::read_json("states_prior.json")
  pars <- jsonlite::read_json("pars_prior.json")
  
  #The NML needs to be copied from S3 or a GitHub repo using download.file
  file.copy("glm3_initial.nml", paste0("glm3-",m,".nml"), overwrite = TRUE)
  file.copy("aed2.nml", paste0("aed2-",m,".nml"), overwrite = TRUE)
  file.copy("aed_phyto_pars.csv", paste0("aed_phyto_pars-",m,".csv"), overwrite = TRUE)
  
  met_numbers <- c("00", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", 
              "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30")

  for(met_number in met_numbers){
    FaaSr::faasr_get_file(server_name="My_Minio_Bucket",
                          remote_folder="met",
                          remote_file=paste0("met-",met_number,".csv"), 
                          local_file=paste0("met-",met_number,".csv"))
  }

  flows <- c("outflow1.csv", "inflow1.csv")
  for(flow in flows){
    FaaSr::faasr_get_file(server_name="My_Minio_Bucket",
                          remote_folder="met",
                          remote_file=flow, 
                          local_file=flow)
  }

  update_configs(m, init, pars, working_directory, config, met_inflow_ensembles)
  
  GLM3r::run_glm(sim_folder = working_directory, nml_file = paste0("glm3-",m,".nml"), verbose = TRUE)
  
  print(working_directory)
  print(list.files("."))
  #var_df <- purrr::map_dfr(focal_depths, function(focal_depth, focal_var, m){
  #  if(focal_depth == 0){
  #    fname <- paste0("WQ-",m,"_surf.csv")
  #  }else{
  #    fname <- paste0("WQ-",m,"_",floor(focal_depth),".csv")
  #  }
  #  df <- read_csv(file.path(working_directory,fname), show_col_types = FALSE) |>
  #    rename(datetime = time) |>
  #    mutate(datetime = as_date(datetime)) |>
  #    select(all_of(focal_vars), datetime) |>
  #    pivot_longer(-datetime, names_to = "variable", values_to = "prediction") |>
  #    mutate(depth_m = as.numeric(focal_depth),
  #           ensemble = m)
  #},
  #focal_var,
  #m
  #)
  
  var_df <- purrr::map_dfr(config$focal_vars, function(focal_var, focal_depths, m){
    df1 <- glmtools::get_var(file = file.path(working_directory, paste0("output-",m,".nc")),
                             var_name = focal_var,
                             reference = "surface",
                             z_out = focal_depths) |>
      pivot_longer(-DateTime, names_to = "depth_m", values_to = "prediction", names_prefix = paste0(focal_var, "_")) |>
      mutate(variable = focal_var,
             depth_m = as.numeric(depth_m),
             ensemble = m,
             datetime = as_datetime(DateTime)) |>
      select(-DateTime)
  },
  focal_depths = config$focal_depths,
  m)
  
  depth_df <- glmtools::get_surface_height(file = file.path(working_directory, paste0("output-",m,".nc"))) |>
    rename(prediction = surface_height) |>
    mutate(variable = "lakeDepth_m_mean",
           depth_m = -1,
           ensemble = m,
           datetime = as_datetime(DateTime)) |>
    select(-DateTime)
  
  #depth_df <- read_csv(file.path(paste0("lake-",m,".csv")), show_col_types = FALSE) |>
  #  select(time, `Lake Level`) |>
  #  rename(datetime = time,
  #         prediction = `Lake Level`) |>
  #  mutate(variable = "lakeDepth_m_mean",
  #                 depth_m = -1,
  #                 ensemble = m,
  #                 datetime = as_datetime(datetime))
  
  secchi_df <- glmtools::get_var(file = file.path(working_directory, paste0("output-",m,".nc")),
                                 var_name = "extc",
                                 reference = "surface",
                                 z_out = 1.0) |>
    rename(prediction = extc_1) |>
    mutate(variable = "secchi",
           depth_m = -1,
           ensemble = m,
           datetime = as_datetime(DateTime),
           prediction = 1.7/prediction) |>
    select(-DateTime)
  
  if(config$include_fluxes){
    
    co2_df <- glmtools::get_var(file = file.path(working_directory, paste0("output-",m,".nc")),
                                var_name = "CAR_atm_co2_flux",
                                reference = "surface",
                                z_out = 0) |>
      rename(prediction = CAR_atm_co2_flux) |>
      mutate(variable = "co2_flux",
             depth_m = -1,
             ensemble = m,
             datetime = as_datetime(DateTime)) |>
      select(-DateTime)
    
    ch4_df <- glmtools::get_var(file = file.path(working_directory, paste0("output-",m,".nc")),
                                var_name = "CAR_ch4_atm",
                                reference = "surface",
                                z_out = 0) |>
      rename(prediction = CAR_ch4_atm) |>
      mutate(variable = "ch4_flux",
             depth_m = -1,
             ensemble = m,
             datetime = as_datetime(DateTime)) |>
      select(-DateTime)
  }else{
    co2_df <- NULL
    ch4_df <- NULL
  }
  
  ens_df <- bind_rows(var_df, secchi_df, co2_df, ch4_df, depth_df) |>
    mutate(datetime = as_date(datetime)) |>
    summarise(prediction = mean(prediction),
              .by = c("datetime","depth_m","ensemble", "variable"))
  
  arrow::write_dataset(ens_df, output_path, partitioning = "ensemble")
  
  unlink(file.path(file.path(working_directory, paste0("output-",m,".nc"))))
  
}
