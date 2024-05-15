run_ensemble <- function(m, met_inflow_ensembles, config){

  ## FaasR
  init <- jsonlite::read_json("states_prior.json")
  pars <- jsonlite::read_json("pars_prior.json")

  file.copy("glm3_initial.nml", paste0("glm3-",m,".nml"), overwrite = TRUE)
  file.copy("aed2.nml", paste0("aed2-",m,".nml"), overwrite = TRUE)
  file.copy("aed_phyto_pars.csv", paste0("aed_phyto_pars-",m,".csv"), overwrite = TRUE)

  update_configs(m, init, pars, working_directory, config, met_inflow_ensembles)

  GLM3r::run_glm(sim_folder = working_directory, nml_file = paste0("glm3-",m,".nml"), verbose = FALSE)

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

  ens_df <- bind_rows(var_df, secchi_df, co2_df, ch4_df, depth_df) |>
    mutate(datetime = as_date(datetime)) |>
    summarise(prediction = mean(prediction),
              .by = c("datetime","depth_m","ensemble", "variable"))

  arrow::write_dataset(ens_df, file.path(working_directory, "test"), partitioning = "ensemble")

  unlink(file.path(file.path(working_directory, paste0("output-",m,".nc"))))

}
