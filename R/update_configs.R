update_configs <- function(m, init, pars, working_directory, config, met_inflow_ensembles){

  update_glm_nml_list <- list()
  update_glm_nml_names <- list()

  list_index <- 1
  update_glm_nml_list[[list_index]] <- as.numeric(unlist(init[[m]]$the_temps))
  update_glm_nml_names[list_index] <- "the_temps"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- as.numeric(unlist(init[[m]]$the_sals))
  update_glm_nml_names[list_index] <- "the_sals"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- as.numeric(unlist(init[[m]]$the_depths))
  update_glm_nml_names[list_index] <- "the_depths"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- length(init[[m]]$the_depths)
  update_glm_nml_names[list_index] <- "num_depths"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- unlist(init[[m]]$lake_depth)
  update_glm_nml_names[list_index] <- "lake_depth"
  list_index <- list_index + 1
  #
  #   update_glm_nml_list[[list_index]] <- 0.0
  #   update_glm_nml_names[list_index] <- "snow_thickness"
  #   list_index <- list_index + 1
  #
  #   update_glm_nml_list[[list_index]] <- init[m]$white_ice_thickness
  #   update_glm_nml_names[list_index] <- "white_ice_thickness"
  #   list_index <- list_index + 1
  #
  #   update_glm_nml_list[[list_index]] <- init[m]$blue_ice_thickness
  #   update_glm_nml_names[list_index] <- "blue_ice_thickness"
  #   list_index <- list_index + 1
  #
  #   update_glm_nml_list[[list_index]] <- init[m]$avg_surf_temp
  #   update_glm_nml_names[list_index] <- "avg_surf_temp"
  #   list_index <- list_index + 1
  #
  update_glm_nml_list[[list_index]] <- as.numeric(unlist(init[[m]]$wq_init_vals))
  update_glm_nml_names[list_index] <- "wq_init_vals"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- unlist(init[[m]]$wq_names)
  update_glm_nml_names[list_index] <- "wq_names"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- length(init[[m]]$wq_names)
  update_glm_nml_names[list_index] <- "num_wq_vars"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- paste0("output-",m)
  update_glm_nml_names[list_index] <- "out_fn"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- pars$lw[[m]]
  update_glm_nml_names[list_index] <- "lw_factor"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- pars$Kw[[m]]
  update_glm_nml_names[list_index] <- "Kw"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- paste0("aed2-",m,".nml")
  update_glm_nml_names[list_index] <- "wq_nml_file"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- met_inflow_ensembles$met_ensemble[m]
  update_glm_nml_names[list_index] <- "meteo_fl"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- met_inflow_ensembles$inflow_ensemble[m]
  update_glm_nml_names[list_index] <- "inflow_fl"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- met_inflow_ensembles$outflow_ensemble[m]
  update_glm_nml_names[list_index] <- "outflow_fl"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- strftime(config$start_datetime,format="%Y-%m-%d %H:%M",tz = "UTC")
  update_glm_nml_names[list_index] <- "start"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- strftime(config$end_datetime,format="%Y-%m-%d %H:%M",tz = "UTC")
  update_glm_nml_names[list_index] <- "stop"
  list_index <- list_index + 1

  update_glm_nml_list[[list_index]] <- config$glm_output_time_step
  update_glm_nml_names[list_index] <- "nsave"
  list_index <- list_index + 1

  #list_index <- 10
  #update_glm_nml_list[[list_index]] <- paste0("WQ-",m,"_")
  #update_glm_nml_names[list_index] <- "csv_point_fname"

  #list_index <- 11
  #update_glm_nml_list[[list_index]] <- paste0("lake-",m)
  #update_glm_nml_names[list_index] <- "csv_lake_fname"

  FLAREr:::update_nml(var_list = update_glm_nml_list,
                      var_name_list = update_glm_nml_names,
                      working_directory = working_directory,
                      nml = paste0("glm3-",m,".nml"))

  ## Updates to aed nml

  update_aed_nml_list <- list()
  update_aed_nml_names <- list()

  list_index_aed <- 1
  Fsed_oxy <- c(pars$Fsed_oxy1[[m]], pars$Fsed_oxy2[[m]], pars$Fsed_oxy3[[m]], pars$Fsed_oxy4[[m]])
  update_aed_nml_list[[list_index_aed]] <- round(Fsed_oxy, 4)
  update_aed_nml_names[list_index_aed] <- "Fsed_oxy"

  list_index_aed <- 2
  update_aed_nml_list[[list_index_aed]] <- paste0("aed_phyto_pars-",m,".csv")
  update_aed_nml_names[list_index_aed] <- "dbase"

  if(list_index_aed > 1){
    FLAREr:::update_nml(update_aed_nml_list,
                        update_aed_nml_names,
                        working_directory = working_directory,
                        paste0("aed2-",m,".nml"))
  }

  ## Updates to phyto csv

  update_phyto_nml_list <- list()
  update_phyto_nml_names <- list()

  list_index_phyto <- 1
  Rgrowth <- c(pars$Rgrowth1[[m]], pars$Rgrowth2[[m]], pars$Rgrowth3[[m]])
  update_phyto_nml_list[[list_index_phyto]] <- Rgrowth
  update_phyto_nml_names[list_index_phyto] <- "R_growth"

  #list_index_phyto <- 2
  K_N <- c(pars$K_N1[[m]], pars$K_N2[[m]], pars$K_N3[[m]])
  update_phyto_nml_list[[list_index_phyto]] <- K_N
  update_phyto_nml_names[list_index_phyto] <- "K_N"

  if(list_index_phyto > 1){
    phytos <- readr::read_csv(file.path(working_directory, paste0("aed_phyto_pars-",m,".csv")) ,show_col_types = FALSE)

    for(k in 1:length(update_phyto_nml_names)){
      phytos[which(stringr::str_detect(phytos$`'p_name'`, update_phyto_nml_names[[k]])),2:ncol(phytos)] <- as.list(update_phyto_nml_list[[k]])
    }

    readr::write_csv(phytos, file.path(working_directory, paste0("aed_phyto_pars-",m,".csv")))
  }

}
