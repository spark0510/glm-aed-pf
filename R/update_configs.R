update_configs <- function(m, init, pars, working_directory, config, met_inflow_ensembles){
  
  update_glm_nml_list <- list()
  update_glm_nml_names <- list()
  
  update_aed_nml_list <- list()
  update_aed_nml_names <- list()
  
  update_phyto_nml_list <- list()
  update_phyto_nml_names <- list()
  
  list_index_glm <- 1
  list_index_aed <- 1
  list_index_phyto <- 1
  
  for(s in 1:length(init[[m]])){
    
    curr_state <- init[[m]][s]
    
    if(curr_state[[1]]$glm_name == "the_temps"){
      update_glm_nml_list[[list_index_glm]] <- as.numeric(unlist(curr_state[[1]]$value))
      update_glm_nml_names[list_index_glm] <- "the_temps"
      list_index_glm <- list_index_glm + 1
    }
    
    if(curr_state[[1]]$glm_name == "the_sals"){
      update_glm_nml_list[[list_index_glm]] <- as.numeric(unlist(curr_state[[1]]$value))
      update_glm_nml_names[list_index_glm] <- "the_sals"
      list_index_glm <- list_index_glm + 1
    }
    
    if(curr_state[[1]]$glm_name == "the_depths"){
      update_glm_nml_list[[list_index_glm]] <- as.numeric(unlist(curr_state[[1]]$value))
      update_glm_nml_names[list_index_glm] <- "the_depths"
      list_index_glm <- list_index_glm + 1
      
      update_glm_nml_list[[list_index_glm]] <- length(as.numeric(unlist(curr_state[[1]]$value)))
      update_glm_nml_names[list_index_glm] <- "num_depths"
      list_index_glm <- list_index_glm + 1
    }
    
    if(curr_state[[1]]$glm_name == "lake_depth"){
      update_glm_nml_list[[list_index_glm]] <- as.numeric(unlist(curr_state[[1]]$value))
      update_glm_nml_names[list_index_glm] <- "lake_depth"
      list_index_glm <- list_index_glm + 1
    }
    #
    #   update_glm_nml_list[[list_index_glm]] <- 0.0
    #   update_glm_nml_names[list_index_glm] <- "snow_thickness"
    #   list_index_glm <- list_index_glm + 1
    #
    #   update_glm_nml_list[[list_index_glm]] <- init[m]$white_ice_thickness
    #   update_glm_nml_names[list_index_glm] <- "white_ice_thickness"
    #   list_index_glm <- list_index_glm + 1
    #
    #   update_glm_nml_list[[list_index_glm]] <- init[m]$blue_ice_thickness
    #   update_glm_nml_names[list_index_glm] <- "blue_ice_thickness"
    #   list_index_glm <- list_index_glm + 1
    #
    #   update_glm_nml_list[[list_index_glm]] <- init[m]$avg_surf_temp
    #   update_glm_nml_names[list_index_glm] <- "avg_surf_temp"
    #   list_index_glm <- list_index_glm + 1
    #
    
    if(curr_state[[1]]$glm_name == "wq_init_vals"){
      update_glm_nml_list[[list_index_glm]] <- as.numeric(unlist(curr_state[[1]]$value))
      update_glm_nml_names[list_index_glm] <- "wq_init_vals"
      list_index_glm <- list_index_glm + 1
    }
    
    if(curr_state[[1]]$glm_name == "wq_names"){
      update_glm_nml_list[[list_index_glm]] <- as.character(unlist(curr_state[[1]]$value))
      update_glm_nml_names[list_index_glm] <- "wq_names"
      list_index_glm <- list_index_glm + 1
      
      update_glm_nml_list[[list_index_glm]] <- length(as.character(unlist(curr_state[[1]]$value)))
      update_glm_nml_names[list_index_glm] <- "num_wq_vars"
      list_index_glm <- list_index_glm + 1
      
    }
  }
  
  update_glm_nml_list[[list_index_glm]] <- paste0("output-",m)
  update_glm_nml_names[list_index_glm] <- "out_fn"
  list_index_glm <- list_index_glm + 1
  
  update_glm_nml_list[[list_index_glm]] <- paste0("aed2-",m,".nml")
  update_glm_nml_names[list_index_glm] <- "wq_nml_file"
  list_index_glm <- list_index_glm + 1
  
  update_glm_nml_list[[list_index_glm]] <- met_inflow_ensembles$met_ensemble[m]
  update_glm_nml_names[list_index_glm] <- "meteo_fl"
  list_index_glm <- list_index_glm + 1
  
  update_glm_nml_list[[list_index_glm]] <- met_inflow_ensembles$inflow_ensemble[m]
  update_glm_nml_names[list_index_glm] <- "inflow_fl"
  list_index_glm <- list_index_glm + 1
  
  update_glm_nml_list[[list_index_glm]] <- met_inflow_ensembles$outflow_ensemble[m]
  update_glm_nml_names[list_index_glm] <- "outflow_fl"
  list_index_glm <- list_index_glm + 1
  
  update_glm_nml_list[[list_index_glm]] <- strftime(config$start_datetime,format="%Y-%m-%d %H:%M",tz = "UTC")
  update_glm_nml_names[list_index_glm] <- "start"
  list_index_glm <- list_index_glm + 1
  
  update_glm_nml_list[[list_index_glm]] <- strftime(config$end_datetime,format="%Y-%m-%d %H:%M",tz = "UTC")
  update_glm_nml_names[list_index_glm] <- "stop"
  list_index_glm <- list_index_glm + 1
  
  update_glm_nml_list[[list_index_glm]] <- config$glm_output_time_step
  update_glm_nml_names[list_index_glm] <- "nsave"
  list_index_glm <- list_index_glm + 1
  
  update_aed_nml_list[[list_index_aed]] <- paste0("aed_phyto_pars-",m,".csv")
  update_aed_nml_names[list_index_aed] <- "dbase"
  list_index_aed <- list_index_aed + 1
  
  for(p in 1:length(pars)){
    
    if(pars[[p]]$par_file == "glm3.nml"){
      
      update_glm_nml_list[[list_index_glm]] <- unlist(pars[[p]]$value[[m]])
      update_glm_nml_names[list_index_glm] <- pars[[p]]$par_name
      list_index_glm <- list_index_glm + 1
      
    }else if(pars[[p]]$par_file == "aed2.nml"){
      
      update_aed_nml_list[[list_index_aed]] <- unlist(pars[[p]]$value[[m]])
      update_aed_nml_names[list_index_aed] <- pars[[p]]$par_name
      list_index_aed <- list_index_aed + 1
      
    }else if(pars[[p]]$par_file == "aed_phyto_pars.csv"){
      
      update_phyto_nml_list[[list_index_phyto]] <- unlist(pars[[p]]$value[[m]])
      update_phyto_nml_names[list_index_phyto] <- pars[[p]]$par_name
      list_index_phyto <- list_index_phyto + 1
      
    }
  }
  
  FLAREr:::update_nml(var_list = update_glm_nml_list,
                      var_name_list = update_glm_nml_names,
                      working_directory = working_directory,
                      nml = paste0("glm3-",m,".nml"))
  
  if(list_index_aed > 1){
    FLAREr:::update_nml(update_aed_nml_list,
                        update_aed_nml_names,
                        working_directory = working_directory,
                        paste0("aed2-",m,".nml"))
  }
  
  if(list_index_phyto > 1){
    phytos <- readr::read_csv(file.path(working_directory, paste0("aed_phyto_pars-",m,".csv")) ,show_col_types = FALSE)
    
    for(k in 1:length(update_phyto_nml_names)){
      phytos[which(stringr::str_detect(phytos$`'p_name'`, update_phyto_nml_names[[k]])),2:ncol(phytos)] <- as.list(update_phyto_nml_list[[k]])
    }
    
    readr::write_csv(phytos, file.path(working_directory, paste0("aed_phyto_pars-",m,".csv")))
  }
  
}
