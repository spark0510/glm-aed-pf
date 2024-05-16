initialize_parameters <- function(config){
  
  par_config <- read_csv(config$par_config_file, show_col_types = FALSE) |> 
    dplyr::filter(par_name %in% config$focal_parameters)
  
  unique_pars <- unique(par_config$par_name)
  
  pars <- list()
  
  for(i in 1:length(unique_pars)){
    
    par <- par_config |> 
      dplyr::filter(par_name == unique_pars[i])
    
    
    pars[[i]] <- list()
    pars[[i]]$par_name <- par$par_name[1] 
    pars[[i]]$par_file <- par$par_file[1] 
    pars[[i]]$value <- list()

  for(m in 1:config$nmembers){
    value <- NULL
    pars[[i]]$value[[m]] <- list()
  for(p in 1:nrow(par)){
    if(par$family[p] == "normal"){
      value<- c (value, rnorm(1, par$dist_par1[p], par$dist_par2[p]))
    }else if(par$family[p] == "uniform"){
      value <- c(value, runif(1, par$dist_par1[p], par$dist_par2[p]))
    }
  }
    pars[[i]]$value[[m]] <- value
  }
  }

  jsonlite::write_json(pars, "pars_prior.json", pretty = TRUE)
  }
  