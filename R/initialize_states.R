initialize_states <- function(config){
  obs <- read_csv(config$historical_insitu, show_col_types = FALSE) |>
    mutate(datetime = as_date(datetime))
  
  states_config <- read_csv(config$states_config_file, show_col_types = FALSE)
  #
  focal_obs <- obs |>
    filter(datetime == config$start_datetime,
           site_id ==config$site_id,
           !is.na(depth_m)) |>
    arrange(depth_m)
  
  obs_depths <- unique(focal_obs$depth_m)
  
  glm_names <- unique(states_config$glm_name)
  
  
  ensemble_init <- list()
  
  for(m in 1:config$nmembers){
    
    init <- list()
    
    for(s in 1:length(glm_names)){
      
      init[[s]] <- list()
      init[[s]]$glm_name <- glm_names[s]
      init[[s]]$value <- list()
      combined_value <- NULL
      
      same_glm_names <- states_config |> filter(glm_name == glm_names[s])
      
      
      for(v in 1:nrow(same_glm_names)){
        
        curr_state <- states_config |> filter(state_names == same_glm_names$state_names[v])
        
        if(curr_state$multi_depth == 1){
          curr_depths <- obs_depths
        }else{
          curr_depths <- 1
        }

        if(!is.na(curr_state$observation)){
          var_obs <- filter(focal_obs, variable == curr_state$observation)$observation
          var_depths <- filter(focal_obs, variable == curr_state$observation)$depth_m
          if(length(var_obs) > 1){
            var_value <- approx(var_depths, var_obs, curr_depths, rule = 2)$y
          }else{
            var_value <- rep(var_obs, length(curr_depths))
          }
          if(curr_state$family == "normal"){
            var_value <- rnorm(length(var_value), var_value, curr_state$dist_par2)
          }else if(curr_state$family == "uniform"){
            warning("Only normal distributions are currently supported for states that use observation to initialize")
          }
          
        }else{
          if(curr_state$family == "normal"){
            var_value <- rnorm(length(curr_depths),curr_state$dist_par1, curr_state$dist_par2)
          }else if(curr_state$family == "uniform"){
            var_value <- runif(length(var_value), curr_state$dist_par1, curr_state$dist_par2)
          }
        }
        var_value <- curr_state$conversion_multiplier * 
          (curr_state$conversion_intercept + curr_state$conversion_slope * var_value)
        

        var_value[which(var_value < curr_state$lower_bound)] <- curr_state$lower_bound
        
        init[[s]]$value  <- unlist(c(init[[s]]$value ,  round(var_value, 4)))
        
      }
    }
    
    if("wq_init_vals" %in% glm_names){
      s <- length(glm_names) + 1
      init[[s]] <- list()
      init[[s]]$glm_name <- "wq_names"
      init[[s]]$value <- pull(states_config |> filter(glm_name == "wq_init_vals"), "state_names")
    }
  
    s <- length(glm_names) + 2
    init[[s]] <- list()
    init[[s]]$glm_name <- "the_depths"
    init[[s]]$value <- obs_depths
    
    ensemble_init[[m]] <- init
  }
  
  jsonlite::write_json(ensemble_init, "states_prior.json", pretty = TRUE)
}
