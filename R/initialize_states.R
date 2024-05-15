initialize_states <- function(config){
  obs <- read_csv(config$historical_insitu, show_col_types = FALSE) |>
    mutate(datetime = as_date(datetime))
  #
  focal_obs <- obs |>
    filter(datetime == config$start_datetime,
           site_id ==config$site_id,
           !is.na(depth_m)) |>
    arrange(depth_m)
  
  obs_depths <- unique(focal_obs$depth_m)
  
  init <- list()
  
  for(m in 1:config$nmembers){
    init[m] <- m
    init[[m]]$the_depths <- obs_depths
    init[[m]]$the_temps <- round(filter(focal_obs, variable == "Temp_C_mean")$observation, 4)
    init[[m]]$the_sals <- rep(0.1, length(obs_depths))
    init[[m]]$lake_depth <- 9.4
    
    oxy_depths <- filter(focal_obs, variable == "DO_mgL_mean")$depth_m
    oxy <- filter(focal_obs, variable == "DO_mgL_mean")$observation*(1000/32)
    init[[m]]$wq_init_vals <- round(approx(oxy_depths, oxy, obs_depths, rule = 2)$y, 4)
    init[[m]]$wq_names <- "OXY_oxy"
    #init[[m]]$wq_init_vals <- c(init[[m]]$wq_init_vals, pmax(rnorm(length(obs_depths), 0.2, 0.1),0))
    init[[m]]$wq_init_vals <- c(init[[m]]$wq_init_vals, pmax(runif(length(obs_depths), 0, 0.5),0))
    init[[m]]$wq_names <- c(init[[m]]$wq_names, "NIT_nit")
    
    #init[[m]]$wq_init_vals <- c(init[[m]]$wq_init_vals, pmax(rnorm(length(obs_depths), 1.5, 0.5),0))
    init[[m]]$wq_init_vals <- c(init[[m]]$wq_init_vals, pmax(runif(length(obs_depths), 0, 2),0))
    init[[m]]$wq_names <- c(init[[m]]$wq_names, "NIT_amm")
    
    init[[m]]$wq_init_vals <- c(init[[m]]$wq_init_vals, pmax(runif(length(obs_depths), 0, 1),0))
    init[[m]]$wq_names <- c(init[[m]]$wq_names, "PHS_frp")
    
    total_doc <- rep((-151.3407 + 29.62654 * filter(focal_obs, variable == "fDOM_QSU_mean")$observation), length(obs_depths))
    init[[m]]$wq_names <- c(init[[m]]$wq_names, "OGM_docr")
    init[[m]]$wq_init_vals <- c(init[[m]]$wq_init_vals, round(total_doc*0.9, 4))
    init[[m]]$wq_names <- c(init[[m]]$wq_names, "OGM_doc")
    init[[m]]$wq_init_vals <- c(init[[m]]$wq_init_vals, round(total_doc*0.1, 4))
    
    total_chla <- rep(filter(focal_obs, variable == "Chla_ugL_mean")$observation, length(obs_depths))
    cyano <- total_chla * 0.1 * (1 / ((1 / 500) * 12))
    init[[m]]$wq_names <- c(init[[m]]$wq_names, "PHY_cyano")
    init[[m]]$wq_init_vals <- c(init[[m]]$wq_init_vals, round(cyano, 4))
    green <- total_chla * 0.4 * (1 / ((1 / 30) * 12))
    init[[m]]$wq_names <- c(init[[m]]$wq_names, "PHY_green")
    init[[m]]$wq_init_vals <- c(init[[m]]$wq_init_vals, round(green, 4))
    diatom <- total_chla * 0.4 * (1 / ((1 / 30) * 12))
    init[[m]]$wq_names <- c(init[[m]]$wq_names, "PHY_diatom")
    init[[m]]$wq_init_vals <- c(init[[m]]$wq_init_vals, round(diatom, 4))
  }
  
  jsonlite::write_json(init, "states_prior.json", pretty = TRUE)
}