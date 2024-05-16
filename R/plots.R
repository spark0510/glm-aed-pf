plot_parameters <- function(par_posterior){
  p <- ggplot(par_posterior, aes(x = value, color = type)) +
    geom_density() +
    facet_wrap(~par, scales = "free")
  
  p
  
  invisible(p)
}

plot_posterior_depth <- function(obs_df, posterior, focal_depths){
  
  obs_df <- posterior |>
    select(variable, observation, depth_m, datetime) |>
    distinct() |>
    filter(!is.na(observation),
           depth_m %in% focal_depths | is.na(depth_m))
  
  p <- posterior |>
    filter(depth_m %in% focal_depths | is.na(depth_m)) |>
    ggplot(aes(x = datetime)) +
    geom_line(aes(y = prediction, group = ensemble)) +
    geom_point(data = obs_df, aes(x = datetime, y = observation), color = "red", alpha = 0.5) +
    geom_vline(aes(xintercept = as_date(config$forecast_start_datetime))) +
    facet_grid(variable~depth_m, scales = "free_y")
  
  p
  
  invisible(p)
  
}

plot_phytos <- function(posterior, prior, obs_df, focal_depth = 1.6){
  
  obs_df <- posterior |>
    select(variable, observation, depth_m, datetime) |>
    distinct() |>
    filter(depth_m == focal_depth | is.na(depth_m),
           !is.na(observation),
           (str_detect(variable, "PHY_") | variable == "Chla_ugL_mean"))
  
  p <- bind_rows(posterior, prior) |>
    filter(depth_m == focal_depth | is.na(depth_m),
           (str_detect(variable, "PHY_") | variable == "Chla_ugL_mean")) |>
    mutate(ensemble = paste0(ensemble,type)) |>
    ggplot(aes(x = datetime)) +
    geom_line(aes(y = prediction, group = ensemble)) +
    geom_point(data = obs_df, aes(x = datetime, y = observation), color = "red", alpha = 0.5) +
    geom_vline(aes(xintercept = as_date(config$forecast_start_datetime))) +
    facet_grid(variable~type, scales = "free_y")
  
  p
  
  invisible(p)
  
}

plot_prior_post <- function(posterior, prior, obs_df, focal_depth = 1.6){
  
  obs_df <- posterior |>
    dplyr::select(variable, observation, depth_m, datetime) |>
    dplyr::distinct() |>
    dplyr::filter(depth_m == focal_depth | is.na(depth_m),
           !is.na(observation),
           str_detect(variable, "PHY_", negate = TRUE))
  
  p <- dplyr::bind_rows(posterior, prior) |>
    dplyr::filter(depth_m == focal_depth | is.na(depth_m),
           stringr::str_detect(variable, "PHY_", negate = TRUE)) |>
    dplyr::mutate(ensemble = paste0(ensemble,type)) |>
    ggplot2::ggplot(ggplot2::aes(x = datetime)) +
    ggplot2::geom_line(ggplot2::aes(y = prediction, group = ensemble)) +
    ggplot2::geom_point(data = obs_df, ggplot2::aes(x = datetime, y = observation), color = "red", alpha = 0.5) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = lubridate::as_date(config$forecast_start_datetime))) +
    ggplot2::facet_grid(variable~type, scales = "free_y")
  
  p 
  
  invisible(p)
}
