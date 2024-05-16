calc_particle_weights <- function(prior, obs, config){
  
  obs_sd <- tibble(variable = config$obs_names,
                   sd = config$obs_sd)
  
  ndays <- length(unique(prior$datetime)) - config$horizon
  
  prior <- prior |> 
    left_join(obs, by = join_by(datetime, depth_m, variable)) |>
    left_join(obs_sd, by = join_by(variable)) |>
    mutate(depth_m = ifelse(depth_m == -1, NA, depth_m)) |>
    mutate(type = "1-prior") |>
    select(-project_id, -duration, -site_id)
  
  weights <- prior |>
    filter(datetime < config$forecast_start_datetime,
           !is.na(observation),
           !is.na(sd)) |>
    mutate(logLL = dnorm(x = prediction, mean = observation, sd = sd, log= TRUE)) |>
    #summarize(sum_logLL = sum(logLL)/n(), count = n(), .by = "ensemble") |>
    summarize(sum_logLL = sum(logLL)/ndays, count = n(), .by = "ensemble") |>
    mutate(relative_weight = exp(sum_logLL) / sum(exp(sum_logLL))) |>
    arrange(ensemble)
  
  return(list(prior = prior, weights = weights))
}