standardize_forecast <- function(posterior, config){
  
  forecast_df <- posterior |> 
    filter(datetime >= config$forecast_start_datetime) |> 
    mutate(project_id = config$project_id,
           duration = "P1D",
           site_id = config$site_id,
           reference_datetime = lubridate::as_datetime(config$forecast_start_datetime),
           model_id = config$model_id,
           family = "ensemble",
           parameter = ensemble) |> 
    select(project_id, model_id, datetime, reference_datetime, duration, site_id,
           depth_m, family, parameter, variable, prediction)
  return(forecast_df)
}