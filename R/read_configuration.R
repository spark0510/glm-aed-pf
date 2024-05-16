read_configuration <- function(file){
  
  config <- yaml::read_yaml(file)
  config$forecast_start_datetime <- lubridate::as_datetime(config$forecast_start_datetime)
  config$focal_depths <- unlist(config$focal_depths)
  config$obs_sd <- unlist(config$obs_sd)
  config$start_datetime <- config$forecast_start_datetime - lubridate::days(config$look_back)
  config$end_datetime <- config$forecast_start_datetime + lubridate::days(config$horizon)
  
  return(config)
}
