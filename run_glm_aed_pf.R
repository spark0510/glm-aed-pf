working_directory <- here::here()
setwd(working_directory)
purrr::walk(list.files("R", recursive = TRUE, full.names = TRUE), function(file){source(file)})
library(tidyverse)

config <- read_configuration(file = "configuration.yaml")

initialize_parameters(config) ## FaaSr - this function generates a json that needs to be copied json to S3


met_inflow_ensembles <- generate_inputs(config) ## FaaSr - copy met and inflow csvs to S3


initialize_states(config) ## FaaSr - this function generates a json that needs to be copied json to S3

ensemble_path <- "ensemble_output"
dir.create(file.path(working_directory, ensemble_path), showWarnings = FALSE)
# Note output_path could be an S3 file system
# run_ensemble() can be run in parallel using FaaSr.  What to do about met_inflow_ensembles?
future::plan("future::multisession", workers = parallel::detectCores())
bench::bench_time(
  furrr::future_walk(1:config$nmembers,
                     .f = function(m, met_inflow_ensembles, config, output_path)
                     {
                       run_ensemble(m, met_inflow_ensembles, config, output_path)
                     },
                     met_inflow_ensembles,
                     config,
                     output_path = file.path(working_directory, ensemble_path)
  )
)

clean_up_run(working_directory)
  
#This pulls together the results into a single parquet
combined_path <- "output"
combine_ensembles(ensemble_path = ensemble_path, combined_path = combined_path)


# Standardize obs and ensemble output
states_config <- read_csv(config$states_config_file, show_col_types = FALSE)

oxy <- states_config |> 
  filter(state_names == "OXY_oxy")

fdom <- states_config |> 
  filter(state_names == "OGM_doc")

prior <- arrow::open_dataset(combined_path) |>
  mutate(variable = ifelse(variable == "temp", "Temp_C_mean", variable),
         variable = ifelse(variable == "PHY_tchla", "Chla_ugL_mean", variable),
         variable = ifelse(variable == "OXY_oxy", "DO_mgL_mean", variable),
         variable = ifelse(variable == "secchi", "Secchi_m_sample", variable),
         variable = ifelse(variable == "CAR_ch4_atm", "ch4flux_umolm2s_mean", variable),
         variable = ifelse(variable == "co2_flux", "co2flux_umolm2s_mean", variable)) |>
  mutate(prediction = ifelse(variable == "DO_mgL_mean", prediction / oxy$conversion_slope,prediction),
         prediction = ifelse(variable == "ch4flux_umolm2s_mean", (prediction * 1000) / (60 * 60 * 24),prediction),
         prediction = ifelse(variable == "co2flux_umolm2s_mean", (prediction * 1000) / (60 * 60 * 24),prediction)) |>
  collect() |>
  pivot_wider(names_from = variable, values_from = prediction) |>
  mutate(fDOM_QSU_mean = (OGM_doc + OGM_docr + fdom$conversion_intercept) / fdom$conversion_slope)  |>
  select(-OGM_doc, -OGM_docr) |>
  pivot_longer(-c("depth_m", "datetime",  "ensemble"), names_to = "variable", values_to = "prediction") |>
  filter(!is.na(prediction))


obs <- read_csv(config$historical_insitu, show_col_types = FALSE) |>
  mutate(datetime = as_date(datetime)) |>
  filter(site_id == config$site_id) |>
  mutate(depth_m = ifelse(is.na(depth_m), -1, depth_m))

out <- calc_particle_weights(prior, obs, config)

prior <- out$prior
weights <- out$weights

pars <- jsonlite::read_json("pars_prior.json")
out <- generate_posteriors(prior, pars, weights)

par_posterior <- out$par_posterior
posterior <- out$posterior

forecast_df <- standardize_forecast(posterior, config)
#Note the posterior is the forecast

arrow::write_parquet(posterior, "forecast.parquet")

pars <- jsonlite::read_json("pars_prior.json")

arrow::write_parquet(par_posterior, "parameters.parquet")

## Extra stuff to visualize forecast

p <- plot_prior_post(posterior, prior, obs_df, focal_depth = 1.6)
p

p <- plot_phytos(posterior, prior, obs_df, focal_depth = 1.6)
p

p <- plot_posterior_depth(obs_df, posterior, focal_depths = config$focal_depths)
p

p <- plot_parameters(par_posterior)
p






