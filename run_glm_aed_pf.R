working_directory <- here::here()
setwd(working_directory)
purrr::walk(list.files("R", recursive = TRUE, full.names = TRUE), function(file){source(file)})
library(tidyverse)
#Step 1: Set up input files for ensemble drivers
#Step 2: Generate parameter distributions and assign to ensemble members
#Step 3: Create matrix to store output and weights
#Step 4: Generate initial nmls for each ensemble member
#Step 5: Loop over ensemble member and run GLM-AED

config <- read_configuration(file = "configuration.yaml")

initialize_parameters(config) ## FaaSr - this function generates a json that needs to be copied json to S3


met_inflow_ensembles <- generate_inputs(config) ## FaaSr - copy met and inflow csvs to S3


initialize_states(config) ## FaaSr - this function generates a json that needs to be copied json to S3


dir.create(file.path(working_directory, "ensemble_output"), showWarnings = FALSE)
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
                     output_path = file.path(working_directory, "ensemble_output")
  )
)

clean_up_run(working_directory)
  
#This pulls together the results
dir.create("output",showWarnings = FALSE)
arrow::open_dataset("ensemble_output/") |> arrow::write_dataset("output")
unlink("ensemble_output", recursive = TRUE)

# Standardize obs and ensemble output
obs <- read_csv(config$historical_insitu, show_col_types = FALSE) |>
  mutate(datetime = as_date(datetime)) |>
  filter(site_id == config$site_id) |>
  mutate(depth_m = ifelse(is.na(depth_m), -1, depth_m))

prior <- arrow::open_dataset("output/") |>
  mutate(variable = ifelse(variable == "temp", "Temp_C_mean", variable),
         variable = ifelse(variable == "PHY_tchla", "Chla_ugL_mean", variable),
         variable = ifelse(variable == "OXY_oxy", "DO_mgL_mean", variable),
         variable = ifelse(variable == "secchi", "Secchi_m_sample", variable),
         variable = ifelse(variable == "CAR_ch4_atm", "ch4flux_umolm2s_mean", variable),
         variable = ifelse(variable == "co2_flux", "co2flux_umolm2s_mean", variable)) |>
  mutate(prediction = ifelse(variable == "DO_mgL_mean", prediction/1000*(32),prediction),
         prediction = ifelse(variable == "ch4flux_umolm2s_mean", (prediction * 1000) / (60 * 60 * 24),prediction),
         prediction = ifelse(variable == "co2flux_umolm2s_mean", (prediction * 1000) / (60 * 60 * 24),prediction)) |>
  collect() |>
  pivot_wider(names_from = variable, values_from = prediction) |>
  mutate(fDOM_QSU_mean = (OGM_doc + OGM_docr + 151.3407) / 29.62654)  |>
  select(-OGM_doc, -OGM_docr) |>
  pivot_longer(-c("depth_m", "datetime",  "ensemble"), names_to = "variable", values_to = "prediction") |>
  filter(!is.na(prediction))

out <- calc_particle_weights(prior, obs, config)

prior <- out$prior
weights <- out$weights

pars <- jsonlite::read_json("pars_prior.json")
out <- generate_posteriors(prior, pars, weights)

par_posterior <- out$par_posterior
posterior <- out$posterior
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






