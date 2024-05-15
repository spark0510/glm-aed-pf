working_directory <- here::here()
setwd(working_directory)
source("R/run_ensemble.R")
source("R/update_configs.R")
source("R/initialize_states.R")
source("R/generate_inputs.R")
source("R/initialize_parameters.R")
library(tidyverse)
#Step 1: Set up input files for ensemble drivers
#Step 2: Generate parameter distributions and assign to ensemble members
#Step 3: Create matrix to store output and weights
#Step 4: Generate initial nmls for each ensemble member
#Step 5: Loop over ensemble member and run GLM-AED
future::plan("future::multisession", workers = 10)

config <- list()
config$nmembers <- 100
config$focal_vars <- c("temp","PHY_tchla", "OXY_oxy", "OGM_doc", "OGM_docr")
config$focal_depths <- c(0.1, 1.6, 5, 9)
config$forecast_start_datetime = as_datetime("2021-07-01")
config$look_back = 20
config$horizon = 30
config$start_datetime = config$forecast_start_datetime - days(config$look_back)
config$end_datetime = config$forecast_start_datetime + days(config$horizon)
config$site_id <- "fcre"
config$met_bucket <- "bio230121-bucket01/flare/drivers/met/gefs-v12"
config$met_endpoint <- "renc.osn.xsede.org"
config$historical_inflow <- "fcre-targets-inflow.csv"
config$historical_met <- NULL
config$historical_insitu <- "https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz"
config$glm_output_time_step <- 12
config$nsamples <- 100
config$obs_names <- c("Temp_C_mean", "Chla_ugL_mean", "DO_mgL_mean", "Secchi_m_sample", "fDOM_QSU_mean")
config$obs_sd <- c(0.5, 2, 2.5, 1.0, 1000)

initialize_parameters(config)
## FaaSr - copy json to S3

met_inflow_ensembles <- generate_inputs(config)

## FaaSr - copy met and inflow csvs to S3

### Initial conditions from data
initialize_states(config)


dir.create(file.path(working_directory, "ensemble_output"), showWarnings = FALSE)
bench::bench_time(
  furrr::future_walk(1:config$nmembers,
                     .f = function(m, met_inflow_ensembles, config)
                       {
                       run_ensemble(m, met_inflow_ensembles, config)
                       },
                     met_inflow_ensembles,
                     config
  )
)

unlink(list.files(working_directory, pattern = "glm3-", full.names = TRUE))
unlink(list.files(working_directory, pattern = "WQ-", full.names = TRUE))
unlink(list.files(working_directory, pattern = "lake-", full.names = TRUE))
unlink(list.files(working_directory, pattern = "aed2-", full.names = TRUE))
unlink(list.files(working_directory, pattern = "aed_phyto_pars-", full.names = TRUE))

dir.create("output",showWarnings = FALSE)
arrow::open_dataset("ensemble_output/") |> arrow::write_dataset("output")
unlink("ensemble_output", recursive = TRUE)

## Post processing of 

obs <- read_csv(config$historical_insitu, show_col_types = FALSE) |>
  mutate(datetime = as_date(datetime)) |>
  filter(site_id == config$site_id) |>
  mutate(depth_m = ifelse(is.na(depth_m), -1, depth_m))

obs_sd <- tibble(variable = config$obs_names,
                 sd = config$obs_sd)

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
  filter(!is.na(prediction)) |>
  left_join(obs, by = join_by(datetime, depth_m, variable)) |>
  left_join(obs_sd, by = join_by(variable)) |>
  mutate(depth_m = ifelse(depth_m == -1, NA, depth_m)) |>
  mutate(type = "1-prior") |>
  select(-project_id, -duration, -site_id)

ndays <- length(unique(prior$datetime))
weights <- prior |>
  filter(datetime < config$forecast_start_datetime,
         !is.na(observation),
         !is.na(sd)) |>
  mutate(logLL = dnorm(x = prediction, mean = observation, sd = sd, log= TRUE)) |>
  #summarize(sum_logLL = sum(logLL)/n(), count = n(), .by = "ensemble") |>
  summarize(sum_logLL = sum(logLL)/10, count = n(), .by = "ensemble") |>
  mutate(relative_weight = exp(sum_logLL) / sum(exp(sum_logLL))) |>
  arrange(ensemble)

nsamples <- 100
samples <- sample(1:nrow(weights), size = nsamples, replace = TRUE, prob = weights$relative_weight)

posterior <- map_dfr(1:nsamples, function(i, samples, prior){
  sample_df <-  prior |>
    filter(ensemble == samples[i]) |>
    mutate(ensemble = i,
           type = "2-posterior")
},
samples,
prior
)

obs_df <- posterior |>
  select(variable, observation, depth_m, datetime) |>
  distinct() |>
  filter(depth_m == 1.6 | is.na(depth_m),
         !is.na(observation))

bind_rows(posterior, prior) |>
  filter(depth_m == 1.6 | is.na(depth_m)) |>
  mutate(ensemble = paste0(ensemble,type)) |>
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = prediction, group = ensemble)) +
  geom_point(data = obs_df, aes(x = datetime, y = observation), color = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = as_date(config$forecast_start_datetime))) +
  facet_grid(variable~type, scales = "free_y")

obs_df <- posterior |>
  select(variable, observation, depth_m, datetime) |>
  distinct() |>
  filter(!is.na(observation),
         depth_m %in% c(0.1, 1.6, 5.0, 9.0) | is.na(depth_m))

posterior |>
  filter(depth_m %in% c(0.1, 1.6, 5.0, 9.0) | is.na(depth_m)) |>
  ggplot(aes(x = datetime)) +
  geom_line(aes(y = prediction, group = ensemble)) +
  geom_point(data = obs_df, aes(x = datetime, y = observation), color = "red", alpha = 0.5) +
  geom_vline(aes(xintercept = as_date(config$forecast_start_datetime))) +
  facet_grid(variable~depth_m, scales = "free_y")

## Parameters
par_posterior <- map_dfr(1:length(names(pars)), function(i, samples, pars, nmembers, nsamples){
  tmp_df <- tibble(par = names(pars)[i],
                   type = c(rep("1-prior",config$nmembers), rep("2-post",nsamples)),
                   value = c(unlist(pars[i][]), unlist(pars[i])[samples]))
},
samples,
pars,
nmembers = config$nmembers,
nsamples
)

ggplot(par_posterior, aes(x = value, color = type)) +
  geom_density() +
  facet_wrap(~par, scales = "free")






