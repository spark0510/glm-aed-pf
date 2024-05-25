generate_posteriors <- function(config, prior, pars, weights){
  nsamples <- config$nsamples
  samples <- sample(1:nrow(weights), size = nsamples, replace = TRUE, prob = weights$relative_weight)
  
  posterior <- map_dfr(1:nsamples, function(i, samples, prior){
    sample_df <-  prior |>
      filter(ensemble == samples[i]) |>
      mutate(ensemble = i,
             type = "2-posterior")
  },
  samples,
  prior)
  
  par_posterior <- map_dfr(1:length(pars), function(i, samples, pars, nmembers, nsamples){
    
    curr_pars <- matrix(unlist(pars[[i]]$value), nrow = config$nmembers)
    num_pars <- ncol(curr_pars)
    tmp_df <- NULL
    for(p in 1:num_pars){
      if(num_pars > 1){
        par_name <- paste0(unlist(pars[[i]]$par_name),"_",p)
      }else{
        par_name <- unlist(pars[[i]]$par_name)
      }
      tmp_df <- bind_rows(tmp_df, 
                          tibble(par = par_name,
                                 type = c(rep("1-prior",config$nmembers), rep("2-post",nsamples)),
                                 value = c(curr_pars[, p], curr_pars[samples, p])))
    }
    return(tmp_df)
  },
  samples,
  pars,
  nmembers = config$nmembers,
  nsamples)
  
  return(list(posterior = posterior, par_posterior = par_posterior, samples = samples))
}

