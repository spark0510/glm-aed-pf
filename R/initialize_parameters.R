initialize_parameters <- function(config){
  
  lw <- rep(NA, config$nmembers)
  Kw <- rep(NA, config$nmembers)
  Rgrowth1 <- rep(NA, config$nmembers)
  Rgrowth2 <- rep(NA, config$nmembers)
  Rgrowth3 <- rep(NA, config$nmembers)
  Fsed_oxy1 <- rep(NA, config$nmembers)
  Fsed_oxy2 <- rep(NA, config$nmembers)
  Fsed_oxy3 <- rep(NA, config$nmembers)
  Fsed_oxy4 <- rep(NA, config$nmembers)
  K_N1 <- rep(NA, config$nmembers)
  K_N2 <- rep(NA, config$nmembers)
  K_N3 <- rep(NA, config$nmembers)
  
  for(m in 1:config$nmembers){
    
    lw <- rnorm(config$nmembers, 1, 0.3)
    Kw <- runif(config$nmembers, 0.6, 0.9)
    Rgrowth1 <- rnorm(config$nmembers, 1, 0.5)
    Rgrowth2 <- rnorm(config$nmembers, 2, 0.5)
    Rgrowth3 <- rnorm(config$nmembers, 3, 0.5)
    Fsed_oxy1 <- rnorm(config$nmembers, -40, 5)
    Fsed_oxy2 <- rnorm(config$nmembers, -30, 5)
    Fsed_oxy3 <- rnorm(config$nmembers, -20, 5)
    Fsed_oxy4 <- rnorm(config$nmembers, -10, 5)
    K_N1 <- rnorm(config$nmembers, 1, 0.5)
    K_N2 <- rnorm(config$nmembers, 1, 0.5)
    K_N3 <- rnorm(config$nmembers, 1, 0.5)
  }
  pars <- list(lw = lw,
               Kw = Kw,
               Rgrowth1 = Rgrowth1, Rgrowth2 = Rgrowth2, Rgrowth3 = Rgrowth3,
               #K_N1 = K_N1, K_N2 = K_N2, K_N3 = K_N3,
               Fsed_oxy1 = Fsed_oxy1, Fsed_oxy2 = Fsed_oxy2, Fsed_oxy3 = Fsed_oxy3, Fsed_oxy4 = Fsed_oxy4)
  
  jsonlite::write_json(pars, "pars_prior.json", pretty = TRUE)
  
}