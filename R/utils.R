
#' 1) Function to turn the suitably structured dataset into a list object for jags mcmc sampling
make_model_list <- function(medalcounts){
  
  # makes list of medal wins for jags model 
  
  df <- read.csv(medalcounts)
  
  current_df <- df %>% filter(competed == TRUE) # countries that competed at edition of Olympic games
  
  m <- current_df$Medals.unique
  m1 <- as.numeric(unlist(current_df$Medals.1.team))
  m2 <- as.numeric(unlist(current_df$Medals.2))
  m3 <- as.numeric(unlist(current_df$Medals.3))
  m4 <- as.numeric(unlist(current_df$Medals.4))
  population <- as.numeric(unlist(current_df$total_pop_july))
  n_country <- nrow(current_df)
  
  medal_data_list <- list(
    M = m,
    M1 = m1,
    M2 = m2,
    M3 = m3,
    M4 = m4,
    N = population,
    n = n_country
  )
  
  medal_data_list
}


#' 2) Function to run JAGS mcmc smapling - adjust as needed
jags_run <- function(jags, #jags model to be run
                     model_data.list, # list of medals and populations
                     burn.in = 15000,
                     model.run = 250000,
                     var.names = c("a", "b", "p1", "p2", "p3", "p4", "X1", "X2", "X3"),# variables to keep track of in mcmc
                     #logit-normal/log-normal, c("mu", "tau","p", "p1", "p2", "p3", "p4")
                     #mixed-beat, c("w","a","b", "p1", "p2", "p3", "p4", "z")
                     inits = list(
                       list(a = 0.1419, b = 739338) #initial values chosen to be cntered around global average medal winning rate to help speed up model convergence
                       
                       
                     )
                     # init_mix <- function() {
                     #   list(
                     #     u = runif(3, 0.1, 0.9),
                     #     log_b = runif(3, 5, 12),
                     #     X1 = runif(1, 0.3, 0.7),
                     #     X2 = runif(1, 0.3, 0.7),
                     #     X3 = runif(1, 0.3, 0.7),
                     #     w = as.numeric(rdirichlet(1, rep(1, 3))),
                     #     z = sample(1:3, n, replace = TRUE)
                     #   )
                     # }
){
  
  data <- model_data.list[c("M1", "M2", "M3", "M4",  "N", "n")]
  
  model.jags <- jags.model(textConnection(jags), 
                           data = data, 
                           n.chains = 1, #change to 4 for convergence checks
                           inits = inits)
  
  model.sim <- update(model.jags, n.iter = burn.in) 
  
  
  model.sim <- coda.samples(model = model.jags, 
                            variable.names = var.names,
                            n.iter = model.run,
                            thin = 25) 
  model.sim # outputs conditonal simulation
}


# 3) Function to tidy mcmc chains to go forward with analysis once satisfied convergence has been reached
tidy_mcmc <- function(mcmc, 
                      medalcounts){
  
  medalcounts <- read.csv(medalcounts)
  
  country_data <- medalcounts  %>%
    filter(competed == TRUE)
  
  # Posterior samples 
  chain_dfs <- lapply(mcmc, as.data.frame)
  psims <- do.call(rbind, chain_dfs)
  
  p1 <- psims[, grep("p1\\[", colnames(psims))]  # Probability of winning exactly 1 medal
  p2 <- psims[, grep("p2\\[", colnames(psims))]  # Probability of winning exactly 2 medals
  p3 <- psims[, grep("p3\\[", colnames(psims))]  # Probability of winning exactly 3 medals
  p4 <- psims[, grep("p4\\[", colnames(psims))]  # Probability of winning exactly 4 medals
  
  post_pc <- p1 + 2 * p2 + 3 * p3 + 4 * p4 # samples of posterior medals per capita for each country 
  
  colnames(post_pc) <- country_data$iso_a3  #adding identifier to samples
  
  # #identifying non-medal winning countries
  # non_medals <- as.vector(country_data%>%
  #                           mutate(medal_winner = Medals.total > 0)%>%
  #                           filter(medal_winner==FALSE)%>%
  #                           select(iso_a3))
  # 
  # # Non-medal winners have done their job in informing mcmc smapling - now wnat to remove
  # # idx <- colnames(post_pc) %in% non_medals$iso_a3
  # # post_pc[, idx] <- NA_real_
  # 
  # #is this not doing the smae thing??? - need to refresh why i did this
  # post_pc[colnames(post_pc) %in% non_medals$iso_a3] <- NA # setting nonmedal winning countries to 0 probability so as not to affect rank 
  
  post_pc
}


# 4) Function to rank each posterior draw 
rank_mcmc <- function(processed_mcmc,
                      medalcounts # for removing non-medal winners for ranking
                      ){
  
  medalcounts <- read.csv(medalcounts)
  
  country_data <- medalcounts  %>%
    filter(competed == TRUE)
  
  non_medals <- as.vector(country_data%>%
                          mutate(medal_winner = Medals.total > 0)%>%
                          filter(medal_winner==FALSE)%>%
                          select(iso_a3))


processed_mcmc[colnames(processed_mcmc) %in% non_medals$iso_a3] <- NA # setting nonmedal winning countries to 0 probability so as not to affect rank
  
  # ranking medal winners
  t(apply(processed_mcmc, 1, function(x) {
    rank(-x, ties.method = "average", na.last = "keep")
  }))
}

# 5) Function to summarise posterior draws for estimating mean and median posterior probabilities and credible intervals
post_prob_summary <- function(proccesed_mcmc){
  # calculating the median posterior probability for each medal winning country
  median_p <- apply(proccesed_mcmc, 2, median, na.rm = TRUE)
  # calculating the mean posterior probability for each medal winning country
  mean_p <- apply(proccesed_mcmc, 2, mean, na.rm = TRUE)
  # calculating the 95% credible intervals for each medal winning country
  cred_p = apply(proccesed_mcmc, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE) 
  
  prob_sums <- data.frame(iso_a3 = names(proccesed_mcmc), 
                          p_mean = mean_p,
                          p_median = median_p,
                          p_credlow = cred_p[1,],
                          p_credhigh = cred_p[2,])
}

# 6) Ranking the distribution of ranked posterior draws on mean and median and calculating associated credible intervals
post_rank_rank <- function(post_ranks){
  
  post <- as.data.frame(post_ranks) %>%
    select(where(~ !any(is.na(.x))))
  # calculating the mean of the posterior distribution of ranks for each country
  average_ranks <- apply(post, 2, mean,  na.rm = TRUE)
  # calculating the median of the posterior distribution of ranks for each country
  median_ranks <- apply(post, 2, median, na.rm = TRUE)
  # calculating the 95% credible interval of the posterior distribution of ranks for each country
  cred_ranks <- apply(post, 2, quantile, probs = c(0.025, 0.975),  na.rm = TRUE)
  
  
  
  ranksums <-data.frame(iso_a3 = names(post),
                        rank_mean = average_ranks,
                        rank_median = median_ranks,
                        rank_credlow = cred_ranks[1,],
                        rank_credhigh = cred_ranks[2,])%>%
    mutate(rank_mean_beta = rank(rank_mean),
           rank_median_beta = rank(rank_median))
  
  ranksums
}

# 7) Function to produce a df of all the results and summaries of the Bayesian ranking algorithm
results <- function(medalcounts,
                    probs.bayesian, 
                    ranks.bayesian){
  
  medalcounts <- read.csv(medalcounts)
  country_data <- medalcounts %>%  
    filter(competed == TRUE)%>%
    mutate(medal_type = case_when(
      is.na(Medals.total) ~ "Non-medalist",
      Medals.total == 0 ~ "Non-medalist", 
      Medals.total != Medals.1.team ~ "Multi-medal winners",
      TRUE ~ "Single medal winners"
    ),
    medal_winner = if_else(
      replace_na(Medals.total, 0) > 0,
      "medal_winner",
      "non_medal_winner"
    ))%>%
    select(country,
           games = slug_game,
           iso_a3, 
           population = total_pop_july,
           medal_type,
           medal_winner,
           medal_total = Medals.total,
           medals_1 = Medals.1.team,
           medals_2 = Medals.2, 
           medals_3 = Medals.3, 
           medals_4 = Medals.4, 
           medals_5 = Medals.5, # Wasn't any here but will leave for future proofing 
           medals_1.team = Medals.team) %>%# Recorded and modelled as single medals but were won in a team event
    arrange(medal_total)%>%
    mutate(rank_total = row_number())
  
  df1 <- left_join(country_data, probs.bayesian, by = "iso_a3")
  
  df2 <- left_join(df1, ranks.bayesian, by = "iso_a3")
  
  df2 %>% mutate(observed_mpm = ifelse(medal_total > 0, (medal_total / population) * 1e6, 0),
                 estimate_mpm =  (p_median * 1e6), #change to p_mean if you prefer to use mean of posterior probability estimates
                 estimate_mpm_credlow = p_credlow*1e6,
                 estimate_mpm_credhigh = p_credhigh*1e6
  ) %>%
    mutate(rank_pc = rank(-observed_mpm, ties.method = "first", na.last = "keep"))
  
  
  
}