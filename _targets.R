
# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) 

#install.packages("crew", type="source")
# library(crew) # allows parelisation of ranking and model runs 

# Set target options:
tar_option_set(
  # controller = crew_controller_local(workers = 2),
  packages = c("tidyverse", "countrycode", "rjags")
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

games <- c("paris-2024") #, "tokyo-2020", "rio-2016", "london-2012", "beijing-2008", "athens-2004" to run analysis on teh otehr datasets


list(
  tar_map(
    values = tibble::tibble(games = games), #just here so can run for multiple games at once
    names = games,
  tar_target(
    name = medalcounts,
    command =  paste0("0_data/0_raw/Medalcounts_", games, ".csv"),
    format = "file", 
    description = "Processed medal counts specific Olympic Games"
  ), 
  tar_target(
    name = model_datalist,
    command = make_model_list(medalcounts),
    description = "Creates a list of medal and population values to be used in JAGS model"
  ),
  tar_target(
    name = jags_beta,
    command = jags_model("beta-2"), 
    description = "Selecting beta model to run analysis. Can alternatively choose `logit-normal` or `mixture-beta` - defined in `R/jags_model.R`"
  ), 
  tar_target(
    name = mcmc_beta, 
    command = jags_run(jags_beta, 
                       model_datalist),
    description = "MCMC samples from the beta model for Olympic medal probabilities"
  ), 
 
  # add in model convergence checks

  
  # actually want this as a simple shiny app so can quickly flick through trace plots etc across teh different variables
  
  tar_target(
    name = procces_mcmc_beta,
    command = tidy_mcmc(mcmc_beta,
                        medalcounts),
    description = "Combined chains of MCMC samples from the beta model Olympic medal probabilities for all countries (keeping non-medal winners for plotting later"
  ), 

  tar_target(
    name = posterior_ranks_beta,
    command = rank_mcmc(procces_mcmc_beta,
                        medalcounts),
    description = "Ranked posterior draws for medal probabilities for ONLY medal winning countries giving a distribution of ranks"
  ),  
  tar_target(
    name = probs_beta, 
    command = post_prob_summary(procces_mcmc_beta), 
    description = "Mean, median and credible intervals for posterior sampled probabilities for Paris 2024 Olympic medal probabilities for ONLY medal winning countrie"
  ),
  tar_target(
    name = ranks_beta,
    command = post_rank_rank(post_ranks = posterior_ranks_beta),
    description = "Ranked mean of posterior ranks of posterior probabilities including credible intervals for Paris 2024 Olympic medal probabilities for ONLY medal winning countries"
  ),
  tar_target(
    name = results.ranking,
    command = results(medalcounts,
                      probs = probs_beta,
                      ranks.bayesian = ranks_beta)
  ),
  tar_target(
    name= export_results,
    command = {
      output_file = paste0("0_data/1_output/BayesianRanking_", games, ".csv")
      write.csv(results.ranking,output_file)
      output_file
    },
    format = "file",
    description = "Outputing results into csv file"
  )
  )

)


