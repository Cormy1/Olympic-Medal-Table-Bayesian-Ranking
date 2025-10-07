# Plot with arrows showing direction and magnitude of shrinkage imposed by bayesian ranking algorithm

shrinkage_plot <- function(results, # Results of Baysian ranking 
                           var.games,  # games to be plotted
                           countries.of.interest = NA,
                           plot_nonmedals = FALSE, # False if you want only medallists plotted
                           epsilon = 5e-7, # control minimum amount of change in per cap to be indicated as an arrow
                           output.folder 
                           ){

  #Data set for plotting
  plot_df <- results %>% mutate(
    # Clasifying direction of shrinkage for colours on the plot
    change_category = case_when(
      abs(estimate_mpm - observed_mpm) < epsilon ~ "No Change",
      estimate_mpm > observed_mpm ~ "Increase",
      TRUE ~ "Decrease"
  )
  )
  
  selected_countries = c(countries.of.interest, arrange(results, rank_mean_beta)[1:10,]$country)
  
  flags.df <- plot_df %>%
    filter(medal_total > 0) %>%
    mutate(year = as.numeric(stringr::str_extract(games, "\\d{4}")),
                  iso_a2 = countrycode::countrycode(iso_a3, "iso3c", "iso2c"),
                  iso_a2 = case_when(
                  country == "Kosovo" ~ "xk",
                  TRUE ~ tolower(iso_a2)
                   ),
      processed_flag = paste0("0_data/0_raw/country_flags/processed/", iso_a2, ".png")
    )
  
  plot_df2 <- plot_df
  
  if (plot_nonmedals == FALSE){ # only plotting non-medallists to inspect shrinkage
    plot_df <- plot_df %>% 
      filter(medal_winner == "medal_winner") 
    
    p <- ggplot(plot_df, aes(x = population))+
      #' Credible intervals of posterior estimates of medals per million
      geom_errorbar( 
        aes(ymin = estimate_mpm_credlow, 
            ymax = estimate_mpm_credhigh),
        linewidth = 0.2,
        width = 0.05,
        color = "gray60",
        alpha = 0.7
      ) +
      geom_segment( 
        data = filter(plot_df, 
                      change_category != "No Change"), # only for countries with "suitable" amount of change
        aes(xend = population, 
            yend = estimate_mpm, 
            y = observed_mpm, 
            color = medal_type),
        arrow = arrow(type = "closed", length = unit(0.08, "inches"), angle = 20)
      ) +
      geom_point(
        data = filter(plot_df, 
                      change_category == "No Change"),
        aes(y = observed_mpm, 
            color = medal_type),
        size = 1.5
      ) +
    # geom_image(
    #   data = filter(flags.df, country %in% selected_countries, slug_game == "paris-2024"),
    #   aes(x = total_pop_july, y = estimate_mpm*(0.8), image = here(processed_flag)),
    #   size = 0.04
    # ) +
    #'Better colours here
    scale_color_manual(
      values = c("Multi-medal winners" = "#911eb4",
                 "Single medal winners" = "#eebc00"),
      labels = c("Multi-medal winners" = "Some multi-medallists", 
                 "Single medal winners" = "Single/team medal winners only")
    )
  }
  else{ # plotting all comering countries to inspect shrinkage
  
plot_df <- plot_df2
    
    p <- ggplot(plot_df, aes(x = population))+
      #' Credible intervals of posterior estimates of medals per million
      geom_errorbar( data = filter(plot_df, medal_winner == "medal_winner"),
        aes(ymin = estimate_mpm_credlow, 
            ymax = estimate_mpm_credhigh),
        linewidth = 0.2,
        width = 0.05,
        color = "gray60",
        alpha = 0.7
      ) +
      geom_segment(data = filter(plot_df, medal_winner == "medal_winner"),
        aes(xend = population, 
            yend = estimate_mpm, 
            y = observed_mpm, 
            color = medal_type),
        arrow = arrow(type = "closed", length = unit(0.08, "inches"), angle = 20)
      )+
      geom_segment(data = filter(plot_df, medal_winner == "non_medal_winner"),
                   aes(xend = population, 
                       yend = estimate_mpm, 
                       y = 0, 
                       color = medal_type),
                   arrow = arrow(type = "closed", length = unit(0.08, "inches"), angle = 20)
      )+
      scale_color_manual(
        values = c("Multi-medal winners" = "#911eb4",
                   "Single medal winners" = "#eebc00", 
                   "Non-medalist" = "#1199ee"),
        labels = c("Multi-medal winners" = "Some multi-medallists", 
                   "Single medal winners" = "Single/team medal winners only", 
                   "Non-medalist" = "No medal winners :(")
      )
    
  }

  #Adding final touches to plot
 p <- p+  scale_x_log10(
   breaks = trans_breaks("log10", function(x) 10^x),
   labels = comma_format()      # shows 1,000 instead of 1e3
 ) +
   scale_y_log10(
     breaks = trans_breaks("log10", function(x) 10^x),
     labels = function(x) sprintf("%.3f", x)
   ) +
   labs(
     x = "Population",
     y = "Medals per Million",
     color = "Medallist Types"
   )+
   # ggtitle(var.games)+ 
    theme_bw() +
    theme(legend.position = c(0.98, 0.98),
          legend.justification = c("right", "top"),
          legend.background = element_blank(),         
          legend.key = element_blank(),                
          legend.title = element_blank(),               
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()
          )
  
  output_file <- paste0("2_Figures/", output.folder, "/Shrinkage_", var.games, ".pdf")
  ggplot2::ggsave(output_file, plot = p, width = 6.5, height = 4.5, dpi = 300)
  
  }