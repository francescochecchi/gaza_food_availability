...............................................................................
### ++ ANALYSIS OF CALORIC AVAILABILITY AND FOOD DIVERSITY IN GAZA (2024) ++ ###
#...............................................................................

#...............................................................................
## ----------- R SCRIPT TO PROCESS AND AGGREGATE NON-PUBLIC DATA ------------ ##
#...............................................................................


#...............................................................................  
### Preparing the 2020 adult NCD survey dataset
#...............................................................................

  #...................................      
  ## Read and recode variables

    # Identify file name
    filename <- paste0(dir_path, 'in/', 
      "gaza_survey2020_kcal_bmi_wt_ht_wc.dta", sep="")
  
    # Read dataframe
    df <- data.frame(haven::read_dta(filename))
    
    # Select necessary variables
    df <- df[, c("DEM04", "DEM06", "HT_avg", "WT_avg", "BMI", "energy")]

    # Rename variables
    colnames(df) <- c("sex", "age", "height", "weight", "bmi", "intake")
    
    # Check missingness and delete missing records
    prop.table(table(complete.cases(df)))
    df <- df[complete.cases(df), ]    
        
    # Add age categories
    df$age_cat <- cut(df$age, breaks = c(40, 50, 60, 70, 120), 
      include.lowest = T, right = F, labels = c("40 to 49yo", "50 to 59yo",
        "60 to 69yo", ">=70yo"))

    # Recode sex
    df$sex <- ifelse(df$sex == 1, "male", "female")

    # Log intake
    df$intake_log <- log(df$intake)
   
  #...................................      
  ## Aggregate dataset: means by age category, sex
    
    # Generate a weight for each individual (1/n), used later for averaging
    df$n_obs <- 1
    df$svy_wt <- 1/nrow(df)
    
    # Aggregate all variables by age category and sex (means)
    df_agg <- aggregate(df[, c("age", "weight", "height", "intake", 
      "intake_log")], by = df[, c("sex", "age_cat")], FUN = mean)
    
    # Add number of observations
    x <- aggregate(list(n_obs = df$n_obs), 
      by = df[, c("sex", "age_cat")], FUN = sum)
    df_agg <- merge(df_agg, x, by = c("age_cat", "sex"), all.x = T)
    
    # Add log intake standard deviation
    x <- aggregate(list(intake_log_sd = df$intake_log), 
      by = df[, c("sex", "age_cat")], FUN = sd)
    df_agg <- merge(df_agg, x, by = c("age_cat", "sex"), all.x = T)
    
    # Add category weights
    x <- aggregate(list(svy_wt = df$svy_wt), 
      by = df[, c("sex", "age_cat")], FUN = sum)
    df_agg <- merge(df_agg, x, by = c("age_cat", "sex"), all.x = T)

    # Save database
    df_agg <- df_agg[order(df_agg$age_cat, df_agg$sex), ]
    write.csv(df_agg, paste0(dir_path,
      "out/gaza_survey2020_kcal_bmi_agg.csv"), row.names = F)
    
    
  #...................................      
  ## Visualise distribution of intake by age and sex
    
    # Untransformed
    pl1 <- ggplot(df, aes(x = intake, colour = age_cat)) +
      geom_density(linewidth = 1) +
      theme_bw() +
      facet_grid(sex ~ age_cat) +
      scale_color_viridis_d("age") +
      scale_x_continuous("intake (Kcal)") +
      theme(legend.position = "none", plot.margin = margin(1,0,0,0, unit="cm"))
    pl2 <- ggplot(df, aes(x = intake_log, colour = age_cat)) +
      geom_density(linewidth = 1, linetype = "11") +
      theme_bw() +
      facet_grid(sex ~ age_cat) +
      scale_color_viridis_d("age") +
      scale_x_continuous("log intake (Kcal)") +
      theme(legend.position = "none", plot.margin = margin(1,0,0,0, unit="cm"))
    ggarrange(pl1, pl2, labels = c("untransformed", "log-transformed"),
      ncol = 1, nrow = 2, font.label = list(size = 10))    
    ggsave(paste0(dir_path, "out/99_adult_intake.png"),
      dpi = "print", units = "cm", height = 20, width = 20)
        
    

#...............................................................................  
### ENDS
#...............................................................................
       