#...............................................................................
### ++ ANALYSIS OF CALORIC AVAILABILITY AND FOOD DIVERSITY IN GAZA (2024) ++ ###
#...............................................................................

#...............................................................................
## ---- R SCRIPT TO ESTIMATE PER-CAPITA CALORIC AVAILABILITY BY STRATUM ----- ##
#...............................................................................


#...............................................................................  
### Preparing inputs for simulation to estimate with uncertainty
#...............................................................................

  #...................................      
  ## Calculate some necessary static quantities

    # Calculate mean Kcal requirement for Gaza population
    req_gaza <- weighted.mean(hh_pars$req, hh_pars$prop) + 
      prop_lact * req_lact + prop_preg * req_preg
    
    # Calculate the pre-war amount of required Kcal/day met through food aid
      # UNRWA refugees: 620,310 got 1675 Kcal/day, 389,680 got 902 Kcal/day
      # 20 % (all non-refugees) received WFP assistance in 4 categories, assumed
      # equally split, with distributions every 3 months
    kcal_unrwa <- 620310 * 1675 + 389680 * 902
    kcal_wfp <- (4 / 365) * (pop / mean_hh_size) * (
      (0.05 * parcels_agg[which(parcels_agg$agency == "WFP" & 
        parcels_agg$version == 1), "kcal"]) +
      (0.05 * parcels_agg[which(parcels_agg$agency == "WFP" & 
        parcels_agg$version == 2), "kcal"]) +
      (0.05 * parcels_agg[which(parcels_agg$agency == "WFP" & 
        parcels_agg$version == 3), "kcal"]) +
      (0.05 * parcels_agg[which(parcels_agg$agency == "WFP" & 
        parcels_agg$version == 4), "kcal"]) )
    kcal_met <- kcal_unrwa + kcal_wfp

    # Compute relative share of food aid recipients - North vs south-central
    prop_reliant$pop <- prop_reliant$pop_share * pop
    prop_reliant$recipients <- prop_reliant$pop *
      prop_reliant$received_food_parcels
    prop_reliant_agg <- aggregate(prop_reliant[, c("pop", "recipients")],
      by = list(area = prop_reliant$location), FUN = sum)
    prop_reliant_agg$prop <- prop_reliant_agg$recipients / 
      sum(prop_reliant_agg$recipients)
    
    # Distribute Kcal food aid to North and south-central
    prop_reliant_agg$kcal_met <- kcal_met * prop_reliant_agg$prop

  #...................................      
  ## Initialise objects for simulation
    
    # Number of runs
    n_runs <- 100

    # Sort truck database
    df_tr <- df_tr[order(df_tr$truck_id), ]
    
    # Unique list of trucks and their number of items
    all_tr <- aggregate(list(n_items = df_tr$n_items), by = list(truck_id = 
      df_tr$truck_id), FUN = sum)
        
    # Loop progress bar   
    pb <- txtProgressBar(min = 1, max = n_runs, style = 3)


  #...................................      
  ## Initialise timeline x run output and populate it with static quantities
    
    # Output per day, per run
    out <- expand.grid(run = 1:n_runs, date = as.Date(date_start:date_end))

    # Add population of North and South-Central
    out <- merge(out, df_po, by = "date", all.x = T)

    # Add daily trucks to North from within Gaza
    out <- merge(out, df_no, by = "date", all.x = T)
    out$n_food_trucks_no <- na.replace(out$n_food_trucks_no, 0)
      
    # Add airdrops and boat deliveries
    x <- df_ab[which(df_ab$area == "north"), c("date", "kcal")]
    colnames(x) <- c("date", "airboat_no")
    out <- merge(out, x, by = "date", all.x = T)
    out$airboat_no <- na.replace(out$airboat_no, 0)
    x <- df_ab[which(df_ab$area == "south-central"), c("date", "kcal")]
    colnames(x) <- c("date", "airboat_so")
    out <- merge(out, x, by = "date", all.x = T)
    out$airboat_so <- na.replace(out$airboat_so, 0)

    # Add specialised food
    x <- df_th[, c("date", "kcal_day")]
    colnames(x) <- c("date", "specialised")
    out <- merge(out, x, by = "date", all.x = T)
    out$specialised <- na.replace(out$specialised, 0)

    # Initialise other variables
    x <- c("pantry", "warehouses", "stocks", "agri", "trucked", "kcal_in")
    out[, paste(x, "no", sep = "_")] <- NA
    out[, paste(x, "so", sep = "_")] <- NA
    out[, grep("pantry", colnames(out))] <- 0
    out[, grep("warehouses", colnames(out))] <- 0
    
    # Initialise starting balance and consumption
    out[, c("kcal_bal_no", "kcal_out_no", "kcal_bal_so", "kcal_out_so")] <- NA 
    
#...............................................................................  
### Implementing simulation
#...............................................................................

for (i in 1:n_runs) {

  #...................................      
  ## Preparatory steps
    
    # Update progress bar
    setTxtProgressBar(pb, i)
  
    # Which rows of output file to affect  
    out_i <- out[which(out$run == i), ]
      
    # Generate random values
      # random pallet error
      pallet_error <- pallet_error_min + (1 - runif(1)) * 
        (pallet_error_max - pallet_error_min)
        
      # random proportions coming from agriculture / livestock
      prop_agri$prop <- prop_agri$min + runif(1) * (prop_agri$max - 
        prop_agri$min)    

      # random values of Kcal in WFP warehouses at start
      df_wh$rand_kcal <- (df_wh$kcal_min + runif(1) * 
        (df_wh$kcal_max - df_wh$kcal_max))

      # mean kcal intake before the war, based on data of 40+yo's
  
        # sample from logged distributions of intake
        df_ad$intake_rand <- exp(qnorm(runif(nrow(df_ad)), df_ad$intake_log, 
          df_ad$intake_log_sd))
        
        # compute ratio of actual to recommended intake
        x <- weighted.mean(df_ad$intake_rand, df_ad$prop_40plus) /
          weighted.mean(df_ad$req, df_ad$prop_40plus)
      
        # apply this ratio to general intake
        act_gaza <- req_gaza * x

  #...................................      
  ## Simulate amount of Kcal trucked in

    # Fresh version of trucking database for the run
    df_tr_i <- df_tr
      
    # Random relative weight contribution of each item, for multi-item trucks
    x <- sapply(all_tr$n_items, function(x) {diff(sort(c(0, runif(x - 1), 1)))})
    df_tr_i$wt_rel <- as.vector(unlist(x))

    # Apply random error in pallet Kg equivalent
    x <- which(df_tr_i$units == "Pallets")
    df_tr_i[x, "kg_truck"]  <- df_tr_i[x, "kg_truck"] * pallet_error
    
    # Compute Kcal and Kg of each item
    df_tr_i$kg_item <- df_tr_i$kg_truck * df_tr_i$wt_rel
    df_tr_i$kg_food <- ifelse(df_tr_i$any_food, df_tr_i$kg_item, 0)
    df_tr_i$kcal_item <- df_tr_i$kg_food * df_tr_i$kcal_kg
    df_tr_i$kcal_item <- na.replace(df_tr_i$kcal_item, 0)

    # Aggregate to daily output
    x <- aggregate(list(trucked = df_tr_i$kcal_item), 
      by = list(date = df_tr_i$date), FUN = sum)
    colnames(x) <- c("date", "trucked_in")
    x$trucked_in <- na.replace(x$trucked_in, 0)

#### ADDITION FOR THIS BRANCH:
    # Inflate total assuming underreporting after 6 May 2024
    x[which(x$date >= as.Date("2024-05-06")), "trucked_in"] <-
      x[which(x$date >= as.Date("2024-05-06")), "trucked_in"] / (1 - under)
####    
    
    # Add trucking data
    out_i <- merge(out_i, x, by = "date", all.x = T)
    out_i <- out_i[order(out_i$date), ]
    
    # Add specialised food to trucked totals, since it is also trucked in
    out_i$trucked_in <- out_i$trucked_in + out_i$specialised
    
    # Distribute trucked Kcal to north and south-central 
    out_i$trucked_no <- sapply(out_i$n_food_trucks_no, function(xx) 
      {sum(sample(kcal_tr$kcal_truck, xx, replace = T))})
    out_i$trucked_so <- out_i$trucked_in - out_i$trucked_no
    out_i$trucked_so <- na.replace(out_i$trucked_so, 0)

  #...................................      
  ## Simulate Kcal daily contributions from non-trucking sources
    
    # Compute unmet Kcal need in North vs south-central
    prop_reliant_agg$kcal_unmet <- act_gaza * prop_reliant_agg$pop - 
      prop_reliant_agg$kcal_met
    prop_reliant_agg$prop_unmet <- prop_reliant_agg$kcal_unmet / 
      (act_gaza * prop_reliant_agg$pop)
    
    # Existing stocks in households from food aid, 
      # adjusted for loss due to household destruction
    x <- as.Date(date_crisis:(date_crisis + 90))
    out_i[which(out_i$date %in% x), "pantry_no"] <- 
      df_dh[which(df_dh$area == "north" & df_dh$date %in% x), "prop_ok"] * 
      (kcal_unrwa + kcal_wfp) * runif(1) * 
      prop_reliant_agg[which(prop_reliant_agg$area == "north"), "prop"]
    out_i[which(out_i$date %in% x), "pantry_so"] <- 
      df_dh[which(df_dh$area == "south-central" & df_dh$date %in% x), 
      "prop_ok"] * (kcal_unrwa + kcal_wfp) * runif(1) * 
      prop_reliant_agg[which(prop_reliant_agg$area == "south-central"), "prop"]
    
    # # Existing stocks in households from private stores, 
    #   # adjusted for loss due to household destruction
    # x <- as.Date(date_crisis:(date_crisis+60))
    # out_i[which(out_i$date %in% x), "pantry_no"] <- 
    #   out_i[which(out_i$date %in% x), "pantry_no"] +
    #   df_dh[which(df_dh$area == "north" & df_dh$date %in% x), "prop_ok"] * 
    #   prop_reliant_agg[which(prop_reliant_agg$area == "north"), "kcal_unmet"] *
    #   runif(1)
    # out_i[which(out_i$date %in% x), "pantry_so"] <- 
    #   out_i[which(out_i$date %in% x), "pantry_so"] +
    #   df_dh[which(df_dh$area == "south-central" & df_dh$date %in% x), "prop_ok"] * 
    #   prop_reliant_agg[which(prop_reliant_agg$area == "south-central"), "kcal_unmet"] *
    #   runif(1)
    
    # Initial warehouse stocks
    out_i[which(out_i$date == date_crisis), "warehouses_no"] <- 
      stock_unrwa *
      prop_reliant_agg[which(prop_reliant_agg$area == "north"), "prop"] +
      df_wh[which(df_wh$agency == "WFP" & df_wh$area == "north"), "rand_kcal"]
    out_i[which(out_i$date == date_crisis), "warehouses_so"] <- 
      stock_unrwa *
      prop_reliant_agg[which(prop_reliant_agg$area == "south-central"), "prop"]+
      df_wh[which(df_wh$agency == "WFP" & df_wh$area == "south-central"), 
        "rand_kcal"]

    # Existing market stocks
    out_i[which(out_i$date %in% as.Date(date_crisis:date_stores_end)), 
      "stocks_no"] <- 
      act_gaza * df_pr[which(df_pr$area == "north"), "prop_operational"] *
      prop_reliant_agg[which(prop_reliant_agg$area == "north"), "pop"] *
      prop_reliant_agg[which(prop_reliant_agg$area == "north"), 
        "prop_unmet"]
    out_i$stocks_no <- na.replace(out_i$stocks_no, 0)
    out_i[which(out_i$date %in% as.Date(date_crisis:date_stores_end)), 
      "stocks_so"] <- 
      act_gaza * df_pr[which(df_pr$area == "south-central"),"prop_operational"]*
      prop_reliant_agg[which(prop_reliant_agg$area == "south-central"), "pop"] *
      prop_reliant_agg[which(prop_reliant_agg$area == "south-central"), 
        "prop_unmet"]
    out_i$stocks_so <- na.replace(out_i$stocks_so, 0)
    
    # From agriculture / livestock
    out_i$agri_no <- act_gaza * 
      prop_reliant_agg[which(prop_reliant_agg$area == "north"), "pop"] *
      prop_agri[findInterval(out_i$date, prop_agri$month_start), "prop"]
    out_i$agri_so <- act_gaza * 
      prop_reliant_agg[which(prop_reliant_agg$area == "south-central"), "pop"] *
      prop_agri[findInterval(out_i$date, prop_agri$month_start), "prop"]

  #...................................      
  ## Compute daily Kcal coming in, consumed and remaining balance; add to output
    
    # Total Kcal coming in
    out_i$kcal_in_no <- rowSums(out_i[, paste(c("pantry", "warehouses", 
      "stocks", "agri", "trucked", "airboat"), "no", sep = "_")], na.rm = T)
    out_i$kcal_in_so <- rowSums(out_i[, paste(c("pantry", "warehouses", 
      "stocks", "agri", "trucked", "airboat"), "so", sep = "_")], na.rm = T)
    out_i <- out_i[order(out_i$date), ]
    
    # For each successive day...
    for(j in 1:nrow(out_i)) {
      # first day...
      if (j == 1) {
        out_i[j, "kcal_bal_no"] <- out_i[j, "kcal_in_no"]
        out_i[j, "kcal_out_no"] <- min(out_i[j, "kcal_bal_no"], 
          out_i[j, "pop_no"] * act_gaza)
        out_i[j, "kcal_bal_so"] <- out_i[j, "kcal_in_so"]
        out_i[j, "kcal_out_so"] <- min(out_i[j, "kcal_bal_so"], 
          out_i[j, "pop_so"] * act_gaza)
      }

      # second day...
      if (j == 2) {
        out_i[j, "kcal_bal_no"] <- out_i[j-1, "kcal_in_no"] - 
          out_i[j-1, "kcal_out_no"] 
        out_i[j, "kcal_out_no"] <- min(out_i[j, "kcal_bal_no"], 
          out_i[j, "pop_no"] * act_gaza)
        out_i[j, "kcal_bal_so"] <- out_i[j-1, "kcal_in_so"] - 
          out_i[j-1, "kcal_out_so"] 
        out_i[j, "kcal_out_so"] <- min(out_i[j, "kcal_bal_so"], 
          out_i[j, "pop_so"] * act_gaza)
      }
              
      # subsequent days...
      if (j > 2) {
        out_i[j, "kcal_bal_no"] <- out_i[j-1, "kcal_bal_no"] - 
          out_i[j-1, "kcal_out_no"] + out_i[j-1, "kcal_in_no"]
        out_i[j, "kcal_out_no"] <- min(out_i[j, "kcal_bal_no"], 
          out_i[j, "pop_no"] * act_gaza)
        out_i[j, "kcal_bal_so"] <- out_i[j-1, "kcal_bal_so"] - 
          out_i[j-1, "kcal_out_so"] + out_i[j-1, "kcal_in_so"]
        out_i[j, "kcal_out_so"] <- min(out_i[j, "kcal_bal_so"], 
          out_i[j, "pop_so"] * act_gaza)
      }
    }  

    # Add to output
    out[which(out$run == i), ] <- out_i[, colnames(out)]
    
} # close i loop
close(pb)    


#...............................................................................  
### Estimating daily Kcal available per capita
#...............................................................................

  #...................................      
  ## Generate daily smoothed per capita Kcal available estimates for each run

    # Compute per-capita daily Kcal intake
    out$kcal_capita_no <- out$kcal_out_no / out$pop_no
    out$kcal_capita_so <- out$kcal_out_so / out$pop_so

    # Reshape long
    x <- out[, c("date", "run", "kcal_capita_no", "kcal_capita_so")]
    kcal_capita <- reshape(x, direction = "long", 
      varying = c("kcal_capita_no", "kcal_capita_so"), 
      idvar = c("date", "run"), timevar = "area",
      times = c("north", "south-central"), v.names = "kcal_capita")    

    # Compute 7-day running mean to smooth over extreme outliers
    kcal_capita <- kcal_capita[order(kcal_capita$area, kcal_capita$run,
      kcal_capita$date), ]
    x <- by(kcal_capita, list(run = kcal_capita$run, area = kcal_capita$area), 
      function(x) {rollmean(x$kcal_capita, k = 7, align = "right", 
        fill = "extend")})
    kcal_capita$kcal_capita_roll <- as.vector(unlist(x))
    
    # Add weeks
    weekno <- as.numeric(kcal_capita$date - date_crisis) %/% 7
    x <- date_crisis + 7 * 0:max(weekno)
    kcal_capita$week <- x[findInterval(kcal_capita$date, x)]

    # Save output
    saveRDS(kcal_capita, paste0(dir_path, "out/03_kcal_capita_runs.rds"))
    
    
  #...................................      
  ## Compute weekly estimates and 80% / 95% confidence intervals by area
    
    # Aggregate by week
    kcal_wk <- aggregate(list(kcal_capita = kcal_capita$kcal_capita_roll),
      by = kcal_capita[, c("week", "area", "run")], mean)
        
    # Compute confidence intervals for all runs
    kcal_wk <- aggregate(list(kcal_capita = kcal_wk$kcal_capita),
      by = kcal_wk[, c("week", "area")], quantile, 
      probs = c(0.5, 0.025, 0.975, 0.10, 0.90))
    kcal_wk <- data.frame(kcal_wk$week, kcal_wk$area, 
      data.frame(kcal_wk$kcal_capita))
    colnames(kcal_wk) <- c("week","area","est","lci95","uci95","lci80","uci80")

#### ALTERATION FOR THIS BRANCH:        
    # Save output
    saveRDS(kcal_wk,paste0(dir_path,"out/03_kcal_capita_by_area_",under,".rds"))

####
  #...................................      
  ## Compute weekly estimates and 80% / 95% confidence intervals for all of Gaza
    
    # Aggregate simulations
    out_all <- out
    out_all$kcal_out <- out_all$kcal_out_no + out_all$kcal_out_so
    out_all$pop <- out_all$pop_no + out_all$pop_so
    out_all <- aggregate(out_all[, c("kcal_out", "pop")],
      by = out_all[, c("date", "run")], sum)
    
    # Compute per-capita daily Kcal intake
    out_all$kcal_capita <- out_all$kcal_out / out_all$pop

    # Compute 7-day running mean to smooth over extreme outliers
    out_all <- out_all[order(out_all$run, out_all$date), ]
    x <- by(out_all, out_all$run, 
      function(x) {rollmean(x$kcal_capita, k = 7, align = "right", 
        fill = "extend")})
    out_all$kcal_capita_roll <- as.vector(unlist(x))
    
    # Add weeks
    weekno <- as.numeric(out_all$date - date_crisis) %/% 7
    x <- date_crisis + 7 * 0:max(weekno)
    out_all$week <- x[findInterval(out_all$date, x)]

    # Aggregate by week
    kcal_wk_all <- aggregate(list(kcal_capita = out_all$kcal_capita_roll),
      by = out_all[, c("week", "run")], mean)
        
    # Compute confidence intervals for all runs
    kcal_wk_all <- aggregate(list(kcal_capita = kcal_wk_all$kcal_capita),
      by = list(week = kcal_wk_all$week), quantile, 
      probs = c(0.5, 0.025, 0.975, 0.10, 0.90))
    kcal_wk_all <- data.frame(kcal_wk_all$week,
      data.frame(kcal_wk_all$kcal_capita))
    colnames(kcal_wk_all) <- c("week","est","lci95","uci95","lci80","uci80")
    
    # Save output
    saveRDS(kcal_wk_all, paste0(dir_path, "out/03_kcal_capita_all_gaza.rds"))

 
             
#...............................................................................  
### ENDS
#...............................................................................    
