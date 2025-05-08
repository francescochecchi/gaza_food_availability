#...............................................................................
### ++ ANALYSIS OF CALORIC AVAILABILITY AND FOOD DIVERSITY IN GAZA (2024) ++ ###
#...............................................................................

#...............................................................................
## ------------------- R SCRIPT TO VISUALISE FINDINGS ----------------------- ##
#...............................................................................


#...............................................................................  
### Visualising quantity of food trucked in
#...............................................................................

  #...................................      
  ## Aggregate data as needed
        
    # Aggregate data by truck
    df_all <- aggregate(df_tr[, c("kg_item","kg_food","kcal_item","n_items", 
      "any_food")], by = df_tr[, c("date", "truck_id")], FUN = sum)
    df_all$any_food <- (df_all$any_food > 0)
    df_all$no_food <- (df_all$any_food == 0)
    
    # Add week
    weekno <- as.numeric(kcal_capita$date - date_crisis) %/% 7
    x <- date_crisis + 7 * 0:max(weekno)
    df_all$week <- x[findInterval(df_all$date, x)]
    
    # Aggregate by week
    df_all$n_trucks <- 1
    df_all <- aggregate(df_all[, c("n_trucks", "kg_item", "kg_food", "kcal_item",
      "any_food", "no_food")], by = list(week = df_all$week), sum)
    df_all <- merge(data.frame(week = x), df_all, by = "week", all.x = T)
    for (i in c("n_trucks", "kg_item", "kg_food", "kcal_item", "any_food", 
      "no_food")) {df_all[, paste0(i, "_d")] <- df_all[, i] / 7}
    df_all[is.na(df_all)] <- 0

  #...................................      
  ## Visualise daily number of food-carrying trucks
    
    # Prepare data
    df <- reshape(df_all[, c("week", "any_food_d", "no_food_d")], 
      direction = "long", varying = c("any_food_d", "no_food_d"),
      idvar = "week", timevar = "category", times = c("some food", "no food"),
      v.names = "number")
    df$category <- factor(df$category, levels = c("no food", "some food"))
    df$lab <- NA
    df[which(df$category == "some food"), "lab"] <- 
      scales::percent(df_all$kg_food_d / df_all$kg_item_d, 1)
    df$period <- ifelse(df$week < as.Date("2024-05-04"), 
      "UNRWA (pre-Rafah operation", "UNRWA (post-Rafah operation)")

    # Plot
    ggplot(df, aes(x = week, y = number, colour = category, fill = category,
      alpha = period)) +
      geom_bar(position = "stack", stat = "identity", just = 0,
        width = 7) +
      geom_text(label = df$lab, size = 2.5, hjust = 0, nudge_y= -3, nudge_x=0.5,
        colour = "white", alpha = 1, fontface = "bold") +
      scale_y_continuous("mean number of trucks per day", expand = c(0,0),
        limits = c(0, 250)) +
      scale_x_date("week starting", breaks = unique(df$week),
        limits = c(date_start, date_end), expand = c(0,0),
        date_labels = "%d-%b-%Y") +
      geom_vline(xintercept = as.Date("2024-05-06"), linetype = "11", 
        colour = palette_gen[1]) +
      annotate("text", x = as.Date("2024-05-10"), y = 240,
        label = "UN no longer able to monitor truck deliveries", hjust = 0,
        colour = palette_gen[1], size = 3) +
      scale_colour_manual("content of truck", values = palette_gen[c(13,7)]) +    
      scale_fill_manual("content of truck", values = palette_gen[c(13,7)]) +    
      scale_alpha_manual("period", values = c(0.50, 0.75)) +
      theme_bw() +
      theme(legend.position = "inside", legend.position.inside = c(0.1, 0.9), 
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm")) +
      guides(alpha = "none")
    ggsave(paste0(dir_path, "out/05_trends_trucks.png"),
      dpi = "print", units = "cm", height = 18, width = 28)  

    
  # #...................................      
  # ## Visualise daily MT trucked in, by food vs. non-food
  #   
  #   # Prepare data
  #   df <- reshape(df_all[, c("week", "kg_item_d", "kg_food_d")], 
  #     direction = "long", varying = c("kg_item_d", "kg_food_d"),
  #     idvar = "week", timevar = "category", times = c("food", "non-food"),
  #     v.names = "mt")
  #   df$category <- factor(df$category, levels = c("non-food", "food"))
  #   df$mt <- df$mt / 1000
  #          
  #   # Plot
  #   ggplot(df, aes(x = week, y = mt, colour = category, fill = category)) +
  #     geom_bar(position = "stack", stat = "identity", alpha = 0.25, just = 0,
  #       width = 7) +
  #     scale_y_continuous("mean number of metric tons per day", expand = c(0,0),
  #       limits = c(0, 5500), breaks = seq(0, 6000, 500)) +
  #     scale_x_date("week starting", breaks = unique(df$week),
  #       limits = c(date_start, date_end), expand = c(0,0),
  #       date_labels = "%d-%b-%Y") +
  #     geom_vline(xintercept = as.Date("2024-05-06"), linetype = "11", 
  #       colour = palette_gen[1]) +
  #     annotate("text", x = as.Date("2024-05-10"), y = 5200,
  #       label = "UN no longer able to monitor truck deliveries", hjust = 0,
  #       colour = palette_gen[1], size = 3) +
  #     scale_colour_manual("category", values = palette_gen[c(13,7)]) +    
  #     scale_fill_manual("category", values = palette_gen[c(13,7)]) +    
  #     theme_bw() +
  #     theme(legend.position = "top", panel.grid.minor.x = element_blank(),
  #       axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
  #       plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"))
  #   ggsave(paste0(dir_path, "out/05_trends_mt_trucked.png"),
  #     dpi = "print", units = "cm", height = 18, width = 28) 
    

#...............................................................................  
### Visualising diversity and caloric value of food trucked in
#...............................................................................
    
  #...................................      
  ## Visualise weekly mean Kcal per Kg of food
    
    # Prepare data
    df_all$kcal_kg <- df_all$kcal_item / df_all$kg_food
    
    # Plot
    pl1 <- ggplot(df_all, aes(x = week, y = kcal_kg)) +
      geom_step(alpha = 0.5, colour = palette_gen[5], linewidth = 1) +
      scale_y_continuous("mean Kcal per Kg food", expand = c(0,0),
        limits = c(2800, 3500), breaks = seq(1600, 4200, 100)) +
      scale_x_date("week starting", breaks = unique(df$week),
        limits = c(date_start, as.Date("2024-05-06")), expand = c(0,0),
        date_labels = "%d-%b-%Y") +
      # geom_vline(xintercept = as.Date("2024-05-06"), linetype = "11", 
      #   colour = palette_gen[1]) +
      # annotate("text", x = as.Date("2024-05-10"), y = 4000,
      #   label = "UN no longer able to monitor truck deliveries", hjust = 0,
      #   colour = palette_gen[1], size = 3) +
      theme_bw() +
      theme(panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.margin = margin(0.5,0.5,0.5,0.5, unit = "cm"))
    ggsave(paste0(dir_path, "out/05_trends_kcal_kg.png"),
      dpi = "print", units = "cm", height = 18, width = 22)  
    
    
  #...................................      
  ## Visualise weekly mean relative Kg food weight share of different food types
    
    # Prepare data
    weekno <- as.numeric(kcal_capita$date - date_crisis) %/% 7
    x <- date_crisis + 7 * 0:max(weekno)
    out_ft$week <- x[findInterval(out_ft$date, x)]
    
    # Aggregate by week
    df <- aggregate(list(kg_cat = out_ft$kg_day), 
      by = out_ft[, c("week", "food_cat")], sum)
    x <- aggregate(list(kg_tot = out_ft$kg_day), 
      by = list(week = out_ft$week), FUN = sum)
    df <- merge(df, x, by = "week", all.x = T)
    df$prop <- df$kg_cat / df$kg_tot
    df$food_cat <- factor(df$food_cat, levels = c("cereals and baked goods",
      "dairy", "fats and oils", "fruit and vegetables", "meat, fish and eggs",
      "pulses", "sugar and confections", "mixed", "specialised", "other"))
    
    # Plot
    pl2 <- ggplot(df, aes(x = week, y = prop, fill = food_cat)) +
      geom_bar(alpha = 0.5, position = "fill", stat = "identity", 
        colour = "grey40", linewidth = 0.25, just = 0) +
      scale_y_continuous("percentage of total food weight", 
        breaks = seq(0, 1, 0.1), labels = scales::percent, expand = c(0, 0)) +
      theme_bw() +
      scale_fill_manual("", values = c(palette_gen[c(15,1,9,4,13,6,2)],
        "azure4",palette_gen[16], "black")) +
      scale_x_date("week starting", breaks = df$week, 
        limits = c(date_start, as.Date("2024-05-06")),
        date_labels = "%d-%b-%Y", expand = c(0, 0)) +
      annotate("text", x = as.Date.character("2023-10-14"), y = 0.2,
        label = "no trucks\nduring first\n2 weeks", size = 2.5) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(), legend.position = "bottom",
        legend.title = element_blank() )
    ggsave(paste0(dir_path, "out/05_trends_food_types.png", sep=""),
      dpi = "print", units = "cm", height = 15, width = 22)  
    
  #...................................      
  ## Combined plot
    
    # Plot  
    ggarrange(pl1 + theme(axis.text.x = element_blank(), 
      axis.title.x = element_blank(), axis.ticks.x = element_blank()), 
      pl2, nrow = 2, labels = c("A", "B"), align = "v", heights = c(1, 2)) 
    
    # Save
    ggsave(paste0(dir_path, "out/05_food_types_combi.png"), 
      dpi = "print", units = "cm", height = 25, width = 22)  


#...............................................................................  
### Visualising contribution of different food sources
#...............................................................................

  #...................................      
  ## Tabulate list and frequency of individual food items
    
    # Prepare data
    df <- df_tr
    df$period <- ifelse(df$date < as.Date("2024-05-06"), 
      "before Rafah operation", "post-Rafah operation")  
    df <- subset(df, any_food)
    
    # Tabulate and save
    x <- as.data.frame.matrix(table(df$item, df$period))
    x$item <- rownames(x)
    x <- x[, c("item", unique(df$period))]
    write.csv(x, paste0(dir_path, "out/05_tab_food_items.csv"), row.names = F)
      
  #...................................      
  ## Tabulate cumulative food sources for North vs South-central
    
    # Prepare data
    x <- c("airboat", "pantry", "warehouses", "stocks", "agri", "trucked")
      # entire period
      df_base <- out[, c("run", "date", paste0(x, "_no"), paste0(x, "_so"))]

  for (i in c("whole_period", "unrwa_period")) {   

    # Reset
    df <- df_base
    x <- c("airboat", "pantry", "warehouses", "stocks", "agri", "trucked")
        
    # Restrict for UNRWA period
    if (i == "unrwa_period") {df <- subset(df, date <= as.Date("2024-05-06"))}

    # Aggregate
    df <- aggregate(df[, c(paste0(x, "_no"), paste0(x, "_so"))], 
      by = list(run = df$run), FUN = sum)
    
    # Reshape long
    df <- reshape(df, direction = "long", varying = list(paste0(x, "_no"), 
      paste0(x, "_so")), idvar = "run", v.names = c("no", "so"),
      timevar = "source", times = c("airboat", "pantry", "warehouses",
      "stocks", "agri", "trucked"))
    
    # Compute median and 95%CI
    df <- aggregate(df[, c("no", "so")], by = list(source = df$source),
      quantile, probs = c(0.50, 0.025, 0.975))
    df <- data.frame(df$source, df$no, df$so)
    colnames(df) <- c("source", "no_est", "no_lci", "no_uci",
      "so_est", "so_lci", "so_uci")
    x <- colnames(df)[2:length(colnames(df))]
    
    # Compose table
      # convert to billions of Kcal
      df[, x] <- apply(df[, x], 2, as.numeric)    
      df[, x] <- df[, x] / 1000000000
      
      # add percentages
      pcts <- as.data.frame(prop.table(as.matrix(df[, x]), 2))
      pcts <- apply(pcts, 2, scales::percent, 0.1)

      # add period totals
      df <- rbind(df, c("total", colSums(df[, x])))
      pcts <- rbind(pcts, rep("100.0%", length(x))) 
      pcts <- data.frame(pcts)

      # add confidence intervals
      df[, x] <- apply(df[, x], 2, function(xx)
        {format(round(as.numeric(xx), digits = 1), nsmall = 1)})
      df$no_ci <- ifelse(df$no_lci == df$no_est, "n/a",
        paste0(df$no_lci, " to ", df$no_uci))
      df$so_ci <- ifelse(df$so_lci == df$so_est, "n/a",
        paste0(df$so_lci, " to ", df$so_uci))

      # paste columns together
      df$all_no <- paste0(df$no_est, " (", df$no_ci, ", ", pcts$no_est, ")")
      df$all_so <- paste0(df$so_est, " (", df$so_ci, ", ", pcts$so_est, ")")
      
      # improve source labels and column names
      df$source <- factor(df$source, levels = c("agri", "airboat", "pantry",
        "stocks", "warehouses", "trucked", "total"),
        labels = c("agriculture", "air / boat drops", "household stocks",
          "market stocks", "warehouse stocks", "trucks", "total"))
      df <- df[, c("source", "all_no", "all_so")]
      colnames(df) <- c("source", "north", "south-central")
      
    # Save
    write.csv(df, paste0(dir_path, "out/05_kcal_by_source_", i, ".csv"), 
      row.names = F)      
  }      

#...............................................................................  
### Visualising daily Kcal available per capita
#...............................................................................
            
  #...................................      
  ## Visualise daily Kcal per capita for North vs south-central
    
    # Prepare data
    df <- kcal_wk
    df$period <- ifelse(df$week < as.Date("2024-05-04"), "pre", "post")
    
    # Plot
    ggplot(df, aes(x = week, y = est, colour = area, fill = area, 
      group = 1, alpha = period)) +
      geom_step(linewidth = 1) +
      geom_stepribbon(aes(ymin = lci95, ymax = uci95), alpha = 0.1, colour=NA) +
      geom_stepribbon(aes(ymin = lci80, ymax = uci80), alpha = 0.2, colour=NA) +
      geom_hline(yintercept = req_gaza, linetype = "11", linewidth = 1,
        colour = palette_gen[16]) +
      theme_bw() +
      scale_colour_manual(values = palette_gen[c(4,12)]) +
      scale_fill_manual(values = palette_gen[c(4,12)]) +
      scale_alpha_manual(values = c(0.25, 0.75)) +
      scale_x_date("week starting", breaks = unique(df$week),
        limits = c(date_crisis, date_end), expand = c(0, 0),
        date_labels = "%d-%b-%Y") +
      geom_vline(xintercept = as.Date("2024-05-06"), linetype = "11", 
        colour = palette_gen[1]) +
      annotate("text", x = as.Date("2024-05-10"), y = 200,
        label = "UN no longer able to monitor truck deliveries", hjust = 0,
        colour = palette_gen[1], size = 3) +
      facet_wrap(. ~ area, nrow = 2) +
      scale_y_continuous("Kcal per person-day", breaks = seq(000, 3800, 200),
        limits = c(0, 3800), expand = c(0,0)) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(), legend.position = "none")
    ggsave(paste0(dir_path, "out/05_trends_kcal_capita.png", sep=""),
      dpi = "print", units = "cm", height = 22, width = 22)  
    
 
#...............................................................................  
### Comparing daily Kg trucked in according to COGAT and UNRWA (whole Gaza)
#...............................................................................

  #...................................      
  ## Compute MT food trucked in according to COGAT
    
    # Subset food aid and truck route
    cogat <- subset(cogat, classification == "Food" & route == "truck")
    
    # Aggregate dataset by date
    x <- aggregate(list(mt = cogat$mt), by = list(date = cogat$date), sum)
    
    # Merge with all-date timeline
    out_cg <- data.frame(date = as.Date(date_start:date_end))
    out_cg <- merge(out_cg, x, by = "date", all.x = T)    
    out_cg$mt <- na.replace(out_cg$mt, 0)

  #...................................      
  ## Compute MT food trucked in according to UNRWA data
    
    # Tally truck data (excluding specialised food: assume also excluded from 
        # 'Food' category per COGAT)
    x <- aggregate(list(kg = df_tr$kg_food), by = list(date = df_tr$date), sum)
        
    # Merge with all-date timeline
    out_un <- data.frame(date = as.Date(date_start:date_end))
    out_un <- merge(out_un, x, by = "date", all.x = T)    
    out_un$mt <- out_un$kg / 1000
    out_un$mt <- na.replace(out_un$mt, 0)
    

  #...................................      
  ## Visualise differences
  
    # Prepare weekly dataset
    out_cg$source <- "COGAT"
    out_un$source <- "UNRWA"
    df <- rbind(out_cg, out_un[, colnames(out_cg)])
    weekno <- as.numeric(df$date - date_crisis) %/% 7
    x <- date_crisis + 7 * 0:max(weekno)
    df$week <- x[findInterval(df$date, x)]
    df <- aggregate(list(mt = df$mt), by = df[, c("week", "source")], sum) 

    # Plot both
    pl_cg1 <- ggplot(df, aes(x = week, y = mt, colour = source, 
      linetype = source)) +
      geom_step(linewidth = 0.75) +
      theme_bw() +
      scale_x_date("date", breaks = unique(df$week), expand = c(0,0),
        date_labels = "%d-%b-%Y") +
      scale_y_continuous("metric tons of food", expand = c(0,0), 
        breaks = seq(0, 100000, 5000), label = comma, limits = c(0, 40000)) +
      scale_colour_manual("source", values = palette_gen[c(6,12)]) +
      scale_linetype_discrete("source") +
      geom_vline(xintercept = as.Date("2024-05-06"), linetype = "11", 
        colour = palette_gen[1]) +
      annotate("text", x = as.Date("2024-05-10"), y = 39000,
        label = "UN no longer able to monitor truck deliveries", hjust = 0,
        colour = palette_gen[1]) +
      theme(legend.position = "top", panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    ggsave(paste0(dir_path, "out/05_cogat_unrwa_diff.png"),
      dpi = "print", units = "cm", height = 15, width = 25)  
    
  #...................................      
  ## Visualise COGAT tonnage distribution and combine plots
    
    # COGAT tonnage distribution
    pl_cg2 <- ggplot(cogat, aes(x = mt)) +
      geom_histogram(binwidth = 1, fill = palette_gen[6], 
        colour = palette_gen[6], alpha = 0.75) +
      theme_bw() +
      scale_x_continuous("metric tons per aid consignment (source: COGAT)", 
        expand = c(0,0), breaks = seq(0, 100, 5)) +
      scale_y_continuous("frequency", label = comma, expand = c(0,0),
        limits = c(0, 25000))
    ggsave(paste0(dir_path, "out/05_cogat_mt_dist.png"),
      dpi = "print", units = "cm", height = 15, width = 25)
    
    # Combination graph
    pl_cg1 + annotation_custom(ggplotGrob(pl_cg2), 
      xmin = as.Date("2023-10-15"), xmax = as.Date("2024-03-10"), 
      ymin = 25000, ymax = 39000)
    ggsave(paste0(dir_path, "out/05_cogat_unrwa_combi.png"),
      dpi = "print", units = "cm", height = 15, width = 25)
    
        

#...............................................................................  
### ENDS
#...............................................................................
    