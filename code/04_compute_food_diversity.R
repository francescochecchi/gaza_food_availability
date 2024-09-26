#...............................................................................
### ++ ANALYSIS OF CALORIC AVAILABILITY AND FOOD DIVERSITY IN GAZA (2024) ++ ###
#...............................................................................

#...............................................................................
## --- R SCRIPT TO AGGREGATE DAILY TRUCKED-IN Kcal AND KG FOOD BY CATEGORY -- ##
#...............................................................................


#...............................................................................  
### Estimating Kg and caloric contribution from different food types
      # mean only; no random error, for simplicity
#...............................................................................

  #...................................      
  ## Preparatory steps

    # Generate new dataset of [f]ood [t]ypes
    df_ft <- df_tr
    
    # Attribute relative weight to each item in multi-item trucks
      # assume equal weights
    x <- sapply(all_tr$n_items, function(x) {rep(1/x, x)})
    df_ft$wt_rel <- as.vector(unlist(x))

    # Restrict dataset to food items only
    df_ft <- subset(df_ft, any_food)
    
    # Compute Kcal and Kg of each item
    df_ft$kg_food <- df_ft$kg_truck * df_ft$wt_rel
    df_ft$kcal_item <- df_ft$kg_food * df_ft$kcal_kg
    df_ft$kcal_item <- na.replace(df_ft$kcal_item, 0)
    
  #...................................      
  ## Unpack food parcels to feature individual items, if known
    
    # For each organisation...
    for (i in c("PRCS", "WCK|ANERA", "WFP", "UNRWA")) {
    
      # subset observations
      x1 <- which(grepl(i, df_ft$sender) & df_ft$item %in% 
        c("food parcels", "food baskets", "food cartons", 
          "ration packs", "ready", "ready meals", "ready to eat food"))
      
      # identify the right parcel(s)
      parcel <- subset(parcels, agency == i & type == "trucked")
      
      # compute relative weight of each parcel item
        # multiple versions: assume same proportions
      parcel$rel_wt <- parcel$kg / sum(parcel$kg)
      
      # produce substitute dataset
      x3 <- data.frame()
      for (j in 1:nrow(parcel)) {
        x <- df_ft[x1, ]
        x$item <- parcel[j, "item"]
        x$kg_food <- x$kg_food * parcel[j, "rel_wt"]
        x$kcal_kg <- parcel[j, "kcal_kg"]
        x$kcal_item <- x$kg_food * x$kcal_kg
        x3 <- rbind(x3, x)
      }
      
      # substitute mixed parcels with specific-item dataset
      df_ft <- df_ft[-x1, ]
      df_ft <- rbind(df_ft, x3)
    }
    
    # Re-add food types
    df_ft <- subset(df_ft, select = -food_cat)
    df_ft <- merge(df_ft, kcal_equi[, c("item", "food_cat")], by = "item",
      all.x = T)
 
    
  #...................................      
  ## Aggregate to daily quantities and add specialised food
    
    # Aggregate to daily, by food type
    out_ft <- aggregate(df_ft[, c("kg_food", "kcal_item")],
      by = df_ft[, c("date", "food_cat")], FUN = sum)
    colnames(out_ft) <- c("date", "food_cat", "kg_day", "kcal_day")
    
    # Append specialised food
    x <- df_th[, c("date", "kcal_day", "kg_day")]
    x$food_cat <- "specialised"
    out_ft <- rbind(out_ft, x[, colnames(out_ft)])

    # Add missing dates
    x <- expand.grid(date = as.Date(date_start:date_end), 
      food_cat = unique(out_ft$food_cat))
    out_ft <- merge(x, out_ft, by = c("date", "food_cat"), all.x = T)
    out_ft$kg_day <- na.replace(out_ft$kg_day, 0)
    out_ft$kcal_day <- na.replace(out_ft$kcal_day, 0)


#...............................................................................  
### ENDS
#...............................................................................    
    