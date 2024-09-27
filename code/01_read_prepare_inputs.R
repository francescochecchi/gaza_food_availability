#...............................................................................
### ++ ANALYSIS OF CALORIC AVAILABILITY AND FOOD DIVERSITY IN GAZA (2024) ++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO READ DATASETS AND PARAMETERS AND MANAGE DATASETS  ----- ##
#...............................................................................


#...............................................................................  
### Reading in data and/or setting parameters
#...............................................................................

  #...................................      
  ## Read data and parameters from each worksheet of main data file

    # Identify file name
    filename <- paste0(dir_path, "in/gaza_food_data.xlsx")
    
    # Read each worksheet
    for (i in excel_sheets(filename)) {
      assign(i, data.frame(read_excel(filename, sheet = i)))}

    # General parameters: read each as its own object, with the right format
    for (i in 1:nrow(general)) {
      if (grepl("date", general[i, "parameter"])) {
        assign(general[i, "parameter"], as.Date(general[i, "value"], "%d%b%Y") )
      } 
      else {
        assign(general[i, "parameter"], as.numeric(general[i, "value"]))
      }
    }

    # Fix dates
    for (i in excel_sheets(filename)) {
      df <- get(i)
      x <- grep("date|month_start", colnames(df), value = T)
      if (length(x) == 0) {next} 
      for (j in x) {
        df[, j] <- suppressWarnings(as.Date(df[, j], "%Y-%m-%d"))
      }
      assign(i, df)
    }
    
  #...................................      
  ## Set or reshape parameters for household composition and Kcal requirements 
    
    # Identify proportion of pregnant and lactating women
    prop_preg <- prop_age_sex[which(prop_age_sex$age == "pregnant"), "female"]
    prop_lact <- prop_age_sex[which(prop_age_sex$age == "lactating"), "female"]
    
    # Convert age-sex proportion to long
    prop_age_sex <- reshape(
      subset(prop_age_sex, ! age %in% c("pregnant", "lactating") ), 
      direction = "long", varying = c("male", "female"), 
      times = c("male", "female"), v.names = "prop", idvar = "age", 
      timevar = "sex")
    prop_age_sex$id <- 1:nrow(prop_age_sex)
    
    # Identify extra intake requirement of pregnant and lactating women
    req_preg <- intake_req[which(intake_req$age == "pregnant"), "female"]
    req_lact <- intake_req[which(intake_req$age == "lactating"), "female"]
    
    # Convert age-sex intake requirement to long
    intake_req <- reshape(
      subset(intake_req, ! age %in% c("pregnant", "lactating") ), 
      direction = "long", varying = c("male", "female"), 
      times = c("male", "female"), v.names = "req", idvar = "age", 
      timevar = "sex")

    # Merge with age-sex proportion
    x <- prop_age_sex
    x$age_cat <- x$age
    x[which(x$id %in% c(1:3, 20:22)), "age_cat"] <- "0 to 4yo"
    x[which(x$id %in% c(7:14, 26:33)), "age_cat"] <- "20 to 59yo"
    x[which(x$id %in% c(15:19, 34:38)), "age_cat"] <- "60 to 100yo"
    x <- aggregate(list(prop = x$prop), by = x[, c("age_cat", "sex")], sum)
    colnames(x)[1] <- "age"
    hh_pars <- merge(intake_req[, c("age", "sex", "req")], x, 
      by = c("age", "sex"), all.x = T)
    hh_pars$age <- factor(hh_pars$age, levels = c("0 to 4yo", "5 to 9yo", 
      "10 to 14yo", "15 to 19yo", "20 to 59yo", "60 to 100yo"))
    hh_pars <- hh_pars[order(hh_pars$age, hh_pars$sex), ]
    hh_pars$id <- 1:nrow(hh_pars)
    
    # Parameter values among adults only
    hh_pars_ad <- hh_pars
    hh_pars_ad$prop <- ifelse(!hh_pars_ad$age 
      %in% c("20 to 59yo", "60 to 100yo"), 0, hh_pars_ad$prop)
    hh_pars_ad$prop <- hh_pars_ad$prop / sum(hh_pars_ad$prop)

    # Identify maximum household size
    max_size <- max(hh_size$n_members)


  #...................................      
  ## Read in or set other parameters

    # Identify number of months to date
    months_todate <- as.integer((date_end - date_crisis) / 30.44)
    
    # Identify food types
    food_types <- sort(unique(kcal_equi$food_cat))
    
    # Figure out caloric value of different food parcels
    parcels$agency <- ifelse(parcels$agency == "WCK", "WCK|ANERA", 
      parcels$agency)
    parcels$kcal <- parcels$kg * parcels$kcal_kg
    parcels_agg <- aggregate(parcels[, c("kg", "kcal")],
      by = parcels[, c("agency", "version", "type")], FUN = sum)
    parcels_agg$kcal_kg <- parcels_agg$kcal / parcels_agg$kg
    

  #...................................      
  ## Read and prepare COGAT aid flow dataset
    
    # Read file
    cogat <- read.csv(paste0(dir_path, "in/gaza_cogat_data_24sep2024.csv"))   

    # Rename variables
    colnames(cogat) <- c("id", "date", "route", "classification", "donor", "mt")

    # Fix dates
    cogat$date <- as.Date(cogat$date, format = "%d %b %Y")
    
    # Fix MT variable
    cogat[which(cogat$mt == "-"), "mt"] <- 0 # assume 0 as opposed to NA
    cogat$mt <- as.numeric(cogat$mt)
    
    # Fix route categories
    cogat[which(cogat$route == "Aerial Route"), "route"] <- "air"
    cogat[which(cogat$route == "Land Crossings"), "route"] <- "truck"
    cogat[which(cogat$route == "Maritime Route (JLOTS)"), "route"] <- "sea"
        
#...............................................................................  
### Preparing various ancillary datasets
#...............................................................................

  #...................................
  ## Aggregate 2020 survey data containing [ad]ult BMI and diet intake

    # Add proportion of population in each group
    x <- prop_age_sex
    x[which(x$id %in% c(11:12, 30:31)), "age_cat"] <- "40 to 49yo"
    x[which(x$id %in% c(13:14, 32:33)), "age_cat"] <- "50 to 59yo"
    x[which(x$id %in% c(15:16, 34:35)), "age_cat"] <- "60 to 69yo"
    x[which(x$id %in% c(17:19, 36:38)), "age_cat"] <- ">=70yo"
    x <- aggregate(list(prop = x$prop), by = x[, c("age_cat", "sex")], sum)
    df_ad <- merge(adult_intake, x, by = c("age_cat", "sex"), all.x = T)

    # Fix survey weights to account for population age-sex share of each group
    df_ad$prop_40plus <- df_ad$prop / sum(df_ad$prop)
    df_ad$svy_wt <- df_ad$prop_40plus / df_ad$svy_wt

    # Add recommended intake requirement by age-sex
    x <- intake_req
    colnames(x)[1] <- "age_broad"
    df_ad$age_broad <- ifelse(df_ad$age >= 60, "60 to 100yo", "20 to 59yo")
    df_ad <- merge(df_ad, x, by = c("age_broad", "sex"), all.x = T)
   
     
  #...................................      
  ## Generate dataset of population in the North vs south-central over time
    
    # Interpolate to whole timeline
    df_po <- data.frame(date = as.Date(date_start : date_end),
      pop_no = NA, pop_so = NA)
    df_po$pop_no <- approx(x = pop_north$date, y = pop_north$pop_north, 
      xout = df_po$date, rule = 2)$y
    df_po$pop_so <- pop - df_po$pop_no

  #...................................      
  ## Trucks to northern Gaza

    # Compute mean daily number of trucks per time segment
    df_no <- trucks_to_north
    df_no$n_trucks_day <- df_no$n_trucks / 
      (as.integer(df_no$date_2 - df_no$date_1) + 1)
    
    # Expand dataset to daily
    x <- data.frame(date = as.Date(date_start : date_end), 
      n_food_trucks = NA)
    for (i in 1:nrow(df_no)) {
      x[which(x$date %in% as.Date(df_no[i, "date_1"] : df_no[i, "date_2"])),
        "n_food_trucks"] <- df_no[i, "n_trucks_day"]  
    }
    df_no <- x
    colnames(df_no) <- c("date", "n_food_trucks_no")
    df_no$n_food_trucks_no <- na.replace(df_no$n_food_trucks_no, 0)
      
  #...................................      
  ## Specialised food deliveries

    # Compute mean daily Kcal from different specialised food types
    df_th <- specialised_food
    df_th$kcal_day <- (df_th$quantity * df_th$kcal_unit) / 
      (as.integer(df_th$date_2 - df_th$date_1) + 1)
    
    # Compute mean daily Kg from different specialised food types
    df_th$kg_day <- (df_th$quantity * df_th$kg_unit) / 
      (as.integer(df_th$date_2 - df_th$date_1) + 1)

    # Expand dataset to daily
    x <- data.frame(date = as.Date(date_start:date_end))
    x[, paste(sort(unique(df_th$food_type)), "kcal", sep = "_")] <- 0
    x[, paste(sort(unique(df_th$food_type)), "kg", sep = "_")] <- 0

    for (i in 1:nrow(df_th)) {
      x[which(x$date %in% as.Date(df_th[i, "date_1"] : df_th[i, "date_2"])),
        paste(df_th[i, "food_type"], "kcal", sep = "_")] <- df_th[i, "kcal_day"]
      x[which(x$date %in% as.Date(df_th[i, "date_1"] : df_th[i, "date_2"])),
        paste(df_th[i, "food_type"], "kg", sep = "_")] <- df_th[i, "kg_day"]  
    }
    
    # Compute total RUTF Kcal contribution
    x$kcal_day <- 
      rowSums(x[, paste(sort(unique(df_th$food_type)), "kcal", sep = "_")])
    
    # Compute total weight in Kg
    x$kg_day <- rowSums(x[, paste(sort(unique(df_th$food_type)), "kg",sep="_")])
    
    # New dataset    
    df_th <- x    
      
      
  #...................................      
  ## Airdrop and boat deliveries

    # Select variables
    df_ab <- boats_airdrops
    df_ab <- df_ab[, c("date", "area", "kcal")]
    
    # Aggregate by date and area
    df_ab <- aggregate(list(kcal = df_ab$kcal), by = df_ab[, c("date", "area")],
      FUN = sum)
    
  #...................................      
  ## Warehouse stocks at baseline
    
    # Aggregate by area
    df_wh <- warehouses
    df_wh <- aggregate(df_wh[, c("kcal_min", "kcal_max")], 
      by = list(area = df_wh$location, agency = df_wh$agency), FUN = sum)
    
  #...................................      
  ## Proportion of private food stores that were functional
    
    # Interpolate proportion of functional stores until date when stocks run out
    df_pr <- private_stores
    x1 <- data.frame()
    for (i in c("north", "south-central")) {
      x <- subset(df_pr, location == i)
      x <- approx(x = x$date, y = x$prop_operational, 
        xout = as.Date(date_crisis:date_stores_end))
      x <- data.frame(x)
      x$area <- i
      x1 <- rbind(x1, x)
    }
    colnames(x1) <- c("date", "prop_operational", "area")
    
    # New dataframe
    df_pr <- x1
    df_pr <- df_pr[order(df_pr$area, df_pr$date), ]
    
  #...................................      
  ## Proportion of households not destroyed
    
    # Interpolate proportion of non-destroyed households until dataset end date
    df_dh <- hh_destroyed
    x1 <- data.frame()
    for (i in c("north", "south-central")) {
      x <- subset(df_dh, location == i)
      x <- approx(x = x$date, y = x$prop_destroyed, 
        xout = as.Date(date_crisis:max(x$date)))
      x <- data.frame(x)
      x$area <- i
      x1 <- rbind(x1, x)
    }
    colnames(x1) <- c("date", "prop_destroyed", "area")
    x1$prop_ok <- 1 - x1$prop_destroyed
    
    # New dataframe
    df_dh <- x1
    df_dh <- df_dh[order(df_dh$area, df_dh$date), ]
    
     
#...............................................................................  
### ENDS
#...............................................................................
     