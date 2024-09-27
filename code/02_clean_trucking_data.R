#...............................................................................
### ++ ANALYSIS OF CALORIC AVAILABILITY AND FOOD DIVERSITY IN GAZA (2024) ++ ###
#...............................................................................

#...............................................................................
## ----- R SCRIPT TO CLEAN AND PREPARE DATA FROM UNRWA TRUCK MANIFESTS  ----- ##
#...............................................................................

#...............................................................................  
### Reading in and cleaning truck manifest dataset
#...............................................................................
    
  #...................................      
  ## Read and streamline data from manifests of [tr]ucks arriving into Gaza

    # Identify and read file
    x <- list.files(path = paste0(dir_path, 'in'), pattern = "Commodities")
    df_tr <- data.frame(suppressWarnings(
      readxl::read_excel(paste0(dir_path, 'in/', x[length(x)]))))
    
    # Restrict and rename columns of interest
    x <- c("Received.Date", "Status", "Cargo.Category", "Description.of.Cargo",
      "Units", "Quantity", "Donating.Country..Organization", "Data.Period")
    df_tr <- df_tr[, x]
    colnames(df_tr) <- c("date", "status", "category", "description",
      "units", "quantity", "sender", "period")

    # Fix date formats
    df_tr$date <- as.Date(df_tr$date)

    # Remove observations with status = pending and missing description
    df_tr <- subset(df_tr, status != "pending")
    df_tr <- subset(df_tr, ! is.na(description))

    # Subset observations within analysis period 
    df_tr <- df_tr[which(df_tr$date <= date_end), ]
    
    # Rename period categories
    df_tr$period <- ifelse(df_tr$period=="Current Data (after Rafah Operation)",
      "after Rafah operation", "before Rafah operation")
 
    # Generate truck id
    df_tr <- df_tr[order(df_tr$date), ]
    df_tr$truck_id <- 1:nrow(df_tr)
    
       
  #...................................      
  ## Compute total weight on truck
    
    # Fix missing / non-standard units
    table(df_tr$units, useNA = "always")
    df_tr[which(is.na(df_tr$units)), "units"] <- "Pallets"

    # Fix missing quantities, assuming all are median number of pallets
    table(df_tr$quantity, useNA = "always")
    df_tr[which(is.na(df_tr$quantity)), "quantity"] <- 
      median(df_tr[which(df_tr$units == "Pallets"), "quantity"], na.rm = T)
    
    # Compute total weight in Kg
    df_tr$kg_truck <- NA
    df_tr$kg_truck <- ifelse(df_tr$units %in% c("Truck", "CTN", "Piece", 
      "Box", "Vehicles", "Tents", "EACH"), truck_kg, df_tr$kg_truck)
    df_tr$kg_truck <- ifelse(df_tr$units %in% c("Ton", "MT"),
      df_tr$quantity * 1000, df_tr$kg_truck)
    df_tr$kg_truck <- ifelse(df_tr$units == "Pallets",
      df_tr$quantity * pallet_kg, df_tr$kg_truck)
    
       
  #...................................      
  ## Harmonise item description column
    
    # Remove extra spaces and convert to lower
    x <- "description"
    df_tr[, x] <- trimws(df_tr[, x], which = "both")
    df_tr[, x] <- tolower(df_tr[, x])
    
    # Harmonise item descriptions
    df_tr[, x] <- gsub("\\bapple\\b", "apples", df_tr[, x])
    df_tr[, x] <- gsub("\\bambulance\\b", "ambulances", df_tr[, x])
    df_tr[, x] <- gsub("\\bbed\\b", "beds", df_tr[, x])
    df_tr[, x] <- gsub("\\bbiscuit\\b", "biscuits", df_tr[, x])
    df_tr[, x] <- gsub("biscuites", "biscuits", df_tr[, x])
    df_tr[, x] <- gsub("\\bblanket\\b", "blankets", df_tr[, x])
    df_tr[, x] <- gsub("\\borange\\b", "oranges", df_tr[, x])
    df_tr[, x] <- gsub("\\bpistachio\\b", "pistachios", df_tr[, x])
    df_tr[, x] <- gsub("body bags", "bodybags", df_tr[, x])
    df_tr[, x] <- gsub("\\bpotatoes chips\\b", "chips", df_tr[, x])
    df_tr[, x] <- gsub("canned food \\(", "canned ", df_tr[, x])
    df_tr[, x] <- gsub("canned corn\037", "canned corn", df_tr[, x])
    df_tr[, x] <- gsub("canned whit beans", "canned beans", df_tr[, x])
    df_tr[, x] <- gsub("canned green beans", "canned beans", df_tr[, x])
    df_tr[, x] <- gsub("canned white beans", "canned beans", df_tr[, x])
    df_tr[, x] <- gsub("canned wihte beans", "canned beans", df_tr[, x])
    df_tr[, x] <- gsub("mixed canned meal", "canned food", df_tr[, x])
    df_tr[, x] <- gsub("canned meal", "canned food", df_tr[, x])
    df_tr[, x] <- gsub("canned green beans with meat", "canned food",df_tr[, x])
    df_tr[, x] <- gsub("canned beans with meat", "canned food",df_tr[, x])
    df_tr[, x] <- gsub("\\bbean\\b", "beans", df_tr[, x])
    df_tr[, x] <- gsub("cola drink", "soda", df_tr[, x])
    df_tr[, x] <- gsub("sages", "sage", df_tr[, x])
    df_tr[, x] <- gsub("semolina", "semolina flour", df_tr[, x])
    df_tr[, x] <- gsub("frozen corn", "corn", df_tr[, x])
    df_tr[, x] <- gsub("frozen peas", "peas", df_tr[, x])
    df_tr[, x] <- gsub("green peas", "peas", df_tr[, x])
    df_tr[, x] <- gsub("catherers", "catheters", df_tr[, x])
    df_tr[, x] <- gsub("chosodate", "chocolate", df_tr[, x])
    df_tr[, x] <- gsub("detergents", "detergent", df_tr[, x])
    df_tr[, x] <- gsub("dates paste", "dates", df_tr[, x])
    df_tr[, x] <- gsub("dukkah spice", "spices", df_tr[, x])
    df_tr[, x] <- gsub("\\bdate\\b", "dates", df_tr[, x])
    df_tr[, x] <- gsub("supply", "supplies", df_tr[, x])
    df_tr[, x] <- gsub("cleanliness", "cleaning", df_tr[, x])
    df_tr[, x] <- gsub("\\bcoffin\\b", "coffins", df_tr[, x])
    df_tr[, x] <- gsub("\\bcroissant\\b", "croissants", df_tr[, x])
    df_tr[, x] <- gsub("\\blemon\\b", "lemons", df_tr[, x])
    df_tr[, x] <- gsub("\\bsupplement\\b", "supplements", df_tr[, x])
    df_tr[, x] <- gsub("\\bvegetable\\b", "vegetables", df_tr[, x])
    df_tr[, x] <- gsub("\\begg\\b", "eggs", df_tr[, x])
    df_tr[, x] <- gsub("extra meat", "meat", df_tr[, x])
    df_tr[, x] <- gsub("mixed fruits", "fruits", df_tr[, x])
    df_tr[, x] <- gsub("mixed sweets", "candies", df_tr[, x])
    df_tr[, x] <- gsub("sweets", "candies", df_tr[, x])
    df_tr[, x] <- gsub("field hospital supplies \\(", "", df_tr[, x])
    df_tr[, x] <- gsub("flower", "flour", df_tr[, x])
    df_tr[, x] <- gsub("sage herb", "sage", df_tr[, x])
    df_tr[, x] <- gsub("sages", "sage", df_tr[, x])
    df_tr[, x] <- gsub("sardines", "sardine", df_tr[, x])
    df_tr[, x] <- gsub("shredded coconut", "coconut", df_tr[, x])
    df_tr[, x] <- gsub("\\bitem\\b", "items", df_tr[, x])
    df_tr[, x] <- gsub("\\bitemss\\b", "items", df_tr[, x])
    df_tr[, x] <- gsub("food items -", "", df_tr[, x])
    df_tr[, x] <- gsub("food items\"", "", df_tr[, x])
    df_tr[, x] <- gsub("foul", "fava beans", df_tr[, x])
    df_tr[, x] <- gsub("flour flour", "flour", df_tr[, x])
    df_tr[, x] <- gsub("ghee", "clarified butter", df_tr[, x])
    df_tr[, x] <- gsub("canned beef", "luncheon", df_tr[, x])
    df_tr[, x] <- gsub("luncheon meat", "luncheon", df_tr[, x])
    df_tr[, x] <- gsub("luncheons", "luncheon", df_tr[, x])
    df_tr[, x] <- gsub("food items \\(", "", df_tr[, x])
    df_tr[, x] <- gsub("\\bparcel\\b" , "parcels", df_tr[, x])
    df_tr[, x] <- gsub("fresh cooked", "cooked", df_tr[, x])
    df_tr[, x] <- gsub("fruits \\(", "", df_tr[, x])
    df_tr[, x] <- gsub("\\bkit\\b", "kits", df_tr[, x])
    df_tr[, x] <- gsub("\\bcan\\b", "cans", df_tr[, x])
    df_tr[, x] <- gsub("\\bjuices\\b", "juice", df_tr[, x])
    df_tr[, x] <- gsub("\\blentil\\b", "lentils", df_tr[, x])
    df_tr[, x] <- gsub("matresses", "mattresses", df_tr[, x])
    df_tr[, x] <- gsub("mattressers", "mattresses", df_tr[, x])
    df_tr[, x] <- gsub("mattressses", "mattresses", df_tr[, x])
    df_tr[, x] <- gsub("\\bmattress\\b", "mattresses", df_tr[, x])
    df_tr[, x] <- gsub("medical supplies -", "", df_tr[, x])
    df_tr[, x] <- gsub("medical supplies \\(", "", df_tr[, x])
    df_tr[, x] <- gsub("intravenous", "iv", df_tr[, x])
    df_tr[, x] <- gsub("chairss", "chairs", df_tr[, x])
    df_tr[, x] <- gsub("\\btank\\b", "tanks", df_tr[, x])
    df_tr[, x] <- gsub("\\bsuppliess\\b", "supplies", df_tr[, x])
    df_tr[, x] <- gsub("medications", "medicine", df_tr[, x])
    df_tr[, x] <- gsub("medicine supplies", "medical supplies", df_tr[, x])
    df_tr[, x] <- gsub("medicine ", "medicine", df_tr[, x])
    df_tr[, x] <- gsub("medicines", "medicine", df_tr[, x])
    df_tr[, x] <- gsub("meficines", "medicine", df_tr[, x])
    df_tr[, x] <- gsub("melons", "melon", df_tr[, x])
    df_tr[, x] <- gsub("mill", "milk", df_tr[, x])
    df_tr[, x] <- gsub("mixed food items", "food items", df_tr[, x])
    df_tr[, x] <- gsub("eat food", "food items", df_tr[, x])
    df_tr[, x] <- gsub("fast food", "food items", df_tr[, x])
    df_tr[, x] <- gsub("nylons", "nylon", df_tr[, x])
    df_tr[, x] <- gsub("\\bonion\\b", "onions", df_tr[, x])
    df_tr[, x] <- gsub("materials", "material", df_tr[, x])
    df_tr[, x] <- gsub("untensils", "utensils", df_tr[, x])
    df_tr[, x] <- gsub("\\bpad\\b", "pads", df_tr[, x])
    df_tr[, x] <- gsub("pistacchios", "pistachios", df_tr[, x])
    df_tr[, x] <- gsub("parcelss", "parcels", df_tr[, x])
    df_tr[, x] <- gsub("\\bpotato\\b", "potatoes", df_tr[, x])
    df_tr[, x] <- gsub("\\bpotato chips\\b", "chips", df_tr[, x])
    df_tr[, x] <- gsub("\\bpotatoes chips\\b", "chips", df_tr[, x])
    df_tr[, x] <- gsub("relief material \"", "", df_tr[, x])
    df_tr[, x] <- gsub("qatari field hospital \\(", "", df_tr[, x])
    df_tr[, x] <- gsub("\\brefrigerator\\b", "refrigerators", df_tr[, x])
    df_tr[, x] <- gsub("\\bhall\\b", "halls", df_tr[, x])
    df_tr[, x] <- gsub("\\bnapkin\\b", "napkins", df_tr[, x])
    df_tr[, x] <- gsub("\\bbag\\b", "bags", df_tr[, x])
    df_tr[, x] <- gsub("\\btarpaulin\\b", "tarpaulins", df_tr[, x])
    df_tr[, x] <- gsub("\\btent\\b", "tents", df_tr[, x])
    df_tr[, x] <- gsub("tiolet", "toilet", df_tr[, x])
    df_tr[, x] <- gsub("tortilla bread", "tortilla", df_tr[, x])
    df_tr[, x] <- gsub("vegetables oil", "vegetable oil", df_tr[, x])
    df_tr[, x] <- gsub("papers", "paper", df_tr[, x])
    df_tr[, x] <- gsub("waters", "water", df_tr[, x])
    df_tr[, x] <- gsub("medications", "medicine", df_tr[, x])
    df_tr[, x] <- gsub("ready-to-eat food", "rutf", df_tr[, x])
    df_tr[, x] <- gsub("\\.", "", df_tr[, x])
    df_tr[, x] <- gsub("\\)", "", df_tr[, x])
    df_tr[, x] <- gsub("\"", "", df_tr[, x])
    df_tr[, x] <- gsub("\\-", "\\+", df_tr[, x])
    df_tr[, x] <- gsub("\\band\\b", "\\+", df_tr[, x])
    

#...............................................................................  
### Creating a long dataset (one row = one trucked-in item) with Kg/Kcal totals
#...............................................................................
    
  #...................................      
  ## Split dataset into one row = one item
    
    # Split description into many columns to capture multiple items
    x <- data.frame(str_split(df_tr$description, pattern = "\\+", 
      simplify = T))
    n_items_max <- ncol(x)
    colnames(x) <- paste0("d", 1:n_items_max)    
    df_tr <- cbind(df_tr, x)
    
    #  Remove white spaces
    for (i in paste0("d", 1:n_items_max) ) {
      df_tr[, i] <- trimws(df_tr[, i], "both")
    }
    
    # List of unique trucked items
    items <- c()
    for (i in paste0("d", 1:n_items_max)  ) {
      items <- c(items, df_tr[, i])}
    items <- sort(unique(items))
    write.csv(items,paste0(dir_path,"out/02_trucked_items.csv"), row.names = F)

    # Assume food items when only item is 'mixed items' or 'emirati aid'
      # these are mostly UAE trucks: https://wam.ae/en/article/b1ryawg-11-truck-uae-aid-convoy-enters-gaza-strip-part
    df_tr$d2 <- ifelse(df_tr$description == "mixed items","food items",df_tr$d2)
    df_tr$d2 <- ifelse(df_tr$description == "emirati aid","food items",df_tr$d2)

    # Eliminate specialised food (added separately)
    for (i in paste0("d", 1:n_items_max)) {
      df_tr[, i] <-  gsub("nutritional supplements", "", df_tr[, i])
      df_tr[, i] <-  gsub("dietary supplements", "", df_tr[, i])
      df_tr[, i] <-  gsub("rutf", "", df_tr[, i])      
    }
    
    # Reshape dataset long
    df_tr <- gather(df_tr, item_no, item, paste0("d", 1:n_items_max))

    # Delete empty rows
    df_tr <- subset(df_tr, item != "")
    
  #...................................      
  ## Add Kcal equivalents and food types
    
    # For specific items (will be missing for mixed items)
    df_tr <- merge(df_tr, kcal_equi[, c("item", "kcal_kg", "food_cat")],all.x=T)

    # Add Kcal equivalents for mixed items: food parcels
        # specific to organisation if data; else, mean of all organisations
    
      # identify items and food category
      x1 <- which(df_tr$item %in% 
        c("food parcels", "food baskets", "food cartons", "ration packs", 
          "ready", "ready meals", "ready to eat food"))
      df_tr[x1, "food_cat"] <- "mixed"
      
      # if sender is an organisation for which we have parcel composition:
      for (i in c("PRCS", "WCK|ANERA", "WFP", "UNRWA")) {
        x2 <- which(grepl(i, df_tr$sender))
        df_tr[base::intersect(x1, x2), "kcal_kg"] <-
          mean(parcels_agg[which(parcels_agg$agency == i & 
            parcels_agg$type == "trucked"), "kcal_kg"])
      }
  
      # if sender is any other agencies - take mean of all trucked food parcels:
      x2 <- which(! grepl("PRCS|WCK|ANERA|WFP|UNRWA", df_tr$sender))
      df_tr[base::intersect (x1, x2), "kcal_kg"] <-
        mean(parcels_agg[which(parcels_agg$type == "trucked"), "kcal_kg"])

    # Add Kcal equivalents for canned food
        # mean of specific canned items, weighted by their quantity
    
      # identify items
      x1 <- which(df_tr$item == "canned food")
      x2 <- which(grepl("canned", df_tr$item))
      
      # compute weighted mean
      x <- aggregate(list(quantity = df_tr[base::setdiff(x2, x1), "quantity"]),
        by = list(item = df_tr[base::setdiff(x2, x1), "item"]), FUN = sum)
      x$wt <- x$quantity / sum(x$quantity)
      x <- merge(x, kcal_equi[, c("item", "kcal_kg")], all.x = T)
      df_tr[x1, "kcal_kg"] <- weighted.mean(x$kcal_kg, x$wt)

    # Add Kcal equivalents for other mixed food items
        # mean of non-NA food items by organisation; else, mean of all org's
  
      # identify items
      x1 <- which(df_tr$food_cat == "mixed" & ! df_tr$item %in% 
        c("food parcels", "food baskets", "food cartons", 
          "ration packs", "ready", "ready meals", "ready to eat food"))
      x3<-which(df_tr$item %in% unique(kcal_equi$item) & !is.na(df_tr$kcal_kg))
        
      # if sender organisation is one for which we have lots of observations:
      for (i in c("PRCS", "WCK|ANERA", "WFP", "UNRWA")) {
        x2 <- which(grepl(i, df_tr$sender))
        x <- aggregate(list(quantity=df_tr[base::intersect(x3, x2),"quantity"]),
          by = df_tr[base::intersect(x3, x2), c("item", "kcal_kg")], FUN = sum)
        x$wt <- x$quantity / sum(x$quantity)
        df_tr[base::intersect(x1,x2), "kcal_kg"]<-weighted.mean(x$kcal_kg, x$wt)
      }
  
      # if sender is any other agencies - take mean of all trucked food items:
      x2 <- which(! grepl("PRCS|WCK|ANERA|WFP|UNRWA", df_tr$sender))
      x3<-which(df_tr$item %in% unique(kcal_equi$item) & !is.na(df_tr$kcal_kg))    
      x <- aggregate(list(quantity = df_tr[x3, "quantity"]),
        by = df_tr[x3, c("item", "kcal_kg")], FUN = sum)
      x$wt <- x$quantity / sum(x$quantity)
      df_tr[base::intersect(x1, x2), "kcal_kg"] <- weighted.mean(x$kcal_kg,x$wt)

    # Figure out whether the item is food
    df_tr$any_food <- ! is.na(df_tr$food_cat) 
    

#...............................................................................  
### Generating distribution of Kcal per truck
      # needed later to estimate the amount of food in non-manifested trucks
      # no random error introduced here for simplicity
#...............................................................................

  #...................................      
  ## Generate distribution

    # Unique list of trucks and their number of items
    df_tr$n_items <- 1
    all_tr <- aggregate(list(n_items = df_tr$n_items), by = list(truck_id = 
      df_tr$truck_id), FUN = sum)
    all_tr <- all_tr[order(all_tr$truck_id), ]
          
    # Equal relative weight contribution of each item, for multi-item trucks
    x <- sapply(all_tr$n_items, function(x) {rep(1/x, x)})
    df_tr$wt_rel <- as.vector(unlist(x))

    # Compute Kcal and Kg of each item
    df_tr$kg_item <- df_tr$kg_truck * df_tr$wt_rel
    df_tr$kg_food <- ifelse(df_tr$any_food, df_tr$kg_item, 0)
    df_tr$kcal_item <- df_tr$kg_food * df_tr$kcal_kg
    df_tr$kcal_item <- na.replace(df_tr$kcal_item, 0)
    
    # Compute Kcal per truck, excluding trucks that had no food
    kcal_tr <- aggregate(list(kcal_truck = df_tr$kcal_item), 
      by = list(truck_id = df_tr$truck_id), FUN = sum)
    kcal_tr <- subset(kcal_tr, kcal_truck > 0)

    
  #...................................      
  ## Visualise
 
    # Visualise distribution
    ggplot(kcal_tr, aes(x = kcal_truck)) +
      geom_histogram(alpha = 0.5, binwidth = 2500000,
        fill = palette_gen[6], colour = palette_gen[6]) +
      theme_bw() +
      scale_y_continuous("number of trucks", breaks = seq(0, 2000, 200),
        expand = c(0,0), limits = c(0, 2000)) +
      scale_x_continuous("Kcal equivalent per truck", 
        breaks = seq(0, 400000000, 25000000), limits = c(0, 400000000),
        labels = unit_format(unit = "M", scale = 1e-6),
        expand = c(0,0))
    ggsave(paste(dir_path, "out/", "03_kcal_per_truck.png", sep=""),
      dpi = "print", units = "cm", height = 15, width = 22)  
      
    
    
    
#...............................................................................  
### ENDS
#...............................................................................
    