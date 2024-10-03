#...............................................................................
### ++ ANALYSIS OF CALORIC AVAILABILITY AND FOOD DIVERSITY IN GAZA (2024) ++ ###
#...............................................................................

#...............................................................................
## ------ R SCRIPT TO LOAD PACKAGES AND SOURCE OTHER ANALYSIS SCRIPTS  ------ ##
#...............................................................................



#...............................................................................
### Preparatory steps
#...............................................................................

  #...................................      
  ## Install or load required R packages
  if (!"pacman" %in% rownames(installed.packages())){install.packages("pacman")}
  
  pacman::p_load(
    ggplot2,       # Data visualization
    ggpubr,        # Arranging multiple plots into a single plot
    ggrepel,       # Improve labelling of plots
    gtools,        # Assist various programming tasks
    lubridate,     # Makes it easier to work with dates and times
    pammtools,     # Produce uncertainty bands for step plots
    readxl,        # Read Excel files
    scales,        # Scaling and formatting data for visualizations
    tidyverse,     # Tidyverse suite of packages
    viridis,       # Colour palettes
    zoo)           # For computing running means

  #...................................      
  ## Starting setup

    # Clean up from previous code / runs
    rm(list=ls(all=T) )
  
    # Set font for Windows or Mac
    suppressWarnings(windowsFonts(Arial = windowsFont("Arial")))
    suppressWarnings(par(family = "Arial"))

    # Set working directory to where this file is stored
    dir_path <- paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/")
    setwd(dir_path)
    print( getwd() )
    dir_path <- gsub("/code", "", dir_path)
    suppressWarnings(dir.create(paste0(dir_path, "out")))
    
    # Initialise random numbers
    set.seed(123)
    
    # Colour-blind palette for graphing
    palette_gen <- viridis(16)
    show_col(palette_gen)
          

#...............................................................................
### Sourcing dependent scripts
#...............................................................................
    
  #...................................
  ## Read and prepare inputs other than UNRWA trucking data
  source(paste0(dir_path, "code/01_read_prepare_inputs.r") )

  #...................................
  ## Read and prepare UNRWA trucking data
  source(paste0(dir_path, "code/02_clean_trucking_data.r") )

  #...................................
  ## Estimate Kcal per capita availability
  source(paste0(dir_path, "code/03_estimate_kcal_capita.r") )
    
  #...................................
  ## Compute food category diversity among food trucked in
  source(paste0(dir_path, "code/04_compute_food_diversity.r") )
    
  #...................................
  ## Visualise estimates
  source(paste0(dir_path, "code/05_visualise_estimates.r") )

    
              
#...............................................................................  
### ENDS
#...............................................................................
     
