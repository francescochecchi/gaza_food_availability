## Food availability in the Gaza Strip during operation Swords of Iron, October 2023 to August 2024: a retrospective analysis
### Description of R scripts and data required to replicate the analysis
September 2024

## Input data
The following input datasets are included in the `/in` directory of the repository:
* `gaza_food_data.xlsx`, each worksheet of which contains a single dataset. the `parameters` tab contains individual parameters. More detail on each dataset is provided in the paper;
* `20240911_Commodities Received.xlsx`, as downloaded from https://www.unrwa.org/what-we-do/gaza-supplies-and-dispatch-tracking. This dataset is a line list of trucks that crossed the Rafah or Kerem Shalom crossings into Gaza, and details on their contents, as maintained by UNRWA;
* `gaza_cogat_data_24sep2014.csv`, as downloaded from https://gaza-aid-data.gov.il/main/, which contains a line list of aid consignments maintained by the Israeli Ministry of Defence.

## R scripts
The `00_master_script.R` code installs and/or loads required R packages, initialises certain parameters and then sources each of the following scripts in order:
* `01_read_prepare_inputs.R` reads and prepares all the parameters and datasets apart from the UNRWA trucking line list;
* `02_clean_trucking_data.R` reads and prepares the UNRWA trucking line list;
* `03_estimate_kcal_capita.R` implements a stochastic simulation to estimate caloric availability per capita, by integrating all available food source data; note that this script might take up to 30-40 minutes to run on a slow laptop.
* `04_compute_food_diversity.R` computes the share of food trucked into Gaza by food category; 
* `05_visualise_estimates.R` generates graphs and tables using the outputs of the previous scripts;
* `99_read_prepare_ncd_survey.R` aggregates a published survey of non-communicable disease prevalence and risk factors among adults in Gaza. This script is included in the repository for transparency, but the source dataset that it reads is deliberately omitted, as our ethics approval only covered publication of aggregate survey data. The aggregate survey tabulation is included as the `adult_intake` worksheet in `gaza_food_data.xlsx` (see above).

## Replicating the analysis
To replicate the analysis, follow these steps:
* Download and unzip the repository to any directory in your computer (other than the Downloads folder, which usually gets wiped automatically). The directory is identified automatically when the code is run.
* Download R and RStudio (see download links on https://posit.co/download/rstudio-desktop/). While R is sufficient to run the analysis, it is recommended to instead run the scripts from the RStudio interface.
* Open and run the entire `00_master_script.R` script (just press Alt+Ctrl+R). This will create an `/out` directory, to which output tables and graphs will be saved automatically. As this scripts calls all the others, it alone is sufficient to replicate the analysis.
