library(JM)
library(tidyverse)

# Load the data
source("load_data.R")

# Load trial list
source("clinical_trials_definition.R")

# Load the functions to perform the analysis
source("recreate_clinical_trials.R")

# Run the analysis ----

BASE_FOLDER <- "<- Folder to save the analysis ->"

ANALYSIS_FOLDER <- paste0(BASE_FOLDER, "higher_caliper/")
CALIPER <- "<- Caliper to use in the matching ->"

# Run the analysis with all the patients
run_analysis(SAVE_FOLDER = ANALYSIS_FOLDER, 
             SKIP_JM = T,
             MONTHS_TEST = 2:24,
             jm_use_arm_of_treatment = TRUE, 
             CALIP = CALIPER)

analysis_data <<- save_analysis_data
