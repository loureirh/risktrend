library(tidyverse)

# Save the original analysis data with the squamous and non-squamous patients
save_analysis_data <- rlang::duplicate(analysis_data)

##### Run the analysis #####

run_analysis <- function(trial_list_ = trial_list,
                         SEED = 12345,
                         MONTHS_TEST = 2:12,
                         SAVE_FOLDER = "",
                         CALIP = 0.001,
                         ...){
    
    trial_results <- map(trial_list_, function(current_trial){
        
        Var1 <- current_trial$Var1
        Var2 <- current_trial$Var2
        
        Var1_LoT <- current_trial$Var1_LoT
        Var2_LoT <- current_trial$Var2_LoT
        
        Trial_name <- current_trial$Trial_name
        
        # Need to make it global so that the plot_medications has access to it
        # TODO: It should be given to the function as a parameter in the future!
        analysis_data <<- data.table::copy(save_analysis_data) %>% 
            mutate(linename = case_when(
                # ARM 1
                linename %in% Var1_LoT ~ Var1,
                # ARM 2
                linename %in% Var2_LoT ~ Var2,
                # Others
                TRUE ~ linename
            )) %>%
            filter(histology %in% current_trial$histology)
        
        results_trial <- plot_medications(c(Var1, Var2), 
                                          Trial_name,
                                          smooth_method = "loess", 
                                          matching_M = 1,
                                          matching_seed = SEED, 
                                          max_time = 24, 
                                          MONTHS_TEST = MONTHS_TEST,
                                          calip = CALIP,
                                          maximum_matches = MAXIMUM_MATCHES,
                                          save_models = TRUE,
                                          save_folder = SAVE_FOLDER,
                                          ...)
        
    })
    
    return(trial_results)
}
