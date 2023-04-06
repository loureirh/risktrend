library(nlme)
library(splines)
library(JM)
library(MatchIt)
library(Cairo)

source("auxiliary_functions.R")

plot_medications <- function(medications, 
                             trial_name = NA,
                             max_time = 12, 
                             smooth_method = "loess",
                             matching_M = 2,
                             matching_seed = 12345,
                             calip = 0.001,
                             MONTHS_TEST = 2:12,
                             save_models = FALSE,
                             save_folder = "/"){
    
    ##### Start Function #####
    
    # Create the save folder in case we want to save the models/figure to the disk
    save_folder <- paste0(save_folder, 
                          str_replace_all(trial_name, ",", "_"), 
                          "/")
    
    if(save_models == TRUE & dir.exists(save_folder) == FALSE){
        dir.create(save_folder)
    }
    
    # Verify which of the medications has the highest number of patients
    # this makes we select the highest number of patients possible in the 
    # matching step
    if(
        nrow(analysis_data %>% 
             filter(linename == medications[1],
                    linenumber == 1) %>%
             distinct(patientid)) > 
        nrow(analysis_data %>% 
             filter(linename == medications[2],
                    linenumber == 1) %>%
             distinct(patientid))
    ){
        medications <- c(medications[1], medications[2])
    } else {
        medications <- c(medications[2], medications[1])
    }
    
    tmp_list <- preprocess_plot_risk_data(analysis_data, medications, max_time)
    pt_medications <- tmp_list[["pt_medications"]]
    baseline_data  <- tmp_list[["baseline_data"]]
    
    if(!is.na(calip)){
        # Perform Propensity Score matching 
        selected_ptids <- match_patients(baseline_data, matching_M, calip,
                                         seed = matching_seed)
        
        # Prepare data, plot_title and plot_breaks for the risk plot 
        matched_data <- data.frame(patientid = selected_ptids$selected_matches_ptids) %>%
            left_join(pt_medications, by = "patientid")
    } else {
        warning("Not performing matching because calip == NA")
        matched_data <- pt_medications
    }
    
    # Save the patientids for later
    if(save_models == TRUE){
        write_csv(matched_data %>% distinct(patientid, linename), 
                  file = paste0(save_folder, "ptids.csv"))
    }
    
    plot_data <- matched_data %>%
        select(monthnum, linename, ROPRO = delta, DeepROPRO = delta_ds) %>%
        pivot_longer(cols = c(ROPRO, DeepROPRO)) %>%
        mutate(name = factor(name, levels = c("ROPRO", "DeepROPRO")))
    
    if(is.na(trial_name)){
        plot_title <- paste0(
            medications[1], " - ", 
            matched_data %>% filter(linename == medications[1]) %>% distinct(patientid) %>% nrow(), 
            " / ",
            medications[2], " - ", 
            matched_data %>% filter(linename == medications[2]) %>% distinct(patientid) %>% nrow())
    } else {
        plot_title <- paste0(
            trial_name, " - ",
            matched_data %>% filter(linename == medications[1]) %>% distinct(patientid) %>% nrow(),
            " patients per arm")
    }
    
    # Save the plot breaks to use in both plots
    plot_breaks <- seq(0, max_time, 2)
    
    # Create the risk plot
    final_plot <- ggplot(plot_data, aes(x = monthnum, y = value, color = linename, fill = linename)) + 
        stat_summary(geom="line", fun=mean, linetype="dashed", alpha = 1/3)+
        geom_smooth(method = smooth_method) + 
        stat_summary(geom="point", fun=mean, alpha = 0.75, shape = 3) +  
        xlab("Time [Months]") + 
        ylab("Risk") + 
        scale_x_continuous(breaks = plot_breaks, limits = c(min(plot_breaks), max(plot_breaks))) + 
        scale_fill_manual(values = c("#0073C2", "#EFC000")) + 
        scale_color_manual(values = c("#0073C2", "#EFC000")) + 
        facet_wrap(~ name, nrow = 2) + 
        theme(legend.position = "bottom") +
        ggtitle(plot_title) +
        theme_survminer()
    
    # Prepare KM
    # Run the JM models
    if(any(is.na(MONTHS_TEST)) == FALSE){
        # Add the significance of the longitudinal plot
        run_jm_models(matched_data = matched_data,
                      MONTHS_TEST = MONTHS_TEST,
                      save_models = save_models, 
                      save_folder = save_folder,
                      longitudinal_variable = "ROPRO")
        
        run_jm_models(matched_data = matched_data,
                      MONTHS_TEST = MONTHS_TEST,
                      save_models = save_models, 
                      save_folder = save_folder,
                      longitudinal_variable = "DeepROPRO")
    }
    
    # Add deltaRisk plot to the list
    final_plot_list <- list(final_plot)
    plot_sizes <- c(6)
    
    # NOTE: We are using the FULL matched data
    km_data <- matched_data %>%
        mutate(eventtime = eventtime / 30) %>%
        filter(monthnum == 0) %>%
        ungroup() 
    
    km_plot_OS <- create_km_plot(km_data, max_time, title = "OS plot",
                                 conf.int = TRUE, palette = c("#0073C2", "#EFC000"),
                                 xlab = "Time [Months]")
    
    # Add OS plot to the plot list
    final_plot_list <- append(final_plot_list, list(km_plot_OS$plot))
    final_plot_list <- append(final_plot_list, list(km_plot_OS$table))
    plot_sizes <- c(plot_sizes, c(4,2))
    
    final_plot <- ggpubr::ggarrange(plotlist = final_plot_list,
                                    nrow = length(plot_sizes),
                                    heights = plot_sizes,
                                    common.legend = TRUE)
    
    final_plot_list <- append(final_plot_list, list(final_plot))
    
    # Save the plot
    if(save_models == TRUE){
        figure_height <- 8
        
        ggsave(paste0(save_folder, "plot.png"), plot = final_plot, 
               width = 7, height = figure_height, dpi = 300)
        ggsave(paste0(save_folder, "plot.pdf"), plot = final_plot, 
               device = cairo_pdf, width = 7, height = figure_height, dpi = 300)
        saveRDS(final_plot_list, paste0(save_folder, "ggplots.RDS"))
    }
    
    out_object <- list(plot = final_plot)
    
    return(out_object)
}

##### Auxiliary functions ##### 

#' Perform baseline ROPRO matching
#'
#' @param baseline_data Data.frame with the baseline ROPRO
#' @param matching_M Number of control patients to match with "treated" patients
#' @param calip Maximum difference of ROPRO value between two matched samples
#' @param seed Seed to make sure the matches are reproducible
#' @param maximum_matches Optional integer that specifies if the `selected_matches_ptids`
#' value should contain a maximum number of matches.
#'
#' @return List with two values "complete_matches_ptids" and "selected_matches_ptids". 
#' If the `maximum_matches` number was specified. Each value is an array of selected
#' patientids from the baseline_data
match_patients <- function(baseline_data, matching_M, 
                           calip, seed = 12345,
                           maximum_matches = NA){
    
    set.seed(seed)
    matching <- matchit(linename ~ pred, 
                        data = baseline_data,
                        caliper = calip)
    
    set.seed(seed)
    matched_dataset <- match.data(matching)
    
    # Create a list to return
    out_object <- list()
    
    # Add to the list the FULL list of matches
    out_object$complete_matches_ptids <- matched_dataset$patientid
    out_object$selected_matches_ptids <- out_object$complete_matches_ptids
    
    # Add to the list the list of matches with a maximum number of patients
    if(isFALSE(is.na(maximum_matches))){
        
        if(length(unique(matched_dataset$subclass)) < maximum_matches){
            maximum_matches <- length(unique(matched_dataset$subclass))
            message("Reducing the number of maximum matches to", 
                    length(unique(matched_dataset$subclass)), 
                    "since there aren't enough patients.")
        }
        
        set.seed(seed)
        sample_strata <- sample(unique(matched_dataset$subclass), maximum_matches)
        
        small_matched_dataset <- matched_dataset %>% filter(subclass %in% sample_strata)
        
        out_object$selected_matches_ptids <- small_matched_dataset$patientid
    }
    
    return(out_object)
}

preprocess_plot_risk_data <- function(analysis_data, medications, max_time){
    pt_medications <- analysis_data %>% 
        group_by(patientid, enhancedcohort) %>%
        filter(is.null(previous_line) | # If we give a previous_line it will check the below condition
                   any( (linenumber == line_number - 1) & (linename == previous_line) )) %>%
        ungroup() %>%
        filter(linenumber == line_number,
               linename %in% medications) %>% 
        mutate(linename = factor(linename, levels = medications))
    
    # Filter the max date 
    pt_medications <- pt_medications %>%
        filter(monthnum <= max_time,
               monthnum >= 0)
    
    # Create a baseline data.frame to use in the matching
    baseline_data <- pt_medications %>%
        mutate(linename = as.integer(linename) - 1) %>% # "Binary" linename for Matching
        filter(monthnum == min(monthnum, na.rm = T))
    
    return(list(pt_medications = pt_medications, baseline_data = baseline_data))
}

create_km_plot <- function(km_data, max_time, title = "", conf.int = FALSE, palette = NULL, ...){
    
    # Perform Cox model to get HR and p-value
    cox_model <- coxph(Surv(eventtime, status) ~ linename,
                       data = km_data)
    temp_summary <- summary(cox_model)
    HR <- temp_summary$coefficients[1, 2]
    p_value <- temp_summary$coefficients[1, 5]
    
    p_val_string <- sprintf("HR: %.3f; p: %.2e", HR, p_value)
    
    # Fit a KM curve
    km.fit <- survfit(Surv(eventtime, status) ~ linename, 
                      data = km_data)
    
    # Do the plot
    km_plot <- ggsurvplot(km.fit, data = km_data, 
                          conf.int = conf.int,
                          palette = palette,
                          xlim = c(0, max_time),
                          surv.median.line = "hv",
                          pval = p_val_string, 
                          risk.table = "absolute",
                          risk.table.y.text = F,
                          break.x.by = 2,
                          ...)
    if(title != ""){
        km_plot$plot <- km_plot$plot + 
            ggtitle(title)
    }
    
    return(km_plot)
}

run_jm_models <- function(matched_data,
                          MONTHS_TEST,
                          longitudinal_variable = "ROPRO",
                          save_models = FALSE,
                          save_folder = ""){
    
    # Calculate the p-values for each time point
    models <- map(MONTHS_TEST, ~ tryCatch({
        return(jm_model(matched_data, .x, 1,
                        longitudinal_variable = longitudinal_variable))
    }, 
    error = function(e){ print(paste("ERROR for time-point", .x)); return(list(p_values = Inf))}
    ))
    
    if(save_models == TRUE){
        saveRDS(models, file = paste0(save_folder, "models_", 
                                      longitudinal_variable, ".RDS"), compress = FALSE)
    }
}

jm_model <- function(matched_data,
                     max_month_num,
                     number_splines,
                     longitudinal_variable = "ROPRO",
                     cox_formula_drug = Surv(eventtime, status) ~ linename){
    
    # Convert the linename into a factor and
    # keep only the values before the time-point
    tmp_matched_data <- matched_data %>%
        filter(monthnum <= max_month_num) %>%
        mutate(linename = factor(linename),
               monthnum = monthnum * 30) %>%
        group_by(patientid) %>%
        mutate(eventtime = if_else(eventtime == max(monthnum), eventtime + 1L, eventtime)) %>%
        ungroup() %>%
        arrange(patientid, monthnum) %>%
        select(patientid, monthnum, linename, eventtime, status, delta, delta_ds,
               # Extract all variables that are used in the Cox formula
               all_of(all.vars(cox_formula_drug)))
    
    tmp_matched_data_surv <- tmp_matched_data %>%
        filter(monthnum == 0) %>%
        mutate(status = if_else(status == 1 & eventtime <= 30 * max_month_num, 
                                1, 0),
               # Add the 1 so that we do not have issued with the JM package 
               # the time of the measurement cannot be the time of death/censoring
               eventtime = pmin(eventtime, 30 * max_month_num + 1)) %>%
        select(patientid, linename, eventtime, status, 
               # Extract all variables that are used in the Cox formula
               all_of(all.vars(cox_formula_drug)))
    
    tmp_matched_data <- tmp_matched_data %>%
        select(-eventtime, -status)
    
    cox_fit_drug <- coxph(cox_formula_drug, data = tmp_matched_data_surv, x = TRUE)
    
    if(longitudinal_variable == "ROPRO"){
        lme_fit_linear_drug <- lme(fixed = delta ~ monthnum*linename - linename,
                                   data = tmp_matched_data,
                                   random = ~ monthnum + 0 | patientid, 
                                   control = lmeControl(opt = "optim"), 
                                   method = "ML")
    } else if(longitudinal_variable == "DeepROPRO"){        
        lme_fit_linear_drug <- lme(fixed = delta_ds ~ monthnum*linename - linename,
                                   data = tmp_matched_data,
                                   random = ~ monthnum + 0 | patientid, 
                                   control = lmeControl(opt = "optim"), 
                                   method = "ML")
    }
    
    # Fit the JM model
    jm_fit_drug <- JM::jointModel(lme_fit_linear_drug, survObject = cox_fit_drug, 
                                  timeVar = "monthnum", method = "piecewise-PH-aGH",
                                  control = list(iter.EM = 500))
    
    out_object <- list(lme_fit_drug = lme_fit_linear_drug,
                       jm_fit_drug = jm_fit_drug)
    
    return(out_object)
}
