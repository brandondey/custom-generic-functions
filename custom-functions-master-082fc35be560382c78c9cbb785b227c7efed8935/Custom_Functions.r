# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-
# rename_var
# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-

rename_var <- function(df, old_vars, new_vars) {
  # rename_var renames old_vars in df to new_vars
  # output: dataframe in global environment named df_new
  # args
  #   df: dataframe
  #   old_vars: named character vector of variable names to be renamed
  #   new_vars: named character vector of variable names to be renamed
  #
  # Improvements 
  #   update df don't create a new object called df_new
  #   Add Checks:
  #     have same number of elements
  #     is a df
  #     output is same dimensions else error
  
  for(c in old_vars) {
    i <- which(old_vars %in% c)
    colnames(df)[tolower(colnames(df)) == c] <- new_vars[i]
    
  }
  return(df) 
  
}


# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-
# flattenCorrMatrix
# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-

flattenCorrMatrix <- function(cor_matrix, p_matrix) {
  # from: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
  # cor_matrix : matrix of the correlation coefficients
  # p_matrix : matrix of the correlation p-values
  
  ut <- upper.tri(cor_matrix)
  data.frame(
    row = rownames(cor_matrix)[row(cor_matrix)[ut]],
    column = rownames(cor_matrix)[col(cor_matrix)[ut]],
    cor  =(cor_matrix)[ut],
    p = p_matrix[ut]
  )
}

# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-
# prettify_logistic_reg
# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-


prettify_logistic_reg <- function(data, formula) {
  # print model stats: accuracy, true positive rate, true negative rate for logistic regression
  #
  # args:
  #   data: dataframe
  #   formula: formula passed to stats::glm(formula = formula, data = data, family = "binomial") 
  #
  # for formula on data
  #   build model
  #   estimate log odds 
  #   convert log odds to probabilities
  #   compute accuracy, true positive rate, true negative rate
  #
  # Improvements:   
  #   pick better probability_threshold using roc curve
  #   force comparision group on vars to what i want
  
  require(stats)
  
  model_1 = stats::glm(formula = formula,
                       data = data, family = "binomial") 
  
  data$predict_log_odds <- predict(model_1)
  data$predict_probabilities <- exp(data$predict_log_odds)/(1+exp(data$predict_log_odds))
  probability_threshold <- .5
  data$predict_outcome <- data$predict_probabilities >= probability_threshold
  data$predicted_correct <- data$diff_tot_sales_t_plt_positive == data$predict_outcome
  
  # Model metrics 
  accuracy = 100*sum(data$predicted_correct) / nrow(data)
  true_positive = 100*nrow(filter(data, predicted_correct == T & diff_tot_sales_t_plt_positive == T)) /  nrow(filter(data, diff_tot_sales_t_plt_positive == T))
  true_negative = 100*nrow(filter(data, predicted_correct == T & diff_tot_sales_t_plt_positive == F)) /  nrow(filter(data, diff_tot_sales_t_plt_positive == F))
  
  print(paste("Accuracy: ", round(accuracy,2),"%", sep = "")) 
  print(paste("True Positive Rate: ", round(true_positive,2),"%", sep = "")) 
  print(paste("True Negative Rate: ", round(true_negative,2),"%", sep = ""))
  
}

# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-
# make_me_md_table
# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-


make_me_md_table <- function(var_names) {
  # puts variables into a markdown table. used to create data dictionaries in .md files.
  #
  # Improvements: 
  #   print columns for: 
  #     data type
  #     levels
  #     summary
  
  require(dplyr) # select functions
  require(magrittr) # pipes
  
  var_names %>% 
    noquote() %>% 
    data.frame(object = .) %>% 
    mutate(object = paste("|", object, "|"), 
           desciption = " |") %>%
    rename("| **object** |" = object, 
           "**Description** |" = desciption) %>%
    rbind("------------- |") %>%
    mutate(id = 1:(length(var_names) + 1)) %>% 
    arrange(desc(id)) %>%
    dplyr::select(-id) %>%
    print.data.frame(row.names = FALSE)  
  
}

# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-
# make_me_md_table_files
# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-
make_me_md_table_files <- function(path) {
  # get all files in path and put into .md table structure. Two columns: File, Description.
  #
  # Improvements: 
  # better sorting File
  
  require(dplyr) # select functions
  require(magrittr) # pipes
  
  list.files(path = path) -> files
  
  # remove readme.me from list
  Filter(function(x) !any(grepl("README.md", x)), files) -> files
  
  files %>% 
    noquote %>% 
    data.frame(file = .) %>% 
    mutate(file = paste("|", file, "|"), 
           desciption = " |") %>%
    rename("| **File** |" = file, 
           "**Description** |" = desciption) %>%
    rbind("------------- |") %>%
    mutate(id = 1:(length(files) + 1)) %>% 
    arrange(desc(id)) %>%
    dplyr::select(-id) %>%
    print.data.frame(row.names = FALSE)  
}

# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-
# NAyer
# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-


NAyer <- function(df) {
  
  # NAyer turns all NA's in numeric columns in df to O and adds NA's to factor levels for factors, returning df.
  # args: 
  #   df: a dataframe
  #
  # Improvements: 
  #
  #
  
  require(purrr) # map
  require(magrittr) # pipes
  
  # numerics
  map_lgl(df, is.numeric) -> is_numeric_v
  
  df[, which(is_numeric_v == T)] -> numeric_df 
  
  for(c in colnames(numeric_df)) {
    df[is.na(df[ , c]), c] <- 0
  }
  
  
  # Factors -- add NA to factor level if column has any NA's.
  map_lgl(df, is.factor) -> is_factor_v
  
  df[, which(is_factor_v == T)] -> factor_df 
  
  for(c in colnames(factor_df)) {
    
    # check if column has any NA rows
     if (any(is.na(df[,c]))) {
       
       # Add NA's to level
       df[,c] <- addNA(df[,c])
     
       } #end if
    
  } # end factor for
  
  return(df) 
}

# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-
# geocode_me
# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-


geocode_me <- function(address, type, secondary, state){
  # Overview
  # Author: 
  #   (Original) SigmaMonstR @ Github.   https://github.com/SigmaMonstR/census-geocoder
  #   (Secondary) Brandon. I updated and annotated a little.
  #
  # About:
  #   geocode_me gets lattitude and longitude coordinates for an address by using the US Census Bureau's geocoder. (For free.)
  #   runs for a single address at a time.
  #   e.g.: census_geocoder(address = "463 W LOCUST ST", secondary = "Milwaukee", type = F, state = "WI")
  #
  # Args:
  #   address: house number and street address
  #   type: have zipcode? if so, use type = "z", if not, anything works.
  #   secondary: city
  #   state: as abbreviation
  #
  # returns
  #  data frame
  
  library(jsonlite)
  library(RCurl)
  
  addy <- paste("street=", gsub(" ", "+", address), sep = "")
  if (type == "z") {
    wild <- paste("zip=", gsub(" ", "+", secondary), sep = "")
  } else {
    wild <- paste("city=", gsub(" ", "+", secondary), sep = "")
  }
  
  state <- paste("state=", gsub(" ", "+", state), sep = "") 
  string <-  paste("https://geocoding.geo.census.gov/geocoder/geographies/address?", addy, "&", wild, "&", state, "&benchmark=4&vintage=4&format=json", sep = "")
  json_file <- fromJSON(getURL(string))
  
  # Check if there are results
  if(length(json_file$result$addressMatches$coordinates) > 0){
    
    # If not, kick back an empty dataframe
    if(is.null(json_file$result$addressMatches$coordinates$x[1]) == TRUE){
      print("no result")
      return(data.frame(
        address = "",
        lat = "",
        lon = ""))
      
    } else{
      
      # Address, lat, lon (keep first match)
      return(data.frame(
        address = as.character(data.frame(json_file$result$addressMatches$matchedAddress)[1, ]),
        lat = as.character(json_file$result$addressMatches$coordinates$y[1]),
        lon = as.character(json_file$result$addressMatches$coordinates$x[1])))
      
    }
  }
}

# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-
# calculate_roc()
# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-


calculate_roc <- function(df, cost_of_fp, cost_of_fn)
{ 
  
  ## Create ROC df: Include vars for cost, false positve rate, false negative rate for 1:n  ## 
  roc <- data.frame(threshold = seq(0, 1, length.out = 100), 
                    tpr = NA, 
                    fpr = NA)
  
  tpr <- function(df, threshold) 
  { 
    sum(df$prediction >= threshold & df$actual == 1) / sum(df$actual == 1) 
  }
  
  fpr <- function(df, threshold) 
  { 
    sum(df$prediction >= threshold & df$actual == 0) / sum(df$actual == 0) 
  }
  
  cost <- function(df, threshold, cost_of_fp, cost_of_fn) # units in what? 
  { 
    sum(df$prediction >= threshold & df$actual == 0) * cost_of_fp + sum(df$prediction < threshold & df$actual == 1) * cost_of_fn 
  }
  
  
  roc$tpr <- sapply(roc$threshold, function(th) tpr(df, th)) 
  roc$fpr <- sapply(roc$threshold, function(th) fpr(df, th)) 
  roc$cost <- sapply(roc$threshold, function(th) cost(df, th, cost_of_fp, cost_of_fn)) 
  
  
  return(roc) 
  
}



# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-
# plot_prediction_type_distribution()
# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-

plot_prediction_type_distribution <- function(df, threshold, show_plot = T) 
{ 
  # About: 
  #   visualizes the Type I and II errors
  # df must have vars named prediction and actual, where prediction is the probability score bfore its round up or down
  #   to match the values of actual.
  
  require(ggplot2)
  require(directlabels)
  
  v <- rep(NA, nrow(df)) 
  v <- ifelse(df$prediction >= threshold & df$actual == 1, "True +", v) 
  v <- ifelse(df$prediction >= threshold & df$actual == 0, "False +", v) 
  v <- ifelse(df$prediction < threshold & df$actual == 1, "False -", v) 
  v <- ifelse(df$prediction < threshold & df$actual == 0, "True -", v) 
  
  df$prediction_type <- v 
  
  ggplot(data = df, 
         aes(x = actual, 
             y = prediction)) +  
    
    geom_jitter(
      aes(color = prediction_type), 
      alpha = 1) + 
    
    
    # True negative region
    geom_ribbon(aes(x = actual -.5, # adjust left boundary of x axis bc axis lims were pushed from jittering
                    ymin = 0, 
                    ymax = threshold), 
                col = "green",
                fill = "green",
                alpha = .15) +
    
    # True Positive region
    geom_ribbon(aes(x = actual +.5, # adjust left boundary of x axis bc axis lims were pushed from jittering
                    ymin = threshold, 
                    ymax = 1), 
                col = "green",
                fill = "green",
                alpha = .15) +
    
    # False Positive region
    geom_ribbon(aes(x = actual - .5, # adjust left boundary of x axis bc axis lims were pushed from jittering
                    ymin = threshold, 
                    ymax = 1 ), 
                col = "red",
                fill = "red",
                alpha = .25) +
    
    # False Negative region
    geom_ribbon(aes(x = actual + .5, # adjust left boundary of x axis bc axis lims were pushed from jittering
                    ymin = 0, 
                    ymax = threshold), 
                col = "red",
                fill = "red",
                alpha = .15) +
    
    
    # plot labels at end points
    directlabels::geom_dl(aes(label = prediction_type, 
                              size = 1),
                          method = list(dl.trans(x = x, 
                                                 y = y), 
                                        "last.points", 
                                        cex = .75)) +
    
    # threshold line
    geom_hline(yintercept = threshold, 
               color = "red",
               linetype = 2,
               alpha = 1) + 
    
    # threshold text
    geom_text(aes(x = .5, 
                  y = threshold,
                  label = sprintf("Threshold at %.2f", threshold)), 
              color = "black",
              alpha = 1) + 
    
    labs(x = "Actual Classification", 
         y = "Predicted Classification",
         title = sprintf("Threshold at %.2f", threshold), 
         caption = sprintf("\n 
                           Graph displays how the model did at classifying observations as either yes (1) or no (0).
                           Whether an observation is one or the other depends on if its probability is above or below %.2f, respectively. 
                           Points displayed are jittered so we can see patterns. In reality, they take values 0 or 1, and not values between 0 and 1.\n\n\n", threshold)
         ) +
    
    theme_bw() +
    
    guides(col = F ) +
    
    theme(plot.title  =  element_text(hjust  =  0.5),
          plot.subtitle  =  element_text(hjust  =  0.5), 
          plot.caption  =  element_text(hjust  =  0, vjust  =  -5)) -> pred_type_plot
  
  
  return(
    if(show_plot == T) 
    {plot(pred_type_plot)} else {pred_type_plot}
  )
  
  
}


# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-
# plot_roc()
# ^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-^-

plot_roc <- function(df, fpc = 1, fnc = 2) 
{ 
  # plot_roc plots 3 graphs to help illustrate how your model perfomed at binary classifcation.  
  #   visualizes cost, optimal threshold curve, and type I and II errors
  #
  #
  # args: 
  #   df: data frame with vars named: threshold, tpr, fpr, cost.
  #   fpc: cost to penalize false positives
  #   fnc: cost to penalize false negatives
  # 
  # Improvements: 
  #   Add ROC stats to graphs: 
  #                 sub_title <- sprintf("Threshold at %.2f.\n Cost of FP =  \n Cost of FN = ", # %d
  #                                       threshold)
  #                                     sub = textGrob(sub_title, gp = gpar(cex = 1), just = "bottom")
  #                   
  # modified from : https://github.com/joyofdata/joyofdata-articles/blob/master/roc-auc/plot_roc.R
  
  
  require(gridExtra)
  require(grid)
  require(ggplot2)
  require(cowplot)
  
  # find the prediction threshold that minimizes the distance from (0,1) on ROC curve coordinate plane.
  calculate_roc(df = df, cost_of_fp = fpc, cost_of_fn = fnc) -> roc
  
  roc %>%
    mutate(distancefrombest = ((1-tpr)^2+(1-(1-fpr))^2)^(1/2)) %>% 
    filter(distancefrombest == min(distancefrombest)) %>% 
    select(threshold) %>% min -> optimal_threshold
  
  threshold <- optimal_threshold
  
  norm_vec <- function(v) (v - min( v )) / diff(range( v ))
  
  idx_threshold = which.min(abs(roc$threshold - threshold)) 
  
  col_ramp <- colorRampPalette(c("green","orange","red","black"))(100) 
  
  col_by_cost <- col_ramp[ceiling(norm_vec(roc$cost)*99)+1] 
  
  # ROC Plot 
  p_roc <- ggplot(roc, aes(fpr,tpr)) +  
    
    geom_line(color = rgb(0, 0, 1, alpha = 0.3)) + 
    
    geom_point(color = col_by_cost, size = 4, alpha = 0.5) + 
    
    geom_line(aes(threshold,threshold), 
              color = rgb(0, 0, 1, alpha = 0.5)) + 
    
    labs(title = sprintf("ROC"), 
         x = "False Pos. Rate", 
         y = "True Pos. Rate") + 
    
    geom_hline(yintercept = roc[idx_threshold, "tpr"], alpha = 0.5, 
               linetype = "dashed") + 
    
    geom_vline(xintercept = roc[idx_threshold, "fpr"], alpha = 0.5, 
               linetype = "dashed" ) + 
    
    theme_bw() +
    
    theme(plot.title  =  element_text(hjust  =  0.5),
          plot.subtitle  =  element_text(hjust  =  0.5), 
          plot.caption  =  element_text(hjust  =  1, vjust  =  -5))
  
  # Plot of Cost Function 
  p_cost <- ggplot(roc, 
                   aes(threshold, cost)) +
    
    geom_line(color = rgb(0, 0, 1, alpha = 0.3)) +
    
    geom_point(color = col_by_cost, size = 4, alpha = 0.5) + 
    
    labs(title = sprintf("Cost Function"), 
         x = "Cost", 
         y = "Threshold") + 
    
    geom_vline(xintercept = threshold, 
               alpha = 0.5, 
               linetype = "dashed", 
               col = "red") +
    
    theme_bw() +
    
    theme(plot.title  =  element_text(hjust  =  0.5),
          plot.subtitle  =  element_text(hjust  =  0.5), 
          plot.caption  =  element_text(hjust  =  1, vjust  =  -5))
  
  
  
  pred_plot_df <- df
  
  
  plot_prediction_type_distribution(df = pred_plot_df,
                                    threshold = optimal_threshold,
                                    show_plot = F) -> pred_type_plot
  
  
  # arrange plots for output
  cowplot::plot_grid(
    p_roc,
    p_cost,
    pred_type_plot,
    align = "v", 
    nrow = 3, 
    rel_heights = c(1/4, 1/4, 1/2))
  
  
  
}




save(
  plot_roc,
  plot_prediction_type_distribution,
  calculate_roc,
  geocode_me,
  NAyer,
  make_me_md_table_files,  
  make_me_md_table,  
  rename_var,
  flattenCorrMatrix,
  prettify_logistic_reg,
  file = '//nike/Common/Information Sciences/Business Analytics/Brandon/R Projects/Functions/Custom_Functions.rds')



## UNDER DEVELOPMENT ##


# put_plots_there <- function(path_to_plots) {
#   # take plots from path to plots and make them show in the readme. 
#   # if no readme, make one.
#   # only grab jpeg, png
#   
#   require(dplyr) # select functions
#   require(magrittr) # pipes
#   
#   list.files(path = path_to_plots) -> plots
#   
#   # remove readme.me from list
#   Filter(function(x) !any(grepl("README.md", x)), plots) -> plots
#   
#   paste("![ ] ",
#         "(", path_to_plots, plots, ")",
#         sep = "") %>%
#     noquote %>% 
#     data.frame(plot = .) %>%
#     print.data.frame(row.names = FALSE)   
#   
#   
# }
# 
# put_plots_there("./Plots/")



