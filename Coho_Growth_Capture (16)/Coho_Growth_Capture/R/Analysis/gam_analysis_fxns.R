# set up function to make predictions
pred_gam <- function(df, mod) {
  
  require(mgcv)
  
  # Make predictions with this
  predictions <- predict(mod, newdata = df,
                         type = "link", se.fit = TRUE)
  # Create df
  pred_df <- data.frame(preds = predictions$fit, se.fit = predictions$se.fit)
  
  # Bind with observed
  pred_df <- cbind(pred_df, df)
  
  # Bind these together
  # Create 95% confidence itnervals for this
  pred_df <- pred_df %>% 
    mutate(fitted = exp(preds),
           lower = exp(preds - (1.96*se.fit)),
           upper = exp(preds + (1.96*se.fit)))
  
  return(pred_df)
  
}

# function to loop through to do model selection
#' @param possible_variables A vector of variables we want to consider
#' @param control_variables A formula in character form that specifies the response, as a function
#' @param df: A dataframe of the data you want to use
#' @param error_dist: The error distribution you want to use for your GAM model

# Create a generalized function to do model selection
model_selex <- function(possible_variables, control_variables,
                        df, error_dist) {
  
  # Get all possible combinations of possible variables
  variables <- unlist(lapply(1:length(possible_variables), combn, 
                             x = possible_variables, simplify = FALSE), 
                      recursive = FALSE)
  
  # Create a list object to store models in
  all_models <- list(length(variables))
  
  # Create a dataframe to store model summary statistics
  mod_statistics <- data.frame(AIC = NA, BIC = NA, R2 = NA,
                               Dev_Exp = NA, model = rep(NA, length(variables))) 
  
  for(i in 1:length(variables)) {
    
    # Setting up loop for model formula
    # Model formula set up, and going through all combinations here
    formula <- as.formula(paste(control_variables, paste(variables[[i]], collapse = "+")))
    
    # Pipe this formula into our model formula for BAM
    model_output <- gam(formula, data = df, 
                        family = error_dist, method = "ML")
    
    # Stick model outputs into list
    all_models[[i]] <- model_output
    
    # Name these model to differentiate them
    names(all_models)[[i]] <- paste("model", i)
    
    print(paste("done w/ model", i, "/", length(variables))) } # End for loop
  
  # Next evaluate AIC, BIC, deviance explained, and R2 for all of these
  for(j in 1:length(all_models)) {
    
    # Differentiate models
    mod_statistics$model[j] <- names(all_models)[[j]]
    # Get AIC
    mod_statistics$AIC[j] <- AIC(all_models[[j]])
    # Get BIC
    mod_statistics$BIC[j] <- BIC(all_models[[j]])
    # Get Deviance Explained
    mod_statistics$Dev_Exp[j] <- summary(all_models[[j]])$dev.expl
    # Get R2
    mod_statistics$R2[j] <- summary(all_models[[j]])$r.sq
    
  }
  
  return(list(all_models, mod_statistics))
  
} # end function
