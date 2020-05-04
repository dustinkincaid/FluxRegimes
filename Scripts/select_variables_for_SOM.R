# Testing methods for variable selection for self-organizing map (SOM) using random forest feature importance, PCA, and correlation analyses

# Helpful website for random forest:
  # https://explained.ai/rf-importance/


# Load packages
  library("tidyverse")
  library("lubridate")
  library("randomForest")
  library("tidymodels")
  library("conflicted")

  conflict_prefer("filter", "dplyr")


# Read in calculated metrics from compile_calculate_allVars.R
  hford <- read_csv("Data/eventMetrics_hford.csv")
  wade <- read_csv("Data/eventMetrics_wade.csv")

  
set.seed(42)
# Random Forest ----
# For the initial feature importance analysis, I'm going to keep all variables! Why not?!
# RF: HFORD ----
  # For NO3 yield ----
    # Select variables
    allvars_hford_NO3 <- hford %>% 
      # Drop columns we won't use in the random forest analysis
      select(-c(site, event_start, season, SRP_kg_km2, event_NO3_SRP)) %>% 
      # Only use complete cases (only 40 total! EEK!)
      na.omit() %>% 
      # Add a Random number column
      mutate(random = sample(x = 1:nrow(.), size = nrow(.), replace = TRUE)) %>% 
      mutate(multipeak = as.factor(ifelse(multipeak == "MP", 1, 0)))
    # Random forest
    # ONLY RUN THIS NEXT STEP IF YOU NEED TO RE-OPTIMIZE THE PARAMETERS ----
      # Helpful code:  
        # https://explained.ai/rf-importance/
      # mtry: Number of variables randomly sampled as candidates at each split
      # ntree: Number of trees to grow.
    
      # Optimize 'mtry' and 'ntree'
      # http://www.rebeccabarter.com/blog/2020-03-25_machine_learning/
      # https://konradsemsch.netlify.app/2019/08/caret-vs-tidymodels-comparing-the-old-and-new/
      # https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/    
        # https://stackoverflow.com/questions/13956435/setting-values-for-ntree-and-mtry-for-random-forest-regression-model
        # https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
        # bestmtry_hf_no3 <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=501, importance = T)
        # Separate testing and training data
        hford_no3_split <- initial_split(allvars_hford_NO3, prop = 0.75)
          # split <- initial_split(credit_data, prop = 0.80, strata = "status")
          # dia_split <- initial_split(diamonds, prop = .1, strata = price)
        # Extract training set from split (won't need test set b/c not testing the fitted model, just using for variable importance)
        hford_no3_train <- training(hford_no3_split)
        hford_no3_test <- testing(hford_no3_split)
        # Create cross-validation object from training data
          # V-fold cross-validation randomly splits the data into V groups of roughly equal size (called "folds"). 
          # A resample of the analysis data consisted of V-1 of the folds while the assessment set contains the final fold. 
          # In basic V-fold cross-validation (i.e. no repeats), the number of resamples is equal to V.
        hford_no3_cv <- vfold_cv(hford_no3_train, v = 3)
          # train_cv <- vfold_cv(df_train, v = 5, strata = "status")
          # dia_vfold <- vfold_cv(dia_train, v = 3, repeats = 1, strata = price)
        # Define a recipe
          # Recipes allow you to specify the role of each variable as an outcome or predictor variable (using a “formula”), 
          # and any pre-processing steps you want to conduct (such as normalization, imputation, PCA, etc).
        hford_no3_recipe <- hford_no3_train %>% 
          recipe(NO3_kg_km2 ~ .) %>% 
          # Encoding "multipeak" as a dummy variable
          step_dummy(all_nominal())
        # Specify the model (this code doesn't actually fit the model; it outlines a description of the model)
        rf_model <-
          # Specify that the model is a random forest
          rand_forest() %>% 
          # Specify that the 'mtry' & 'trees' need to be tuned
          set_args(mtry = tune(),
                   trees = tune()) %>% 
          # Select the engine/package that underlies the model
          # First we'll try randomForest vs. ranger
          # set_engine("randomForest", importance = T) %>%
          set_engine("ranger", importance = "permutation") %>% 
          # Set mode
          set_mode("regression")
        # Put it all together (again, we haven't implemented the preprocessing steps in the recipe or fit the model; we've just written the framework)
        rf_workflow <-
          workflow() %>% 
          # Add the recipe
          add_recipe(hford_no3_recipe) %>% 
          # Add the model
          add_model(rf_model)
        # Tune the parameters
          # Specify the grid of paramters to be tested
          # rf_grid <- 
          #   grid_random(mtry() %>% range_set(c(1, 30)),
          #               trees() %>% range_set(c(300, 1001)),
          #               size = 500)
          # Or set more manually
          rf_grid <- expand.grid(mtry = c(1:30),
                                 trees = c(101, 201, 301, 401, 501, 601, 701, 801, 901, 1001))
        # Extract the results (this took ~ 6 minutes for 300 combinations of parameters)
        rf_tune_results <- rf_workflow %>% 
          tune_grid(resamples = hford_no3_cv,
                    grid = rf_grid,
                    # Other options include 'rsq' and 'ccc'
                    metrics = metric_set(rmse))
        # Print
          # Can also do: collect_metrics(rf_tune_results)
        rf_tune_results %>% collect_metrics %>% arrange(mean)
        # Show best
        show_best(rf_tune_results, "rmse", n = 9)
        # Select best (first)
        select_best(rf_tune_results, metric = "rmse")
              # 1st try using randomForest
              #   mtry trees
              #   <int> <dbl>
              # 1    28   401
                # 2nd time with v = 2 vs. 3, so maybe set back to 3?
                #    mtry trees
                #   <int> <dbl>
                # 1    11   201
              # 3rd time repeating 1st time
              #    mtry trees
              #   <int> <dbl>
              # 1    26   301
                # 1st try using ranger
                #    mtry trees
                #   <int> <dbl>
                # 1    27   201
                # Std error was lower than randomForest, so let's use ranger
        # select_by_one_std_err() uses the "one-standard error rule" (Breiman _el at, 1984) that selects the most simple model 
        # that is within one standard error of the numerically optimal results.
        select_by_one_std_err(rf_tune_results, mtry, trees, metric = "rmse")
              #    mtry trees .metric .estimator  mean     n std_err .best
              #   <int> <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <dbl>
              # 1     2   301 rmse    standard    15.3     3    4.05  12.5
              # … with 1 more variable: .bound <dbl>
    # END OPTIMIZATION ----
      
      # Then run randomForest with optimized parameter values
      # rf_hford_NO3 <- randomForest(NO3_kg_km2 ~ ., data = allvars_hford_NO3, mtry = 15, ntree = 501, importance = T)
      rf_hford_NO3 <- randomForest(NO3_kg_km2 ~ ., allvars_hford_NO3, mtry = 27, ntree = 201, importance = T)
      # R^2 = 74.61
      # Then extract permutation importances
      imp_hford_NO3 <- importance(rf_hford_NO3, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
      # Repeat 2 more times
      rf_hford_NO3_2 <- randomForest(NO3_kg_km2 ~ ., allvars_hford_NO3, mtry = 27, ntree = 201, importance = T)
      imp_hford_NO3_2 <- importance(rf_hford_NO3_2, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
      rf_hford_NO3_3 <- randomForest(NO3_kg_km2 ~ ., allvars_hford_NO3, mtry = 27, ntree = 201, importance = T)
      imp_hford_NO3_3 <- importance(rf_hford_NO3_3, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
      # Combine and average
      imp_hford_NO3_mean <- bind_rows(imp_hford_NO3, imp_hford_NO3_2, imp_hford_NO3_3) %>% 
        group_by(variable) %>% summarize(IncMSE_per = mean(IncMSE_per))
      
      # Code for plotting perm. importances
      imp_hford_NO3_mean %>% 
        ggplot(aes(x = reorder(variable, IncMSE_per), y = IncMSE_per)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        ggtitle("NO3_kg_km2")
      
      # Remove objects
      rm(imp_hford_NO3, imp_hford_NO3_2, imp_hford_NO3_3, rf_hford_NO3, rf_hford_NO3_2, rf_hford_NO3_3)
  
    # USE TINYMODELS APPROACH FOR OPTIMIZATION AND FIT ----
    # Here I tune the parameters, fit the rf model, and extract permutation importances using tinymodels
    # I used the tuning code above
    # http://www.rebeccabarter.com/blog/2020-03-25_machine_learning/
    # https://konradsemsch.netlify.app/2019/08/caret-vs-tidymodels-comparing-the-old-and-new/
    # https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/
      # # Separate testing and training data
      # hford_no3_split <- initial_split(allvars_hford_NO3, prop = 0.75)
      #   # split <- initial_split(credit_data, prop = 0.80, strata = "status")
      #   # dia_split <- initial_split(diamonds, prop = .1, strata = price)
      # # Extract training set from split (won't need test set b/c not testing the fitted model, just using for variable importance)
      # hford_no3_train <- training(hford_no3_split)
      # hford_no3_test <- testing(hford_no3_split)
      # # Create cross-validation object from training data
      #   # V-fold cross-validation randomly splits the data into V groups of roughly equal size (called "folds"). 
      #   # A resample of the analysis data consisted of V-1 of the folds while the assessment set contains the final fold. 
      #   # In basic V-fold cross-validation (i.e. no repeats), the number of resamples is equal to V.
      # hford_no3_cv <- vfold_cv(hford_no3_train, v = 3)
      #   # train_cv <- vfold_cv(df_train, v = 5, strata = "status")
      #   # dia_vfold <- vfold_cv(dia_train, v = 3, repeats = 1, strata = price)
      # # Define a recipe
      #   # Recipes allow you to specify the role of each variable as an outcome or predictor variable (using a “formula”), 
      #   # and any pre-processing steps you want to conduct (such as normalization, imputation, PCA, etc).
      # hford_no3_recipe <- hford_no3_train %>% 
      #   recipe(NO3_kg_km2 ~ .) %>% 
      #   # Encoding "multipeak" as a dummy variable
      #   step_dummy(all_nominal())
      # # Specify the model (this code doesn't actually fit the model; it outlines a description of the model)
      # rf_model <-
      #   # Specify that the model is a random forest
      #   rand_forest() %>% 
      #   # Specify that the 'mtry' & 'trees' need to be tuned
      #   set_args(mtry = tune(),
      #            trees = tune()) %>% 
      #   # Select the engine/package that underlies the model
      #   # First we'll try randomForest vs. ranger
      #   # set_engine("randomForest", importance = T) %>%
      #   set_engine("ranger", importance = "permutation") %>% 
      #   # Set mode
      #   set_mode("regression")
      # # Put it all together (again, we haven't implemented the preprocessing steps in the recipe or fit the model; we've just written the framework)
      # rf_workflow <-
      #   workflow() %>% 
      #   # Add the recipe
      #   add_recipe(hford_no3_recipe) %>% 
      #   # Add the model
      #   add_model(rf_model)
      # # Tune the parameters
      #   # Specify the grid of paramters to be tested
      #   # rf_grid <- 
      #   #   grid_random(mtry() %>% range_set(c(1, 30)),
      #   #               trees() %>% range_set(c(300, 1001)),
      #   #               size = 500)
      #   # Or set more manually
      #   rf_grid <- expand.grid(mtry = c(1:30),
      #                          trees = c(101, 201, 301, 401, 501, 601, 701, 801, 901, 1001))
      # # Extract the results (this took ~ 6 minutes for 300 combinations of parameters)
      # rf_tune_results <- rf_workflow %>% 
      #   tune_grid(resamples = hford_no3_cv,
      #             grid = rf_grid,
      #             # Other options include 'rsq' and 'ccc'
      #             metrics = metric_set(rmse))
      # # Print
      #   # Can also do: collect_metrics(rf_tune_results)
      # rf_tune_results %>% collect_metrics %>% arrange(mean)
      # # Show best
      # show_best(rf_tune_results, "rmse", n = 9)
      # # Select best (first)
      # select_best(rf_tune_results, metric = "rmse")
      #       # 1st try using randomForest
      #       #   mtry trees
      #       #   <int> <dbl>
      #       # 1    28   401
      #         # 2nd time with v = 2 vs. 3, so maybe set back to 3?
      #         #    mtry trees
      #         #   <int> <dbl>
      #         # 1    11   201
      #       # 3rd time repeating 1st time
      #       #    mtry trees
      #       #   <int> <dbl>
      #       # 1    26   301
      #         # 1st try using ranger
      #         #    mtry trees
      #         #   <int> <dbl>
      #         # 1    27   201
      #         # Std error was lower than randomForest, so let's use ranger
      # # select_by_one_std_err() uses the "one-standard error rule" (Breiman _el at, 1984) that selects the most simple model 
      # # that is within one standard error of the numerically optimal results.
      # select_by_one_std_err(rf_tune_results, mtry, trees, metric = "rmse")
      #       #    mtry trees .metric .estimator  mean     n std_err .best
      #       #   <int> <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <dbl>
      #       # 1     2   301 rmse    standard    15.3     3    4.05  12.5
      #       # … with 1 more variable: .bound <dbl>
      # # Fit and extract permutation importances
      #   # Using randomForest
      #   fit_hf_no3_rf <- 
      #     rand_forest(mtry = 27, trees = 201, mode = "regression") %>% 
      #     set_engine("randomForest", importance = T) %>% 
      #     fit(NO3_kg_km2 ~ ., data = juice(prep(hford_no3_recipe)))
      #   # Extract
      #   imp_hf_no3_rf <- 
      #     cbind(rownames(fit_hf_no3_rf$fit$importance), fit_hf_no3_rf$fit$importance) %>% 
      #     as_tibble() %>% 
      #     select(predictor = V1, `%IncMSE`) %>% 
      #     mutate(`%IncMSE` = as.numeric(`%IncMSE`)) %>% 
      #     arrange(desc((`%IncMSE`)))
      #   # Example code for plotting permutation importances
      #   imp_hf_no3_rf %>%
      #     ggplot(aes(x = reorder(predictor, `%IncMSE`), y = `%IncMSE`)) +
      #     geom_bar(stat = "identity") +
      #     coord_flip() +
      #     ggtitle("NO3_kg_km2")
        
        
      # Try conditional permutation permutation importance to deal with collinear features
      # https://www.r-bloggers.com/be-aware-of-bias-in-rf-variable-importance-metrics/
      # This reduced the # of predictors to 3 (too few?) and I couldn't figure out which they were (coded by #)
      # library(party)
      # rf3 <- cforest(
      #   NO3_kg_km2 ~ .,
      #   data = allvars_hford_NO3 %>% mutate(multipeak = as.factor(ifelse(multipeak == "MP", 1, 0))),
      #   control = cforest_unbiased(mtry = 27, ntree = 201)
      # )
      # 
      # create_crfplot <- function(rf, conditional = TRUE){
      #   
      #   imp <- rf %>%
      #     varimp(conditional = conditional) %>% 
      #     as_tibble() %>% 
      #     rownames_to_column("Feature") %>% 
      #     rename(Importance = value)
      #   
      #   p <- ggplot(imp, aes(x = reorder(Feature, Importance), y = Importance)) +
      #        geom_bar(stat = "identity", fill = "#53cfff", width = 0.65) +
      #        coord_flip() + 
      #        theme_light(base_size = 20) +
      #        theme(axis.title.x = element_text(size = 15, color = "black"),
      #              axis.title.y = element_blank(),
      #              axis.text.x  = element_text(size = 15, color = "black"),
      #              axis.text.y  = element_text(size = 15, color = "black")) 
      #   return(p)
      # }
      # 
      # # not conditional
      # create_crfplot(rf3, conditional = FALSE)
      # 
      #   imp <- rf3 %>%
      #     varimp(conditional = T) %>% 
      #     as_tibble() %>% 
      #     rownames_to_column("Feature") %>% 
      #     rename(Importance = value)
      # END USE TINYMODELS APPROACH FOR OPTIMIZATION AND FIT ----
        
      
  # For SRP yield ----
    # Select variables
    allvars_hford_srp <- hford %>% 
      select(-c(site, event_start, season, NO3_kg_km2, event_NO3_SRP)) %>% 
      na.omit() %>% 
      mutate(random = sample(x = 1:nrow(.), size = nrow(.), replace = TRUE)) %>% 
      mutate(multipeak = as.factor(ifelse(multipeak == "MP", 1, 0)))
    # Random forest
      # ONLY RUN THIS NEXT STEP IF YOU NEED TO RE-OPTIMIZE THE PARAMETERS ----
      # Optimize 'mtry' & 'ntrees'
        # Separate testing and training data
        hford_srp_split <- initial_split(allvars_hford_srp, prop = 0.75)
        # Extract training set from split (won't need test set b/c not testing the fitted model, just using for variable importance)
        hford_srp_train <- training(hford_srp_split)
        # Create cross-validation object from training data
        hford_srp_cv <- vfold_cv(hford_srp_train, v = 3)
        # Define a recipe
        hford_srp_recipe <- hford_srp_train %>% 
          recipe(SRP_kg_km2 ~ .) %>% 
          # Encoding "multipeak" as a dummy variable
          step_dummy(all_nominal())
        # Specify the model (this code doesn't actually fit the model; it outlines a description of the model)
        rf_model <-
          rand_forest() %>% 
          set_args(mtry = tune(),
                   trees = tune()) %>% 
          # set_engine("randomForest", importance = T) %>%
          set_engine("ranger", importance = "permutation") %>% 
          set_mode("regression")
        # Put it all together (again, we haven't implemented the preprocessing steps in the recipe or fit the model; we've just written the framework)
        rf_workflow <-
          workflow() %>% 
          add_recipe(hford_srp_recipe) %>% 
          add_model(rf_model)
        # Tune the parameters
        rf_grid <- expand.grid(mtry = c(1:30),
                               trees = c(101, 201, 301, 401, 501, 601, 701, 801, 901, 1001))
        # Extract the results (this took ~ 6 minutes for 300 combinations of parameters)
        rf_tune_results <- rf_workflow %>% 
          tune_grid(resamples = hford_srp_cv,
                    grid = rf_grid,
                    # Other options include 'rsq' and 'ccc'
                    metrics = metric_set(rmse))
        # Print
        # Can also do: collect_metrics(rf_tune_results)
        rf_tune_results %>% collect_metrics %>% arrange(mean)
        # Show best
        show_best(rf_tune_results, "rmse", n = 9)
        # Select best (first)
        select_best(rf_tune_results, metric = "rmse")
        # select_by_one_std_err() uses the "one-standard error rule" (Breiman _el at, 1984) that selects the most simple model 
        # that is within one standard error of the numerically optimal results.
        select_by_one_std_err(rf_tune_results, mtry, trees, metric = "rmse")
      # END OPTIMIZATION ----
        
        # Apply optimized parameters and extract permutation importances
        x <- allvars_hford_srp %>% select(-SRP_kg_km2)
        y <- allvars_hford_srp[["SRP_kg_km2"]]
        # Run randomForest w/ optimized paramter values
        rf_hford_SRP <- randomForest(SRP_kg_km2 ~ ., data = allvars_hford_srp, mtry = 3, ntree = 801, importance = T)
        # R^2 ONLY 22.09% (no good)
        # Extract permutation importances here
        imp_hford_SRP <- importance(rf_hford_SRP, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
        # Repeat 2 more times
        rf_hford_SRP_2 <- randomForest(SRP_kg_km2 ~ ., data = allvars_hford_srp, mtry = 3, ntree = 801, importance = T)
        imp_hford_SRP_2 <- importance(rf_hford_SRP_2, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
        rf_hford_SRP_3 <- randomForest(SRP_kg_km2 ~ ., data = allvars_hford_srp, mtry = 3, ntree = 801, importance = T)
        imp_hford_SRP_3 <- importance(rf_hford_SRP_3, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")  
        # Combine and average
        imp_hford_SRP_mean <- bind_rows(imp_hford_SRP, imp_hford_SRP_2, imp_hford_SRP_3) %>% 
          group_by(variable) %>% summarize(IncMSE_per = mean(IncMSE_per))
        
        # Code for plotting perm. importances
        imp_hford_SRP_mean %>% 
          ggplot(aes(x = reorder(variable, IncMSE_per), y = IncMSE_per)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          ggtitle("SRP_kg_km2")
        
      # Remove objects
      rm(imp_hford_SRP, imp_hford_SRP_2, imp_hford_SRP_3, rf_hford_SRP, rf_hford_SRP_2, rf_hford_SRP_3)        
      
      
  # For NO3 to SRP yield ratios ----
    # Select variables
    allvars_hford_NO3toSRP <- hford %>% 
      select(-c(site, event_start, season, NO3_kg_km2, SRP_kg_km2)) %>% 
      na.omit() %>% 
      mutate(random = sample(x = 1:nrow(.), size = nrow(.), replace = TRUE)) %>% 
      mutate(multipeak = as.factor(ifelse(multipeak == "MP", 1, 0)))
    # Random forest
      # ONLY RUN THIS NEXT STEP IF YOU NEED TO RE-OPTIMIZE THE PARAMETERS ----
      # Optimize 'mtry' & 'ntrees'
        # Separate testing and training data
        hford_NO3toSRP_split <- initial_split(allvars_hford_NO3toSRP, prop = 0.75)
        # Extract training set from split (won't need test set b/c not testing the fitted model, just using for variable importance)
        hford_NO3toSRP_train <- training(hford_NO3toSRP_split)
        # Create cross-validation object from training data
        hford_NO3toSRP_cv <- vfold_cv(hford_NO3toSRP_train, v = 3)
        # Define a recipe
        hford_NO3toSRP_recipe <- hford_NO3toSRP_train %>% 
          recipe(event_NO3_SRP ~ .) %>% 
          # Encoding "multipeak" as a dummy variable
          step_dummy(all_nominal())
        # Specify the model (this code doesn't actually fit the model; it outlines a description of the model)
        rf_model <-
          rand_forest() %>% 
          set_args(mtry = tune(),
                   trees = tune()) %>% 
          # set_engine("randomForest", importance = T) %>%
          set_engine("ranger", importance = "permutation") %>% 
          set_mode("regression")
        # Put it all together (again, we haven't implemented the preprocessing steps in the recipe or fit the model; we've just written the framework)
        rf_workflow <-
          workflow() %>% 
          add_recipe(hford_NO3toSRP_recipe) %>% 
          add_model(rf_model)
        # Tune the parameters
        rf_grid <- expand.grid(mtry = c(1:30),
                               trees = c(101, 201, 301, 401, 501, 601, 701, 801, 901, 1001))
        # Extract the results (this took ~ 6 minutes for 300 combinations of parameters)
        rf_tune_results <- rf_workflow %>% 
          tune_grid(resamples = hford_NO3toSRP_cv,
                    grid = rf_grid,
                    # Other options include 'rsq' and 'ccc'
                    metrics = metric_set(rmse))
        # Print
        # Can also do: collect_metrics(rf_tune_results)
        rf_tune_results %>% collect_metrics %>% arrange(mean)
        # Show best
        show_best(rf_tune_results, "rmse", n = 9)
        # Select best (first)
        select_best(rf_tune_results, metric = "rmse")
        # select_by_one_std_err() uses the "one-standard error rule" (Breiman _el at, 1984) that selects the most simple model 
        # that is within one standard error of the numerically optimal results.
        select_by_one_std_err(rf_tune_results, mtry, trees, metric = "rmse")
      # END OPTIMIZATION ----
    
        # Apply optimized parameters and extract permutation importances
        x <- allvars_hford_NO3toSRP %>% select(-event_NO3_SRP)
        y <- allvars_hford_NO3toSRP[["event_NO3_SRP"]]

        rf_hford_NO3toSRP <- randomForest(event_NO3_SRP ~ ., data = allvars_hford_NO3toSRP, mtry = 3, ntree = 101, importance = T)
        # R2 = 16.07 (boo!)
        # Extract and plot feature importance here
        imp_hford_NO3toSRP <- importance(rf_hford_NO3toSRP, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
        # Repeat 2 more times
        rf_hford_NO3toSRP_2 <- randomForest(event_NO3_SRP ~ ., data = allvars_hford_NO3toSRP, mtry = 3, ntree = 101, importance = T)
        imp_hford_NO3toSRP_2 <- importance(rf_hford_NO3toSRP_2, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
        rf_hford_NO3toSRP_3 <- randomForest(event_NO3_SRP ~ ., data = allvars_hford_NO3toSRP, mtry = 3, ntree = 101, importance = T)
        imp_hford_NO3toSRP_3 <- importance(rf_hford_NO3toSRP_3, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
        # Combine and average
        imp_hford_NO3toSRP_mean <- bind_rows(imp_hford_NO3toSRP, imp_hford_NO3toSRP_2, imp_hford_NO3toSRP_3) %>% 
          group_by(variable) %>% summarize(IncMSE_per = mean(IncMSE_per))
        
        # Code for plotting perm. importances
        imp_hford_NO3toSRP_mean %>% 
          ggplot(aes(x = reorder(variable, IncMSE_per), y = IncMSE_per)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          ggtitle("NO3:SRP yield ratio")
        
      # Remove objects
      rm(imp_hford_NO3toSRP, imp_hford_NO3toSRP_2, imp_hford_NO3toSRP_3, rf_hford_NO3toSRP, rf_hford_NO3toSRP_2, rf_hford_NO3toSRP_3,
         x, y)          
    
        
    # How to summarize this?
    # Give a variable a value of 1 if it's > than random AND non-negative and a 0 if it's less than random or negative
    imp_hford_NO3 <- imp_hford_NO3_mean %>% mutate(value = ifelse((IncMSE_per > .$IncMSE_per[.$variable == "random"] & IncMSE_per > 0), 1, 0))
    imp_hford_SRP <- imp_hford_SRP_mean %>% mutate(value = ifelse((IncMSE_per > .$IncMSE_per[.$variable == "random"] & IncMSE_per > 0), 1, 0))
    imp_hford_NO3toSRP <- imp_hford_NO3toSRP_mean %>% mutate(value = ifelse((IncMSE_per > .$IncMSE_per[.$variable == "random"] & IncMSE_per > 0), 1, 0))
    # Join these
    imp_hford_all <- bind_rows(imp_hford_NO3, imp_hford_SRP, imp_hford_NO3toSRP) %>% 
      group_by(variable) %>% 
      summarize(value_tot = sum(value)) %>% 
      arrange(desc(value_tot))
    # Remove objects
    rm(imp_hford_NO3_mean, imp_hford_SRP_mean, imp_hford_NO3toSRP_mean)
    
    
# Let's look at correlations to see if we should not include certain variables
  corr_hford <- hford %>% 
    ungroup() %>% 
    select(-c(site, event_start, season, NO3_kg_km2, SRP_kg_km2, event_NO3_SRP)) %>% 
    cor(use = "complete.obs")
    # as.data.frame() %>%
    # write.csv("Data/allvars_corr_hford.csv")
  corr_wade <- wade %>% 
    ungroup() %>% 
    select(-c(site, event_start, season, NO3_kg_km2, SRP_kg_km2, event_NO3_SRP)) %>% 
    cor(use = "complete.obs")
    # as.data.frame() %>% 
    # write.csv("Data/allvars_corr_wade.csv")    
  
  
# For the initial feature importance I'm going to keep most variables except for those that are _*d variations (e.g., 1d vs. 7d preEvent means)
mostvars_hford <- hford %>% 
  select(-c(rain_preEvent_14d, q_preEvent_mean_7d, gw_preEvent_mean_7d_well5))


# RF: WADE ----
# Repeat for Wade
  # For NO3 yield ----
    # Select variables
    allvars_wade_NO3 <- wade %>% 
      # Drop columns we won't use in the random forest analysis
      select(-c(site, event_start, season, SRP_kg_km2, event_NO3_SRP)) %>% 
      # Only use complete cases (only 40 total! EEK!)
      na.omit() %>% 
      # Add a Random number column
      mutate(random = sample(x = 1:nrow(.), size = nrow(.), replace = TRUE)) %>% 
      mutate(multipeak = as.factor(ifelse(multipeak == "MP", 1, 0)))
    # Random forest
      # Helpful code:  
        # https://explained.ai/rf-importance/
      # mtry: Number of variables randomly sampled as candidates at each split
      # ntree: Number of trees to grow.

      # ONLY RUN THIS NEXT STEP IF YOU NEED TO RE-OPTIMIZE THE PARAMETERS ----
      # Optimize 'mtry' and 'ntree'
      # http://www.rebeccabarter.com/blog/2020-03-25_machine_learning/
      # https://konradsemsch.netlify.app/2019/08/caret-vs-tidymodels-comparing-the-old-and-new/
      # https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/    
        # https://stackoverflow.com/questions/13956435/setting-values-for-ntree-and-mtry-for-random-forest-regression-model
        # https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
        # bestmtry_hf_no3 <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=501, importance = T)
        # Separate testing and training data
        wade_no3_split <- initial_split(allvars_wade_NO3, prop = 0.75)
          # split <- initial_split(credit_data, prop = 0.80, strata = "status")
          # dia_split <- initial_split(diamonds, prop = .1, strata = price)
        # Extract training set from split (won't need test set b/c not testing the fitted model, just using for variable importance)
        wade_no3_train <- training(wade_no3_split)
        wade_no3_test <- testing(wade_no3_split)
        # Create cross-validation object from training data
          # V-fold cross-validation randomly splits the data into V groups of roughly equal size (called "folds"). 
          # A resample of the analysis data consisted of V-1 of the folds while the assessment set contains the final fold. 
          # In basic V-fold cross-validation (i.e. no repeats), the number of resamples is equal to V.
        wade_no3_cv <- vfold_cv(wade_no3_train, v = 3)
          # train_cv <- vfold_cv(df_train, v = 5, strata = "status")
          # dia_vfold <- vfold_cv(dia_train, v = 3, repeats = 1, strata = price)
        # Define a recipe
          # Recipes allow you to specify the role of each variable as an outcome or predictor variable (using a “formula”), 
          # and any pre-processing steps you want to conduct (such as normalization, imputation, PCA, etc).
        wade_no3_recipe <- wade_no3_train %>% 
          recipe(NO3_kg_km2 ~ .) %>% 
          # Encoding "multipeak" as a dummy variable
          step_dummy(all_nominal())
        # Specify the model (this code doesn't actually fit the model; it outlines a description of the model)
        rf_model <-
          # Specify that the model is a random forest
          rand_forest() %>% 
          # Specify that the 'mtry' & 'trees' need to be tuned
          set_args(mtry = tune(),
                   trees = tune()) %>% 
          # Select the engine/package that underlies the model
          # First we'll try randomForest vs. ranger
          # set_engine("randomForest", importance = T) %>%
          set_engine("ranger", importance = "permutation") %>% 
          # Set mode
          set_mode("regression")
        # Put it all together (again, we haven't implemented the preprocessing steps in the recipe or fit the model; we've just written the framework)
        rf_workflow <-
          workflow() %>% 
          # Add the recipe
          add_recipe(wade_no3_recipe) %>% 
          # Add the model
          add_model(rf_model)
        # Tune the parameters
          # Specify the grid of paramters to be tested
          # rf_grid <- 
          #   grid_random(mtry() %>% range_set(c(1, 30)),
          #               trees() %>% range_set(c(300, 1001)),
          #               size = 500)
          # Or set more manually
          rf_grid <- expand.grid(mtry = c(1:30),
                                 trees = c(101, 201, 301, 401, 501, 601, 701, 801, 901, 1001))
        # Extract the results (this took ~ 6 minutes for 300 combinations of parameters)
        rf_tune_results <- rf_workflow %>% 
          tune_grid(resamples = wade_no3_cv,
                    grid = rf_grid,
                    # Other options include 'rsq' and 'ccc'
                    metrics = metric_set(rmse))
        # Print
          # Can also do: collect_metrics(rf_tune_results)
        rf_tune_results %>% collect_metrics %>% arrange(mean)
        # Show best
        show_best(rf_tune_results, "rmse", n = 9)
        # Select best (first)
        select_best(rf_tune_results, metric = "rmse")
              # 1st try using randomForest
              #   mtry trees
              #   <int> <dbl>
              # 1    28   401
                # 2nd time with v = 2 vs. 3, so maybe set back to 3?
                #    mtry trees
                #   <int> <dbl>
                # 1    11   201
              # 3rd time repeating 1st time
              #    mtry trees
              #   <int> <dbl>
              # 1    26   301
                # 1st try using ranger
                #    mtry trees
                #   <int> <dbl>
                # 1    27   201
                # Std error was lower than randomForest, so let's use ranger
        # select_by_one_std_err() uses the "one-standard error rule" (Breiman _el at, 1984) that selects the most simple model 
        # that is within one standard error of the numerically optimal results.
        select_by_one_std_err(rf_tune_results, mtry, trees, metric = "rmse")
              #    mtry trees .metric .estimator  mean     n std_err .best
              #   <int> <dbl> <chr>   <chr>      <dbl> <int>   <dbl> <dbl>
              # 1     2   301 rmse    standard    15.3     3    4.05  12.5
              # … with 1 more variable: .bound <dbl>
      # END OPTIMIZATION ----
      
      # Then run randomForest with optimized parameter values
      # rf_wade_NO3 <- randomForest(NO3_kg_km2 ~ ., data = allvars_wade_NO3, mtry = 15, ntree = 501, importance = T)
      rf_wade_NO3 <- randomForest(NO3_kg_km2 ~ ., allvars_wade_NO3, mtry = 15, ntree = 101, importance = T)
      # R^2 = 69.57
      # Then extract permutation importances
      imp_wade_NO3 <- importance(rf_wade_NO3, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
      # Repeat 2 more times
      rf_wade_NO3_2 <- randomForest(NO3_kg_km2 ~ ., allvars_wade_NO3, mtry = 15, ntree = 101, importance = T)
      imp_wade_NO3_2 <- importance(rf_wade_NO3_2, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
      rf_wade_NO3_3 <- randomForest(NO3_kg_km2 ~ ., allvars_wade_NO3, mtry = 15, ntree = 101, importance = T)
      imp_wade_NO3_3 <- importance(rf_wade_NO3_3, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
      # Combine and average
      imp_wade_NO3_mean <- bind_rows(imp_wade_NO3, imp_wade_NO3_2, imp_wade_NO3_3) %>% 
        group_by(variable) %>% summarize(IncMSE_per = mean(IncMSE_per))
      
      # Code for plotting perm. importances
      imp_wade_NO3_mean %>% 
        ggplot(aes(x = reorder(variable, IncMSE_per), y = IncMSE_per)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        ggtitle("NO3_kg_km2")
      
      # Remove objects
      rm(imp_wade_NO3, imp_wade_NO3_2, imp_wade_NO3_3, rf_wade_NO3, rf_wade_NO3_2, rf_wade_NO3_3)       

      
  # For SRP yield ----
    # Select variables
    allvars_wade_srp <- wade %>% 
      select(-c(site, event_start, season, NO3_kg_km2, event_NO3_SRP)) %>% 
      na.omit() %>% 
      mutate(random = sample(x = 1:nrow(.), size = nrow(.), replace = TRUE)) %>% 
      mutate(multipeak = as.factor(ifelse(multipeak == "MP", 1, 0)))
    # Random forest
      # ONLY RUN THIS NEXT STEP IF YOU NEED TO RE-OPTIMIZE THE PARAMETERS ----
      # Optimize 'mtry' & 'ntrees'
        # Separate testing and training data
        wade_srp_split <- initial_split(allvars_wade_srp, prop = 0.75)
        # Extract training set from split (won't need test set b/c not testing the fitted model, just using for variable importance)
        wade_srp_train <- training(wade_srp_split)
        # Create cross-validation object from training data
        wade_srp_cv <- vfold_cv(wade_srp_train, v = 3)
        # Define a recipe
        wade_srp_recipe <- wade_srp_train %>% 
          recipe(SRP_kg_km2 ~ .) %>% 
          # Encoding "multipeak" as a dummy variable
          step_dummy(all_nominal())
        # Specify the model (this code doesn't actually fit the model; it outlines a description of the model)
        rf_model <-
          rand_forest() %>% 
          set_args(mtry = tune(),
                   trees = tune()) %>% 
          # set_engine("randomForest", importance = T) %>%
          set_engine("ranger", importance = "permutation") %>% 
          set_mode("regression")
        # Put it all together (again, we haven't implemented the preprocessing steps in the recipe or fit the model; we've just written the framework)
        rf_workflow <-
          workflow() %>% 
          add_recipe(wade_srp_recipe) %>% 
          add_model(rf_model)
        # Tune the parameters
        rf_grid <- expand.grid(mtry = c(1:30),
                               trees = c(101, 201, 301, 401, 501, 601, 701, 801, 901, 1001))
        # Extract the results (this took ~ 6 minutes for 300 combinations of parameters)
        rf_tune_results <- rf_workflow %>% 
          tune_grid(resamples = wade_srp_cv,
                    grid = rf_grid,
                    # Other options include 'rsq' and 'ccc'
                    metrics = metric_set(rmse))
        # Print
        # Can also do: collect_metrics(rf_tune_results)
        rf_tune_results %>% collect_metrics %>% arrange(mean)
        # Show best
        show_best(rf_tune_results, "rmse", n = 9)
        # Select best (first)
        select_best(rf_tune_results, metric = "rmse")
        # select_by_one_std_err() uses the "one-standard error rule" (Breiman _el at, 1984) that selects the most simple model 
        # that is within one standard error of the numerically optimal results.
        select_by_one_std_err(rf_tune_results, mtry, trees, metric = "rmse")
      # END OPTIMIZATION ----
        
        # Apply optimized parameters and extract permutation importances
        x <- allvars_wade_srp %>% select(-SRP_kg_km2)
        y <- allvars_wade_srp[["SRP_kg_km2"]]
        # Run randomForest w/ optimized paramter values
        rf_wade_SRP <- randomForest(SRP_kg_km2 ~ ., data = allvars_wade_srp, mtry = 20, ntree = 201, importance = T)
        # R^2 ONLY 22.09% (no good)
        # Extract permutation importances here
        imp_wade_SRP <- importance(rf_wade_SRP, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
        # Repeat 2 more times
        rf_wade_SRP_2 <- randomForest(SRP_kg_km2 ~ ., data = allvars_wade_srp, mtry = 20, ntree = 201, importance = T)
        imp_wade_SRP_2 <- importance(rf_wade_SRP_2, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
        rf_wade_SRP_3 <- randomForest(SRP_kg_km2 ~ ., data = allvars_wade_srp, mtry = 20, ntree = 201, importance = T)
        imp_wade_SRP_3 <- importance(rf_wade_SRP_3, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")  
        # Combine and average
        imp_wade_SRP_mean <- bind_rows(imp_wade_SRP, imp_wade_SRP_2, imp_wade_SRP_3) %>% 
          group_by(variable) %>% summarize(IncMSE_per = mean(IncMSE_per))
        
        # Code for plotting perm. importances
        imp_wade_SRP_mean %>% 
          ggplot(aes(x = reorder(variable, IncMSE_per), y = IncMSE_per)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          ggtitle("SRP_kg_km2")

      # Remove objects
      rm(imp_wade_SRP, imp_wade_SRP_2, imp_wade_SRP_3, rf_wade_SRP, rf_wade_SRP_2, rf_wade_SRP_3)         
      
      
  # For NO3 to SRP yield ratios ----
    # Select variables
    allvars_wade_NO3toSRP <- wade %>% 
      select(-c(site, event_start, season, NO3_kg_km2, SRP_kg_km2)) %>% 
      na.omit() %>% 
      mutate(random = sample(x = 1:nrow(.), size = nrow(.), replace = TRUE)) %>% 
      mutate(multipeak = as.factor(ifelse(multipeak == "MP", 1, 0)))
    # Random forest
      # ONLY RUN THIS NEXT STEP IF YOU NEED TO RE-OPTIMIZE THE PARAMETERS ----
      # Optimize 'mtry' & 'ntrees'
        # Separate testing and training data
        wade_NO3toSRP_split <- initial_split(allvars_wade_NO3toSRP, prop = 0.75)
        # Extract training set from split (won't need test set b/c not testing the fitted model, just using for variable importance)
        wade_NO3toSRP_train <- training(wade_NO3toSRP_split)
        # Create cross-validation object from training data
        wade_NO3toSRP_cv <- vfold_cv(wade_NO3toSRP_train, v = 3)
        # Define a recipe
        wade_NO3toSRP_recipe <- wade_NO3toSRP_train %>% 
          recipe(event_NO3_SRP ~ .) %>% 
          # Encoding "multipeak" as a dummy variable
          step_dummy(all_nominal())
        # Specify the model (this code doesn't actually fit the model; it outlines a description of the model)
        rf_model <-
          rand_forest() %>% 
          set_args(mtry = tune(),
                   trees = tune()) %>% 
          # set_engine("randomForest", importance = T) %>%
          set_engine("ranger", importance = "permutation") %>% 
          set_mode("regression")
        # Put it all together (again, we haven't implemented the preprocessing steps in the recipe or fit the model; we've just written the framework)
        rf_workflow <-
          workflow() %>% 
          add_recipe(wade_NO3toSRP_recipe) %>% 
          add_model(rf_model)
        # Tune the parameters
        rf_grid <- expand.grid(mtry = c(1:30),
                               trees = c(101, 201, 301, 401, 501, 601, 701, 801, 901, 1001))
        # Extract the results (this took ~ 6 minutes for 300 combinations of parameters)
        rf_tune_results <- rf_workflow %>% 
          tune_grid(resamples = wade_NO3toSRP_cv,
                    grid = rf_grid,
                    # Other options include 'rsq' and 'ccc'
                    metrics = metric_set(rmse))
        # Print
        # Can also do: collect_metrics(rf_tune_results)
        rf_tune_results %>% collect_metrics %>% arrange(mean)
        # Show best
        show_best(rf_tune_results, "rmse", n = 9)
        # Select best (first)
        select_best(rf_tune_results, metric = "rmse")
        # select_by_one_std_err() uses the "one-standard error rule" (Breiman _el at, 1984) that selects the most simple model 
        # that is within one standard error of the numerically optimal results.
        select_by_one_std_err(rf_tune_results, mtry, trees, metric = "rmse")
      # END OPTIMIZATION ----
    
        # Apply optimized parameters and extract permutation importances
        x <- allvars_wade_NO3toSRP %>% select(-event_NO3_SRP)
        y <- allvars_wade_NO3toSRP[["event_NO3_SRP"]]

        rf_wade_NO3toSRP <- randomForest(event_NO3_SRP ~ ., data = allvars_wade_NO3toSRP, mtry = 29, ntree = 101, importance = T)
        # R2 = 16.07 (boo!)
        # Extract and plot feature importance here
        imp_wade_NO3toSRP <- importance(rf_wade_NO3toSRP, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
        # Repeat 2 more times
        rf_wade_NO3toSRP_2 <- randomForest(event_NO3_SRP ~ ., data = allvars_wade_NO3toSRP, mtry = 29, ntree = 101, importance = T)
        imp_wade_NO3toSRP_2 <- importance(rf_wade_NO3toSRP_2, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
        rf_wade_NO3toSRP_3 <- randomForest(event_NO3_SRP ~ ., data = allvars_wade_NO3toSRP, mtry = 29, ntree = 101, importance = T)
        imp_wade_NO3toSRP_3 <- importance(rf_wade_NO3toSRP_3, type = 1, scale = F) %>% as_tibble(rownames = "variable") %>% rename(IncMSE_per = "%IncMSE")
        # Combine and average
        imp_wade_NO3toSRP_mean <- bind_rows(imp_wade_NO3toSRP, imp_wade_NO3toSRP_2, imp_wade_NO3toSRP_3) %>% 
          group_by(variable) %>% summarize(IncMSE_per = mean(IncMSE_per))
        
        # Code for plotting perm. importances
        imp_wade_NO3toSRP_mean %>% 
          ggplot(aes(x = reorder(variable, IncMSE_per), y = IncMSE_per)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          ggtitle("NO3:SRP yield ratio")
        
      # Remove objects
      rm(imp_wade_NO3toSRP, imp_wade_NO3toSRP_2, imp_wade_NO3toSRP_3, rf_wade_NO3toSRP, rf_wade_NO3toSRP_2, rf_wade_NO3toSRP_3,
         x, y)         
    
        
    # How to summarize this?
    # Give a variable a value of 1 if it's > than random AND non-negative and a 0 if it's less than random or negative
    imp_wade_NO3 <- imp_wade_NO3_mean %>% mutate(value = ifelse((IncMSE_per > .$IncMSE_per[.$variable == "random"] & IncMSE_per > 0), 1, 0))
    imp_wade_SRP <- imp_wade_SRP_mean %>% mutate(value = ifelse((IncMSE_per > .$IncMSE_per[.$variable == "random"] & IncMSE_per > 0), 1, 0))
    imp_wade_NO3toSRP <- imp_wade_NO3toSRP_mean %>% mutate(value = ifelse((IncMSE_per > .$IncMSE_per[.$variable == "random"] & IncMSE_per > 0), 1, 0))
    # Join these
    imp_wade_all <- bind_rows(imp_wade_NO3, imp_wade_SRP, imp_wade_NO3toSRP) %>% 
      group_by(variable) %>% 
      summarize(value_tot = sum(value)) %>% 
      arrange(desc(value_tot))
    # Remove objects
    rm(imp_wade_NO3_mean, imp_wade_SRP_mean, imp_wade_NO3toSRP_mean)
    
    
    
# PCA ----
# Random Forest didn't seem very effective at reducing variables, maybe we try PCA to reduce the dimensionality of the input data
# PCA 
  # Notes on PCA:
    # http://www.lauradhamilton.com/introduction-to-principal-component-analysis-pca
      # This site is good basic intro to the concept of PCA; it also recommends that it is typically a good idea to normalize the data first;
      # Because PCA seeks to identify the principal components with the highest variance, if the data are not properly normalized, attributes with large values 
      # and large variances (in absolute terms) will end up dominating the first principal component when they should not. Normalizing the data gets each attribute 
      # onto more or less the same scale, so that each attribute has an opportunity to contribute to the principal component analysis.
    
  # Questions: how to prepare the data for PCA? Normalize (mean = 0) and/or make sure data are normally distributed?
  # Look at distributions of the data
    
    







  allvars_hford <- allvars_hford %>% 
    # These variables were highly correlated with another variable
    select(-c(rain_preEvent_14d, q_event_max, q_event_dQRate_cmsPerHr, q_preEvent_mean_7d, SoilTemp_mean_preEvent_1, SoilTemp_mean_preEvent_3)) %>% 
    # Also just going to focus on Pit 3 for now, so dropping Pit 1 vars for now
    select(-c(ends_with("1"))) %>% 
    # And to make this initial analyses more simple, I'm going to remove these vars as well
    select(-c(rain_int_mmPERmin_mean, rain_preEvent_1d, rain_preEvent_30d, DO_mean_preEvent_3))
# I'm going to remove the following variables b/c of corrs > +/- 0.7
  allvars_wade <- allvars_wade %>% 
    # I'm going to remove the same variables here as I did for Wade for simplicity this time around
    select(-c(rain_preEvent_14d, q_event_max, q_event_dQRate_cmsPerHr, q_preEvent_mean_7d, SoilTemp_mean_preEvent_6)) %>% 
    # Also just going to focus on Pit 6 for now, so dropping Pit 1 vars for now
    select(-c(ends_with("1"))) %>% 
    # And to make this initial analyses more simple, I'm going to remove these vars as well
    select(-c(rain_int_mmPERmin_mean, rain_preEvent_30d, DO_mean_preEvent_6))    
  
# Look at histograms of all variables
allvars_hford %>% 
  pivot_longer(cols = rain_event_total_mm:VWC_mean_preEvent_3, names_to = "var", values_to = "value") %>% 
  ggplot(aes(value)) +
    facet_wrap(~ var, scales = "free") +
    geom_histogram()

# I'm just going to log transform all of the variables for now
# I also calculate z-scores here (x-mean(x)/sd(x))
hford_transformed <- allvars_hford %>% 
  # To make redox non-negative, let's add 1000 to each value
  mutate(Redox_mean_preEvent_3 = Redox_mean_preEvent_3 + 1000) %>% 
  # rain_preEvent_7d and time_sinceLastEvent have 0's, so let's add 1 to them before log transformation
  mutate(rain_preEvent_7d = rain_preEvent_7d + 1,
         time_sinceLastEvent = time_sinceLastEvent + 1) %>% 
  # Log transformations
  mutate_at(vars(c(rain_event_total_mm:VWC_mean_preEvent_3)),
            .funs = list(~ log(.))) %>% 
  # This is equivalent to the z-score formula
  # https://www.r-bloggers.com/r-tutorial-series-centering-variables-and-generating-z-scores-with-the-scale-function/
  mutate_at(vars(c(rain_event_total_mm:VWC_mean_preEvent_3)),
            .funs = list(~ scale(., center = TRUE, scale = TRUE)))

# I also calculate z-scores here (x-mean(x)/sd(x))
wade_transformed <- allvars_wade %>% 
  # To make redox non-negative, let's add 1000 to each value
  mutate(Redox_mean_preEvent_6 = Redox_mean_preEvent_6 + 1000) %>% 
  # rain_preEvent_7d and time_sinceLastEvent have 0's, so let's add 1 to them before log transformation
  mutate(rain_preEvent_1d = rain_preEvent_1d + 1,
         rain_preEvent_7d = rain_preEvent_7d + 1,
         time_sinceLastEvent = time_sinceLastEvent + 1) %>% 
  # Log transformations
  mutate_at(vars(c(rain_event_total_mm:VWC_mean_preEvent_6)),
            .funs = list(~ log(.))) %>% 
  # This is equivalent to the z-score formula
  # https://www.r-bloggers.com/r-tutorial-series-centering-variables-and-generating-z-scores-with-the-scale-function/
  mutate_at(vars(c(rain_event_total_mm:VWC_mean_preEvent_6)),
            .funs = list(~ scale(., center = TRUE, scale = TRUE))) %>% 
  # Remove the 1 winter event
  filter(season != "winter")

# Write data to CSV
hford_transformed %>% 
  mutate(event_start = as.character(event_start)) %>% 
  write_csv("Data/transformed_vars_hungerford.csv")

wade_transformed %>% 
  mutate(event_start = as.character(event_start)) %>% 
  write_csv("Data/transformed_vars_wade.csv")
  