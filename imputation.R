
#### SETUP ####

# install.packages("VIM")
# install.packages("mice")

library(VIM)
library(mice)


#### CREATE BASE DATASET ####

# df <- dfe %>%
#   select(individual_id,
#           cluster_num, 
#           casedef_who, casedef_inc_trace, casedef_exc_trace,
#           geoclass,
#           gender, agegp5, agegp10,
#           tbhx, tbhx_current,
#           symptom_cough, symptom_cough_prolonged, symptom_fever, symptom_weightloss, symptom_nightsweat, symptomscreen, 
#           eligible_rec, screen_pos,
#           radscore, cxr_abnormality_score,
#           res_xpert_a, res_xpert_b, res_xpert_binary, res_culture_a) %>%
#   subset(eligible_rec %in% 1)


# # Convert specified variables to factors
# vars_to_convert <- names(df)[-which(names(df) == "cxr_abnormality_score")]
# df[vars_to_convert] <- lapply(df[vars_to_convert], factor)


# Visualise missing data
aggr(df)

#pct_complete_case(df)


#### STEP 1 ####

# Impute missing data on survey TB case status – where no valid culture result


#### * Subset data ####

# Create dataframe subset to those with 1+ positve Ultra result including those with and without a valid culture result
df1 <- df %>%
  subset((res_xpert_a != 0 | res_xpert_b !=0)) 



#### * Impute data ####

# Select the variables for imputation
variables_for_imputation <- c("res_xpert_a", "res_xpert_b", "tbhx", "tbhx_current")

# Impute missing values for the variable "case" using the selected variables
imputed_data <- complete(
  mice(
    df1[, c("case", variables_for_imputation)], 
    method = "pmm", 
    m = 100,
    action = "long"
  )
)

# Replace the imputed values for "case" in the original dataframe
df1$case <- imputed_data$case



#### STEP 2 ####

# Impute missing data on survey TB case status – where no valid Ultra result


#### * Subset data ####

# Create dataframe subset to those eligible for sputum testing including those with and without valid Ultra results
df2 <- df %>%
  subset(screen_pos %in% 1) %>%
  subset(is.na(res_xpert_binary))

# Combine imputed dataframe from step 1 with dataframe containing sputum eligible but not Ultra positive participants
df3 <- rbind(df1, df2)



#### * Impute data ####

# Select the variables for imputation
variables_for_imputation <- c("agegp5", "gender", "geoclass",
                              "cxr_abnormality_score", 
                              "symptom_cough", "symptom_cough_prolonged", "symptom_fever", "symptom_weightloss", "symptom_nightsweat",
                              "tbhx", "tbhx_current")

# Impute missing values for the variable "case" using the selected variables
imputed_data <- complete(
  mice(
    df3[, c("case", variables_for_imputation)], 
    method = "pmm", 
    m = 1,
    action = "long"
  )
)

# Replace the imputed values for "case" in the original dataframe
df3$case <- imputed_data$case



#### COMBINE DATA ####

df4 <- df %>%
  subset(is.na(screen_pos)) # Participants not eligible for sputum examination (including non-participants)

df5 <- rbind(df3, df4) %>%
  arrange(individual_id) # Combine with participants eligible for sputum examination 

# df5 now contains all data of original df, but with multiple imputation of case status completed

rm(df2, df3, df4, variables_for_imputation, imputed_data)

