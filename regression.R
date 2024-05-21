#### PREVALENCE ESTIMATES ####


#### SETUP ####

#### * Install packages ####

# install.packages("sandwich")
# install.packages("lmtest")
# install.packages("arm")
# install.packages("survey")
# install.packages("binom")
# install.packages("dplyr")

library(sandwich)
library(lmtest)
#library(arm)
library(survey)
library(binom)
library(dplyr)

# detach("arm", unload = TRUE) # arm package interferes with dplyr select function


#### * Create colourscheme ####

colour1 <- "#80CDC1" #Light aqua
colour2 <- "#018571" # Dark teal
colour3 <- "#B2ABD2" # Light purple
colour4 <- "#5E3C99" # Dark purple
colour5 <- "#404040" # Dark grey
colour6 <- "#FFC107" # (Primary) Bright amber
colour7 <- "#BDBDBD" # Light grey

# Gradient scheme using colour6
colour6a <- "#FFF350"
colour6b <- "#FFEC47"
colour6c <- "#FFE53E"
colour6d <- "#FFDB35"
colour6e <- "#FFD22C"
colour6f <- "#FFC107"

# Updated gradient scheme using colour6
colour6a <- "#FFF350"
colour6b <- "#FFD13F"
colour6c <- "#FFAA33"
colour6d <- "#FF8026"
colour6e <- "#FF551A"
colour6f <- "#FF2A0D"



#### * Create base dataframe ####

df <- dfe %>%
  subset(eligible_rec %in% 1) %>%
  select(individual_id,
         cluster_num, 
         casedef_who, casedef_inc_trace, casedef_exc_trace,
         geoclass,
         gender, agegp5, agegp10,
         tbhx, tbhx_current,
         symptom_cough, symptom_cough_prolonged, symptom_fever, symptom_weightloss, symptom_nightsweat, symptomscreen, 
         eligible_rec, screen_pos,
         radscore, cxr_abnormality_score, cxr_finding,
         res_xpert_a, res_xpert_b, res_xpert_binary, res_culture_a,
         reg)


# Select desired case definition:
#   casedef_who
#   casedef_inc_trace
#   casedef_exc_trace

# Rename chosen case definition to case
df <- df %>%
  rename(case = casedef_who)



#### * Create weights for unequal cluster size (for Model 1, 2) ####

# Calculate the size of each cluster
cluster_sizes <- table(df$cluster_num)

# Calculate the total number of participants in the study
total_population <- nrow(df)

# Create a weight variable as the total study population size divided by the cluster size
df$weight_cluster <- total_population / cluster_sizes[df$cluster_num]



#### * Create weights for group combos (for Model 3) ####

# Count N for each combination of cluster, age group, and sex - eligible population (N)
eligible_count <- df %>%
  subset(eligible_rec %in% 1) %>%
  group_by(cluster_num, agegp10, gender) %>%
  summarise(N = n(), .groups = 'drop')

# Count n for each combination of cluster, age group, and sex - survey participants (n)
participant_count <- df %>%
  subset(reg %in% 1) %>%
  group_by(cluster_num, agegp10, gender) %>%
  summarise(n = n(), .groups = 'drop')

# Merge the counts and calculate weights
df <- df %>%
  left_join(participant_count, by = c("cluster_num", "agegp10", "gender")) %>%
  left_join(eligible_count, by = c("cluster_num", "agegp10", "gender")) %>%
  mutate(n = ifelse(is.na(n), 0, n), # Set n to 0 if it is NA
         weight_group = ifelse(n == 0, 0, N / n)) # Set weight to 0 if n is 0

rm(eligible_count, participant_count)


#### * Combine cluster size and group weights (Model 3) ####

# Combine cluster weights with group weights
df <- df %>%
  mutate(weight_comb = weight_cluster * weight_group)


# Remove unused data
rm(cen, cen_hh, cen_in, clusters, dfd, int, lab, pop_agesex, pop_aldeia, rad, reg, cluster_sizes, total_population)



####   CLUSTER LEVEL ANALYSIS   ####

#### Crude Prevalence ####


#### * Results table ####

# Create dataframe for cluster level analysis results
a <- c("Cases", "Prevalence", "Standard_dev", "Standard_error", "Lower_95", "Upper_95")


# Urban

df1 <- df %>%
  subset(!is.na(case)) %>%
  subset(geoclass %in% "Urban") %>%
  select(cluster_num, geoclass, case) %>% 
  mutate(n = 1) %>%
  group_by(cluster_num, geoclass) %>%
  summarize(sum_case = sum(case), sum_n = sum(n)) %>%
  mutate(rate = sum_case / sum_n * 100000)


# Number of cases
ca_cases <- sum(df1$sum_case)

# Mean prevalence
ca_mean <- mean(df1$rate)

# Standard deviation
ca_stdev <- sd(df1$rate)

# Standard error
ca_se <- ca_stdev / sqrt(50)

# 95% confidence limits of mean prevalence
ca_95LL <- ca_mean - (ca_se * 2)
ca_95UL <- ca_mean + (ca_se * 2)

b <- c(ca_cases, ca_mean, ca_stdev, ca_se, ca_95LL, ca_95UL)


# Rural

df1 <- df %>%
  subset(!is.na(case)) %>%
  subset(geoclass %in% "Rural") %>%
  select(cluster_num, geoclass, case) %>% 
  mutate(n = 1) %>%
  group_by(cluster_num, geoclass) %>%
  summarize(sum_case = sum(case), sum_n = sum(n)) %>%
  mutate(rate = sum_case / sum_n * 100000)


# Number of cases
ca_cases <- sum(df1$sum_case)

# Mean prevalence
ca_mean <- mean(df1$rate)

# Standard deviation
ca_stdev <- sd(df1$rate)

# Standard error
ca_se <- ca_stdev / sqrt(50)

# 95% confidence limits of mean prevalence
ca_95LL <- ca_mean - (ca_se * 2)
ca_95UL <- ca_mean + (ca_se * 2)

c <- c(ca_cases, ca_mean, ca_stdev, ca_se, ca_95LL, ca_95UL)


# Male

df1 <- df %>%
  subset(!is.na(case)) %>%
  subset(gender %in% "Male") %>%
  select(cluster_num, geoclass, case) %>% 
  mutate(n = 1) %>%
  group_by(cluster_num, geoclass) %>%
  summarize(sum_case = sum(case), sum_n = sum(n)) %>%
  mutate(rate = sum_case / sum_n * 100000)


# Number of cases
ca_cases <- sum(df1$sum_case)

# Mean prevalence
ca_mean <- mean(df1$rate)

# Standard deviation
ca_stdev <- sd(df1$rate)

# Standard error
ca_se <- ca_stdev / sqrt(50)

# 95% confidence limits of mean prevalence
ca_95LL <- ca_mean - (ca_se * 2)
ca_95UL <- ca_mean + (ca_se * 2)

d <- c(ca_cases, ca_mean, ca_stdev, ca_se, ca_95LL, ca_95UL)


# Female

df1 <- df %>%
  subset(!is.na(case)) %>%
  subset(gender %in% "Female") %>%
  select(cluster_num, geoclass, case) %>% 
  mutate(n = 1) %>%
  group_by(cluster_num, geoclass) %>%
  summarize(sum_case = sum(case), sum_n = sum(n)) %>%
  mutate(rate = sum_case / sum_n * 100000)


# Number of cases
ca_cases <- sum(df1$sum_case)

# Mean prevalence
ca_mean <- mean(df1$rate)

# Standard deviation
ca_stdev <- sd(df1$rate)

# Standard error
ca_se <- ca_stdev / sqrt(50)

# 95% confidence limits of mean prevalence
ca_95LL <- ca_mean - (ca_se * 2)
ca_95UL <- ca_mean + (ca_se * 2)

e <- c(ca_cases, ca_mean, ca_stdev, ca_se, ca_95LL, ca_95UL)


# Agegp10 == "15-24y"

df1 <- df %>%
  subset(!is.na(case)) %>%
  subset(agegp10 %in% "15-24") %>%
  select(cluster_num, geoclass, case) %>% 
  mutate(n = 1) %>%
  group_by(cluster_num, geoclass) %>%
  summarize(sum_case = sum(case), sum_n = sum(n)) %>%
  mutate(rate = sum_case / sum_n * 100000)


# Number of cases
ca_cases <- sum(df1$sum_case)

# Mean prevalence
ca_mean <- mean(df1$rate)

# Standard deviation
ca_stdev <- sd(df1$rate)

# Standard error
ca_se <- ca_stdev / sqrt(50)

# 95% confidence limits of mean prevalence
ca_95LL <- ca_mean - (ca_se * 2)
ca_95UL <- ca_mean + (ca_se * 2)

f <- c(ca_cases, ca_mean, ca_stdev, ca_se, ca_95LL, ca_95UL)


# Agegp10 == "25-34y"

df1 <- df %>%
  subset(!is.na(case)) %>%
  subset(agegp10 %in% "25-34") %>%
  select(cluster_num, geoclass, case) %>% 
  mutate(n = 1) %>%
  group_by(cluster_num, geoclass) %>%
  summarize(sum_case = sum(case), sum_n = sum(n)) %>%
  mutate(rate = sum_case / sum_n * 100000)


# Number of cases
ca_cases <- sum(df1$sum_case)

# Mean prevalence
ca_mean <- mean(df1$rate)

# Standard deviation
ca_stdev <- sd(df1$rate)

# Standard error
ca_se <- ca_stdev / sqrt(50)

# 95% confidence limits of mean prevalence
ca_95LL <- ca_mean - (ca_se * 2)
ca_95UL <- ca_mean + (ca_se * 2)

g <- c(ca_cases, ca_mean, ca_stdev, ca_se, ca_95LL, ca_95UL)



# Agegp10 == "35-44y"

df1 <- df %>%
  subset(!is.na(case)) %>%
  subset(agegp10 %in% "35-44") %>%
  select(cluster_num, geoclass, case) %>% 
  mutate(n = 1) %>%
  group_by(cluster_num, geoclass) %>%
  summarize(sum_case = sum(case), sum_n = sum(n)) %>%
  mutate(rate = sum_case / sum_n * 100000)


# Number of cases
ca_cases <- sum(df1$sum_case)

# Mean prevalence
ca_mean <- mean(df1$rate)

# Standard deviation
ca_stdev <- sd(df1$rate)

# Standard error
ca_se <- ca_stdev / sqrt(50)

# 95% confidence limits of mean prevalence
ca_95LL <- ca_mean - (ca_se * 2)
ca_95UL <- ca_mean + (ca_se * 2)

h <- c(ca_cases, ca_mean, ca_stdev, ca_se, ca_95LL, ca_95UL)


# Agegp10 == "45-54y"

df1 <- df %>%
  subset(!is.na(case)) %>%
  subset(agegp10 %in% "45-54") %>%
  select(cluster_num, geoclass, case) %>% 
  mutate(n = 1) %>%
  group_by(cluster_num, geoclass) %>%
  summarize(sum_case = sum(case), sum_n = sum(n)) %>%
  mutate(rate = sum_case / sum_n * 100000)


# Number of cases
ca_cases <- sum(df1$sum_case)

# Mean prevalence
ca_mean <- mean(df1$rate)

# Standard deviation
ca_stdev <- sd(df1$rate)

# Standard error
ca_se <- ca_stdev / sqrt(50)

# 95% confidence limits of mean prevalence
ca_95LL <- ca_mean - (ca_se * 2)
ca_95UL <- ca_mean + (ca_se * 2)

i <- c(ca_cases, ca_mean, ca_stdev, ca_se, ca_95LL, ca_95UL)


# Agegp10 == "55-64y"

df1 <- df %>%
  subset(!is.na(case)) %>%
  subset(agegp10 %in% "55-64") %>%
  select(cluster_num, geoclass, case) %>% 
  mutate(n = 1) %>%
  group_by(cluster_num, geoclass) %>%
  summarize(sum_case = sum(case), sum_n = sum(n)) %>%
  mutate(rate = sum_case / sum_n * 100000)


# Number of cases
ca_cases <- sum(df1$sum_case)

# Mean prevalence
ca_mean <- mean(df1$rate)

# Standard deviation
ca_stdev <- sd(df1$rate)

# Standard error
ca_se <- ca_stdev / sqrt(50)

# 95% confidence limits of mean prevalence
ca_95LL <- ca_mean - (ca_se * 2)
ca_95UL <- ca_mean + (ca_se * 2)

j <- c(ca_cases, ca_mean, ca_stdev, ca_se, ca_95LL, ca_95UL)


# Agegp10 == "65+y"

df1 <- df %>%
  subset(!is.na(case)) %>%
  subset(agegp10 %in% "65+") %>%
  select(cluster_num, geoclass, case) %>% 
  mutate(n = 1) %>%
  group_by(cluster_num, geoclass) %>%
  summarize(sum_case = sum(case), sum_n = sum(n)) %>%
  mutate(rate = sum_case / sum_n * 100000)


# Number of cases
ca_cases <- sum(df1$sum_case)

# Mean prevalence
ca_mean <- mean(df1$rate)

# Standard deviation
ca_stdev <- sd(df1$rate)

# Standard error
ca_se <- ca_stdev / sqrt(50)

# 95% confidence limits of mean prevalence
ca_95LL <- ca_mean - (ca_se * 2)
ca_95UL <- ca_mean + (ca_se * 2)

k <- c(ca_cases, ca_mean, ca_stdev, ca_se, ca_95LL, ca_95UL)



# Total

# Create dataframe for cluster level analysis
df1 <- df %>%
  subset(!is.na(case)) %>%
  select(cluster_num, geoclass, case) %>% 
  mutate(n = 1) %>%
  group_by(cluster_num, geoclass) %>%
  summarize(sum_case = sum(case), sum_n = sum(n)) %>%
  mutate(rate = sum_case / sum_n * 100000)


# Number of cases
ca_cases <- sum(df1$sum_case)

# Mean prevalence
ca_mean <- mean(df1$rate)

# Standard deviation
ca_stdev <- sd(df1$rate)

# Standard error
ca_se <- ca_stdev / sqrt(50)

# 95% confidence limits of mean prevalence
ca_95LL <- ca_mean - (ca_se * 2)
ca_95UL <- ca_mean + (ca_se * 2)

l <- c(ca_cases, ca_mean, ca_stdev, ca_se, ca_95LL, ca_95UL)



# Join results into one dataframe

Urban <- round(data.frame(t(b)), 2)
Rural <- round(data.frame(t(c)), 2)
Male <- round(data.frame(t(d)), 2)
Female <- round(data.frame(t(e)), 2)
ag15.24 <- round(data.frame(t(f)), 2)
ag25.34 <- round(data.frame(t(g)), 2)
ag35.44 <- round(data.frame(t(h)), 2)
ag45.54 <- round(data.frame(t(i)), 2)
ag55.64 <- round(data.frame(t(j)), 2)
ag65 <- round(data.frame(t(k)), 2)
Total <- round(data.frame(t(l)), 2)


prevalence_ca <- bind_rows(Urban, Rural, Male, Female, ag15.24, ag25.34, ag35.44, ag45.54, ag55.64,ag65, Total)
colnames(prevalence_ca) <- a

prevalence_ca$Group <- c("Urban", "Rural","Male", "Female", 
                         "15-24y" , "25-34y", "35-44y", "45-54y", "55-64y", "65+y",
                         "Total")

prevalence_ca <- prevalence_ca[, c("Group", "Cases", "Standard_dev", "Standard_error", "Prevalence", "Lower_95", "Upper_95")]


# Remove excess dataframes and values
rm(a, b, c, d, e, f, g, h, i, j, k, l,
   ca_mean, ca_stdev, ca_se, ca_95LL, ca_95UL, Urban, Rural, Male, Female,
   ag15.24, ag25.34, ag35.44, ag45.54, ag55.64, ag65, 
   Total)




#### * Figures #### 

# Histogram of cluster frequency by prevalence
p1 <- ggplot(df1, aes(x = rate)) +
  geom_histogram(binwidth = 500, fill = colour1, color = colour5) +
  labs(title = "Histogram of cluster frequency, by prevalence",
       x = "Prevalence per 100,000 population",
       y = "Frequency") +
  theme_classic2()

p1


# Prevalence rates by group
prevalence_ca$Group <- factor(prevalence_ca$Group, 
                              levels = c("Urban", "Rural", "Male", "Female", 
                                         "15-24y" , "25-34y", "35-44y", "45-54y", "55-64y", "65+y",
                                         "Total"))  

p2 <- ggplot(prevalence_ca, aes(x = Group, y = Prevalence)) +
  geom_col(aes(fill = Group), color = colour5, position = "dodge") +
  geom_errorbar(aes(ymin = Lower_95, ymax = Upper_95), width = 0.25, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("Urban" = colour1, "Rural" = colour2, 
                               "Male" = colour3, "Female" = colour4, 
                               "15-24y" = colour6a, "25-34y" = colour6b, "35-44y" = colour6c, 
                               "45-54y" = colour6d, "55-64y" = colour6e, "65+y" = colour6f,
                               "Total" = colour7)) +
  labs(title = "Cluster-Level Analysis of Prevalence, by Group",
       x = "Group",
       y = "Prevalence Rate (per 100,000") +
  theme_classic2() +
  guides(fill = "none")

p2



#### INDIVIDUAL LEVEL ANALYSIS ####

#### ~Model 1~ ####

# Robust standard errors, no missing value imputation, no weighting, limited to participants


#### * Create dataframe ####

df1 <- df %>%
  subset(!is.na(case)) %>%
  select(cluster_num, agegp10, gender, geoclass, case, weight_cluster) %>%
  mutate(n = 1)


# Create a vector of variable names that you do not want to convert to factors
vars_to_exclude <- c("cxr_abnormality_score", "weight_cluster")

# Get the variable names to convert by excluding the specified variables
vars_to_convert <- setdiff(names(df1), vars_to_exclude)

# Convert the specified variables to factors
df1[vars_to_convert] <- lapply(df1[vars_to_convert], factor)

rm(vars_to_convert, vars_to_exclude)


#### * Create model ####

# Create survey design object with weights
design <- svydesign(id = ~cluster_num, strata = ~geoclass, weights = ~weight_cluster, data = df1)

# Fit the logistic regression model with survey design
model <- svyglm(case ~ geoclass + agegp10 + gender, family = quasibinomial(link = "logit"), design = design)

# Predict probability of the outcome
df1$predicted_prob <- predict(model, design = design, type = "response")

# Aggregate prediction of overall prevalence rate
agg_predictions_tot <- data.frame(predicted_prob = mean(df1$predicted_prob))

# Calculate prevalence rate per 100,000 people
agg_predictions_tot$prevalence_rate_per_100000 <- agg_predictions_tot$predicted_prob * 100000 


#### * Robust SE for overall estimate CI ####

# Calculate robust SE for overall prevalence
vcov_matrix <- vcovHC(model, type = "HC0")
se_overall_prevalence <- sqrt(vcov_matrix["(Intercept)", "(Intercept)"]) * 100000

# Calculate the confidence limits for the prevalence rate using robust standard errors
z_value <- qnorm(0.975) # Z-value for 95% confidence interval
agg_predictions_tot$lower_ci <- agg_predictions_tot$prevalence_rate_per_100000 - (z_value * se_overall_prevalence)
agg_predictions_tot$upper_ci <- agg_predictions_tot$prevalence_rate_per_100000 + (z_value * se_overall_prevalence)


#### * Robust SE for group estimates CIs ####

# Aggregate prediction of prevalence rate by group
agg_predictions_group <- df1 %>%
  group_by(geoclass, agegp10, gender) %>%
  summarize(predicted_prob = mean(predicted_prob), .groups = 'drop')

# Calculate prevalence rate per 100,000 people
agg_predictions_group$prevalence_rate_per_100000 <- agg_predictions_group$predicted_prob * 100000

# Function to calculate standard errors for predicted probabilities
calculate_group_se <- function(model, group_data) {
  model_matrix <- model.matrix(model)
  vcov_matrix <- vcovHC(model, type = "HC0")
  
  group_se <- apply(group_data, 1, function(row) {
    # Initialize the row matrix for the group
    row_matrix <- model_matrix[1, , drop = FALSE]
    row_matrix[] <- 0
    row_matrix[, "(Intercept)"] <- 1
    
    # Set the values for each predictor in the group
    for (predictor in names(row)) {
      predictor_name <- paste(predictor, row[predictor], sep = "")
      if (predictor_name %in% colnames(row_matrix)) {
        row_matrix[, predictor_name] <- 1
      }
    }
    
    # Calculate the variance for the row
    var_pred <- row_matrix %*% vcov_matrix %*% t(row_matrix)
    
    # Return the standard error
    return(sqrt(var_pred))
  })
  
  return(group_se)
}

# Prepare group data for SE calculation
group_data <- agg_predictions_group %>%
  select(geoclass, agegp10, gender)

# Calculate standard errors for group-level predictions
agg_predictions_group$se_group <- calculate_group_se(model, group_data)

# Calculate confidence intervals for group-level predictions
agg_predictions_group <- agg_predictions_group %>%
  mutate(lower_ci = prevalence_rate_per_100000 - (z_value * se_group * 100000),
         upper_ci = prevalence_rate_per_100000 + (z_value * se_group * 100000))


#### * Results ####

# Final results
model_1_overall <- agg_predictions_tot
model_1_bygroup <- agg_predictions_group

rm(agg_predictions_group, agg_predictions_tot, vcov_matrix, group_data, model, design, se_overall_prevalence, z_value, calculate_group_se)




#### ~Model 2 - version 2~ ####

#### * Run imputation script ####

# < See separate script - Imputation_1.R >


#### * Create dataframe ####
df1 <- df5 %>%
  subset(!is.na(case)) %>% # limits df to participants only
  select(cluster_num, agegp10, gender, geoclass, case, cxr_finding, res_xpert_a, res_xpert_b, res_culture_a, weight_cluster) %>%
  mutate(n = 1)


# Create a vector of variable names that you do not want to convert to factors
vars_to_exclude <- c("cxr_abnormality_score", "weight_cluster")

# Get the variable names to convert by excluding the specified variables
vars_to_convert <- setdiff(names(df1), vars_to_exclude)

# Convert the specified variables to factors
df1[vars_to_convert] <- lapply(df1[vars_to_convert], factor)

rm(vars_to_convert, vars_to_exclude)


#### * Create model ####

# Create survey design object with weights
design <- svydesign(id = ~cluster_num, strata = ~geoclass, weights = ~weight_cluster, data = df1)

# Fit the logistic regression model with survey design
model <- svyglm(case ~ geoclass + agegp10 + gender, family = quasibinomial(link = "logit"), design = design)

# Predict probability of the outcome
df1$predicted_prob <- predict(model, design = design, type = "response")

# Aggregate prediction of overall prevalence rate
agg_predictions_tot <- data.frame(predicted_prob = mean(df1$predicted_prob))

# Calculate prevalence rate per 100,000 people
agg_predictions_tot$prevalence_rate_per_100000 <- agg_predictions_tot$predicted_prob * 100000 


#### * Robust SE for overall estimate CI ####

# Calculate robust SE for overall prevalence
vcov_matrix <- vcovHC(model, type = "HC0")
se_overall_prevalence <- sqrt(vcov_matrix["(Intercept)", "(Intercept)"]) * 100000

# Calculate the confidence limits for the prevalence rate using robust standard errors
z_value <- qnorm(0.975) # Z-value for 95% confidence interval
agg_predictions_tot$lower_ci <- agg_predictions_tot$prevalence_rate_per_100000 - (z_value * se_overall_prevalence)
agg_predictions_tot$upper_ci <- agg_predictions_tot$prevalence_rate_per_100000 + (z_value * se_overall_prevalence)


#### * Robust SE for group estimates CIs ####

# Aggregate prediction of prevalence rate by group
agg_predictions_group <- df1 %>%
  group_by(geoclass, agegp10, gender) %>%
  summarize(predicted_prob = mean(predicted_prob), .groups = 'drop')

# Calculate prevalence rate per 100,000 people
agg_predictions_group$prevalence_rate_per_100000 <- agg_predictions_group$predicted_prob * 100000

# Function to calculate standard errors for predicted probabilities
calculate_group_se <- function(model, group_data) {
  model_matrix <- model.matrix(model)
  vcov_matrix <- vcovHC(model, type = "HC0")
  
  group_se <- apply(group_data, 1, function(row) {
    # Initialize the row matrix for the group
    row_matrix <- model_matrix[1, , drop = FALSE]
    row_matrix[] <- 0
    row_matrix[, "(Intercept)"] <- 1
    
    # Set the values for each predictor in the group
    for (predictor in names(row)) {
      predictor_name <- paste(predictor, row[predictor], sep = "")
      if (predictor_name %in% colnames(row_matrix)) {
        row_matrix[, predictor_name] <- 1
      }
    }
    
    # Calculate the variance for the row
    var_pred <- row_matrix %*% vcov_matrix %*% t(row_matrix)
    
    # Return the standard error
    return(sqrt(var_pred))
  })
  
  return(group_se)
}

# Prepare group data for SE calculation
group_data <- agg_predictions_group %>%
  select(geoclass, agegp10, gender)

# Calculate standard errors for group-level predictions
agg_predictions_group$se_group <- calculate_group_se(model, group_data)

# Calculate confidence intervals for group-level predictions
agg_predictions_group <- agg_predictions_group %>%
  mutate(lower_ci = prevalence_rate_per_100000 - (z_value * se_group * 100000),
         upper_ci = prevalence_rate_per_100000 + (z_value * se_group * 100000))


#### * Results ####

# Final results
model_2_overall <- agg_predictions_tot
model_2_bygroup <- agg_predictions_group

rm(agg_predictions_group, agg_predictions_tot, vcov_matrix, group_data, model, design, se_overall_prevalence, z_value, calculate_group_se)





#### ~Model 3~ ####


#### * Create dataframe ####

# Retrieve dataset with imputation
df1 <- df5 %>%
  subset(!is.na(case)) # limits df to participants only


# Create a vector of variable names that you do not want to convert to factors
vars_to_exclude <- c("cxr_abnormality_score", "weight_cluster", "weight_group", "weight_comb")

# Get the variable names to convert by excluding the specified variables
vars_to_convert <- setdiff(names(df1), vars_to_exclude)

# Convert the specified variables to factors
df1[vars_to_convert] <- lapply(df1[vars_to_convert], factor)

rm(vars_to_convert, vars_to_exclude)


#### * Create model ####

# Create survey design object with weights
design <- svydesign(id = ~cluster_num, strata = ~geoclass, weights = ~weight_comb, data = df1)

# Fit the logistic regression model with survey design
model <- svyglm(case ~ geoclass + agegp10 + gender, family = quasibinomial(link = "logit"), design = design)

# Predict probability of the outcome
df1$predicted_prob <- predict(model, type = "response")


# Aggregate prediction of overall prevalence rate
agg_predictions_tot <- data.frame(predicted_prob = mean(df1$predicted_prob))

# Calculate prevalence rate per 100,000 people
agg_predictions_tot$prevalence_rate_per_100000 <- agg_predictions_tot$predicted_prob * 100000 



#### * Robust SE for overall estimate CI ####

# Calculate robust SE for overall prevalence
vcov_matrix <- vcovHC(model, type = "HC0")
se_overall_prevalence <- sqrt(vcov_matrix["(Intercept)", "(Intercept)"]) * 100000

# Calculate the confidence limits for the prevalence rate using robust standard errors
z_value <- qnorm(0.975) # Z-value for 95% confidence interval
agg_predictions_tot$lower_ci <- agg_predictions_tot$prevalence_rate_per_100000 - (z_value * se_overall_prevalence)
agg_predictions_tot$upper_ci <- agg_predictions_tot$prevalence_rate_per_100000 + (z_value * se_overall_prevalence)


#### * Robust SE for group estimates CIs ####

# Aggregate prediction of prevalence rate by group
agg_predictions_group <- df1 %>%
  group_by(geoclass, agegp10, gender) %>%
  summarize(predicted_prob = mean(predicted_prob), .groups = 'drop')

# Calculate prevalence rate per 100,000 people
agg_predictions_group$prevalence_rate_per_100000 <- agg_predictions_group$predicted_prob * 100000

# Function to calculate standard errors for predicted probabilities
calculate_group_se <- function(model, group_data) {
  model_matrix <- model.matrix(model)
  vcov_matrix <- vcovHC(model, type = "HC0")
  
  group_se <- apply(group_data, 1, function(row) {
    # Initialize the row matrix for the group
    row_matrix <- model_matrix[1, , drop = FALSE]
    row_matrix[] <- 0
    row_matrix[, "(Intercept)"] <- 1
    
    # Set the values for each predictor in the group
    for (predictor in names(row)) {
      predictor_name <- paste(predictor, row[predictor], sep = "")
      if (predictor_name %in% colnames(row_matrix)) {
        row_matrix[, predictor_name] <- 1
      }
    }
    
    # Calculate the variance for the row
    var_pred <- row_matrix %*% vcov_matrix %*% t(row_matrix)
    
    # Return the standard error
    return(sqrt(var_pred))
  })
  
  return(group_se)
}

# Prepare group data for SE calculation
group_data <- agg_predictions_group %>%
  select(geoclass, agegp10, gender)

# Calculate standard errors for group-level predictions
agg_predictions_group$se_group <- calculate_group_se(model, group_data)

# Calculate confidence intervals for group-level predictions
agg_predictions_group <- agg_predictions_group %>%
  mutate(lower_ci = prevalence_rate_per_100000 - (z_value * se_group * 100000),
         upper_ci = prevalence_rate_per_100000 + (z_value * se_group * 100000))


#### * Results ####

# Final results
model_3_overall <- agg_predictions_tot
model_3_bygroup <- agg_predictions_group

rm(agg_predictions_group, agg_predictions_tot, vcov_matrix, group_data, model, design, se_overall_prevalence, z_value, calculate_group_se)

