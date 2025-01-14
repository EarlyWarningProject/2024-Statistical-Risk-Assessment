# Code to run main models
# Now amended to do lots of investigation and comparison stuff.
# Aug 28, 2024


library(purrr)
library(dplyr)
library(glmnet)
library(ggplot2)
library(pROC)

setwd("/Users/nmbryce/Documents/R/SRA/2024-Statistical-Risk-Assessment/3.Modeling")
savenameaffix = "27Sept2024"

# Function to use at estimation time ====
elogit.fit <- function(ytrain, Xtrain, Xtest, alpha = .5, usepredictors=NULL){
    
  #Don't deal with missingness -- assume it has already been dealt with
  #to force user to recognize it on the data side.
  xtrain = as.matrix(subset(Xtrain, select = usepredictors))
  xtest = as.matrix(subset(Xtest, select = usepredictors))
  set.seed(90035)
  
  # CJH: adding this to just fit the glmnet so we can see regularization path
  elastic.model <- glmnet(y = ytrain, x = xtrain, alpha = alpha, family = "binomial")
  
  #CJH: then back to this to produce cross-validated fit we need
  elastic.cv <- cv.glmnet(y=ytrain, x=xtrain, alpha=alpha, family="binomial")
  coeffs <- coef(elastic.cv, s = "lambda.min")
  elastic.predictions = predict(elastic.cv,newx=xtest, s="lambda.min", type="response")
  
  risk <- as.numeric(elastic.predictions)
  #out <- list(risk, coeffs, elastic.cv)
  out = list()
  out$risk = risk
  out$coeffs = coeffs
  out$elastic.cv = elastic.cv
  out$elastic.model = elastic.model #CJH: outputting this now as well
  return(out)
}

# Read in data ====
alldata = read.csv("prepared2023predictors-2024-09-11.csv")

# Define models ====

outcomenames = c("mk_onset","mk_onset_1","mk_onset_2", "mk_onset_1or2")

#create interaction term
alldata <- alldata %>% 
  mutate(mean_mk_onset_interaction = mean_mk_onset*years_in_dataset)

# Model "1"

predictornames.fullest <- c("widetargeting", "narrowtargeting", "v2csgender_binary", 
                            "v2mecenefm_binary", "partyban.new.0", "partyban.new.1", 
                            "partyban.new.2", "minorityrule", "judicialreform", 
                            "religiousfreedom", "pol_killing_approved", "freediscussion", 
                            "social_inequality", "even_civilrights", "repress_civilsoc", 
                            "social_power_dist", "ses_power_dist", "wdi.popsize.log2", 
                            "gdppcgrowth.combined", "tradeshare.log2.combined", 
                            "imr.fwd.fill.sqrt", "efindex", "discrimpop", "reg.na", "reg.eap", 
                            "reg.sca", "reg.mna", "reg.eur", "ios.iccpr1",  "battledeaths.ln2", 
                            "coup.try.5yr", "mk_ongoing_count_log", "mean_mk_onset_interaction", 
                            "mk_onset_prev_year", "newcountry", "mk_ever", "year_sq", "year",
                            "countryage_new_ln", "includesnonstate")


predictornames.offsetdecay <- c("widetargeting", "narrowtargeting", "v2csgender_binary", 
                                "v2mecenefm_binary", "partyban.new.0", "partyban.new.1", 
                                "partyban.new.2", "minorityrule", "judicialreform", 
                                "religiousfreedom", "pol_killing_approved", "freediscussion", 
                                "social_inequality", "even_civilrights", "repress_civilsoc", 
                                "social_power_dist", "ses_power_dist", "wdi.popsize.log2", 
                                "gdppcgrowth.combined", "tradeshare.log2.combined", 
                                "imr.fwd.fill.sqrt", "efindex", "discrimpop", "reg.na", "reg.eap", 
                                "reg.sca", "reg.mna", "reg.eur", "ios.iccpr1",  "battledeaths.ln2", 
                                "coup.try.5yr", "mk_ongoing_count_log", "mean_mk_onset_interaction", 
                                "mk_onset_prev_year", "newcountry", "mk_ever", "year_sq", "year",
                                "countryage_new_ln", "includesnonstate","offset_decay")

predictornames.simple <- c("un_imr_sqrt", "mk_ongoing_count_log")




# 
# # Model 3: The old model, though on somewhat new data 
# predictornames.old = c("mk_ongoing","mk_ever", 
#                       "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca", 
#                       "countryage_new_ln", "popsize.ln2.combined", "imr.sqrt", 
#                       "gdppcgrowth.combined", "ios.iccpr1","includesnonstate",
#                       "minorityrule", "elf.ethnic", "battledeaths.ln2",
#                       "candidaterestriction", "partyban","judicialreform",
#                       "religiousfreedom", "pol_killing_approved",
#                       "freemove_men4","freemove_women4", "freediscussion",
#                       "social_inequality","even_civilrights","repress_civilsoc",
#                       "social_power_dist", "ses_power_dist","tradeshare.ln.combined",
#                       "coup.try.5yr")
# 
# setdiff(predictornames.old, names(alldata))

# Just to review:
# What's in the old variables but not new?
#setdiff(predictornames.old, predictornames.fullest)
# What's in new and not the old?
#setdiff(predictornames.fullest, predictornames.old)


# Non-prediction variables worth keeping around
extravariables = c("ewp_name","ccode","year")

#-----------------------------------------------------#
# Run models ==== 
# Do it in a tedious way, repeating code for each model
# to keep separate records 
#-----------------------------------------------------#

### Model 1: new, full ====

# Construct training and test data, pausing to look at missingness.
traindata.full = alldata %>% filter(year<=2021) %>% 
  select(all_of(c(predictornames.fullest, outcomenames, extravariables)))
nrow(traindata.full)
table(traindata.full$year)


#now remove missing to compare
traindata.full = traindata.full %>% filter(complete.cases(.))
nrow(traindata.full)
table(traindata.full$year)


# Repeat for 2022's forecasts
# Construct training and test data, pausing to look at missingness.
traindata.full.2022 = alldata %>% filter(year<=2020) %>% 
  select(all_of(c(predictornames.fullest, outcomenames, extravariables)))
nrow(traindata.full.2022)
table(traindata.full.2022$year)

#now remove missing to compare
traindata.full.2022 = traindata.full.2022 %>% filter(complete.cases(.))
nrow(traindata.full.2022)
table(traindata.full.2022$year)

# Test data:
testdata.full = alldata %>% filter(year==2023) %>% 
                      select(all_of(c(predictornames.fullest, extravariables)))

testdata.full.2022 = alldata %>% filter(year==2022) %>% 
  select(all_of(c(predictornames.fullest, extravariables)))

# Use the lagged years in the test so we aren't extrapolating
# We'll turn this off...
#testdata.full$year = testdata.full$year_l1
#testdata.full$year_sq = testdata.full$year_sq_l1

nrow(testdata.full)

testdata.full = testdata.full %>% filter(complete.cases(.))
nrow(testdata.full)

testdata.full.2022 = testdata.full.2022 %>% filter(complete.cases(.))
nrow(testdata.full.2022)


model.full.out = elogit.fit(ytrain=traindata.full$mk_onset_1or2, 
                  Xtrain = traindata.full,
                  Xtest = testdata.full,
                  alpha = .5,
                  usepredictors = predictornames.fullest)

# Check the regularization info 
elastic.model = model.full.out$elastic.model
colnames(elastic.model$beta) <- colnames(predictornames.fullest)
# Plot the regularization paths without labels
plot(elastic.model, xvar = "lambda", label = FALSE)

lambda_min_index <- which.min(elastic.model$lambda)
coef_values <- elastic.model$beta[, lambda_min_index]

# Add custom labels using text()
for (i in 1:length(coef_values)) {
  if (abs(coef_values[i]) > 0.01) { # Filter to label significant coefficients
    text(x = .1+log(elastic.model$lambda[lambda_min_index]), 
         y = coef_values[i], 
         labels = predictornames.fullest[i], 
         pos = 4, cex = 0.7, col = "black")
  }
}

# Check just one variable of interest at a time:
thisvar = "mk_ongoing_count_log"
#thisvar = "mk_ever"
coef_values = elastic.model$beta[which(predictornames.fullest==thisvar), ]

lambda_values = elastic.model$lambda
coef_data = data.frame(Lambda = lambda_values, Coefficient = as.numeric(coef_values))

print(coef_data)
plot(log(lambda_values), coef_values, pch = 19, 
     xlab = "log(Lambda)", ylab = "Coefficient", 
     main = paste("Coefficient Path for", thisvar))


# Save the cv.glmnet object to a file
saveRDS(model.full.out$elastic.cv, file = "main_full_cv_glmnet_object_11Sep2024.rds")


model.full.out$coeffs

full.coefs.df = data.frame(
  variable = rownames(model.full.out$coeffs),
  coef.full = as.vector(model.full.out$coeffs))

#Add odds ratio
full.coefs.df = full.coefs.df %>% mutate(or.full = exp(coef.full))

results.full = data.frame(ewp_name = testdata.full$ewp_name, 
                     risk_1or2.from2023 = model.full.out$risk) %>%
  mutate(risk_rank.from2023 = rank(desc(risk_1or2.from2023), na.last = "keep")) %>% 
  arrange(desc(risk_1or2.from2023))

# For last year (2022-based) predictions
model.full.2022.out = elogit.fit(ytrain=traindata.full.2022$mk_onset_1or2, 
                            Xtrain = traindata.full.2022,
                            Xtest = testdata.full.2022,
                            alpha = .5,
                            usepredictors = predictornames.fullest)

model.full.2022.out$coeffs

full.2022.coefs.df = data.frame(
  variable = rownames(model.full.2022.out$coeffs),
  coef.full.2022 = as.vector(model.full.2022.out$coeffs))

#Add odds ratio
full.2022.coefs.df = full.2022.coefs.df %>% 
  mutate(or.full.2022 = exp(coef.full.2022))

results.full.2022 = data.frame(ewp_name = testdata.full.2022$ewp_name, 
                          risk_1or2.from2022 = model.full.2022.out$risk) %>%
  mutate(risk_rank.from2022 = rank(desc(risk_1or2.from2022), na.last = "keep")) %>% 
  arrange(desc(risk_1or2.from2022))

#View(results.full.2022)

### Put together outputs in desired format ====

# first get one-year ahead forecast
model.1yr.out = elogit.fit(ytrain=traindata.full$mk_onset_1, 
                            Xtrain = traindata.full,
                            Xtest = testdata.full,
                            alpha = .5,
                            usepredictors = predictornames.fullest)

results.1yr = data.frame(ewp_name = testdata.full$ewp_name, 
                         risk.2024only.from2023 = model.1yr.out$risk) %>%
  mutate(riskrank.2024only.from2023 = rank(desc(risk.2024only.from2023), na.last = "keep")) 

# Now merge everything up
bothforecasts <- results.full %>%
  left_join(results.1yr, by = "ewp_name")

# Merge in the prior year forecasts

bothforecastsandprior = bothforecasts %>% left_join(results.full.2022, by="ewp_name")

# Now merge that with dataset3
finalresults <- bothforecastsandprior %>%
  left_join(testdata.full, by = "ewp_name")

finalresults = finalresults %>% 
  rename(risk.2024and2025.from2023 = risk_1or2.from2023,
         rank.2024and2025.from2023 = risk_rank.from2023,
         risk.2023and2024.from2022 = risk_1or2.from2022, 
         rank.2023and2024.from2022 = risk_rank.from2022)

View(finalresults)

# Round as per Vincent's request
finalresults <- finalresults %>%
  mutate_if(is.numeric, ~signif(.x, 4))

# Now add 2022-based forecasts for comparison in prior year
write.csv(finalresults, file=paste0("final_forecasts_data_",savenameaffix,".csv"))

### Model 2: offset decay

# Construct training and test data, pausing to look at missingness.
traindata.offsetdecay = alldata %>% filter(year<=2021) %>% 
  select(all_of(c(predictornames.offsetdecay, outcomenames, extravariables)))
nrow(traindata.offsetdecay)
table(traindata.offsetdecay$year)

#now remove missing to compare
traindata.offsetdecay = traindata.offsetdecay %>% filter(complete.cases(.))
nrow(traindata.offsetdecay)
table(traindata.offsetdecay$year)

testdata.offsetdecay = alldata %>% filter(year==2023) %>% 
  select(all_of(c(predictornames.offsetdecay, extravariables)))

nrow(testdata.offsetdecay)

testdata.offsetdecay = testdata.offsetdecay %>% filter(complete.cases(.))
nrow(testdata.offsetdecay)

model.offsetdecay.out = elogit.fit(ytrain=traindata.offsetdecay$mk_onset_1or2, 
                             Xtrain = traindata.offsetdecay,
                             Xtest = testdata.offsetdecay,
                             alpha = .5,
                             usepredictors = predictornames.offsetdecay)

model.offsetdecay.out$coeffs


offsetdecay.coefs.df = data.frame(
  variable = rownames(model.offsetdecay.out$coeffs),
  coef.offsetdecay = as.vector(model.offsetdecay.out$coeffs))

#Add odds ratio
offsetdecay.coefs.df = offsetdecay.coefs.df %>% mutate(or.offsetdecay = exp(coef.offsetdecay))

results.offsetdecay = data.frame(ewp_name = testdata.offsetdecay$ewp_name, 
                           risk_1or2.offsetdecay = model.offsetdecay.out$risk) %>%
  mutate(risk_rank.offsetdecay = rank(desc(risk_1or2.offsetdecay), na.last = "keep")) %>% 
  arrange(desc(risk_1or2.offsetdecay))

View(results.offsetdecay)

### Compare Regularization Paths

#reguluar model graph
coeffs <- as.matrix(model.full.out$elastic.cv$glmnet.fit$beta)
lambda_values <- model.full.out$elastic.cv$lambda

# Identify and remove the intercept
coeffs_no_intercept <- coeffs[rownames(coeffs) != "(Intercept)", ]
variable_names_no_intercept <- rownames(coeffs_no_intercept)

# Plot the regularization path without the intercept
matplot(log(lambda_values), t(coeffs_no_intercept), type = "l", lty = 1, col = 1:nrow(coeffs_no_intercept),
        xlab = "Log(Lambda)", ylab = "Coefficients", main = "Regularization Path - Regular Model")
abline(h = 0, lty = 2)

#od model graph
# Extract coefficients matrix and lambda values for the offset decay model
od_coeffs <- as.matrix(model.offsetdecay.out$elastic.cv$glmnet.fit$beta)
lambda_values_od <- model.offsetdecay.out$elastic.cv$lambda

# Identify and remove the intercept
od_coeffs_no_intercept <- od_coeffs[rownames(od_coeffs) != "(Intercept)", ]
od_variable_names_no_intercept <- rownames(od_coeffs_no_intercept)

# Plot the regularization path without the intercept
matplot(log(lambda_values_od), t(od_coeffs_no_intercept), type = "l", lty = 1, col = 1:nrow(od_coeffs_no_intercept),
        xlab = "Log(Lambda)", ylab = "Coefficients", main = "Regularization Path - OD Model")
abline(h = 0, lty = 2)


#Table of drop out times - regular model

# Initialize vector to store drop-out times
drop_out_times <- rep(NA, nrow(coeffs_no_intercept))
names(drop_out_times) <- rownames(coeffs_no_intercept)

# Identify drop-out times for each variable
for (i in 1:nrow(coeffs_no_intercept)) {
  # Find indices where coefficient is non-zero
  non_zero_idx <- which(coeffs_no_intercept[i, ] != 0)
  
  if (length(non_zero_idx) > 0) {
    # Record the lambda value where coefficient first becomes zero
    drop_out_times[i] <- lambda_values[min(non_zero_idx)]
  } else {
    drop_out_times[i] <- NA
  }
}

# Create a dataframe with variable names and drop-out times
drop_out_df <- data.frame(
  Variable = names(drop_out_times),
  Reg_DropOutTime = log(drop_out_times)
)

# Sort the dataframe by drop-out times from low to high (least important first)
drop_out_df_sorted <- drop_out_df[order(drop_out_df$Reg_DropOutTime), ]

# Add DropOutOrder column, with the least important variable labeled as 1
drop_out_df_sorted$Reg_DropOutOrder <- seq_len(nrow(drop_out_df_sorted))

# Save the dataframe to a CSV file
library(data.table)
fwrite(drop_out_df_sorted, "reg_model_dropouts_11Sep.csv")


#table - od model

# Extract coefficients matrix and lambda values for the offset decay model
od_coeffs_matrix <- as.matrix(model.offsetdecay.out$elastic.cv$glmnet.fit$beta)
lambda_values_od <- model.offsetdecay.out$elastic.cv$lambda

# Initialize vector to store drop-out times
od_drop_out_times <- rep(NA, nrow(od_coeffs_matrix))
names(od_drop_out_times) <- rownames(od_coeffs_matrix)

# Identify drop-out times for each variable
for (i in 1:nrow(od_coeffs_matrix)) {
  # Find indices where coefficient is non-zero
  non_zero_idx <- which(od_coeffs_matrix[i, ] != 0)
  
  if (length(non_zero_idx) > 0) {
    # Record the lambda value where coefficient first becomes zero
    od_drop_out_times[i] <- lambda_values_od[min(non_zero_idx)]
  } else {
    od_drop_out_times[i] <- NA
  }
}

# Create a dataframe with variable names and drop-out times for the offset decay model
od_drop_out_df <- data.frame(
  Variable = names(od_drop_out_times),
  OD_DropOutTime = log(od_drop_out_times)
)

# Sort the dataframe by drop-out times from low to high (least important first)
od_drop_out_df_sorted <- od_drop_out_df[order(od_drop_out_df$OD_DropOutTime), ]

# Add DropOutOrder column, with the least important variable labeled as 1
od_drop_out_df_sorted$OD_DropOutOrder <- seq_len(nrow(od_drop_out_df_sorted))

# Save the dataframe to a CSV file
fwrite(od_drop_out_df_sorted, "od_model_dropouts_11Sep.csv")

# Merge dropout times for comparison
merged_drop_out_df <- left_join(od_drop_out_df_sorted, drop_out_df_sorted, by = "Variable", suffix = c("_OD", "_Reg"))

# Print the merged dataframe for comparison
print(merged_drop_out_df)

# save the merged dataframe
fwrite(merged_drop_out_df, "merged_model_dropouts_11Sep.csv")

# Get the lambda with the minimum cross-validation error
lambda_min_reg <- model.full.out$elastic.cv$lambda.min
cat("Lambda with minimum cross-validation error for regular model:", log(lambda_min), "\n")

lambda_min_od <- model.offsetdecay.out$elastic.cv$lambda.min
cat("Lambda with minimum cross-validation error for od model:", log(lambda_min_od), "\n")


### Model 3: simple

# Construct training and test data, pausing to look at missingness.
traindata.simple = alldata %>% filter(year<=2021) %>% 
  select(all_of(c(predictornames.simple, outcomenames, extravariables)))
nrow(traindata.simple)
table(traindata.simple$year)

#now remove missing to compare
traindata.simple = traindata.simple %>% filter(complete.cases(.))
nrow(traindata.simple)
table(traindata.simple$year)

testdata.simple = alldata %>% filter(year==2023) %>% 
  select(all_of(c(predictornames.simple, extravariables)))

nrow(testdata.simple)

testdata.simple = testdata.simple %>% filter(complete.cases(.))
nrow(testdata.simple)

model.simple.out = elogit.fit(ytrain=traindata.simple$mk_onset_1or2, 
                                   Xtrain = traindata.simple,
                                   Xtest = testdata.simple,
                                   alpha = .5,
                                   usepredictors = predictornames.simple)

model.simple.out$coeffs


simple.coefs.df = data.frame(
  variable = rownames(model.simple.out$coeffs),
  coef.simple = as.vector(model.simple.out$coeffs))

#Add odds ratio
simple.coefs.df = simple.coefs.df %>% mutate(or.simple = exp(coef.simple))

results.simple = data.frame(ewp_name = testdata.simple$ewp_name, 
                                 risk_1or2.simple = model.simple.out$risk) %>%
  mutate(risk_rank.simple = rank(desc(risk_1or2.simple), na.last = "keep")) %>% 
  arrange(desc(risk_1or2.simple))

View(results.simple)

### Merge the results for comparison ====

# merge coefficients
all_coefs <- full.coefs.df %>%
  full_join(offsetdecay.coefs.df, by="variable") 
 # full_join(simple.coefs.df, by="variable") 
# %>% full_join(fomchange.coefs.df, by = "variable") %>%
#  full_join(nofom.coefs.df, by="variable") %>%

all_coefs <- all_coefs %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 4)))

#make the sparsity more evident
all_coefs <- all_coefs %>%
  mutate(across(where(is.numeric), ~ifelse(. == 0.000, "0", .)))

print(all_coefs)
write.csv(file=paste0("coefficients_",savenameaffix, ".csv"), all_coefs)

# Compile forecasts and save
merged_results <- results.full %>%
  full_join(results.offsetdecay, by="ewp_name") 
 # full_join(results.simple, by="ewp_name") 
#%>% full_join(results.fomchange, by = "ewp_name") %>%
#  full_join(results.nofom, by = "ewp_name") %>%
 
merged_results <- merged_results %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 4)))

merged_results = merged_results %>% arrange(desc(risk_1or2.from2023))
View(merged_results)
write.csv(file=paste0("forecasts_2models_",savenameaffix,".csv"), 
          merged_results)


### Setup FOOS testing regime ====

# Create a more generic training-testing function:
traintestyear = function(varnames, lasttrainyear, dvname="mk_onset_1or2"){
  
  traindata = alldata %>% filter(year<=lasttrainyear) %>% 
    select(all_of(c(varnames, outcomenames, extravariables))) %>% 
    filter(complete.cases(.))
  
  testdata = alldata %>% filter(year==lasttrainyear+2) %>% 
    select(all_of(c(varnames, extravariables, "mk_onset_1or2"))) %>%
    filter(complete.cases(.))
  
  trueoutcomes = alldata %>% filter(year==lasttrainyear+2) %>%
    select(mk_onset_1or2)
 
  ytrain = traindata$mk_onset_1or2

  model.out = elogit.fit(ytrain=ytrain, 
                             Xtrain = traindata,
                             Xtest = testdata,
                             alpha = .5,
                             usepredictors = varnames)
  
  minidata = data.frame(risk=model.out$risk, 
                        ewp_name=testdata$ewp_name, 
                        yearfrom = lasttrainyear+2,
                        ytrue=testdata$mk_onset_1or2)
  
  minidata = minidata %>% mutate(riskrank = rank(-risk, ties.method = "min"))
  
  recalls = getrecalls(data=minidata)
  #R = data.frame(year=lasttrainyear+2, recalls=recalls, minidata=minidata)
  R = list()
  R$year= lasttrainyear+2
  R$recalls = recalls
  R$minidata = minidata
  return(R)
}

getrecalls = function(data){
  numevents = sum(data$ytrue)
  
  riskdiff.annual = mean(data$risk[data$ytrue==1])-mean(data$risk[data$ytrue==0])
  riskratio.annual = mean(data$risk[data$ytrue==1])/mean(data$risk[data$ytrue==0])
  
  caught10 = data %>% arrange(desc(risk)) %>% slice(1:10) %>%
    summarise(sum_ytrue = sum(ytrue, na.rm = TRUE)) %>% as.numeric()
  
  caught20 = data %>% arrange(desc(risk)) %>% slice(1:20) %>%
    summarise(sum_ytrue = sum(ytrue, na.rm = TRUE)) %>% as.numeric()
  
  caught30 = data %>% arrange(desc(risk)) %>% slice(1:30) %>%
    summarise(sum_ytrue = sum(ytrue, na.rm = TRUE)) %>% as.numeric()
  
  R = data.frame(numevents=numevents, caught10=caught10, 
                 caught20=caught20,caught30=caught30, 
                 riskdiff.annual=riskdiff.annual, riskratio.annual=riskratio.annual)
  return(R)
}

#traintestyear(varnames=predictornames.fullest, dvname = "sdads",lasttrainyear =1986)

# Define a vector of values to iterate over
# 1987 commonly used.
lasttrainyear.series <- seq(1991, 2020, by=1)

# Run with full new model (model 1)
#recallcheck_full <- data.frame(year = numeric(), 
#                        numevents = numeric(),
#                        caught10 = numeric(),
#                        caught20 = numeric(),
#                        caught30 = numeric())

recall_list = list()

FOOSdatalist = list()

varnamesnow = predictornames.fullest
#varnamesnow = predictornames.offsetdecay
#varnamesnow = predictornames.simple
#savenameaffix = "4Oct2023_neither"

for (j in 1:length(lasttrainyear.series)){
  lasttrainyear=lasttrainyear.series[j]
  print(lasttrainyear)
  
  thisout = traintestyear(lasttrainyear = lasttrainyear, 
                    varnames=varnamesnow, dvname = "sdads")
  #recallcheck_full = rbind(recallcheck_full,thisout)
  
  FOOSdatalist[[j]] = thisout$minidata
  
  recall_list[[j]] = thisout$recalls
}
              
FOOSdata_all = do.call(rbind, FOOSdatalist)
recalldata_all = do.call(rbind, recall_list)

FOOSdata_all = FOOSdata_all %>% rename(year = yearfrom)
#View(recallcheck_full) 

### Use two year mins/maxes as would-be forecasts.
yearmin = min(FOOSdata_all$year)+1
yearmax = max(FOOSdata_all$year)+2

allforecasts = alldata %>% filter(year>=yearmin & year<=yearmax) %>% 
 select(year, ewp_name, ccode, mk_onset)

# Add new columns for the previous and two years prior
allforecasts <- allforecasts %>% 
  mutate(prev_year = year - 1,
         two_years_prior = year - 2)

# Get "risk" and "rank" from the previous year
allforecasts <- allforecasts %>% 
  left_join(FOOSdata_all %>% 
              select(ewp_name, year, risk, riskrank), 
            by = c("ewp_name" = "ewp_name", "prev_year" = "year")) %>% 
  rename(risk_prev = risk, 
         rank_prev = riskrank)

# Get "risk" and "rank" from two years prior
allforecasts <- allforecasts %>% 
  left_join(FOOSdata_all %>% 
              select(ewp_name, year, risk, riskrank), 
            by = c("ewp_name" = "ewp_name", "two_years_prior" = "year")) %>% 
  rename(risk_two_years_prior = risk, 
         rank_two_years_prior = riskrank)

# Print the modified onsetdata with new columns

allforecasts = allforecasts %>% 
  mutate(maxrisk2 = pmax(risk_prev, risk_two_years_prior, na.rm=TRUE)) %>%
  mutate(minrank2 = pmin(rank_prev, rank_two_years_prior, na.rm=TRUE))

View(allforecasts)

# Just the onsets, for recall purposes:
onsetforecasts = allforecasts %>% filter(mk_onset==1)

write.csv(allforecasts, file=paste0("FOOSforecasts_",savenameaffix,".csv"))

# Recall 30 on this:
mean(onsetforecasts$minrank2<=30)

# Recall "averaging by year":
yearlyrecall30 = onsetforecasts %>% group_by(year) %>% 
  summarize(yearrecall30=mean(minrank2<=30))

View(yearlyrecall30)
table(yearlyrecall30$yearrecall30)
mean(yearlyrecall30$yearrecall30, na.rm=T)

# A plot of (max)risk in y=1 vs. y=0 cases
distplot = ggplot(allforecasts, aes(x = maxrisk2, fill = as.factor(mk_onset))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"), 
                    labels = c("No onset follows", "Onset follows")) +
  labs(fill = "Group",
       x = "Risk",
       y = "Density",
       title = "Density of Risk by Group - OD Model",
       subtitle = "Grouped by onset following") +
  theme_minimal()

print(distplot)
ggsave(paste0("distplotFOOS_maxrisk2_",savenameaffix,".pdf"), plot = distplot, width=7, height=5, dpi=300)


## Other metrics using the max/min2 approach ====
(risk.mean.y1 = mean(allforecasts$maxrisk2[allforecasts$mk_onset==1],na.rm=T))
(risk.mean.y0 = mean(allforecasts$maxrisk2[allforecasts$mk_onset==0],na.rm=T))
(risk.median.y1 = median(allforecasts$maxrisk2[allforecasts$mk_onset==1],na.rm=T))
(risk.median.y0 = median(allforecasts$maxrisk2[allforecasts$mk_onset==0],na.rm=T))


(risk.mean.diff = risk.mean.y1 - risk.mean.y0)
(risk.median.diff = risk.median.y1 - risk.median.y0)

(risk.mean.ratio = risk.mean.y1/risk.mean.y0)
(risk.median.ratio = risk.median.y1/risk.median.y0)

# Plot empirical CDF of risk for countries with onsets
fractioncaught_025 = (1-ecdf(onsetforecasts$maxrisk2)(0.025))

catchplot = ggplot(onsetforecasts, aes(x = maxrisk2)) +
  stat_ecdf(geom = "step", colour = "blue") +
  labs(title = "OD Model - Empirical CDF of Risk for Countries with onsets after",
       x = "risk",
       y = "fraction caught") +
  theme_minimal() +
  scale_x_reverse() +
  geom_vline(xintercept = 0.025, linetype="dotted", color = "red") +
  geom_hline(yintercept = fractioncaught_025, linetype="dotted", color = "red") +
  annotate("text", x = 0.025, y = fractioncaught_025, label = sprintf("%.2f", fractioncaught_025), 
           hjust = -0.5, vjust = 0.5, color = "red")

print(catchplot)
ggsave(filename = paste0("catchplot_",savenameaffix,".pdf"), catchplot)


# Fraction of true among those at or above 4%"
onsetforecasts %>% 
  summarise(fraction_at_or_over_0.04 = mean(maxrisk2 >= 0.04, na.rm = TRUE),
            fraction_at_or_over_0.025 = mean(maxrisk2 >= 0.025, na.rm = TRUE))

# Get false positive per true positive. Could get from that table or by:
meanytrueatthreshold = allforecasts %>% filter(maxrisk2>.04) %>% 
  summarize(meany = mean(mk_onset, na.rm=TRUE))

# false positive to true positive
1/meanytrueatthreshold - 1

# Create a sample data frame
# Create the ROC curve
roc_curve <- roc(allforecasts$mk_onset, allforecasts$maxrisk2)

# Print the AUC
cat("AUC:", auc(roc_curve), "\n")

# Plot the ROC curve
plot(roc_curve, main="ROC Curve", col="blue", lwd=2)




STOPHEREERROR





# Additional analysis for 2024 update

### Comparison b/w 2023 and 2024 training data

#now remove 2021 to compare to last year's observations
traindata.no2021 = traindata.full %>% 
  filter(year != 2021)

nrow(traindata.no2021)

#import 2023 training data for comparison

traindata.2023 <- read.csv("/Users/nmbryce/Documents/R/SRA/2024-Statistical-Risk-Assessment/3.Modeling/traindata2023.csv")
nrow(traindata.2023)

8571-8232

#see what's different between the two
differences <- traindata.no2021 %>% 
  anti_join(traindata.2023, by = c("ccode","year")) %>% 
  select(ewp_name, year)

View(differences)
names(traindata.no2021)
names(traindata.2023)

traindata.2023 <- traindata.2023 %>% 
  select(country,year,ccode,everything())

traindata.no2021 <- traindata.no2021 %>% 
  select(ewp_name,year,ccode,everything())

# Rows in traindata.no2021 but not in traindata.2023
n_in_no2021_not_2023 <- nrow(traindata.no2021 %>% anti_join(traindata.2023, by = c("ccode", "year")))

# Rows in traindata.2023 but not in traindata.no2021
n_in_2023_not_no2021 <- nrow(traindata.2023 %>% anti_join(traindata.no2021, by = c("ccode", "year")))

# Print the results
n_in_no2021_not_2023
n_in_2023_not_no2021

in_2023_not_no2021 <- traindata.2023 %>% anti_join(traindata.no2021, by = c("ccode", "year"))
in_no2021_not_2023 <- traindata.no2021 %>% anti_join(traindata.2023, by = c("ccode", "year"))

View(in_2023_not_no2021)
View(in_no2021_not_2023)

write.csv(in_2023_not_no2021,"in2023not2024.csv")

write.csv(in_no2021_not_2023,"in2024not2023.csv")

398-64


View(traindata.2023)


# Get correlation of risk scores b/w 2023 and 2024 update

forecasts2024 <- read.csv("/Users/nmbryce/Documents/R/SRA/2024-Statistical-Risk-Assessment/3.Modeling/final_forecasts_data_26Sept2024.csv")
forecasts2023 <- read.csv("/Users/nmbryce/Documents/R/SRA/2023-Statistical-Risk-Assessment-github/3.Modeling/final_forecasts_data_9Nov_hackregion.csv")

#remove risk.2023and2024.from2022 from 2024 forecasts

forecasts2024 <- forecasts2024 %>% 
  select(-risk.2023and2024.from2022)

mergedforecasts <-  forecasts2024 %>% 
  full_join(forecasts2023, by = c("ccode")) %>% 
  select(ccode,risk.2024and2025.from2023, risk.2023and2024.from2022)

View(mergedforecasts)


correlation <- cor(mergedforecasts$risk.2024and2025.from2023, mergedforecasts$risk.2023and2024.from2022, use = "complete.obs")


#See what ranking would have been using 2023 model on 2024 data

# Load the model object from the .rds file
model_path <- "/Users/nmbryce/Documents/R/SRA/2023-Statistical-Risk-Assessment-github/3.Modeling/model_full_out_27Sep_2024.rds"
model.2023 <- readRDS(model_path)

#Need to transform the 2024 data to match the 2023 training data

data2024 = read.csv("/Users/nmbryce/Documents/R/SRA/2024-Statistical-Risk-Assessment/2.Make-new-base-data/2023-alldata--2024-09-11.csv")

#names(data2024)

#need to change newcountry and countryage based on "years in dataset" instead of independence

data2024 <- data2024 %>% 
  mutate(countryage_new_ln = log2(year - datastartyear),
    newcountry = ifelse(countryage_new_ln <= 2, 1, 0))


#fix scale for GDP per cap growth

data2024 <- data2024 %>% 
  mutate(gdppcgrowth.combined = gdppcgrowth.combined/100) 

#fix scale for tradeshare

data2024 <- data2024 %>% 
  mutate(tradeshare.log2.combined = log2(2^tradeshare.log2.combined / 100))

#need to create "reg.afr"
data2024 <- data2024 %>%
  mutate(
    reg.afr = if_else(
      reg.na == 0 & reg.eap == 0 & reg.eur == 0 & reg.mna == 0 & reg.sca == 0,
      1, 0
    )
  )


#View(data2024)

predictornames.2023 <- c("mean_mk_onset","mk_onset_prev_year", "year", "year_sq",
                            "mk_ever", "mk_ongoing_count_log", "newcountry",
                            "widetargeting", "narrowtargeting",
                            "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca", 
                            "countryage_new_ln", "wdi.popsize.log2",
                            "gdppcgrowth.combined", "ios.iccpr1","includesnonstate",
                            "minorityrule", "battledeaths.ln2","judicialreform",
                            "religiousfreedom", "pol_killing_approved","freediscussion",
                            "social_inequality","even_civilrights","repress_civilsoc",
                            "social_power_dist", "ses_power_dist","tradeshare.log2.combined",
                            "coup.try.5yr", "efindex", "discrimpop",
                            "partyban.new.0","partyban.new.1", "partyban.new.2",
                            "v2csgender_binary","v2mecenefm_binary", "imr.fwd.fill.sqrt")

extravariables.2023 = c("ewp_name","ccode")

# Test data:
testdata.2024 = data2024 %>% filter(year==2023) %>% 
  select(all_of(c(predictornames.2023,extravariables.2023)))

# Separate the predictors and extravariables
predictors_2024 <- testdata.2024[, predictornames.2023]  # Only predictors
extravariables_2024 <- testdata.2024[, c("ewp_name", "ccode", "year")]  # Keep extravariables

nrow(predictors_2024)

testdata.2024 = testdata.2024 %>% filter(complete.cases(.))
nrow(testdata.2024)


# Step 3: Make predictions
predictions <- predict(model.2023$elastic.cv, 
                       newx = as.matrix(predictors_2024), 
                       s = "lambda.min", type = "response")

# Create a results data frame
results.comparison <- data.frame(ewp_name = extravariables_2024$ewp_name, 
                                risk_2023model = as.vector(predictions)) %>%
  mutate(rank_2023model = rank(desc(risk_2023model), na.last = "keep")) %>% 
  arrange(desc(risk_2023model)) %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 4)))

View(results.comparison)


#round the original results
results.full <- results.full %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 4)))

#merge with results from new model 

full.comparison <- results.full %>% 
  full_join(results.comparison, by = c("ewp_name"))

full.comparison <- full.comparison %>% 
  mutate(risk_difference = risk_1or2.from2023 - risk_2023model,
    rank_difference = risk_rank.from2023 - rank_2023model)

View(full.comparison)

data2024 %>% 
  filter(country == "South Sudan") %>% 
  select(year,mk_onset)

write.csv(file=paste0("forecasts_comparisons_",savenameaffix,".csv"), 
          full.comparison)




# Below this is analysis from the 2023 update

# Great. 
# Alright now we have 
# We want to use an older data file to see what "used to be missing"
# at some prior time. 
# I think let's use sept 16 (2023) "usedata".
# Might need to get variables names used at the time.
sept16data = read.csv("/Users/chadhazlett/Dropbox/chad_transfer/USHMM/EWP/2023SRA/usedata_16sept2023.csv")

predictornames.fullest.sept16 <- c("mk_responsible_ever", "mk_location_ever", 
                                   "widetargeting", "narrowtargeting",
                                   "reg.afr", "reg.eap", "reg.eur", "reg.mna", "reg.sca", 
                                   "countryage_new_ln", "popsize.ln2.combined",
                                   "gdppcgrowth.combined", "ios.iccpr1","includesnonstate",
                                   "minorityrule", "battledeaths.ln2","judicialreform",
                                   "religiousfreedom", "pol_killing_approved","freediscussion",
                                   "social_inequality","even_civilrights","repress_civilsoc",
                                   "social_power_dist", "ses_power_dist","tradeshare.ln.combined",
                                   "coup.try.5yr", "efindex", "discrimpop", "ldiscrimpop",
                                   "partyban.new.0","partyban.new.1", "partyban.new.2",
                                   "v2csgender_binary","v2mecenefm_binary", "imr.lag.2")

# Check:  
setdiff(predictornames.fullest.sept16, names(sept16data))

sept16_keydata = sept16data %>% filter(mk_onset_1or2==1) %>% select(all_of(c("year", "ewp_name", predictornames.fullest.sept16))) 

sept16_missingkeydata = sept16_keydata  %>% 
  gather(variable, value, -ewp_name, -year) %>%
  filter(is.na(value)) %>%
  select(ewp_name, year, variable) %>%
  distinct()

sept16_wide_missingkeydata <- sept16_missingkeydata %>%
  group_by(ewp_name, year) %>%
  summarise(missing_vars = paste(variable, collapse = ", "))

View(sept16_wide_missingkeydata)

#FOOSdata_all_witholdmissing <- FOOSdata_all %>% rename(year=yearfrom)

FOOSdata_all_witholdmissing <- FOOSdata_all %>%
  left_join(sept16_wide_missingkeydata %>% mutate(missingbefore = 1), 
            by = c("year", "ewp_name")) %>%
  mutate(missingbefore = if_else(is.na(missingbefore), 0, 1)) 


FOOSdata_all_witholdmissing = FOOSdata_all_witholdmissing %>% arrange(desc(missingbefore))


View(FOOSdata_all_witholdmissing)

write.csv(FOOSdata_all_witholdmissing, 
          file=paste0("FOOSrecord_missingmark_",savenameaffix,".csv"))

# Compute metrics using the entire FOOSdata_all to avoid annualization issues:

# Explore missingness ====

# start with years we ought to have onset predictions:
keydata = alldata %>% filter(mk_onset_1or2==1) %>% select(all_of(c("year", "ewp_name", predictornames.fullest))) 

library(tidyr)

missingkeydata = keydata %>% 
  gather(variable, value, -ewp_name, -year) %>%
  filter(is.na(value)) %>%
  select(ewp_name, year, variable) %>%
  distinct()

#View(missingkeydata)
#write.csv(missingkeydata, file = "missingkeydata.csv")

# Go back to a wide(ish) format, pasting the variables with missingness into a list
wide_missingkeydata <- missingkeydata %>%
  group_by(ewp_name, year) %>%
  summarise(missing_vars = paste(variable, collapse = ", "))

View(wide_missingkeydata)
write.csv(wide_missingkeydata, file=paste("wide_missingkeydata_",savenameaffix,".csv"))


# Explore country age and risk of onset

library(ggplot2)

ggplot(alldata, aes(x = countryage_new_ln, y = mk_onset_1or2)) +
  geom_point(alpha = 0.5) +  # Dots for individual data points with a slight transparency
  geom_smooth(se = TRUE) +
  labs(title = "Probability of mk_onset based on countryage", 
       x = "Country Age", 
       y = "Probability of mk_onset") +
  theme_minimal()

summary(lm(alldata$mk_onset_1or2~I(alldata$countryage_new<=2)))


### Sensitivity-speicifity ROC and AUC

# Load the package
library(pROC)



make_error_stop


### Basement ====
# Below this line there is some oldish code pertaining to checking 
# recall no the older models, but with an older strategy.


## Repeat with old model
recallcheck_old <- data.frame(year = numeric(), 
                             numevents = numeric(),
                             caught10 = numeric(),
                             caught20 = numeric(),
                             caught30 = numeric())


for (j in 1:length(lasttrainyear.series)){
  lasttrainyear=lasttrainyear.series[j]
  print(lasttrainyear)
  
  thisout = traintestyear(lasttrainyear = lasttrainyear, 
                          varnames=predictornames.old, dvname = "sdads")
  recallcheck_old = rbind(recallcheck_old, thisout)
}


#View(recallcheck_old) 

# Overall:
#recall_metrics_old$recall10 = sum(recallcheck_old$caught10)/sum(recallcheck_old$numevents)
#recall_metrics_old$recall20 = sum(recallcheck_old$caught20)/sum(recallcheck_old$numevents)
#recall_metrics_old$recall30 = sum(recallcheck_old$caught30)/sum(recallcheck_old$numevents)

#recall_metrics_old

## Repeat with model 5 -- "alt"  -- full but without iccpr
recallcheck_alt <- data.frame(year = numeric(), 
                                    numevents = numeric(),
                                    caught10 = numeric(),
                                    caught20 = numeric(),
                                    caught30 = numeric())

for (j in 1:length(lasttrainyear.series)){
  lasttrainyear=lasttrainyear.series[j]
  print(lasttrainyear)
  
  thisout = traintestyear(lasttrainyear = lasttrainyear, 
                          varnames=predictornames.alt, dvname = "sdads")
  recallcheck_alt = rbind(recallcheck_alt, thisout)
}


#View(recallcheck_alt) 


# Put all the model metrics together
recall_table <- tibble(
  Metric = c("recall10", "recall20", "recall30","numonsets"),
  full = NA_real_,
  fullalt = NA_real_,
  old = NA_real_)

recall_table <- recall_table %>%
  mutate(
    full = case_when(
      Metric == "recall10" ~ sum(recallcheck_full$caught10)/sum(recallcheck_full$numevents),
      Metric == "recall20" ~ sum(recallcheck_full$caught20)/sum(recallcheck_full$numevents),
      Metric == "recall30" ~ sum(recallcheck_full$caught30)/sum(recallcheck_full$numevents),
      Metric == "numonsets" ~ sum(recallcheck_full$numevents)),
    
    fullalt = case_when(
      Metric == "recall10" ~ sum(recallcheck_alt$caught10)/sum(recallcheck_alt$numevents),
      Metric == "recall20" ~ sum(recallcheck_alt$caught20)/sum(recallcheck_alt$numevents),
      Metric == "recall30" ~ sum(recallcheck_alt$caught30)/sum(recallcheck_alt$numevents),
      Metric == "numonsets" ~ sum(recallcheck_alt$numevents)),
    
    # fom_change = case_when(
    #   Metric == "recall10" ~ sum(recallcheck_fomchange$caught10)/sum(recallcheck_fomchange$numevents),
    #   Metric == "recall20" ~ sum(recallcheck_fomchange$caught20)/sum(recallcheck_fomchange$numevents),
    #   Metric == "recall30" ~ sum(recallcheck_fomchange$caught30)/sum(recallcheck_fomchange$numevents),
    #   Metric == "numonsets" ~ sum(recallcheck_fomchange$numevents)),
    # 
    # nofom = case_when(
    #   Metric == "recall10" ~ sum(recallcheck_nofom$caught10)/sum(recallcheck_nofom$numevents),
    #   Metric == "recall20" ~ sum(recallcheck_nofom$caught20)/sum(recallcheck_nofom$numevents),
    #   Metric == "recall30" ~ sum(recallcheck_nofom$caught30)/sum(recallcheck_nofom$numevents),
    #   Metric == "numonsets" ~ sum(recallcheck_nofom$numevents)),
    # 
    old = case_when(
      Metric == "recall10" ~ sum(recallcheck_old$caught10)/sum(recallcheck_old$numevents),
      Metric == "recall20" ~ sum(recallcheck_old$caught20)/sum(recallcheck_old$numevents),
      Metric == "recall30" ~ sum(recallcheck_old$caught30)/sum(recallcheck_old$numevents),
      Metric == "numonsets" ~ sum(recallcheck_old$numevents)),
    )

  recall_table <- recall_table %>% mutate_if(is.numeric, ~round(., 2))
  
  print(recall_table)
  
 # write.csv(file = paste0("FOOScomparison_",savenameaffix,".csv"), recall_table)

  # Ensure the necessary library is installed and then load it
  
  # Create the spaghetti plot
  my_plot <- ggplot(alldata, aes(x = year, y = ldiscrimpop, color = as.factor(ewp_name))) +
    geom_line(aes(group = ewp_name), size = 0.5) +
    labs(
      title = "Spaghetti plot of ldiscrimpop over years",
      x = "Year",
      y = "ldiscrimpop",
      color = "ewp_name"
    ) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_color_viridis_d()  # Optional: Change color scale
  
  # Display the plot
  print(my_plot)
  
  # Save the plot to a PDF
  ggsave("my_plot.pdf", plot = my_plot, width = 10, height = 7, units = "in")
  
  plot_discrimpop <- ggplot(alldata, aes(x = year, y = ldiscrimpop, color = as.factor(ewp_name))) +
    geom_line(aes(group = ewp_name), size = 0.5) +
    labs(
      title = "Spaghetti plot of ldiscrimpop over years",
      x = "Year",
      y = "ldiscrimpop",
      color = "Country"
    ) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_color_viridis_d()  # Optional: Change color scale
  
  # Display the plot
  print(plot_discrimpop)
  
  # Save the plot to a PDF
  ggsave("spaghetti_discrimpop.pdf", plot = plot_discrimpop, width = 10, height = 7, units = "in")
  
  
  ethiopia_data <- alldata %>%
    filter(ewp_name == "Ethiopia") 
  
  ggplot(ethiopia_data, aes(x = year, y = gdppcgrowth.combined)) +
    geom_line() +  # Use geom_line for line plots
    labs(
      title = "GDPpc growth over time in Ethiopia",
      x = "year",
      y = "growth"
    ) +
    theme_minimal()
  