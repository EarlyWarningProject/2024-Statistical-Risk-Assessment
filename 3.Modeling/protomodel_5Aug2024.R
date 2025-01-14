# Code to run main models
# Now amended to do lots of investigation and comparison stuff.
# Aug 5, 2024


library(purrr)
library(dplyr)
library(glmnet)
library(ggplot2)
library(pROC)

savenameaffix = "5Aug2024"
# Function to use at estimation time ====
elogit.fit <- function(ytrain, Xtrain, Xtest, alpha = .5, usepredictors=NULL){
    
  #Don't deal with missingness -- assume it has already been dealt with
  #to force user to recognize it on the data side.
  xtrain = as.matrix(subset(Xtrain, select = usepredictors))
  xtest = as.matrix(subset(Xtest, select = usepredictors))
  set.seed(90035)
  elastic.cv <- cv.glmnet(y=ytrain, x=xtrain, alpha=alpha, family="binomial")
  coeffs <- coef(elastic.cv, s = "lambda.min")
  elastic.predictions = predict(elastic.cv,newx=xtest, s="lambda.min", type="response")
  
  risk <- as.numeric(elastic.predictions)
  #out <- list(risk, coeffs, elastic.cv)
  out = list()
  out$risk = risk
  out$coeffs = coeffs
  out$elastic.cv = elastic.cv
  return(out)
}

# Read in data ====
alldata = read.csv("2024-Statistical-Risk-Assessment/3.Modeling/prepared2023predictors-2024-08-05.csv")

# Define models ====

outcomenames = c("mk_onset","mk_onset_1","mk_onset_2", "mk_onset_1or2")

# Model "1"

predictornames.fullest <- c("year", "widetargeting", "narrowtargeting", "womencivsoc", 
                            "govcensorship", "partyban.new.0", "partyban.new.1", 
                            "partyban.new.2", "minorityrule", "judicialreform", 
                            "religiousfreedom", "pol_killing_approved", "freediscussion", 
                            "social_inequality", "even_civilrights", "repress_civilsoc", 
                            "social_power_dist", "ses_power_dist", "wdi.popsize.log2.combined", 
                            "wdi.gdppcgrow.new.combined", "wdi.trade.new.log2.combined", 
                            "imr.fwd.fill.sqrt", "efindex", "discrimpop", "reg.na", "reg.eap", 
                            "reg.sca", "reg.mna",  "ios.iccpr1",  "battledeaths.ln2", 
                            "coup.try.5yr", "mk_ongoing_count_log", "mean_mk_onset", 
                            "mk_onset_prev_year", "newcountry", "mk_ever", "year_sq", 
                            "countryage_new_ln", "includesnonstate")

predictornames.battledeaths <- c("year", "widetargeting", "narrowtargeting", "womencivsoc", 
                                 "govcensorship", "partyban.new.0", "partyban.new.1", 
                                 "partyban.new.2", "minorityrule", "judicialreform", 
                                 "religiousfreedom", "pol_killing_approved", "freediscussion", 
                                 "social_inequality", "even_civilrights", "repress_civilsoc", 
                                 "social_power_dist", "ses_power_dist", "wdi.popsize.log2.combined", 
                                 "wdi.gdppcgrow.new.combined", "wdi.trade.new.log2.combined", 
                                 "imr.fwd.fill.sqrt", "efindex", "discrimpop", "reg.na", "reg.eap", 
                                 "reg.sca", "reg.mna",  "ios.iccpr1",  "battledeaths.ln2", 
                                 "coup.try.5yr", "mk_ongoing_count_log", "mean_mk_onset", 
                                 "mk_onset_prev_year", "newcountry", "mk_ever", "year_sq", 
                                 "countryage_new_ln", "includesnonstate",
                                 "battledeaths.low.ln2","battledeaths.high.ln2")

predictornames.offsetdecay <- c("year", "widetargeting", "narrowtargeting", "womencivsoc", 
                                 "govcensorship", "partyban.new.0", "partyban.new.1", 
                                 "partyban.new.2", "minorityrule", "judicialreform", 
                                 "religiousfreedom", "pol_killing_approved", "freediscussion", 
                                 "social_inequality", "even_civilrights", "repress_civilsoc", 
                                 "social_power_dist", "ses_power_dist", "wdi.popsize.log2.combined", 
                                 "wdi.gdppcgrow.new.combined", "wdi.trade.new.log2.combined", 
                                 "imr.fwd.fill.sqrt", "efindex", "discrimpop", "reg.na", "reg.eap", 
                                 "reg.sca", "reg.mna",  "ios.iccpr1",  "battledeaths.ln2", 
                                 "coup.try.5yr", "mk_ongoing_count_log", "mean_mk_onset", 
                                 "mk_onset_prev_year", "newcountry", "mk_ever", "year_sq", 
                                 "countryage_new_ln", "includesnonstate",
                                 "offset_decay")


predictornames.offsetdecayminus2 <- c("year", "widetargeting", "narrowtargeting", "womencivsoc", 
                                "govcensorship", "partyban.new.0", "partyban.new.1", 
                                "partyban.new.2", "minorityrule", "judicialreform", 
                                "religiousfreedom", "pol_killing_approved", "freediscussion", 
                                "social_inequality", "even_civilrights", "repress_civilsoc", 
                                "social_power_dist", "ses_power_dist", "wdi.popsize.log2.combined", 
                                "wdi.gdppcgrow.new.combined", "wdi.trade.new.log2.combined", 
                                "imr.fwd.fill.sqrt", "efindex", "discrimpop", "reg.na", "reg.eap", 
                                "reg.sca", "reg.mna",  "ios.iccpr1",  "battledeaths.ln2", 
                                "coup.try.5yr", "mk_ongoing_count_log", "mean_mk_onset", 
                                "mk_onset_prev_year", "newcountry", "mk_ever", "year_sq", 
                                "countryage_new_ln", "includesnonstate",
                                "offset_decay_minus2")                          

# Check:  
setdiff(predictornames.fullest, names(alldata))



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
extravariables = c("country","ccode","year","year_l1","year_sq_l1")

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

# Save the cv.glmnet object to a file
saveRDS(model.full.out$elastic.cv, file = "main_full_cv_glmnet_object_5Aug2024.rds")


model.full.out$coeffs

full.coefs.df = data.frame(
  variable = rownames(model.full.out$coeffs),
  coef.full = as.vector(model.full.out$coeffs))

#Add odds ratio
full.coefs.df = full.coefs.df %>% mutate(or.full = exp(coef.full))

results.full = data.frame(country = testdata.full$country, 
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

results.full.2022 = data.frame(country = testdata.full.2022$country, 
                          risk_1or2.from2022 = model.full.2022.out$risk) %>%
  mutate(risk_rank.from2022 = rank(desc(risk_1or2.from2022), na.last = "keep")) %>% 
  arrange(desc(risk_1or2.from2022))

View(results.full.2022)

### Put together outputs in desired format ====

# first get one-year ahead forecast
model.1yr.out = elogit.fit(ytrain=traindata.full$mk_onset_1, 
                            Xtrain = traindata.full,
                            Xtest = testdata.full,
                            alpha = .5,
                            usepredictors = predictornames.fullest)

results.1yr = data.frame(country = testdata.full$country, 
                         risk.2024only.from2023 = model.1yr.out$risk) %>%
  mutate(riskrank.2024only.from2023 = rank(desc(risk.2024only.from2023), na.last = "keep")) 

# Now merge everything up
bothforecasts <- results.full %>%
  left_join(results.1yr, by = "country")

# Merge in the prior year forecasts

bothforecastsandprior = bothforecasts %>% left_join(results.full.2022, by="country")

# Now merge that with dataset3
finalresults <- bothforecastsandprior %>%
  left_join(testdata.full, by = "country")

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

results.offsetdecay = data.frame(country = testdata.offsetdecay$country, 
                           risk_1or2.offsetdecay = model.offsetdecay.out$risk) %>%
  mutate(risk_rank.offsetdecay = rank(desc(risk_1or2.offsetdecay), na.last = "keep")) %>% 
  arrange(desc(risk_1or2.offsetdecay))

View(results.offsetdecay)


### Model 3: battledeaths high and low  ====

# Construct training and test data, pausing to look at missingness.
traindata.bd = alldata %>% filter(year<=2021) %>% 
  select(all_of(c(predictornames.battledeaths, outcomenames, extravariables)))
nrow(traindata.bd)
table(traindata.bd$year)

#now remove missing to compare
traindata.bd = traindata.bd %>% filter(complete.cases(.))
nrow(traindata.bd)
table(traindata.bd$year)

testdata.bd = alldata %>% filter(year==2023) %>% 
  select(all_of(c(predictornames.battledeaths, extravariables)))

nrow(testdata.bd)

testdata.bd = testdata.bd %>% filter(complete.cases(.))
nrow(testdata.bd)

model.bd.out = elogit.fit(ytrain=traindata.bd$mk_onset_1or2, 
                            Xtrain = traindata.bd,
                            Xtest = testdata.bd,
                            alpha = .5,
                            usepredictors = predictornames.battledeaths)

bd.coefs.df = data.frame(
  variable = rownames(model.bd.out$coeffs),
  coef.bd = as.vector(model.bd.out$coeffs))

#Add odds ratio
bd.coefs.df = bd.coefs.df %>% mutate(or.bd = exp(coef.bd))

results.bd = data.frame(country = testdata.bd$country, 
                          risk_1or2.bd = model.bd.out$risk) %>%
  mutate(risk_rank.bd = rank(desc(risk_1or2.bd), na.last = "keep")) %>% 
  arrange(desc(risk_1or2.bd))

View(results.bd)


### Merge the results for comparison ====

# merge coefficients
all_coefs <- full.coefs.df %>%
  full_join(offsetdecay.coefs.df, by="variable") %>%
  full_join(bd.coefs.df, by="variable") 
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
  full_join(results.offsetdecay, by="country") %>%
  full_join(results.bd, by="country") 
#%>% full_join(results.fomchange, by = "country") %>%
#  full_join(results.nofom, by = "country") %>%
 
merged_results <- merged_results %>%
  mutate(across(where(is.numeric), ~round(.x, digits = 4)))

merged_results = merged_results %>% arrange(desc(risk_1or2.from2023))
View(merged_results)
write.csv(file=paste0("forecasts_3models_",savenameaffix,".csv"), 
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
                        country=testdata$country, 
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
lasttrainyear.series <- seq(1991, 2019, by=1)

# Run with full new model (model 1)
#recallcheck_full <- data.frame(year = numeric(), 
#                        numevents = numeric(),
#                        caught10 = numeric(),
#                        caught20 = numeric(),
#                        caught30 = numeric())

recall_list = list()

FOOSdatalist = list()

varnamesnow = predictornames.fullest
#varnamesnow = predictornames.nomean
#varnamesnow = predictornames.noprev
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
 select(year, country, ccode, mk_onset)

# Add new columns for the previous and two years prior
allforecasts <- allforecasts %>% 
  mutate(prev_year = year - 1,
         two_years_prior = year - 2)

# Get "risk" and "rank" from the previous year
allforecasts <- allforecasts %>% 
  left_join(FOOSdata_all %>% 
              select(country, year, risk, riskrank), 
            by = c("country" = "country", "prev_year" = "year")) %>% 
  rename(risk_prev = risk, 
         rank_prev = riskrank)

# Get "risk" and "rank" from two years prior
allforecasts <- allforecasts %>% 
  left_join(FOOSdata_all %>% 
              select(country, year, risk, riskrank), 
            by = c("country" = "country", "two_years_prior" = "year")) %>% 
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
       title = "Density of Risk by Group",
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
  labs(title = "Empirical CDF of Risk for Countries with onsets after",
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

sept16_keydata = sept16data %>% filter(mk_onset_1or2==1) %>% select(all_of(c("year", "country", predictornames.fullest.sept16))) 

sept16_missingkeydata = sept16_keydata  %>% 
  gather(variable, value, -country, -year) %>%
  filter(is.na(value)) %>%
  select(country, year, variable) %>%
  distinct()

sept16_wide_missingkeydata <- sept16_missingkeydata %>%
  group_by(country, year) %>%
  summarise(missing_vars = paste(variable, collapse = ", "))

View(sept16_wide_missingkeydata)

#FOOSdata_all_witholdmissing <- FOOSdata_all %>% rename(year=yearfrom)

FOOSdata_all_witholdmissing <- FOOSdata_all %>%
  left_join(sept16_wide_missingkeydata %>% mutate(missingbefore = 1), 
            by = c("year", "country")) %>%
  mutate(missingbefore = if_else(is.na(missingbefore), 0, 1)) 


FOOSdata_all_witholdmissing = FOOSdata_all_witholdmissing %>% arrange(desc(missingbefore))


View(FOOSdata_all_witholdmissing)

write.csv(FOOSdata_all_witholdmissing, 
          file=paste0("FOOSrecord_missingmark_",savenameaffix,".csv"))

# Compute metrics using the entire FOOSdata_all to avoid annualization issues:

# Explore missingness ====

# start with years we ought to have onset predictions:
keydata = alldata %>% filter(mk_onset_1or2==1) %>% select(all_of(c("year", "country", predictornames.fullest))) 

library(tidyr)

missingkeydata = keydata %>% 
  gather(variable, value, -country, -year) %>%
  filter(is.na(value)) %>%
  select(country, year, variable) %>%
  distinct()

#View(missingkeydata)
#write.csv(missingkeydata, file = "missingkeydata.csv")

# Go back to a wide(ish) format, pasting the variables with missingness into a list
wide_missingkeydata <- missingkeydata %>%
  group_by(country, year) %>%
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
  my_plot <- ggplot(alldata, aes(x = year, y = ldiscrimpop, color = as.factor(country))) +
    geom_line(aes(group = country), size = 0.5) +
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
  print(my_plot)
  
  # Save the plot to a PDF
  ggsave("my_plot.pdf", plot = my_plot, width = 10, height = 7, units = "in")
  
  plot_discrimpop <- ggplot(alldata, aes(x = year, y = ldiscrimpop, color = as.factor(country))) +
    geom_line(aes(group = country), size = 0.5) +
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
    filter(country == "Ethiopia") 
  
  ggplot(ethiopia_data, aes(x = year, y = gdppcgrowth.combined)) +
    geom_line() +  # Use geom_line for line plots
    labs(
      title = "GDPpc growth over time in Ethiopia",
      x = "year",
      y = "growth"
    ) +
    theme_minimal()
  