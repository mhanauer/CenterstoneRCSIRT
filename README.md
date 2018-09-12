---
---
title: "CCPEBaseline"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library packages
```{r}
library(lavaan)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)
library(Amelia)
library(plyr)
library(paran)
```


Load data.  Just get the actual data for now don't worry about sub group analyses.  
Add a state ID variable so we can differential them later on
```{r}
#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/HealthCapitalScale/7-13-18HCSData")
#CIL = read.csv("CIL HCS Dataset_07172018.csv", header = TRUE)
#CKY = read.csv("CKY HCS Dataset_07172018.csv", header = TRUE)
head(CIL)
CIL = cbind(CIL[c("SourceClient_ID", "good_health", "manage_health", "knows_conditions", "phys_activity", "manage_mhealth", "has_goals", "not_overwhelmed", "has_pcp", "similar_goals", "health_literacy", "no_future_hosp", "no_ED_use", "knows_meds", "takes_meds", "no_concern_side_effects", "can_cook", "access_nut_food", "has_money_food", "eats_nut_food", "health_friendly_home", "accessible_home", "living_sit_satisfaction", "has_home", "safe_neighborhood", "near_supports", "has_transport", "others_support_health", "social_activity", "no_one_opposes", "has_money_for_family", "manage_money", "has_money_for_health", "ed_level_satisfaction", "job_satisfaction", "able_to_not_smoke", "able_to_not_use")])
CIL$StateID = rep(0, dim(CIL)[1])

CKY = cbind(CKY[c("SourceClient_ID","good_health", "manage_health", "knows_conditions", "phys_activity", "manage_mhealth", "has_goals", "not_overwhelmed", "has_pcp", "similar_goals", "health_literacy", "no_future_hosp", "no_ED_use", "knows_meds", "takes_meds", "no_concern_side_effects", "can_cook", "access_nut_food", "has_money_food", "eats_nut_food", "health_friendly_home", "accessible_home", "living_sit_satisfaction", "has_home", "safe_neighborhood", "near_supports", "has_transport", "others_support_health", "social_activity", "no_one_opposes", "has_money_for_family", "manage_money", "has_money_for_health", "ed_level_satisfaction", "job_satisfaction", "able_to_not_smoke", "able_to_not_use")])
CKY$StateID = rep(1, dim(CKY)[1])


CIL_CKY = data.frame(rbind(CIL, CKY))
write.csv(CIL_CKY, "CIL_CKY.csv", row.names = FALSE)
CIL_CKY = read.csv("CIL_CKY.csv", header = TRUE)
head(CIL_CKY)
dim(CIL_CKY)
CIL_CKY$can_cook = as.integer(CIL_CKY$can_cook)

CIL_CKY_Complete = na.omit(CIL_CKY)
dim(CIL_CKY_Complete)

```
Now let us load in the demographics
Get rid of immigration status only Ill has it so we can rbind them
Then we can merge on SourceID for the full file then subset below for full analysis
Now merge them on 
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/HealthCapitalScale/7-13-18HCSData")
CILDemo = read.csv("Brief HCS - IL - Demographics - 20180813.csv", header = TRUE)
CKYDemo = read.csv("Brief HCS - KY - Demographics - 20180813.csv", header = TRUE)
CILDemo$Immigration.Status = NULL
# need to rename without the period number 3
head(CKYDemo)
CIL_CKY_Demo = rbind(CILDemo, CKYDemo)
names(CIL_CKY_Demo)[3] = "SourceClient_ID"
CIL_CKY = merge(CIL_CKY, CIL_CKY_Demo, by = "SourceClient_ID", all = TRUE)
head(CIL_CKY)
```
Now let us see how much data is misisng 
```{r}
CIL_CKYComplete = na.omit(CIL_CKY)
dim(CIL_CKYComplete)
dim(CIL_CKY)
CIL_CKYDemo = CIL_CKY
```
Let us get some descriptives and get rid of those who are under 18
```{r}
# Get rid of client ID don't need it any more
CIL_CKYDemo$SourceClient_ID = NULL
CIL_CKYDemo$Last.Service.Date = NULL
CIL_CKYDemo$Source.System = NULL
CIL_CKYDemo$Data.Warehouse.Client.ID = NULL
CIL_CKYDemo$Ethnicity = NULL
CIL_CKYDemo = subset(CIL_CKYDemo, Age > 17)
demoCIL_CKY = apply(CIL_CKYDemo, 2, function(x){describe.factor(x)})
```


There is a rouge 0 and 7.  Probably should get rid of those.
```{r}
CIL_CKY= subset(CIL_CKY, Age > 17)
CIL_CKY = cbind(CIL_CKY[c("manage_health" , "manage_mhealth" ,"similar_goals" ,"no_concern_side_effects" ,"has_money_food" ,"health_friendly_home" , "has_transport" , "social_activity" , "has_money_for_health" , "ed_level_satisfaction")])

descriptivesCIL_CKY = apply(CIL_CKY, 2, function(x){describe.factor(x)})
descriptivesCIL_CKY
CIL_CKY = apply(CIL_CKY, 2, function(x){ifelse(x > 5, NA, ifelse(x < 1, NA, x))})
CIL_CKY = data.frame(CIL_CKY)
describe.factor(CIL_CKY$similar_goals)
describe.factor(CIL_CKY$has_money_for_health)
describe(CIL_CKY)

```
Get the percentage of missing data
```{r}
dim(CIL_CKY)
CIL_CKYComplete = na.omit(CIL_CKY)
dim(CIL_CKY_Complete)
1-(dim(CIL_CKYComplete)[1] / dim(CIL_CKY)[1])

CIL_CKYMissingDiag =  amelia(CIL_CKY)
summary(CIL_CKYMissingDiag)
```
CFA Full
```{r}
model1 = 'HCA =~ good_health+ manage_health+ knows_conditions+ phys_activity+ manage_mhealth+ has_goals+ not_overwhelmed+ has_pcp+ similar_goals+ health_literacy+ no_future_hosp+ no_ED_use+ knows_meds+ takes_meds+ no_concern_side_effects+ can_cook+ access_nut_food+ has_money_food+ eats_nut_food+ health_friendly_home+ accessible_home+ living_sit_satisfaction+ has_home+ safe_neighborhood+ near_supports+ has_transport+ others_support_health+ social_activity+ no_one_opposes+ has_money_for_family+ manage_money+ has_money_for_health+ ed_level_satisfaction+ job_satisfaction'
fit1 = cfa(model1, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKYDemo)
summary(fit1, fit.measures = TRUE)
```


Final CFA model
```{r}
model1  ='HCA =~ manage_health + manage_mhealth +similar_goals +no_concern_side_effects +has_money_food +health_friendly_home + has_transport + social_activity + has_money_for_health + ed_level_satisfaction'

fit1 = cfa(model1, estimator = "MLR",  missing = "fiml", std.lv = TRUE, data = CIL_CKYDemo)
summary(fit1, fit.measures = TRUE)


fit1Complete = cfa(model1, estimator = "MLR", std.lv = TRUE, data = CIL_CKYDemo)
summary(fit1Complete, fit.measures = TRUE)

```

Now let us try measurement invariance with gender then make race 
```{r}
modelGender = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = CIL_CKYDemo, group = "Gender", strict = TRUE)

describe.factor(CIL_CKYDemo$Race)
CIL_CKYDemoRace = subset(CIL_CKYDemo, Race != "UNKNOWN")
describe.factor(CIL_CKYDemoRace$Race)
CIL_CKYDemoRace$Race = ifelse(CIL_CKYDemoRace$Race == "WHITE OR CAUCASIAN", 1, 0)
describe.factor(CIL_CKYDemoRace$Race)

modelRace = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = CIL_CKYDemoRace, group = "Race", strict = TRUE)
```

Reliabiltiy
```{r}
omegaItems = omega(CIL_CKY); summary(omegaItems)
```
Parrell analyses
Now try both versions of parrallel analysis Only works with complete data

Use Common Factor Analysis instead of PCA, and using a more conservative approach to factor retention (parrallel can extract too many factors) See Glorfeld(1995): https://drive.google.com/file/d/1HehR1z1qY4GZkMy8YKqPqHA1coKGOdMB/view?usp=sharing
```{r}
CIL_CKYComplete = na.omit(CIL_CKY)
paran(CIL_CKYComplete, centile = 95, iterations = 1000, graph = TRUE, cfa = TRUE)
```
MAP and VSS tests and EFA tests
```{r}
vss(CIL_CKY, n = 4)
BAHCS_10EFA = fa(r = CIL_CKY, nfactors = 3)
BAHCS_10EFA
fa.diagram(BAHCS_10EFA)
```
Compare partial credit versus graded response model
PCM isn't converging likely causing the GRM to run better 
Generalized partial credit model does not make the assumption that items are ordered in the way that they should be (as we move up the threshold go from lower to higher)
```{r}
fitOrdGRM = grm(data = CIL_CKY, constrained = FALSE)
fitOrdPCM = gpcm(data = CIL_CKY, constraint = "gpcm", start.val = "random")
summary(fitOrdPCM)
AIC(fitOrdPCM)
BIC(fitOrdPCM)
AIC(fitOrdGRM)
BIC(fitOrdGRM)
```
Graded response model with and without missing data
Constrained means the discrimination parametr is equal
```{r}
# With missing data
# Graded response model
fitOrdGSM = grm(data = CIL_CKY, constrained = FALSE)
summary(fitOrdGRM)

information(fitOrdGRM, c(-3, 1))

plot(fitOrdGRM, category = 1, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM, category = 2, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM, category = 3, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)

plot(fitOrdGRM, category = 4, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1)


vals <- plot(fitOrdGRM, type = "IIC", items = 0, plot = FALSE, zrange = c(-3,3)) 

plot(vals[, "z"], 1 / sqrt(vals[, "test.info"]), type = "l", lwd = 2, xlab = "Ability", ylab = "Standard Error", main = "Standard Error of Measurement")

```
DIFF for items

Race reference: white
Gender refernce: male
State reference: Illinois

```{r}
describe(CIL_CKYDemo)

CIL_CKYDemo$OtherRace  = as.factor(ifelse(CIL_CKYDemo$Race == "WHITE OR CAUCASIAN", 0, 1))

CIL_CKYDemo$Female  = as.factor(ifelse(CIL_CKYDemo$Race == "MALE", 0, 1))


write.csv(CIL_CKYDemo, "CIL_CKYDemo.csv", row.names = FALSE)

CIL_CKYDemo = read.csv("CIL_CKYDemo.csv", header = TRUE)

CIL_CKYDemo$Gender

raceDIF = lordif(resp.data = CIL_CKY, group = CIL_CKYDemo$OtherRace, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(raceDIF)
plot(raceDIF)

genderDIF = lordif(resp.data = CIL_CKY, group = CIL_CKYDemo$Female, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(genderDIF)
plot(genderDIF)

stateDIF = lordif(resp.data = CIL_CKY, group = CIL_CKYDemo$StateID, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(stateDIF)
plot(stateDIF)

```

