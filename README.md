---
title: "CCPEBaseline"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
The first step is setting the working directory to the location of the data with setwd
The next step is reading the data, which I do using read.csv.  I use header = TRUE so that the first column in the data is treated as the variable name.

Then I subset the data with only the items that I want, which are the items and the following demographics: age, gender, ethnicity, sexual orientation.

Then I write the itemsOnly dataset as a csv, because I want to reupload.  When I reupload it using read.csv, I can specificy the values that I want to treat as NA. 

Then I go through and change the words (i.e. Strongly Agree, Agree) into numbers.

Finally, I write itemsOnly dataset as a csc and reupload it in order for R to read the values as integers (needed for later analysis below).
```{r}
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(lordif)

#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/RCS/Data")
#dat = read.csv("AAC_RCS_Intake_Clean.csv", header = TRUE)
head(dat)
itemsOnly = dat[,c(4, 7:10, 26:60)]
head(itemsOnly)
write.csv(itemsOnly, "itemsOnly.csv", row.names = FALSE)
itemsOnly = read.csv("itemsOnly.csv", header= TRUE, na.strings = c("Don't Know", "Not Applicable", "Refused", "#N/A", "-999", "-888", "-777"))


itemsOnly = data.frame(apply(itemsOnly, 2, function(x){ifelse(x == "Strongly Agree", 5,ifelse(x == "Agree", 4,ifelse(x == "Sometimes", 3, ifelse(x == "Disagree", 2, ifelse(x == "Strongly Disagree",1, x)))))}))


write.csv(itemsOnly, "itemsOnly.csv", row.names = FALSE)
itemsOnly = read.csv("itemsOnly.csv", header = TRUE)
head(itemsOnly)
```
We have established unidimensionality with CFA.  Now we are doing IRT.  Need to run the graded response model for polytomous items.  


```{r}
itemsOnlyIRT = itemsOnly[c("V33", "V32", "V27", "V31", "V7", "V29", "V4", "V15", "V21", "V35")]
itemsOnlyIRT =rename(itemsOnlyIRT, replace = c("V33" = "Item1", "V32" = "Item2", "V27" = "Item3", "V31"="Item4", "V7"="Item5", "V29"="Item6", "V4"="Item7", "V15"="Item8", "V21"="Item9", "V35"="Item10"))

descript(itemsOnlyIRT)
#fa.parallel(itemsOnlyIRT, fa = "fa")


fitOrd1 = grm(data = itemsOnlyIRT, constrained = TRUE, Hessian  = TRUE)
fitOrd2 = grm(data = itemsOnlyIRT, Hessian  = TRUE)
summary(fitOrd2)

```
Now we can do some additional model checking here
```{r}
margins(fitOrd2)
margins(fitOrd2, type = "three")
information(fitOrd2, c(-4, 4))

anova(fitOrd1, fitOrd2)
```
Try plots here just for fun
```{r}
par(mfrow = c(2, 2))

#plot(fitOrd2, category = 3, lwd = 2, cex = 1.2, legend = TRUE, cx = -4.5, cy = 0.85, xlab = "Latent Trait", cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.1, type = "OCCu")

#Plot all item information functions
plot(fitOrd2, type = "IIC")
#Plot just certain information item information is the same for polytomous items see ltm manual
plot(fitOrd2, items = c(5:7), type = "IIC")

# Plot ICC's for each response category just do one at a time easier
plot(fitOrd2, items = 3, type = "OCCu")

vals <- plot(fitOrd2, type = "IIC", items = 0, plot = FALSE, zrange = c(-3,3)) 

plot(vals[, "z"], 1 / sqrt(vals[, "test.info"]), type = "l", lwd = 2, xlab = "Ability", ylab = "Standard Error", main = "Standard Error of Measurement")
```
Dif for gender.  
```{r}
itemsOnly$G1..Gender. = ifelse(itemsOnly$G1..Gender. == 3, NA, itemsOnly$G1..Gender.)
Gender = itemsOnly$G1..Gender.
# Get rid of small category

Resp = itemsOnly[c("V33", "V32", "V27", "V31", "V7", "V29", "V4", "V15", "V21", "V35")]
Resp =rename(Resp, replace = c("V33" = "Item1", "V32" = "Item2", "V27" = "Item3", "V31"="Item4", "V7"="Item5", "V29"="Item6", "V4"="Item7", "V15"="Item8", "V21"="Item9", "V35"="Item10"))
genderDIF = lordif(Resp, Gender, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(genderDIF)
plot(genderDIF)

```
Now try Dif with race
```{r}
# Let's try with race
itemsOnly$G3..Race. = ifelse(itemsOnly$G3..Race. > 3, 4, itemsOnly$G3..Race.)
Race = itemsOnly$G3..Race.
# Get rid of small category
Resp = itemsOnly[c("V33", "V32", "V27", "V31", "V7", "V29", "V4", "V15", "V21", "V35")]
Resp =rename(Resp, replace = c("V33" = "Item1", "V32" = "Item2", "V27" = "Item3", "V31"="Item4", "V7"="Item5", "V29"="Item6", "V4"="Item7", "V15"="Item8", "V21"="Item9", "V35"="Item10"))

raceDIF = lordif(Resp, Race, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(raceDIF)
plot(raceDIF)
```
Now get dif with sexual orientation
```{r}
itemsOnly$G1a..Sexual.Orientation. = ifelse(itemsOnly$G1a..Sexual.Orientation.>1, 2,itemsOnly$G1a..Sexual.Orientation.)
sexOrien = itemsOnly$G1a..Sexual.Orientation.
Resp = itemsOnly[c("V33", "V32", "V27", "V31", "V7", "V29", "V4", "V15", "V21", "V35")]
Resp =rename(Resp, replace = c("V33" = "Item1", "V32" = "Item2", "V27" = "Item3", "V31"="Item4", "V7"="Item5", "V29"="Item6", "V4"="Item7", "V15"="Item8", "V21"="Item9", "V35"="Item10"))
sexOrienDIF = lordif(Resp, sexOrien, criterion = "Chisqr", alpha = .01, minCell = 5)
summary(sexOrienDIF)
plot(sexOrienDIF)

```





