## Exercise: logistic regression
## ───────────────────────────────────

##   Use the NH11 data set that we loaded earlier.
#Note: deleted script above the exercise, included script to load data below:
NH11 <- readRDS("dataSets/NatHealth2011.rds")

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).

#Review explanatory variables and check for NA's
summary(NH11[, c("everwrk", "age_p", "r_maritl")]) 
#Collapse "ever worked" to Yes, No and NA's.
NH11$everwrk <- factor(NH11$everwrk, levels = c("1 Yes", "2 No"))
#Collapse marital status to convert other to NA's
NH11$r_maritl <- factor(NH11$r_maritl, levels = c("1 Married - spouse in household", 
                                                  "7 Never married", 
                                                  "5 Divorced", 
                                                  "4 Widowed", 
                                                  "8 Living with partner", 
                                                  "6 Separated"))
#Create model to predict "ever worked"
everwrkmod1 <- glm(everwrk ~ age_p + r_maritl, data = NH11, family = "binomial")
everwork.tab <- coef(summary(everwrkmod1))
everwork.tab[,"Estimate"] <- exp(coef(everwrkmod1))
everwork.tab
##   2. Predict the probability of working for each level of marital
##      status.
predictwork <- with(NH11, expand.grid(age_p = mean(age_p),
                                     r_maritl = c("1 Married - spouse in household", 
                                                  "7 Never married", 
                                                  "5 Divorced", 
                                                  "4 Widowed", 
                                                  "8 Living with partner", 
                                                  "6 Separated")))
cbind(predictwork, predict(everwrkmod1, type = "response",
                           se.fit = TRUE, interval = "confidence",
                           newdata = predictwork))
#The probability of working for each level of marital status is:
#Married - spouse in household = 13%
#Never married = 18%
#Divorced = 7%
#Widowed = 23%
#Living with partner = 9%
#Separated = 12%

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.
