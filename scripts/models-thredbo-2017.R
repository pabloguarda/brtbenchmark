library(leaps)

# MODELS THREDBO 2017 -------------------------------------------------------------



# FINAL MODELS ------------------------------------------------------------


# - MLR ---------------------------------------------------------------------

mlr_speed_allCategories = lm(-g1 ~  a+b+c+d+e+f
                               ,data = subset(scores2014))

summary(mlr_speed_allCategories)

mlr_speed_full_subcategories = lm(-g1 ~ highAffluence+largeStopSpacing+dedicatedRightOfWay+a2+a3+intersectionTreatments+(a5>0)+(b2>0)+allDoorBoarding+g5
                       ,data = subset(speedITDP))

summary(mlr_speed_full_subcategories)

mlr_speed_parsimonious_subcategories <- lm(formula = -speed ~highAffluence+largeStopSpacing+dedicatedRightOfWay+intersectionTreatments+allDoorBoarding
                                , data = subset(speedITDP))

summary(mlr_speed_subcategories)



# - OL (5 Levels) -----------------------------------------------------------

ol5_speed_allCategories = polr(formula = speed5 ~a+b+c+d+e+f
                               , data = subset(speedITDP), Hess = TRUE)


summary(ol5_speed_allCategories)


ol5_speed_full_subcategories <- polr(formula = speed5 ~highAffluence+largeStopSpacing+dedicatedRightOfWay+intersectionTreatments+(b2>0)+allDoorBoarding+g5
                                             , data = subset(speedITDP), Hess = TRUE)

summary(ol5_speed_full_subcategories)

ol5_speed_parsimonious_subcategories <- polr(formula = speed5 ~highAffluence+largeStopSpacing+dedicatedRightOfWay+intersectionTreatments+allDoorBoarding
                                , data = subset(speedITDP), Hess = TRUE)

summary(ol5_speed_parsimonious_subcategories)


# - OL (3 Levels) -----------------------------------------------------------

ol3_speed_allCategories = polr(formula = speed3 ~a+b+c+d+e+f
                               , data = subset(speedITDP), Hess = TRUE)


ol3_speed_full_subcategories <- polr(formula = speed3 ~highAffluence+largeStopSpacing+dedicatedRightOfWay+intersectionTreatments+(b2>0)+allDoorBoarding+(g5)
                                             , data = subset(speedITDP), Hess = TRUE)


summary(ol3_speed_full_subcategories)

ol3_speed_parsimonious_subcategories <- polr(formula = speed3 ~highAffluence+largeStopSpacing+dedicatedRightOfWay+intersectionTreatments+allDoorBoarding
     , data = subset(speedITDP), Hess = TRUE)

summary(ol3_speed_parsimonious_subcategories)

# - BL (2 Levels) -----------------------------------------------------------

bl_speed_allCategories <- glm(speed2 ~a+b+c+d+e+f,family=binomial(link='logit'),data=speedITDP)
summary(bl_speed_allCategories)

bl_speed_full_subcategories <- glm(speed2 ~highAffluence+largeStopSpacing+dedicatedRightOfWay+a2+intersectionTreatments+(a5>0)+(b2>0)+allDoorBoarding+(g5),family=binomial(link='logit'),data=speedITDP)
summary(bl_speed_full_subcategories)

bl_speed_parsimonious_subcategories <- glm(speed2 ~highAffluence+largeStopSpacing+dedicatedRightOfWay+intersectionTreatments+allDoorBoarding,family=binomial(link='logit'),data=speedITDP)
summary(bl_speed_parsimonious_subcategories)

model.df <- tidy(model)
model.df <- model.df[1:(dim(model.df)[1]-1),] #No fixed effects

model.df <- model.df %>% 
  mutate(or = exp(estimate),  # Odds ratio/gradient
         var.diag = diag(vcov(model)),  # Variance of each coefficient
         or.se = sqrt(or^2 * var.diag))  # Odds-ratio adjusted 

# Odd-ratios
#From this discussion: https://stackoverflow.com/questions/26417005/odds-ratio-and-confidence-intervals-from-glmer-output
# temp <- as.data.frame(confint(model,parm="beta_",method="Wald"))

# cc <- confint(model,parm="beta_")  ## slow (~ 11 seconds)
# ctab <- cbind(est=fixef(gm1),cc)
se <- sqrt(diag(vcov(model)))
tab <- cbind(Est = fixef(model), LL = fixef(model) - 1.96 * se, UL = fixef(model) + 1.96 * se)
tab <- exp(tab)
tab <- as.data.frame(tab)
tab$term <- row.names(tab)
row.names(tab) <- NULL
colnames(tab) <- c("or","CILower","CIUpper","term")

#Create a unique string for confidence interval
tab$CI <- str_c("[",roundToDigits(data=tab$CILower,digits),", ",roundToDigits(data=tab$CIUpper,digits),"]")

#Merge with existing output
model.df <- merge(model.df,tab[c("CILower","CIUpper","CI","term")],by=c("term"))

#Replace parenthesis by "." from predictors names (needs to be before the replacement of the levels of the predictors)
model.df <- as.data.frame(sapply(model.df,function(x) gsub("[()]", ".", x)))

#Replace : by x
model.df <- as.data.frame(sapply(model.df,function(x) gsub(":", " x ", x)))

# a)Mixing Both Databases (with data manually collected) --------------------------------------------------------------


# i) Speed ----------------------------------------------------------------


# MLR ---------------------------------------------------------------------
# - ITDP --------------------------------------------------------------------

model2014aCommercialSpeed = lm(-g1 ~  a1+a2+a3+a4+a5+b2
                               ,data = subset(scores2014))

summary(model2014aCommercialSpeed)

# layout(matrix(c(1,2,3,4),2,2))
influence(model2014aCommercialSpeed)
plot(model2014aCommercialSpeed)

step = stepAIC(lm(-g1 ~a1+a2+a3+a4+a5+b1+b2+b3+b4+b5+b6+b7+c1+c2+c3+c4+c5+d1+d2+d3+d4+d5+e1+e2+f1+f2+f3+f4+f5+f6,data = subset(scores2014)),direction="both")

step$anova

summary(model2014bCommercialSpeed)


#Info 2013

modelITDP2013= lm(abs(g5) ~  factor(a3)
                  # +a+b+d+e+f
                  # a+b+c+d+e+f+g+
                  # +corridor_length
                  ,data = subset(scores2013))

summary(modelITDP2013)

# - BRTData -----------------------------------------------------------------

model2014aSpeedBRTData = lm(speed_BRTData_2017 ~  a1+a2+a3+a4+a5 + pop_density_2017
                            # +a+b+d+e+f
                            # a+b+c+d+e+f+g+
                            # +corridor_length
                            ,data = subset(scores2014))


summary(model2014aSpeedBRTData)

model2014aSpeedBRTData = lm(speed_BRTData_2017 ~  a
                            # +a+b+d+e+f
                            # a+b+c+d+e+f+g+
                            # +corridor_length
                            ,data = subset(scores2014))


summary(model2014aSpeedBRTData)

dim(model.frame(model2014aSpeedBRTData))




# OL (Ordinal Logit)-----------------------------------------------------------


#In 2013, only two corridors have scores different than zero in the speed indicator. 
scores2013$g1

#In 2014, there is more variability
scores2014$g1



# 2014 --------------------------------------------------------------------

# 5 intervals -------------------------------------------------------------

data = scores2014
data$speedInterval = NA

#We will first transform the scores into 5 categories. As the category increases, the corridor's speed increases. 
data$speedInterval[data$g1==10] = 1
data$speedInterval[data$g1==6] = 2
data$speedInterval[data$g1==3] = 3
data$speedInterval[data$g1==1] = 4
data$speedInterval[data$g1==0] = 5
data$speedInterval = as.factor(data$speedInterval)

# data$speedInterval= factor(data$speedInterval, levels = c("low","mid-low","mid","mid-high","high"), labels = c("-10","-6","-3","-1","0"))

# OLSpeedITDP2014_Full= polr(formula = speedInterval ~ a1+a2+a3+a4+a5+b2+ (g5>0)
#                                     , data = subset(data), Hess = TRUE)
# 
# summary(OLSpeedITDP2014_Full)


#Category Level

OLSpeedITDP2014_AllCategories= polr(formula = speedInterval ~ a+b+c+d+e+f
                           , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_AllCategories)

OLSpeedITDP2014_SignificantCategories= polr(formula = speedInterval ~ a
                                    , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_SignificantCategories)

#Subcategory Level
OLSpeedITDP2014_AllSubcategories= polr(formula = speedInterval ~ a1+a2+a3+a4+a5
                                    , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_AllSubcategories)

OLSpeedITDP2014_SignificantSubcategories= polr(formula = speedInterval ~ a2+a4+a5+d1+d3+g5
                                       , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_SignificantSubcategories)

#Adding items from point deduction section

OLSpeedITDP2014_1 = polr(formula = speedInterval ~ a3+a4+a5+(g5>0)
                         , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_1)

#Adding data from 2013. In this case it is necessary to transform the scale of the 


#Model for further analyses
model <- OLSpeedITDP2014_SignificantSubcategories

#List of Regression coefficients
model.coef.table <-coef(summary(model))
model.coef <- data.frame(model.coef.table)

#Confidence Intervals
ci <- confint(model)

#Odds ratios
exp(cbind(OR = coef(model),ci))

#Pvalues
model.coef$pval = round((pnorm(abs(model.coef$t.value), lower.tail = FALSE) * 2),2)

model.coef

data = scores
data1 = merge(subset(data,year==2013),scores2013[c("city","country","system","corridor","speedInterval","g2","g3","g4","g5")],by=c("city","country","system","corridor"),all.x=TRUE)
data2 = merge(subset(data,year==2014),scores2014[c("city","country","system","corridor","speedInterval","g2","g3","g4","g5")],by=c("city","country","system","corridor"),all.x=TRUE)
data = rbind(data1,data2)

data$speedInterval[data$g1=="-10"] = 1; data$speedInterval[data$g1=="-6"] = 2; data$speedInterval[data$g1=="-3"] = 3; data$speedInterval[data$g1=="-1"] = 4; data$speedInterval[data$g1=="0"] = 5
data$speedInterval = as.factor(data$speedInterval)

data$g2[data$g2=="-5"] = 1; data$speedInterval[data$g2=="0"] = 2
data$g2 = as.factor(data$g2)

#2013

data = scores2013

data$speedInterval[data$g1==10] = 1
data$speedInterval[data$g1==6] = 2
data$speedInterval[data$g1==3] = 3
data$speedInterval[data$g1==1] = 4
data$speedInterval[data$g1==0] = 5
data$speedInterval = as.factor(data$g1)

# data$g1= factor(data$g1, levels = c("low","mid-low","mid","mid-high","high"), labels = c("-10","-6","-3","-1","0"))

model_ordinal_speed_ITDP_2014= polr(formula = speedInterval ~ a1+a2+a3+a4+a5+b2+ (g5>0)
                                    , data = subset(data), Hess = TRUE)

summary(model_ordinal_speed_ITDP_2014)

cTable <- coef(summary(model_ordinal_speed_ITDP_2014))

#Conf Int 
ci <- confint(model_ordinal_speed_ITDP_2014)

#Odds ratios
exp(cbind(OR = coef(model_ordinal_speed_ITDP_2014),ci))

data = scores
data1 = merge(subset(data,year==2013),scores2013[c("city","country","system","corridor","speedInterval","g2","g3","g4","g5")],by=c("city","country","system","corridor"),all.x=TRUE)
data2 = merge(subset(data,year==2014),scores2014[c("city","country","system","corridor","speedInterval","g2","g3","g4","g5")],by=c("city","country","system","corridor"),all.x=TRUE)
data = rbind(data1,data2)

data$speedInterval[data$g1=="-10"] = 1; data$speedInterval[data$g1=="-6"] = 2; data$speedInterval[data$g1=="-3"] = 3; data$speedInterval[data$g1=="-1"] = 4; data$speedInterval[data$g1=="0"] = 5
data$speedInterval = as.factor(data$speedInterval)

data$g2[data$g2=="-5"] = 1; data$speedInterval[data$g2=="0"] = 2
data$g2 = as.factor(data$g2)


# 3 intervals (OL1) -------------------------------------------------------------

data = scores2014
data$g1 = as.numeric(data$g1)
data$speedInterval = NA

#We will first transform the scores into 5 categories. As the category increases, the corridor's speed increases. 
data$speedInterval[data$g1>5] = 1 #data$g1[data$g1==10 || data$g1==6] = 1
data$speedInterval[data$g1<=5 & data$g1>0] = 2
data$speedInterval[data$g1==0] = 3
data$speedInterval = as.factor(data$speedInterval)

#Category Level

OL1SpeedITDP2014_AllCategories= polr(formula = speedInterval ~ a+b+c+d+e+f
                                    , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_AllCategories)

OLSpeedITDP2014_SignificantCategories= polr(formula = speedInterval ~ a
                                            , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_SignificantCategories)

#Subcategory Level
OLSpeedITDP2014_AllSubcategories= polr(formula = speedInterval ~ a1+a2+a3+a4+a5
                                       , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_AllSubcategories)

OLSpeedITDP2014_SignificantSubcategories= polr(formula = speedInterval ~a2+a4+a5+d1+d3+g5
                                               , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_SignificantSubcategories)


#Model for further analyses
model <- OLSpeedITDP2014_SignificantSubcategories

#List of Regression coefficients
model.coef.table <-coef(summary(model))
model.coef <- data.frame(model.coef.table)

#Confidence Intervals
ci <- confint(model)

#Odds ratios
exp(cbind(OR = coef(model),ci))

#Pvalues
model.coef$pval = round((pnorm(abs(model.coef$t.value), lower.tail = FALSE) * 2),2)

model.coef

#Analysis of the results of the 2013 and 2014 data

# (BL) Binary Logistic Regression ----------------------------------------------


data = scores2014
data$g1 = as.numeric(abs(data$g1))


#We will first transform the scores into 2 categories. As the category increases, the corridor's speed increases. 
scores2013$speedInterval = 1
scores2013$speedInterval[scores2013$g1>0] = 0 #data$g1[data$g1==10 || data$g1==6] = 1

scores2014$speedInterval = 1
scores2014$speedInterval[scores2014$g1>0] = 0 #data$g1[data$g1==10 || data$g1==6] = 1

# data$speedInterval = as.factor(data$speedInterval)

#Category Level
model2014 <- glm(speedInterval ~a+b+c+d+e+f,family=binomial(link='logit'),data=scores2014)
summary(model2014)

#Subcategory Level
model2014 <- glm(speedInterval ~a1+a2+a3+a4+a5+b2+f1,family=binomial(link='logit'),data=scores2014)
summary(model2014)

stepModel2014 <- stepAIC(glm(speedInterval ~a1+a2+a3+a4+a5+b2+f1,family=binomial(link='logit'),data=scores2014), direction="both")

summary(stepModel2014)

stepAllModel2014 <- stepAIC(glm(speedInterval~ a1+a2+a3+a4+a5+b1+b2+b3+b4+b5+b6+b7+d2,family=binomial(link='logit'),data=scores2014), direction="both")

summary(stepAllModel2014)

# +c1+c2+c3+c4+c5
# +f1+f2+f3+f4+f5+f6
# step2Model2014 <- regsubsets(glm(speedInterval ~a1+a2+a3+a4+a5+b2+f1,family=binomial(link='logit'),data=scores2014), nbest=)
# 
# summary(step2Model2014)


stepModelb2014 <- stepAIC(glm(speedInterval~ a4+a5+b2+b4+b5+d1+d3+g5,family=binomial(link='logit'),data=scores2014), direction="both")
summary(stepModelb2014)

model2014 <- glm(speedInterval ~a1+a2+a3+a4+a5+b2+d1,family=binomial(link='logit'),data=scores2014)
summary(model2014)

model2014 <- glm(speedInterval ~(a1)+a2+a4+(a5)+b2+(d1>0)+d3+(g5>0),family=binomial(link='logit'),data=scores2014)
summary(model2014)

model2014 <- glm(speedInterval ~(a1)+a4+(b4>0 & b5>0)+(d1>0)+d3,family=binomial(link='logit'),data=scores2014)
summary(model2014)

model2014 <- glm(speedInterval ~a2+a4+a5+(b5>0)+(b4>0)+d1+d3+g5,family=binomial(link='logit'),data=scores2014)
summary(model2014)

#MLR model
model2014 <- lm(speedInterval ~a4+a5+(b2)+b4+b5+d1+d3+g5,data=scores2014)
summary(model2014)


model2013 <- glm(speedInterval ~a4,family=binomial(link='logit'),data=scores2013)
summary(model2013)

#Speed regressoin with BRTdata and ITDP datasets
modelSpeedITDPBRTData <- lm(speedInterval ~speed_BRTData_2017,data=scores2014)
summary(modelSpeedITDPBRTData)








# 2013 --------------------------------------------------------------------

data = scores2013
data$speedInterval = NA

# 2 intervals -------------------------------------------------------------



# 2013 & 2014 -------------------------------------------------------------
# OL (3 intervals) -------------------------------------------------------------

data = scores
data$g1 = as.numeric(abs(data$g1))
data$speedInterval = NA

#We will first transform the scores into 5 categories. As the category increases, the corridor's speed increases. 
data$speedInterval[data$g1>50] = 1 #data$g1[data$g1==10 || data$g1==6] = 1
data$speedInterval[data$g1<=50 & data$g1>0] = 2
data$speedInterval[data$g1==0] = 3
data$speedInterval = as.factor(data$speedInterval)


#Category Level


OLSpeedITDP2014_AllCategories= polr(formula = speedInterval ~ a+b+c+d+e+f
                                    , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_AllCategories)

OLSpeedITDP2014_SignificantCategories= polr(formula = speedInterval ~ a
                                            , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_SignificantCategories)




#Subcategory Level

OLSpeedITDP2014_AllSubcategories= polr(formula = speedInterval ~a1+a2+a3+a4+a5+b2+(d1>0)+f1
                                         , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_AllSubcategories)


OLSpeedITDP2014_AllSubcategories50= polr(formula = speedInterval ~ (a1>50)+(a2>50)+(a3>50)+(a4>50)+(a5>50)+(b2>50)+(g5<0)
                                       , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_AllSubcategories50)

OLSpeedITDP2014_AllSignificantSubcategories33= polr(formula = speedInterval ~ (a1<33)+(a1>=33 & a1<67)+(a2<50)
                                                    +(a3<33)+(a3>=33 & a3<67)+(a4<33)+(a4>=33 & a4<67)+(a5<33)+(a5>=33 & a5<67)
                                                    +(b2<33)+(b2>=33 & b2<67)
                                         , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_AllSignificantSubcategories33)


OLSpeedITDP2014_SignificantSubcategories= polr(formula = speedInterval ~(a1)+(a3)+(b2)+(d1>0)+f1
                                                 , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_SignificantSubcategories)

OLSpeedITDP2014_SignificantSubcategories50= polr(formula = speedInterval ~(a3>50)+(a4>50)+(a5>50)+(b2>50)
                                               , data = subset(data), Hess = TRUE)


OLSpeedITDP2014_SignificantSubcategories33= polr(formula = speedInterval ~(a2<50)
                                                 +(a3<33)+(a3>=33 & a3<67)+(a4<33)+(a4>=33 & a4<67)+(a5<33)+(a5>=33 & a5<67)
                                                 +(b2<33)+(b2>=33 & b2<67)
                                                 , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_SignificantSubcategories33)

OLSpeedITDP2014_SignificantSubcategories= polr(formula = speedInterval ~a4+a5+(b2)+b4+b5+d1+d3+g5
                                                 , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_SignificantSubcategories)


OLSpeedITDP2014_SignificantSubcategories= polr(formula = speedInterval ~a4+a5+d1+d3
                                               , data = subset(data), Hess = TRUE)

summary(OLSpeedITDP2014_SignificantSubcategories)


#Model for further analyses
model <- OLSpeedITDP2014_SignificantSubcategories

#List of Regression coefficients
model.coef.table <-coef(summary(model))
model.coef <- data.frame(model.coef.table)

#Confidence Intervals
ci <- confint(model)

#Odds ratios
exp(cbind(OR = coef(model),ci))

#Pvalues
model.coef$pval = round((pnorm(abs(model.coef$t.value), lower.tail = FALSE) * 2),2)

model.coef









# ii) Throughput --------------------------------------------------------------------

scores2014$throughputInterval = 1
# scores2014$g2 =as.numeric(scores2014$g2)
scores2014$throughputInterval[scores2014$g2>0] = 0 #data$g1[data$g1==10 || data$g1==6] = 1

modelThroughput2014 <- glm(throughputInterval ~log(productivity_BRTData_2017),family=binomial(link='logit'),data=scores2014)
summary(modelThroughput2014)

modelThroughput2014 <- lm(throughputInterval ~ log(productivity_BRTData_2017),data=scores2014)
summary(modelThroughput2014)

#Throughput regressoin with BRTdata and ITDP datasets

modelThroughputITDPBRTData <- lm(throughputInterval ~speed_BRTData_2017,data=scores2014)
summary(modelSpeedITDPBRTData)


# Speed and Throughput ----------------------------------------------------


# ITDP --------------------------------------------------------------------

model2014SpeedThroughput = lm(g2 ~  g1
                               ,data = subset(scores2014))

summary(model2014SpeedThroughput)


# BRTData -----------------------------------------------------------------

scores2014$peak_load_2017 = as.character(scores2014$peak_load_2017)

model2014SpeedThroughputBRTData = lm(peak_load_2017 ~  speed_BRTData_2017
                              ,data = subset(scores2014))

summary(model2014SpeedThroughputBRTData)


# iii) Productivity ------------------------------------------------------------


model2014LogProductivityBRTData = lm(log(productivity_BRTData_2017) ~ log(a)
                                     # +a+b+d+e+f
                                     # a+b+c+d+e+f+g+
                                     # +corridor_length
                                     ,data = subset(scores2014))


summary(model2014aProductivityBRTData)
dim(model.frame(model2014aProductivityBRTData))

#Score by subcateogry
model2014aProductivityBRTData = lm(productivity_BRTData_2017 ~ a1+a2+a3+a4+a5+b2
                                   # +a+b+d+e+f
                                   # a+b+c+d+e+f+g+
                                   # +corridor_length
                                   ,data = subset(scores2014))


summary(model2014aProductivityBRTData)

#Overall Score
model2014bProductivityBRTData = lm(productivity_BRTData_2017 ~ score
                                   # +a+b+d+e+f
                                   # a+b+c+d+e+f+g+
                                   # +corridor_length
                                   ,data = subset(scores2014))


summary(model2014bProductivityBRTData)

#Intercept for each class of BRT

model2014c1ProductivityBRTData = lm(productivity_BRTData_2017 ~ factor(classification)
                                    # +a+b+d+e+f
                                    # a+b+c+d+e+f+g+
                                    # +corridor_length
                                    ,data = subset(scores2014))


summary(model2014c1ProductivityBRTData)



model2014c2ProductivityBRTData = lm(productivity_BRTData_2017 ~ factor(classification)+pop_density_2017
                                    # +a+b+d+e+f
                                    # a+b+c+d+e+f+g+
                                    # +corridor_length
                                    ,data = subset(scores2014,pop_density_2017>200))


summary(model2014c2ProductivityBRTData)

model2014dProductivityBRTData = lm(productivity_BRTData_2017 ~ factor(classification)+pop_density
                                   # +a+b+d+e+f
                                   # a+b+c+d+e+f+g+
                                   # +corridor_length
                                   ,data = subset(scores2014,pop_density_2017>200))


summary(model2014dProductivityBRTData)


scores2014b = subset(scores2014,scores2014$pop_density_2017>200)

plot(scores2014b$pop_density_2017,scores2014b$productivity_BRTData_2017)

dim(model.frame(model2014cProductivityBRTData))



View(model.frame(model2014aProductivityBRTData))






# b) Only ITDP Data ----------------------------------------------------------


model201SpeedITDP = lm(log(productivity_BRTData_2017) ~ log(a)
                       # +a+b+d+e+f
                       # a+b+c+d+e+f+g+
                       # +corridor_length
                       ,data = subset(scores2014))


# Speed ITDP (ordinal and MLR) -------------------------------------------------------------------

# Througput ----------------------------------------------------------------

data = scores2014

data$g2[data$g2 == 0] = 1
data$g2[data$g2==5] = 0

model0= lm(g2 ~ a1+a2+a3+a4+a5
           ,data = subset(data))
summary(model0)


# c) Only BRTData ---------------------------------------------------------


# Productivity ------------------------------------------------------------


modelBRTDataProductivityDensity = lm(log(productivity) ~ log(pop_density)
                                     # +a+b+d+e+f
                                     # a+b+c+d+e+f+g+
                                     # +corridor_length
                                     ,data = subset(brt_corridors))

summary(modelBRTDataProductivityDensity)


modelBRTDataProductivityDensity1 = lm(productivity ~ pop_density
                                      # +a+b+d+e+f
                                      # a+b+c+d+e+f+g+
                                      # +corridor_length
                                      ,data = subset(itdp_cities))

summary(modelBRTDataProductivityDensity1)

modelBRTDataProductivityDensityLog1 = lm(log(productivity) ~ log(pop_density)
                                         # +a+b+d+e+f
                                         # a+b+c+d+e+f+g+
                                         # +corridor_length
                                         ,data = subset(itdp_cities))

summary(modelBRTDataProductivityDensityLog1)




# iv) City Level ----------------------------------------------------------

modelBRTDataProductivityDensity1 = lm(productivity ~ pop_density + score
                                      # +a+b+d+e+f
                                      # a+b+c+d+e+f+g+
                                      # +corridor_length
                                      ,data = subset(itdp_cities))

summary(modelBRTDataProductivityDensity1)


