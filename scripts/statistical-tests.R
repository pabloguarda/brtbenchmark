
# a) Means differences -------------------------------------------------------

# Dimensions ITDP ---------------------------------------------------------

# Algorithm ---------------------------------------------------------------
# i) Input Parameters --------------------------------------------------------

letters = c("a","b","c","d","e","f","g","score","total_score")
# letters = c("a","b","c","d","e","f","g")
list_test_variables = c(itdp$ITDP2014,letters)
# data = scores
group_variable = "country"
group_value= "China"
# weight_variable = "corridor_length"
weight_variable = NA
significance=0.05
# ii) City Level (two-tailed)  ---------------------------------------------------------------
data = itdp_cities
unit=c("country","city")
# - Two-Tailed --------------------------------------------------------------

alternative = "two.tailed"

list_strengths = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                   ,alternative,significance)$strengths

list_weaknesses = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                    ,alternative,significance)$weaknesses


list_nodifference = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                    ,alternative,significance)$no_difference

# View(list_strengths); View(list_weaknesses); View(list_nodifference)

strengths_weaknesses_China = rbind(data.frame(type="strengths",list_strengths),data.frame(type="weaknesses",list_weaknesses),data.frame(type="no_difference",list_nodifference))

# View(strengths_weaknesses_China)
# - Weaknesses (one-tailed) -------------------------------------------------
alternative = "less"

list_weaknesses = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                     ,alternative,significance)$weaknesses

list_no_weaknesses= weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                       ,alternative,significance)$no_weaknesses

# View(list_weaknesses); View(list_nodifference)

weaknesses_China = rbind(data.frame(type="weaknesses",list_weaknesses),data.frame(type="no_difference",list_no_weaknesses))

# View(weaknesses_China)
# - Strengths (one-tailed) ---------------------------------------------------------------
alternative = "greater"

list_strengths = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                    ,alternative,significance)$strengths

list_no_strengths= weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                      ,alternative,significance)$no_strengths

# View(list_strengths); View(list_no_strengths)

strengths_China = rbind(data.frame(type="strengths",list_strengths),data.frame(type="no_difference",list_no_strengths))

# View(strengths_China)

# iii) System Level---------------------------------------------------
data = itdp_systems
unit=c("country","city","system")
# - Two-Tailed --------------------------------------------------------------

alternative = "two.tailed"

list_strengths = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                    ,alternative,significance)$strengths

list_weaknesses = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                     ,alternative,significance)$weaknesses


list_nodifference = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                       ,alternative,significance)$no_difference

# View(list_strengths); View(list_weaknesses); View(list_nodifference)

strengths_weaknesses_China = rbind(data.frame(type="strengths",list_strengths),data.frame(type="weaknesses",list_weaknesses),data.frame(type="no_difference",list_nodifference))

# View(strengths_weaknesses_China)
# - Weaknesses (one-tailed) -------------------------------------------------
alternative = "less"

list_weaknesses = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                     ,alternative,significance)$weaknesses

list_no_weaknesses= weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                       ,alternative,significance)$no_weaknesses

# View(list_weaknesses); View(list_nodifference)

weaknesses_China = rbind(data.frame(type="weaknesses",list_weaknesses),data.frame(type="no_difference",list_no_weaknesses))

# View(weaknesses_China)
# - Strengths (one-tailed) ---------------------------------------------------------------
alternative = "greater"

list_strengths = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                    ,alternative,significance)$strengths

list_no_strengths= weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                      ,alternative,significance)$no_strengths

# View(list_strengths); View(list_no_strengths)

strengths_China = rbind(data.frame(type="strengths",list_strengths),data.frame(type="no_difference",list_no_strengths))

# View(strengths_China)

# iv) Corridor Level---------------------------------------------------

data = scores
unit=c("country","city","system","corridor")

# - Anova China vs no China -----------------------------------------------------------------

letters = c("score","total_score")
# letters = c("a","b","c","d","e","f","g")
list_test_variables = c(itdp$ITDP2014,letters)
group_variable = "country"
group_value= "China"
# weight_variable = "corridor_length"
weight_variable = NA
significance=0.05
data = scores
unit=c("country","city","system","corridor")
# data = subset(scores, classification=="Basic")
# data = subset(scores, classification=="Bronze")
# data = subset(scores, classification=="Silver")
# data = subset(scores, classification=="Gold")

list_strengths = weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$strengths
list_weaknesses= weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$weaknesses
list_nodifference = weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$no_difference

# View(list_strengths); View(list_weaknesses); View(list_nodifference)

strengths_weaknesses_China = rbind(data.frame(type="strengths",list_strengths),data.frame(type="weaknesses",list_weaknesses),data.frame(type="no_difference",list_nodifference))

# View(strengths_weaknesses_China)
# - Anova China vs no China 2013 and 2014 -----------------------------------------------------------------

letters = c("score","total_score")
# letters = c("a","b","c","d","e","f","g")
list_test_variables = c(itdp$ITDP2014,letters)
group_variable = "country"
group_value= "China"
# weight_variable = "corridor_length"
weight_variable = NA
significance=0.05

data = subset(scores, year=="2013")
unit=c("country","city","system","corridor")

list_strengths = weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$strengths
list_weaknesses= weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$weaknesses
list_nodifference = weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$no_difference

strengths_weaknesses_China_2013 = rbind(data.frame(type="strengths",list_strengths),data.frame(type="weaknesses",list_weaknesses),data.frame(type="no_difference",list_nodifference))

data = subset(scores, year=="2014")

list_strengths = weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$strengths
list_weaknesses= weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$weaknesses
list_nodifference = weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$no_difference

strengths_weaknesses_China_2014 = rbind(data.frame(type="strengths",list_strengths),data.frame(type="weaknesses",list_weaknesses),data.frame(type="no_difference",list_nodifference))

data = subset(scores)

list_strengths = weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$strengths
list_weaknesses= weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$weaknesses
list_nodifference = weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$no_difference

strengths_weaknesses_China = rbind(data.frame(type="strengths",list_strengths),data.frame(type="weaknesses",list_weaknesses),data.frame(type="no_difference",list_nodifference))

strengths_weaknesses_China_year = rbind(data.frame(year="2013",strengths_weaknesses_China_2013),data.frame(year="2014",strengths_weaknesses_China_2014),data.frame(year="2013-2014",strengths_weaknesses_China))

# v) Trade-off Analysis -------------------------------------
# Input  ------------------------------------------------------------------

confidence_level = 0.95

#Matriz de las debilidades y fortalezas de los sistemas
strengths_weaknesses = strengths_weaknesses_China_year
# Regression Model --------------------------------------------------------


#Regressions productivity and total score (cities)
mlr_itdp_all_cities = lm(productivity~ total_score,data=subset(itdp_cities)
                  # +(country=="China")+(country=="China")*total_score
) 
summary(mlr_itdp_all_cities)

mlr_itdp_china_cities = lm(productivity~ total_score,data=subset(itdp_cities,country=="China")) 
summary(mlr_itdp_china_cities)
dim(model.frame(mlr_itdp_china_cities))

mlr_itdp_nochina_cities= lm(productivity~ total_score,data=subset(itdp_cities,country!="China")) 
summary(mlr_itdp_nochina_cities)
dim(model.frame(mlr_itdp_nochina_cities))

#Regressions productivity and total score (corridors/systems)
mlr_itdp_all = lm(productivity~ total_score,data=subset(scores)
                  # +(country=="China")+(country=="China")*total_score
                  ) 
summary(mlr_itdp_all)

mlr_itdp_china = lm(productivity~ total_score,data=subset(scores,country=="China")) 
summary(mlr_itdp_china)
dim(model.frame(mlr_itdp_china))

mlr_itdp_nochina= lm(productivity~ total_score,data=subset(scores,country!="China")) 
summary(mlr_itdp_nochina)
dim(model.frame(mlr_itdp_nochina))


#Regressions productivity and scores divided by dimension(corridors/systems)

#a) Using data from all BRTs
itdp_model_all= lm(productivity~  
                       a+b+c+d+e+f+g
                     # +d+e 
                     # e da incorrectamente con signo negativo, pero no es significativo, se sacó
                     # +g da incorrectamente con signo positivo
                     ,data = subset(scores))
# ,data = subset(brt_corridors,system_maturity>0 & is.na(system_maturity)==FALSE))

summary(itdp_model_all)

itdp_model_all_1= lm(productivity~  
                     b+c+d+f
                   ,data = subset(scores))
summary(itdp_model_all_1)

#b) Using all only data from Chinese BRTs
itdp_model_china= lm(productivity~  
                 a+b+c+d+e+f+g
               # +d+e 
               # e da incorrectamente con signo negativo, pero no es significativo, se sacó
               # +g da incorrectamente con signo positivo
               ,data = subset(scores,country=="China"))
# ,data = subset(brt_corridors,system_maturity>0 & is.na(system_maturity)==FALSE))

summary(itdp_model_china)

itdp_model_china_1= lm(productivity~  
                       a+b+c+d+f
                     # +d+e 
                     # e da incorrectamente con signo negativo, pero no es significativo, se sacó
                     # +g da incorrectamente con signo positivo
                     ,data = subset(scores,country=="China"))
# ,data = subset(brt_corridors,system_maturity>0 & is.na(system_maturity)==FALSE))

summary(itdp_model_china_1)

#c) Using all only data from non-Chinese BRTs
itdp_model_nochina= lm(productivity~  
                       a+b+c+d+e+f+g
                     # +d+e 
                     # e da incorrectamente con signo negativo, pero no es significativo, se sacó
                     # +g da incorrectamente con signo positivo
                     ,data = subset(scores,country!="China"))
# ,data = subset(brt_corridors,system_maturity>0 & is.na(system_maturity)==FALSE))

summary(itdp_model_nochina)
# Calculation "Elasticities" ------------------------------------------------

model = itdp_model_china #Including one explanatory variable per each dimensions veluated in the ITDP standard. 
# model = mlr_itdp_china #Including only one variable to capture the effect of BRT standard scores as a whole

temp = confint(model, level = confidence_level)
  
beta_matrix = data.frame(left_beta=temp[,1],mean_beta = rowMeans(temp),right_beta = temp[,2])

beta_matrix$beta = NA ; beta_matrix$variable = row.names(beta_matrix)
# Calculation Productivity Improvement ------------------------------------------------


#Maximum score per subcategory
strengths_weaknesses = merge(strengths_weaknesses,itdp2014[c("id","max_score")],by.x="id_variable",by.y="id",all.x=TRUE)
strengths_weaknesses$max_score = as.numeric(strengths_weaknesses$max_score)
strengths_weaknesses$id_variable = as.character(strengths_weaknesses$id_variable)

#Identification of the category of the subcategory
strengths_weaknesses$category_variable= NA
for(i in 1:dim(strengths_weaknesses)[1]){
  strengths_weaknesses$category_variable[i] = str_sub(strengths_weaknesses$id_variable[i],1,1)  
}


if("total_score" %in% beta_matrix$variable){
  
  strengths_weaknesses$left_beta = NA
  strengths_weaknesses$mean_beta = NA
  strengths_weaknesses$right_beta = NA
  strengths_weaknesses$left_beta[strengths_weaknesses$category_variable %in% c("a","b","c","d","e","f","g")] = subset(beta_matrix,variable=="total_score")$left_beta
  strengths_weaknesses$mean_beta[strengths_weaknesses$category_variable %in% c("a","b","c","d","e","f","g")] = subset(beta_matrix,variable=="total_score")$mean_beta
  strengths_weaknesses$right_beta[strengths_weaknesses$category_variable %in% c("a","b","c","d","e","f","g")] = subset(beta_matrix,variable=="total_score")$right_beta
  
  #Como la regresión es hecha con el puntaje, para poder aplicar este factor y calcular la diferencia en productividad, se transforma a escala de puntos nuevamente la diferencia
  strengths_weaknesses$left_productivity = round(with(strengths_weaknesses,left_beta*Difference*max_score/100),0)
  strengths_weaknesses$mean_productivity = round(with(strengths_weaknesses,mean_beta*Difference*max_score/100),0)
  strengths_weaknesses$right_productivity = round(with(strengths_weaknesses,right_beta*Difference*max_score/100),0)
  strengths_weaknesses$interval_productivity = round(with(strengths_weaknesses,right_productivity-left_productivity),0)
  
  strengths_weaknesses$significant = "No"
  strengths_weaknesses$significant[with(strengths_weaknesses,(left_productivity>0 & right_productivity>0) | (left_productivity<0 & right_productivity))] = "Yes"
  
  #Ranking based on mean productivity
  strengths_weaknesses$ranking = order(strengths_weaknesses$mean_productivity,decreasing = TRUE)
}

if("total_score" %in% beta_matrix$variable == FALSE){
  strengths_weaknesses = merge(strengths_weaknesses,beta_matrix,all.x=TRUE,by.x="category_variable",by.y="variable")  
  
  #En este caso los betas están en las mismas unidades (porcentaje) que las diferencias, asi que no se hace conversión a unidad de puntaje
  strengths_weaknesses$left_productivity = round(with(strengths_weaknesses,left_beta*Difference),0)
  strengths_weaknesses$mean_productivity = round(with(strengths_weaknesses,mean_beta*Difference),0)
  strengths_weaknesses$right_productivity = round(with(strengths_weaknesses,right_beta*Difference),0)
  strengths_weaknesses$interval_productivity = round(with(strengths_weaknesses,right_productivity-left_productivity),0)
  
  strengths_weaknesses$significant = "No"
  strengths_weaknesses$significant[with(strengths_weaknesses,(left_productivity>0 & right_productivity>0) | (left_productivity<0 & right_productivity<0))] ="Yes"
  
  #Ranking based on mean productivity
  strengths_weaknesses$ranking = order(strengths_weaknesses$mean_productivity,decreasing = TRUE)
  
  
}
# Export to Tableau -------------------------------------------------------

save(strengths_weaknesses,file=str_c(data.base,"output/strengths_weaknesses_tradeoff.Rdata"))
write.csv(strengths_weaknesses,file = str_c(data.base,"outputstrengths_weaknesses_tradeoff.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

# - Corridor level ----------------------------------------------------------


#Agrego la columna de putnajes máximo por categoría a cada uno de los indicadores para poder calcular la mejora esperada en el indicador

letters_temp = c("a","b","c","d","e","f","g")
letters_temp2014 = c(itdp$ITDP2014,letters_temp)
strengths_weaknesses_China$score = NA

strengths_weaknesses_China_year = merge(strengths_weaknesses_China_year,itdp2014[c("id","max_score")],by.x="id_variable",by.y="id",all.x=TRUE)
strengths_weaknesses_China = merge(strengths_weaknesses_China,itdp2014[c("id","max_score")],by.x="id_variable",by.y="id",all.x=TRUE)


# # Validation --------------------------------------------------------------
# 
# mean(subset(scores,country=="China" & year=="2013")$e)-mean(subset(scores,country!="China" & year=="2013")$e)
# mean(subset(scores,country=="China" & year=="2014")$e)-mean(subset(scores,country!="China" & year=="2014")$e)
# mean(subset(scores,country=="China")$e)-mean(subset(scores,country!="China")$e)

# - Two-tailed --------------------------------------------------------------
alternative = "two.tailed"
# weight_variable="corridor_length"
# weight_variable=NA
list_strengths = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                    ,alternative,significance)$strengths

list_weaknesses = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                     ,alternative,significance)$weaknesses

list_nodifference = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                       ,alternative,significance)$no_difference

# View(list_strengths); View(list_weaknesses); View(list_nodifference)

strengths_weaknesses_China = rbind(data.frame(type="strengths",list_strengths),data.frame(type="weaknesses",list_weaknesses),data.frame(type="no_difference",list_nodifference))

# View(strengths_weaknesses_China)
# - Weaknesses(one-tailed) --------------------------------------------------

list_weaknesses = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                     ,alternative,significance)$weaknesses

list_no_weaknesses= weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                       ,alternative,significance)$no_difference

# View(list_weaknesses); View(list_no_weaknesses)

weaknesses_China = rbind(data.frame(type="weaknesses",list_weaknesses),data.frame(type="no_difference",list_no_weaknesses))

# View(weaknesses_China)
# - Strengths (one-tailed) ---------------------------------------------------------------

letters = c("a","b","c","d","e","f","g","score","total_score")
# letters = c("a","b","c","d","e","f","g")
list_test_variables = c(itdp$ITDP2014,letters)
data = scores
group_variable = "country"
group_value= "China"
weight_variable = "corridor_length"
alternative = "greater"
unit=c("country","city","system")
significance=0.05

list_strengths = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                     ,alternative,significance)$strengths

list_no_strengths= weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                       ,alternative,significance)$no_strengths

# View(list_strengths); View(list_no_strengths)

strengths_China = rbind(data.frame(type="strengths",list_strengths),data.frame(type="no_difference",list_no_strengths))

# View(strengths_China)

# - BRT Basics ------------------------------------------------------------

#Corridors/Systems
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",alternative="two.tailed",test_variable = "a")
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "a") #Weighted Length
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "demand",alternative="two.tailed",test_variable = "a") #Weighted Demand

#City Level
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "a")
#System Level
weighted.mean.test(data = itdp_systems,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "a")

#Analysis by subindicator:
weighted.mean.test(data = itdp_cities,test_variable = "a1", group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed")
weighted.mean.test(data = itdp_cities,test_variable = "a2", group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed")
weighted.mean.test(data = itdp_cities,test_variable = "a3", group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed")
weighted.mean.test(data = itdp_cities,test_variable = "a4", group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed")
weighted.mean.test(data = itdp_cities,test_variable = "a5", group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed")
# - Service Planning  -----------------------------------------------------
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",alternative="two.tailed",test_variable = "b")
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "b")
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "demand",alternative="two.tailed",test_variable = "b")

weighted.mean.test(data = itdp_cities,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "b")
# - Infrastructure --------------------------------------------------------
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",alternative="two.tailed",test_variable = "c")
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "c")
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "demand",alternative="two.tailed",test_variable = "c")

weighted.mean.test(data = itdp_cities,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "c")
# - Station Design and Station-bus Interface --------------------------------
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",alternative="two.tailed",test_variable = "d")
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "d")
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "demand",alternative="two.tailed",test_variable = "d")

weighted.mean.test(data = itdp_cities,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "d")
# - Quality of Service & Passenger Information Systems ----------------------
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",alternative="two.tailed",test_variable = "e")
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "e")
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "demand",alternative="two.tailed",test_variable = "e")
# - Integration and Access ------------------------------------------------
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",alternative="two.tailed",test_variable = "f")
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "f")
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "demand",alternative="two.tailed",test_variable = "f")
# - Point Deductions ------------------------------------------------------
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",alternative="two.tailed",test_variable = "g")
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "g")
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "demand",alternative="two.tailed",test_variable = "g")

# Metro system ------------------------------------------------------------
# - Anova -----------------------------------------------------------------

data=scores
group_variable = "metro"
group_value = 1

list_strengths = weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$strengths
list_weaknesses= weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$weaknesses
list_nodifference = weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$no_difference

# View(list_strengths); View(list_weaknesses); View(list_nodifference)

strengths_weaknesses_China = rbind(data.frame(type="strengths",list_strengths),data.frame(type="weaknesses",list_weaknesses),data.frame(type="no_difference",list_nodifference))

# View(strengths_weaknesses_China)


# Speed -----------------------------------------------------------
# - BRTData ----------------------------------------------------------------

#Corridors

#Chinese Corridors
a1 = length(subset(brt_corridors,brt_corridors$country=="China" & is.na(brt_corridors$speed)==FALSE)$speed)
b1 = length(subset(brt_corridors,brt_corridors$country=="China")$speed)
c1 = round(100*a1/b1,2)
mean(subset(brt_corridors,brt_corridors$country=="China" & is.na(brt_corridors$speed)==FALSE)$speed)

#Other corridors
a2 = length(subset(brt_corridors,brt_corridors$country!="China" & is.na(brt_corridors$speed)==FALSE)$speed)
b2 = length(subset(brt_corridors,brt_corridors$country!="China")$speed)
c2 = round(100*a2/b2,2)
mean(subset(brt_corridors,brt_corridors$country!="China" & is.na(brt_corridors$speed)==FALSE)$speed)

#Corridors
#Weighting by demand
weighted.mean.test(data = brt_corridors,group_variable = "country",group_value = "China",weight_variable = "demand",alternative="two.tailed",test_variable = "speed")

#Weighting by corridor length
weighted.mean.test(data = brt_corridors,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "speed")

#Weighting equally
weighted.mean.test(data = brt_corridors,group_variable = "country",group_value = "China",alternative="two.tailed",test_variable = "speed")

#Chinese Systems

#Weighting by demand
weighted.mean.test(data = brt_systems,test_variable = "speed",group_variable = "country",group_value = "China",weight_variable = "demand",alternative="two.tailed")

#Weighting by system length
weighted.mean.test(data = brt_systems,test_variable = "speed",group_variable = "country",group_value = "China",weight_variable = "system_length",alternative="two.tailed")

#Weighting equally
weighted.mean.test(data = brt_systems,test_variable = "speed",group_variable = "country",group_value = "China",alternative="two.tailed")
# - ITDP --------------------------------------------------------------------

#Corridors
#Weighting by demand
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "demand",alternative="two.tailed",test_variable = "speed")

#Weighting by corridor length
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed",test_variable = "speed")

#Weighting equally
weighted.mean.test(data = scores,group_variable = "country",group_value = "China",alternative="two.tailed",test_variable = "speed")

# - Class BRT ---------------------------------------------------------------
TukeyHSD(aov(speed ~ factor(classification),scores))

TukeyHSD(aov(productivity ~ factor(country=="China"),scores))
round(anova(lm(productivity ~ factor(country=="China"),data))$P[1],3)



# Station spacing BRTdata ----------------------------------------------------
# - System ------------------------------------------------------------------

#System

#China
mean(with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$station_spacing)==FALSE),station_spacing))
#Rest of the world
mean(with(subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$station_spacing)==FALSE),station_spacing))

mean_station_spacing_length = with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$station_spacing)==FALSE),wtd.mean(station_spacing,system_length))
sd_station_spacing_length = sqrt(with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$station_spacing)==FALSE),wtd.var(station_spacing,system_length)))
mean_station_spacing_demand = with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$station_spacing)==FALSE),wtd.mean(station_spacing,demand))
sd_station_spacing_demand = sqrt(with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$station_spacing)==FALSE),wtd.var(station_spacing,demand)))

#Weighting by demand

weighted.mean.test(data = brt_systems,test_variable = "station_spacing",group_variable = "country",group_value = "China",weight_variable = "demand",alternative="two.tailed")

#Weighting by system length
weighted.mean.test(data = brt_systems,test_variable = "station_spacing",group_variable = "country",group_value = "China",weight_variable = "system_length",alternative="two.tailed")

#Weighting equally
weighted.mean.test(data = brt_systems,test_variable = "station_spacing",group_variable = "country",group_value = "China",alternative="two.tailed")

# Productivity ----------------------------------------------------
# - BRTData ------------------------------------------------------------------

#System

#Weighting by demand
weighted.mean.test(data = brt_systems,test_variable = "productivity",group_variable = "country",group_value = "China",weight_variable = "demand",alternative="two.tailed")

#Weighting by system length
weighted.mean.test(data = brt_systems,test_variable = "productivity",group_variable = "country",group_value = "China",weight_variable = "system_length",alternative="two.tailed")

#Weighting equally
weighted.mean.test(data = brt_systems,test_variable = "productivity",group_variable = "country",group_value = "China",alternative="two.tailed")
# - ITDP --------------------------------------------------------------------

# #Weighting by demand
# weighted.mean.test(data = scores,test_variable = "productivity",group_variable = "country",group_value = "China",weight_variable = "demand",alternative="two.tailed")
# 
# #Weighting by system length
# weighted.mean.test(data = scores,test_variable = "productivity",group_variable = "country",group_value = "China",weight_variable = "corridor_length",alternative="two.tailed")
# 
# #Weighting equally
# weighted.mean.test(data = scores,test_variable = "productivity",group_variable = "country",group_value = "China",alternative="two.tailed")
# - Ranking ITDP ------------------------------------------------------------

summary(lm(productivity ~ factor(classification),scores))
summary(lm(speed ~ factor(classification),scores))

anova(lm(speed ~ factor(classification),scores))
anova(lm(productivity ~ factor(classification),scores))

TukeyHSD(aov(productivity ~ factor(classification),scores))

# Peak frequency BRTdata ----------------------------------------------------
# - System ----------------------------------------------------------------

#China
mean(with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$peak_frequency)==FALSE),peak_frequency))
#Rest of the world
mean(with(subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$peak_frequency)==FALSE),peak_frequency))

mean_peak_frequency_length = with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$peak_frequency)==FALSE),wtd.mean(peak_frequency,system_length))
sd_peak_frequency_length = sqrt(with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$peak_frequency)==FALSE),wtd.var(peak_frequency,system_length)))
mean_peak_frequency_demand = with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$peak_frequency)==FALSE),wtd.mean(peak_frequency,demand))
sd_peak_frequency_demand = sqrt(with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$peak_frequency)==FALSE),wtd.var(peak_frequency,demand)))

wtd.t.test(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$peak_frequency)==FALSE)$peak_frequency,
           subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$peak_frequency)==FALSE)$peak_frequency,
           weight=subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$peak_frequency)==FALSE)$demand,
           weighty=subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$peak_frequency)==FALSE)$demand
           ,alternative = "lower")

#Weighting by system length
wtd.t.test(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$peak_frequency)==FALSE)$peak_frequency,
           subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$peak_frequency)==FALSE)$peak_frequency,
           weight=subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$peak_frequency)==FALSE)$system_length,
           weighty=subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$peak_frequency)==FALSE)$system_length
           ,alternative = "greater")

#Weighting equally
weights::wtd.t.test(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$peak_frequency)==FALSE)$peak_frequency,
           subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$peak_frequency)==FALSE)$peak_frequency,
           weight=subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$peak_frequency)==FALSE)$one,
           weighty=subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$peak_frequency)==FALSE)$one
           ,alternative = "greater")

