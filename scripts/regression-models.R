######################################################################################################
# ######################## MODELS 2015 ################################################################
######################################################################################################
# Models ------------------------------------------------------------------

model0= lm(abs(g1) ~  c1+c2+c3+c4+c5
           # +a+b+d+e+f
           # a+b+c+d+e+f+g+
           # +corridor_length
           ,data = subset(scores2013))

summary(model0)


model1= lm(total_score ~  gdp+bus_split+corridor_length
                   # a+b+c+d+e+f+g+
                   # +corridor_length
                   ,data = subset(scores))

summary(model1)


# model2= lm(total_score ~  gdp+bus_split+corridor_length
#            # a+b+c+d+e+f+g+
#            # +corridor_length
#            ,data = subset(brt_corridors))
# 
# summary(model2)

# variables_cervero1 = paste0("log(pop_density) + corridor_length")

# Demand ------------------------------------------------------------------


model_cervero = lm(log(demand) ~ log(pop_density) + corridor_length
                   ,data = subset(brt_corridors))

summary(model_cervero)

#Lo que una persona recibiría de ingresos en promedio en un día del año
#Pondero por dos asumiendo que hace dos viajes en promedio al día
brt_corridors$ratio_fare = with(brt_corridors,2*bus_fare/(gdp/365))


brt_corridors$bus_riders = with(brt_corridors,population*bus_split/100)

brt_corridors$waiting_time= 60/(2*brt_corridors$peak_frequency)


model_cervero2 = lm(log(demand) ~ log(pop_density) + corridor_length
                    +bus_fare
                    # +gdp
                    # +peak_frequency
                    +waiting_time
                    # +bus_split
                    # +factor(continent)
                    # +x_exclusive_lanes
                    # +speed
                    # +factor(integrated_feeders)
                    +station_spacing #La gente camina más si las estaciones están más separadas
                    # +gdp
                    # +ratio_fare
                    ,data = subset(brt_corridors))
  
summary(model_cervero2)

dim(model.frame(model_cervero2))

model_ridership_pgr = lm(log(demand) ~ 
                        +log(pop_density) 
                        +corridor_length
                        +bus_fare
                        # +score
                        +a+b+c+d+e+f+g
                        +car_split
                        # +ratio_fare
                        # +gdp
                        # +peak_frequency
                        # +system_maturity
                        +waiting_time
                        # +bus_split
                        # +factor(continent)
                        # +x_exclusive_lanes
                        # +speed
                        # +factor(integrated_feeders)
                        +station_spacing
                        # +log(ratio_fare)
                        ,data = subset(brt_corridors))
summary(model_ridership_pgr)

dim(model.frame(model_ridership_pgr))

model_ridership_pgr1 = glm.nb(demand ~ 
                           # +log(population)
                         +pop_density
                         +corridor_length
                         +bus_fare
                         # +score
                         +waiting_time
                         +b+c+d+e+f+g
                         # +a+b+c+d+e+f+g
                         
                         # +score, 
                         ,link = log
                         ,data = subset(brt_corridors))
summary(model_ridership_pgr1)

# Scores ITDP -------------------------------------------------------------
brt_corridors$years_corridor_operating = 2014-brt_corridors$year_corridor_commenced
brt_corridors$years_corridor_operating2 = (brt_corridors$years_corridor_operating)^2
brt_corridors$log_years_corridor_operating = log(brt_corridors$years_corridor_operating)
brt_corridors$pop_density2 = (brt_corridors$pop_density)^2
brt_corridors$system_maturity2 = (brt_corridors$system_maturity)^2
brt_corridors$log_system_maturity = log(brt_corridors$system_maturity)

brt_corridors$fare_revenues = with(brt_corridors,bus_fare*demand)
summary(brt_corridors$pop_density)
itdp_model= lm(total_score ~  
               # +log(corridor_length)
               +gdp
               # +ratio_fare
               +I(log(bus_fare)) #Proxy de cuanto cuesta construir el sistema, asumiendo que el subsidio es constante
               # +fare_revenues
               +system_maturity
               +I(system_maturity^2)
               +years_corridor_operating
               # +years_corridor_operating2
               # +log(system_maturity)
               # +demand
               # +log(year_corridor_operating)
               +pop_density
               +I(pop_density^2)
#                +(pop_density>347.2 & pop_density<2209)
#                +(pop_density>=2209)
               +bus_split

               # +factor(integrated_feeders)
               # +station_spacing
               # +factor(offboard_payment=="None")
               # +factor(country=="China")
               # +cost_per_km
               # +a+b+c+d+e+f
                   # +g
                   # +corridor_length
              ,data = subset(brt_corridors,is.na(year_corridor_commenced)==FALSE))
                   # ,data = subset(brt_corridors,system_maturity>0 & is.na(system_maturity)==FALSE))

summary(itdp_model)

itdp_model1= lm(total_score ~  
                +corridor_length
                +gdp
               +bus_fare #Proxy de cuanto cuesta construir el sistema, asumiendo que el subsidio es constante
               +system_maturity
               +years_corridor_operating
               +pop_density
               +bus_split
               ,data = subset(brt_corridors,is.na(year_corridor_commenced)==FALSE))

summary(itdp_model1)


itdp_model_final= lm(total_score ~  
                     +log(I(corridor_length))
                     # +I((corridor_length)^2)
                     # +log(bus_fare)
                     # +bus_fare
                     # +car_split
                     +system_maturity
                     +I(system_maturity^2)
                     # +I(exp(system_maturity))
                     +I(years_corridor_operating)
                     # +I(years_corridor_operating^2)
                     # +I(log(years_corridor_operating))
                     +I(log(pop_density))
#                      +pop_density
#                      +I(pop_density^2)
                     +bus_split
                ,data = subset(brt_corridors,is.na(year_corridor_commenced)==FALSE
                               &years_corridor_operating>=0
                               ))

summary(itdp_model_final)

car::vif(itdp_model_final)

dim(model.frame(itdp_model_final))


influence(itdp_model)

# Relative importance of each parameter in the model (relaimpo package)


dim(model.frame(itdp_model))

summary(brt_corridors$score[is.na(brt_corridors$score)==FALSE])
summary(brt_corridors$gdp[is.na(brt_corridors$gdp)==FALSE])

brt_corridors$bus_split
# Speed BRTdata -------------------------------------------------------------------

dim(brt_corridors)

brt_corridors$x_exclusive_lanes = with(brt_corridors,exclusive_lanes_length/corridor_length)
brt_corridors$x_segregated_lanes = with(brt_corridors,segregated_lanes_length/corridor_length)
brt_corridors$x_segregated_counterflow_lanes = with(brt_corridors,segregated_counterflow_lanes_length/corridor_length)
brt_corridors$pax_density_hr = with(brt_corridors,demand/corridor_length)/24*0.8

model_cristina = lm(speed ~ station_spacing
                    # +(offboard_payment=="All"|offboard_payment=="Partial")
                    +exclusive_lanes_length
                    +segregated_counterflow_lanes_length
                    # +x_segregated_lanes_length
                    # +year_corridor_commenced
                    # +speed+demand
                    # pop_density
                    ,data = subset(brt_corridors,is.na(offboard_payment)==FALSE))

summary(model_cristina)
dim(model.frame(model_cristina))


model_cristina2 = lm(speed ~ station_spacing
                     # +(offboard_payment=="All")
                     +pax_density_hr
                     +x_exclusive_lanes
                     +x_segregated_counterflow_lanes
                     +peak_frequency
                     # +cost_per_km
                     #                     +x_segregated_lanes
                     # +year_corridor_commenced
                     # pop_density
                     ,data = subset(brt_corridors,is.na(offboard_payment)==FALSE))

summary(model_cristina2)
dim(model.frame(model_cristina2))


model_speed = lm(speed ~ 
             # a+b+d+e+f+g
             +a+c+e
             # +speed
             +demand
             +station_spacing
             +year_corridor_commenced
             # +a+b
            # + pop_density
             ,data = subset(brt_corridors))

summary(model_speed)

model_pgr = lm(peak_load ~ 
                 station_spacing
                               # +score
                               +a+b+c+d+e+f
                               # +pax_density_hr
#                                +x_exclusive_lanes
#                                +x_segregated_counterflow_lanes
#                                +peak_frequency
                               ,data = subset(brt_corridors,is.na(offboard_payment)==FALSE))

summary(model_pgr)
dim(model.frame(model_pgr))
# Speed ITDP (ordinal and MLR) -------------------------------------------------------------------

scores2013$g1
model0= lm(abs(g1) ~
             a+b+c+d+e+f
#              +a1+a2+a3+a4+a5
#            +d4 #Signo inconsistente
#            +d1
#            +d2
#            +factor(d3>0)
#            +d5
           
           # +abs(g3)
           # +abs(g4)
           # +c+d+e+f
           # +a+b+d+e+f
           # a+b+c+d+e+f+g+
           # +corridor_length
           ,data = subset(scores2014))
summary(model0)

data = scores2014

data$g1[data$g1=="-10"] = 1
data$g1[data$g1=="-6"] = 2
data$g1[data$g1=="-3"] = 3
data$g1[data$g1=="-1"] = 4
data$g1[data$g1=="0"] = 5
data$g1 = as.factor(data$g1)

# data$g1= factor(data$g1, levels = c("low","mid-low","mid","mid-high","high"), labels = c("-10","-6","-3","-1","0"))

model_ordinal= polr(formula = g1 ~ a+e+c+d
                    , data = subset(data), Hess = TRUE)

summary(model_ordinal)

data = scores
data1 = merge(subset(data,year==2013),scores2013[c("city","country","system","corridor","g1","g2","g3","g4","g5")],by=c("city","country","system","corridor"),all.x=TRUE)
data2 = merge(subset(data,year==2014),scores2014[c("city","country","system","corridor","g1","g2","g3","g4","g5")],by=c("city","country","system","corridor"),all.x=TRUE)
data = rbind(data1,data2)

data$g1[data$g1=="-10"] = 1; data$g1[data$g1=="-6"] = 2; data$g1[data$g1=="-3"] = 3; data$g1[data$g1=="-1"] = 4; data$g1[data$g1=="0"] = 5
data$g1 = as.factor(data$g1)

data$g2[data$g2=="-5"] = 1; data$g1[data$g2=="0"] = 2
data$g2 = as.factor(data$g2)


# data$g1= factor(data$g1, levels = c("low","mid-low","mid","mid-high","high"), labels = c("-10","-6","-3","-1","0"))

model_ordinal1= polr(formula = g1 ~ a+b+c+d
                     , data = subset(data,year==2013), Hess = TRUE)

summary(model_ordinal1)

subset(scores2014,g1!=0)$country
scores2013$g2
#Intersection Treatment (a4)
mean(subset(scores2014,country=="China")$a4)#China
mean(subset(scores2014,country!="China")$a4) #Rest of the world


#Distance between stations (d1)
mean(subset(scores2014,country=="China")$d1)#China
mean(subset(scores2014,country!="China")$d1) #Rest of the world

#Speeds
table(subset(scores2014,country=="China")$g1)
table(subset(scores2014,country!="China")$g1)

table(subset(scores2013,country=="China")$g1)
table(subset(scores2013,country!="China")$g1)

#Throughput
table(subset(scores2014,country=="China")$g2)
table(subset(scores2014,country!="China")$g2)

#Overcrowding
table(subset(scores2014,country=="China")$g6)
table(subset(scores2014,country!="China")$g6)

#Infrastructure (c)
mean(subset(scores2014,country=="China")$c1)
mean(subset(scores2014,country!="China")$c1)

mean(subset(scores2014,country=="China")$c2)
mean(subset(scores2014,country!="China")$c2)

mean(subset(scores2014,country=="China")$c3)
mean(subset(scores2014,country!="China")$c3) 

# Speed vs Scores ---------------------------------------------------------

# model0= lm(productivity ~
#              total_score+demand
#            ,data = subset(itdp_cities))
# summary(model0)

model0= lm(speed ~ a+b+c+d+e+f+g
           ,data = subset(itdp_cities))
summary(model0)

model0= lm(productivity ~a+b+c+d+e+f+g
           ,data = subset(itdp_cities))
summary(model0)

model0= lm(productivity ~total_score
           ,data = subset(itdp_cities))
summary(model0)

model0= lm(productivity ~b+c+d+f
           ,data = subset(itdp_cities))
summary(model0)
# IDTP 2013 Scores --------------------------------------------------------

#En esta regresión se esperaría que los los estimadores sean iguales a 1.

itdp2013_model= lm(total_score ~  a+b+c+d+e+f
                   # +g
                   # +corridor_length
                   ,data = subset(scores2013))

summary(itdp2013_model)

itdp2013_model1= lm(total_score ~  a+b+c+d+e+corridor_length+factor(country)
                   ,data = subset(scores2013))

summary(itdp2013_model1)

itdp2013_model2= lm(total_score ~ a3+(corridor_length>10)
                    ,data = subset(scores2013))

summary(itdp2013_model2)

itdp2013_model3= lm(total_score ~ a3+a4
                    ,data = subset(scores2013))

summary(itdp2013_model3)

modelWRI_2013 = lm(total_score ~ gdp+bus_split
                                 ,data = subset(scores2013))

summary(modelWRI_2013)

dim(model.frame(modelWRI_2013))

modelWRI_2014 = lm(total_score ~ gdp+bus_split+(country=="China")
                                 ,data = subset(scores2014))

summary(modelWRI_2014)

modelWRI = lm(total_score ~ bus_split+corridor_length+(country=="China")
              # gdp+
              ,data = subset(scores))

summary(modelWRI)

itdp2014_model2= lm(total_score ~ a3+a4
                    ,data = subset(scores2014))

summary(itdp2014_model2)

itdp2014_model2= lm(total_score ~ a3+a4
                    ,data = subset(scores2014))

summary(itdp2014_model2)


View(data.frame(predict(itdp2013_model2,scores2013),scores2013$net_score))
# BRT Data ----------------------------------------------------------------

model1= lm(speed ~  maturity                  #peak_load+
                   # +g
                    +
                   +corridor_length
                   ,data = subset(brt_corridors))

summary(model1)


# 3) Predictions -------------------------------------------------------------

# a) Score Predicted ---------------------------------------------------------

brt_corridors$score_predicted = 0
brt_corridors$score_predicted = as.numeric(predict.lm(itdp_model,brt_corridors))
brt_corridors$diff_prediction = with(brt_corridors,score_predicted-total_score)
brt_corridors$x_diff_prediction = with(brt_corridors,diff_prediction/total_score)

#Classification Predicted
brt_corridors$classification_predicted = NA
brt_corridors$classification_predicted[with(brt_corridors,score_predicted<=55)]="Basic"
brt_corridors$classification_predicted[with(brt_corridors,score_predicted>55 & score_predicted<=69)]="Bronze"
brt_corridors$classification_predicted[with(brt_corridors,score_predicted>69 & score_predicted<=84)]="Silver"
brt_corridors$classification_predicted[with(brt_corridors,score_predicted>84)]="Gold"
brt_corridors$classification_predicted = factor(brt_corridors$classification_predicted, levels = c("Basic","Bronze","Silver","Gold"), labels = c("Basic","Bronze","Silver","Gold"))
#Error porcentual de prediccion menor al 10%
mean(abs(brt_corridors$x_diff_prediction[is.na(brt_corridors$x_diff_prediction)==FALSE]))

dim(subset(brt_corridors,is.na(total_score)==FALSE))
dim(subset(brt_corridors,is.na(score_predicted)==FALSE))

#Validation Sample
dim(subset(brt_corridors,is.na(score_predicted)==FALSE))

#Sample for Predictions (cities not evaluated by ITDP)
View(subset(subset(brt_corridors,is.na(score_predicted)==FALSE & is.na(total_score)==TRUE)))

View(brt_corridors)

#Prediction of the change the total score over the time (using the marginal probabilities given the number of years the corridor will be operating [year_system_commenced])


#Variable dummy para ver si predijo correctamente el grupo de BRT 

brt_corridors$correct_classification = "Correct"
brt_corridors$correct_classification[with(brt_corridors,is.na(classification)==TRUE)] = "No information (ITDP)"
brt_corridors$correct_classification[with(brt_corridors,is.na(classification_predicted)==TRUE)] = "No Prediction" #Ocurre porque los input del modelo no están disponibles en BRTdata
brt_corridors$correct_classification[with(brt_corridors,classification!=classification_predicted & is.na(classification)==FALSE)] = "Incorrect"

save(brt_corridors,file=str_c(project,"/Databases/BRTdata_ITDP_corridors.Rdata"))
write.csv(brt_corridors,file = str_c(project,"/Databases/BRTdata_ITDP_corridors.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")


round(table(brt_corridors$correct_classification)/sum(table(brt_corridors$correct_classification)),2)
round(table(brt_corridors$correct_classification)/sum(table(brt_corridors$correct_classification)),2)

View(brt_corridors)
# b) Marginal Effects -----------------------------------------------------

# predictions = brt_corridors
# predictions = predictions[0,]
# 
# temp_data = model.frame(itdp_model)
# 
# min_temp = min(temp_data$pop_density[is.na(temp_data$pop_density)==FALSE])
# max_temp = max(temp_data$pop_density[is.na(temp_data$pop_density)==FALSE])
# interval_temp = 1000
# 
# temp_data$score_predicted = 0
# temp_data = lapply(temp_data,mean)
# 
# variable = "pop_density"
# i=1
# count = 1
# i=0
# for (i in seq(0,round((max_temp+interval_temp)/interval_temp,1)*interval_temp,1000)){
#   
#   temp_data[[variable]][count] = i
#   temp_data= as.data.frame(rbind(temp_data,lapply(temp_data,mean)))
#   temp_data[["score_predicted"]][count] = predict.lm(itdp_model,temp_data)
#   count = count + 1
# }

# install.packages("visreg")

# http://myweb.uiowa.edu/pbreheny/publications/visreg.pdf
library("visreg")


#Solucionar problema con la observacion 141 que me impide calcular el efecto marginal con visreg

itdp_model1 = lm(total_score ~ 
                   bus_split
                 +log(corridor_length)
                 # +car_split
                # +I(bus_split^2)
#                 +pop_density
#                 +I(pop_density^2)
                +I(log(pop_density))
                +I(metro_length/population)
                +system_maturity
                +I(system_maturity^2)
                +log(years_corridor_operating)
                # +I(years_corridor_operating^2)
                # +I(years_corridor_operating^2)
                # +continent
                # +I(years_corridor_operating^2)
                
                
                ,data = subset(brt_corridors,is.na(system_maturity)==FALSE & is.na(pop_density)==FALSE&years_corridor_operating>0)[c(-140,-165),])

summary(itdp_model1)
dim(model.frame(itdp_model1))
visreg(itdp_model1,"pop_density")
visreg(itdp_model1,"system_maturity")
visreg(itdp_model1,"bus_split")

summary(brt_corridors$pop_density[is.na(brt_corridors$pop_density)==FALSE])
  
  
visreg(itdp_model_final,"corridor_length")
visreg(itdp_model_final,"system_maturity")
visreg(itdp_model_final,"system_maturity")
visreg(itdp_model1,"pop_density")

# - ITDP --------------------------------------------------------------------


# 1) MLR ------------------------------------------------------------------

# Model Estimation --------------------------------------------------------

model_itdp = lm(I((total_score)) ~ 
            # +log(metro_length)
            +I(log(corridor_length))
          #           +corridor_length+I(corridor_length^2)
          +I(log(pop_density))
          #           +pop_density+I(pop_density^2)
          +I((corridors_city_length-corridor_length))
          +I(metro_length)
          
          ,data = subset(scores))

summary(model_itdp)

# Predictions -------------------------------------------------------------

scores$score_predicted = 0
scores$score_predicted = as.numeric(predict.lm(itdp,scores))
scores$diff_prediction = with(scores,score_predicted-total_score)
scores$x_diff_prediction = with(scores,diff_prediction/total_score)

#Error porcentual de prediccion menor al 10%
mean(abs(scores$x_diff_prediction[is.na(scores$x_diff_prediction)==FALSE]))

#Classification Predicted
scores$classification_predicted = NA
scores$classification_predicted[with(scores,score_predicted<=55)]="Basic"
scores$classification_predicted[with(scores,score_predicted>55 & score_predicted<=69)]="Bronze"
scores$classification_predicted[with(scores,score_predicted>69 & score_predicted<=84)]="Silver"
scores$classification_predicted[with(scores,score_predicted>84)]="Gold"
scores$classification_predicted = factor(scores$classification_predicted, levels = c("Basic","Bronze","Silver","Gold"), labels = c("Basic","Bronze","Silver","Gold"))




# 2) Ordered Logit -----------------------------------------------------------

# a) Descriptive Statistics --------------------------------------------------

scores$network_length = with(scores,corridors_city_length-corridor_length)
# table_statistics = table(subset(scores, network_length=0)[c("classification","country")])
# table_statistics = subset(scores, network_length>=0)
table_statistics = subset(scores)
table_statistics$classification = factor(table_statistics$classification, levels = c("1","2","3","4"), labels = c("Basic BRT","Bronze","Silver","Gold"))
table_statistics$china = "Other"
table_statistics$china[table_statistics$country=="China"] = "China"

dim(table_statistics)

margin.table(table_statistics,2)
sweep(scores,1,rowtot,"/")
# data.frame(CrossTable(table_statistics$classification,table_statistics$china
#            ,prop.r=FALSE,prop.c=FALSE,prop.t=FALSE,prop.chisq = FALSE))

#Average network length based on China and class of BRT
xtabs(~ classification + china, table_statistics)

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(table_statistics, row.vars = c("china", "year"), col.vars = "classification",type="j")

crosstab(table_statistics, row.vars = c("china"), col.vars = "classification",type="j")
# b) Model Estimation --------------------------------------------------------
scores$classification = factor(scores$classification, levels = c("Basic BRT","Bronze","Silver","Gold"), labels = c("1","2","3","4"))

#Reducing the number of levels to show the predictive power increase significantly.
scores$classification_gold = 0
scores$classification_gold[as.numeric(scores$classification)==2|as.numeric(scores$classification)==3] = 1
scores$classification_gold[as.numeric(scores$classification)==4] = 2
scores$classification_gold = factor(scores$classification_gold, levels = c("0","1","2"), labels = c("0","1","2"))

scores$network_length = with(scores,corridors_city_length-corridor_length)
scores$network_length[scores$network_length<0] = 0

#Model to make predictions
itdp_ordinal = polr(formula = classification ~ 
                      +log(corridor_length)
                    +log(pop_density)
                    # +pop_density)
                    # +(corridors_city_length-corridor_length))
                    # +metro_length)
                    # +factor(country=="China")
                    +factor(country=="China")*metro_length
                    
                    # +metro_length/corridors_city_length)
                    # +metro_length>0)
                    # +metro_length)
                    
                    # +factor(country=="Brazil")
                    # +factor(country=="Mexico")
                    # +factor(country) 
                    
                    
                    
                    # , data = subset(scores,year==2013 & (corridors_city_length-corridor_length)>0), Hess = TRUE)
                    , data = subset(scores,year==2013 & (network_length)>=0), Hess = TRUE)

summary(itdp_ordinal)
dim(model.frame(itdp_ordinal))


scores$network_length2 = (scores$network_length)^2
itdp_ordinal_all = polr(formula = classification ~ 
                          +log(corridor_length)
                        +(network_length>0)
                        +(metro_length>0)
                        +log(pop_density)
                        +factor(country=="China")*(metro_length>0)
                        +(year==2013)
                        # +factor(continent=="Latin America")
                        # +factor(continent=="Europe")
                        # +gdp
                        # +pop_density)
                        # +(corridors_city_length-corridor_length))
                        
                        # +network_length2
                        # +metro_length)+factor(country=="China")
                        
                        # +(year==2013)*factor(country=="China")
                        # +factor(country=="China")*(network_length>0)
                        # +metro_length/corridors_city_length)
                        # +log(gdp))# 
                        # +factor(country=="Brazil")# +factor(country=="Mexico")# +factor(country) 
                        , data = subset(scores,(network_length)>=0), Hess = TRUE)

dim(subset(scores,(network_length)>=0))
summary(itdp_ordinal_all)
dim(model.frame(itdp_ordinal_all))

itdp_ordinal_final = polr(formula = classification ~ 
                          +log(corridor_length)
                          +network_length
                          +(metro_length)
                          +log(pop_density)
                          +factor(country=="China")
                          +factor(continent=="Latin America")
#                           +factor(continent=="Northern America")
#                           +factor(continent=="Europe")
                          +factor(country=="China")*(metro_length)
                          # +factor(country=="China")
                          # +factor(continent=="Latin America")*metro_length
                          , data = subset(scores,(network_length)>=0), Hess = TRUE,method="logistic")

summary(itdp_ordinal_final)


itdp_ordinal_china = polr(formula = classification ~ 
                            +log(corridor_length)
                          # +network_length)
                          +metro_length
                          +log(pop_density)
                          +(year==2013)
                          , data = subset(scores,country=="China" & network_length>=0), Hess = TRUE)
summary(itdp_ordinal_china)
dim(model.frame(itdp_ordinal_china))

itdp_ordinal_nochina = polr(formula = classification ~ 
                              +log(corridor_length)
                            +network_length
                            +metro_length
                            +log(pop_density)
                            +(year==2013)
                            , data = subset(scores,country!="China" & network_length>=0), Hess = TRUE)

summary(itdp_ordinal_nochina)
dim(model.frame(itdp_ordinal_nochina))

itdp_ordinal_2013 = polr(formula = classification ~ 
                           +log(corridor_length)
                         +network_length
                         +metro_length
                         +log(pop_density)
                         +factor(country=="China")*metro_length
                         , data = subset(scores,year==2013 & network_length>=0), Hess = TRUE)

summary(itdp_ordinal_2013)

itdp_ordinal_2014 = polr(formula = classification ~ 
                           +log(corridor_length)
                         +network_length
                         +metro_length
                         +log(pop_density)
                         +factor(country=="China")*metro_length
                         , data = subset(scores,year==2014 & network_length>=0), Hess = TRUE)

summary(itdp_ordinal_2014)
# Test model --------------------------------------------------------------

pop_quintile = quantile(scores$population[is.na(scores$population)==FALSE],probs = seq(1,10)/10)

itdp_ordinal_highpopulation = polr(formula = classification ~ 
                           +log(corridor_length)
                         +network_length
                         +metro_length
                         +log(pop_density)
#                          +surface
#                          +population
                         +factor(country=="China")*metro_length
                         , data = subset(scores, network_length>=0 & population>=pop_quintile[5]), Hess = TRUE)

summary(itdp_ordinal_highpopulation)

itdp_ordinal_lowpopulation = polr(formula = classification ~ 
                                    +log(corridor_length)
                                  +network_length
                                  +metro_length
                                  +log(pop_density)
                                  +factor(country=="China")*metro_length
                                
                                  , data = subset(scores, network_length>=0 & population<pop_quintile[6]), Hess = TRUE)

summary(itdp_ordinal_lowpopulation)

scores$surface = with(scores,population/pop_density)

itdp_ordinal_population = polr(formula = classification ~ 
                                  +log(corridor_length)
                                  +I((network_length/(surface)))
                                  +I((metro_length/(surface)))
                                  +log(pop_density)
                                # *I(metro_length/(surface))
                                  +factor(country=="China")
                                  , data = subset(scores, network_length>=0), Hess = TRUE)

summary(itdp_ordinal_population)

density_quintile = quantile(scores$pop_density[is.na(scores$pop_density)==FALSE],probs = seq(1,3)/3)


scores$high_densit

itdp_ordinal_density = polr(formula = classification ~ 
                                 +log(corridor_length)
                               +I((network_length/(surface)))
                               +I((metro_length/(surface)))
                               +log(pop_density)
                               # *I(metro_length/(surface))
                               +factor(country=="China")
                               
                               , data = subset(scores, network_length>=0), Hess = TRUE)

summary(itdp_ordinal_population)

itdp_ordinal_test = polr(formula = classification ~ 
                            +log(corridor_length)
                          +network_length
                          +(metro_length)
                          +log(pop_density)
                          +factor(country=="China")
                          +factor(continent=="Latin America")
                          #                           +factor(continent=="Northern America")
                          #                           +factor(continent=="Europe")
                          +factor(country=="China")*(metro_length)
                          # +factor(country=="China")
                          # +factor(continent=="Latin America")*metro_length
                          , data = subset(scores,(network_length)>=0), Hess = TRUE,method="logistic")

summary(itdp_ordinal_test)

# 2013 --------------------------------------------------------------------


data2013 = scores2013

data2013$classification = as.factor(data2013$classification)
data2013$classification = factor(data2013$classification, levels = c("Basic","Bronze","Silver","Gold"), labels = c("Basic","Bronze","Silver","Gold"))

#f2 : Integration with other public transport
data2013$f2= factor(data2013$f2, levels = c("0","1","2","3"), labels = c("Null","Low","Mid","High"))


itdp_ordinal_scores2013 = polr(formula = f2 ~ 
                                     +corridor_length
#                                    +network_length
                                     +(metro_length>0)
                                     +pop_density
#                                    #                          +surface
#                                    #                          +population
#                                    +factor(country=="China")*metro_length
                                   , data = subset(data2013), Hess = TRUE)

summary(itdp_ordinal_scores2013)

# c) Marginal Effects --------------------------------------------------------
# - Pop Density---------------------------------------------------------

temp_data = expand.model.frame(itdp_ordinal_final,~corridor_length+pop_density)

no_vars = c("factor(country == \"China\")","pop_density")
temp_list = colnames(model.frame(temp_data))

#Means of the variables
temp_prediction = lapply(temp_data[,-which(temp_list %in% no_vars)],mean)


newdat <- data.frame(
  temp_prediction,
  country = rep(0:1, each = 200),
  pop_density = rep(seq(from = min(temp_data$pop_density), to = max(temp_data$pop_density), length.out = 100), 2))


newdat$country[newdat$country==0] = "Other countries"
newdat$country[newdat$country==1] = "China"

newdat <- cbind(newdat, predict(itdp_ordinal_final, newdat, type = "probs"))

lnewdat <- reshape2::melt(newdat, id.vars = colnames(newdat[,-which(colnames(newdat) %in% c("1","2","3","4"))]),
                variable_name = "Level", value_name="Probability")

lnewdat$Level = factor(lnewdat$Level, levels = c("1","2","3","4"), labels = c("Basic BRT","Bronze","Silver","Gold"))


ggplot(lnewdat, aes(x = pop_density, y = value, colour = Level)) +
  geom_line() + facet_grid(. ~ country, labeller="label_both")+theme3

ggsave('figures/BRT_Density.jpg', width=30, height=20, unit="cm", dpi=300)
# - Corridor Length---------------------------------------------------------

temp_data = expand.model.frame(itdp_ordinal_final,~corridor_length+pop_density)

no_vars = c("factor(country == \"China\")","corridor_length")
temp_list = colnames(model.frame(temp_data))

#Means of the variables
temp_prediction = lapply(temp_data[,-which(temp_list%in% no_vars)],mean)


newdat <- data.frame(
  temp_prediction,
  country = rep(0:1, each = 200),
  corridor_length = rep(seq(from = min(temp_data$corridor_length), to = max(temp_data$corridor_length), length.out = 100), 2))


newdat$country[newdat$country==0] = "Other countries"
newdat$country[newdat$country==1] = "China"

newdat <- cbind(newdat, predict(itdp_ordinal_final, newdat, type = "probs"))

lnewdat <- melt(newdat, id.vars = colnames(newdat[,-which(colnames(newdat) %in% c("1","2","3","4"))]),
                variable_name = "Level", value_name="Probability")

lnewdat$Level = factor(lnewdat$Level, levels = c("1","2","3","4"), labels = c("Basic BRT","Bronze","Silver","Gold"))


ggplot(lnewdat, aes(x = corridor_length, y = value, colour = Level)) +
  geom_line() + facet_grid(. ~ country, labeller="label_both")+theme3

ggsave(str_c(project,"/Graphics/BRT_Corridor_Length.jpg"), width=30, height=20, unit="cm", dpi=300)
# - Metro Length ------------------------------------------------------------
temp_data = expand.model.frame(itdp_ordinal_final,~corridor_length+pop_density)

no_vars = c("factor(country == \"China\")","metro_length")
temp_list = colnames(model.frame(temp_data))

#Means of the variables
temp_prediction = lapply(temp_data[,-which(temp_list%in% no_vars)],mean)


newdat <- data.frame(
  temp_prediction,
  country = rep(0:1, each = 200),
  metro_length = rep(seq(from = min(temp_data$metro_length), to = max(temp_data$metro_length), length.out = 100), 2))


newdat$country[newdat$country==0] = "Other countries"
newdat$country[newdat$country==1] = "China"

newdat <- cbind(newdat, predict(itdp_ordinal_final, newdat, type = "probs"))

lnewdat <- melt(newdat, id.vars = colnames(newdat[,-which(colnames(newdat) %in% c("1","2","3","4"))]),
                variable_name = "Level", value_name="Probability")

lnewdat$Level = factor(lnewdat$Level, levels = c("1","2","3","4"), labels = c("Basic BRT","Bronze","Silver","Gold"))


ggplot(lnewdat, aes(x = metro_length, y = value, colour = Level)) +
  geom_line() + facet_grid(. ~ country, labeller="label_both")+theme3

ggsave(str_c(project,"/Graphics/BRT_Metro_Network.jpg"), width=30, height=20, unit="cm", dpi=300)
# - Bus Network Length ------------------------------------------------------------
temp_data = expand.model.frame(itdp_ordinal_final,~corridor_length+pop_density)

no_vars = c("factor(country == \"China\")","network_length")
temp_list = colnames(model.frame(temp_data))

#Means of the variables
temp_prediction = lapply(temp_data[,-which(temp_list%in% no_vars)],mean)


newdat <- data.frame(
  temp_prediction,
  country = rep(0:1, each = 200),
  network_length = rep(seq(from = min(temp_data$network_length), to = max(temp_data$network_length), length.out = 100), 2))


newdat$country[newdat$country==0] = "Other countries"
newdat$country[newdat$country==1] = "China"

newdat <- cbind(newdat, predict(itdp_ordinal_final, newdat, type = "probs"))

lnewdat <- melt(newdat, id.vars = colnames(newdat[,-which(colnames(newdat) %in% c("1","2","3","4"))]),
                variable_name = "Level", value_name="Probability")

lnewdat$Level = factor(lnewdat$Level, levels = c("1","2","3","4"), labels = c("Basic BRT","Bronze","Silver","Gold"))


ggplot(lnewdat, aes(x = network_length, y = value, colour = Level)) +
  geom_line() + facet_grid(. ~ country, labeller="label_both")+theme3

ggsave(str_c(project,"/Graphics/BRT_Bus_Network.jpg"), width=30, height=20, unit="cm", dpi=300)
# d) Predictions -------------------------------------------------------------

#Predictions

model=itdp_ordinal_population
scores_prediction = subset(scores,year==2014)

scores_prediction$classification_predicted = predict(model,scores_prediction, type = "class")

scores_prediction$correct_classification = "Correct"
scores_prediction$correct_classification[with(scores_prediction,is.na(classification)==TRUE)] = "No information (ITDP)"
scores_prediction$correct_classification[with(scores_prediction,is.na(classification_predicted)==TRUE)] = "No Prediction" #Ocurre porque los input del modelo no están disponibles en BRTdata
scores_prediction$correct_classification[with(scores_prediction,classification!=classification_predicted & is.na(classification)==FALSE)] = "Incorrect"

table(scores_prediction$correct_classification)/sum(table(scores_prediction$correct_classification))

temp = abs(with(subset(scores_prediction,correct_classification=="Incorrect"),as.numeric(classification_predicted)-as.numeric(classification)))

mean(temp[is.na(temp)==FALSE])



# View(scores_prediction)
save(brt_corridors,file=str_c(project,"/Databases/BRTdata_ITDP_corridors.Rdata"))
write.csv(brt_corridors,file = str_c(project,"/Databases/BRTdata_ITDP_corridors.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

# ANOVA DIFFERENCES -------------------------------------------------------


anova_total_score = lm(I((total_score)) ~ factor(country=="China")
          ,data = subset(scores))

anova(anova_total_score)$P[1]
coefficients(summary(anova_total_score))

# Speed vs Productivity -----------------------------------------------

itdp = lm(I((speed)) ~ 
            # +log(metro_length)
            # +I(total_score)
          #           +corridor_length+I(corridor_length^2)
          +I(productivity)
          #           +pop_density+I(pop_density^2)
#           +I((corridors_city_length-corridor_length))
#           +I(metro_length)
          
          ,data = subset(scores))

summary(itdp)



# 1) Productivity vs Scores --------------------------------------------------

# SLR models -------------------------------------------------------------

productivity_design_model = lm(productivity ~ net_score 
                                 # net_score*(country=="China")
                              , data = subset(scores)
                              
                              )

summary(productivity_design_model)
dim(model.frame(productivity_design_model))[1]


productivity_design_2013_model = lm(productivity ~ net_score
                              , data = subset(scores,year=="2013")
                              
)

summary(productivity_design_2013_model)

dim(model.frame(productivity_design_2013_model))[1]

productivity_design_2014_model = lm(productivity ~ net_score
                                    , data = subset(scores,year=="2014")
                                    
)

summary(productivity_design_2014_model)

dim(model.frame(productivity_design_2014_model))[1]







# MLR Models --------------------------------------------------------------

productivity_categories_model= lm(productivity~  
                                        b+I(c+d)+f
                                      # +e da incorrectamente con signo negativo
                                      # +g da incorrectamente con signo positivo
                                      ,data = subset(scores))
# ,data = subset(brt_corridors,system_maturity>0 & is.na(system_maturity)==FALSE))

summary(productivity_categories_model)

productivity_categories_all_model= lm(productivity~  
                               a+b+c+d+e+f+g
                             # +e da incorrectamente con signo negativo
                             # +g da incorrectamente con signo positivo
                             ,data = subset(scores))
# ,data = subset(brt_corridors,system_maturity>0 & is.na(system_maturity)==FALSE))

summary(productivity_categories_all_model)
car::vif(productivity_categories_all_model)

#2013
productivity_categories_2013_model= lm(productivity~  
                                         a+b+c+d+e+f+g
                                       ,data = subset(scores,year=="2013"))


summary(productivity_categories_2013_model)

#2014
productivity_categories_2014_model= lm(productivity~  
                                         a+b+c+d+e+f+g
                                       ,data = subset(scores,year=="2014"))


summary(productivity_categories_2014_model)


# 2) Productivity vs Scores and Interaction China


# SLR Models --------------------------------------------------------------

productivity_design_china_model = lm(productivity ~ net_score*(country=="China")
                               , data = subset(scores)
                               
)

summary(productivity_design_china_model)
dim(model.frame(productivity_design_model))[1]

# MLR Models --------------------------------------------------------------

productivity_china_model= lm(productivity~  
                               a+b+c+d+e+f+g
                             # +e da incorrectamente con signo negativo
                             # +g da incorrectamente con signo positivo
                             ,data = subset(scores,country=="China"))
# ,data = subset(brt_corridors,system_maturity>0 & is.na(system_maturity)==FALSE))

summary(productivity_china_model)


#2013
productivity_categories_2013_model= lm(productivity~  
                                         a+b+c+d+e+f+g
                                       # +e da incorrectamente con signo negativo
                                       # +g da incorrectamente con signo positivo
                                       +(country=="China")
                                       ,data = subset(scores,year=="2013"))


summary(productivity_categories_2013_model)
#All
productivity_interaction_china_model= lm(productivity~  
                                           a+b+c+d+f
                                         # +e da incorrectamente con signo negativo
                                         # +g da incorrectamente con signo positivo
                                         +(country=="China")
                                         ,data = subset(scores))
# ,data = subset(brt_corridors,system_maturity>0 & is.na(system_maturity)==FALSE))

summary(productivity_interaction_china_model)

# Demand vs Scores --------------------------------------------------------


itdp_demand_model= lm(demand~  
                 a+b+d+f
               # +c,e da incorrectamente con signo negativo
               # +g da incorrectamente con signo positivo
               
               ,data = subset(scores,country=="China"))
# ,data = subset(brt_corridors,system_maturity>0 & is.na(system_maturity)==FALSE))

summary(itdp_demand_model)


dim(model.frame(itdp_demand_model))









######################################################################################################
#################### MODELS THREDBO 2017 #############################################################
######################################################################################################
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




