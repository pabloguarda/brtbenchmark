
# Thredbo 2017 ------------------------------------------------------------


# a) Category Level ----------------------------------------------------------

m1 = mlr_speed_allCategories
m2 = ol5_speed_allCategories
m3 = ol3_speed_allCategories
m4 = bl_speed_allCategories


b = stargazer(m1,m2,m3,m4
              # ,m4,m5,m6 
              # omit.stat=c("f","theta")
              ,title="Regression coefficients"
              # ,model.names=FALSE,model.numbers=TRUE
              ,coef = list(summary(m1)$coef[c(2:dim(summary(m1)$coef)[1],1),"Estimate"]
                           ,summary(m2)$coef[c(2:dim(summary(m2)$coef)[1],1),"Value"]
                           ,summary(m3)$coef[c(2:dim(summary(m3)$coef)[1],1),"Value"]
                           ,summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"Estimate"]
                           
                           # ,summary(m5)$coef[c(2:dim(summary(m5)$coef)[1],1),"Value"]
                           # ,summary(m6)$coef[c(2:dim(summary(m6)$coef)[1],1),"Value"]
                           
              )
              # ,apply.coef=multiply.by.100
              # , object.names = TRUE,model.numbers = FALSE
              # ,model.names=c("OL5 model","OL3 model","BL Model", "MLR Model")
              ,se = list(round(summary(m1)$coef[c(2:dim(summary(m1)$coef)[1],1),"t value"],1)
                         ,round(summary(m2)$coef[c(2:dim(summary(m2)$coef)[1],1),"t value"],1)
                         ,round(summary(m3)$coef[c(2:dim(summary(m3)$coef)[1],1),"t value"],1)
                         ,round(summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"z value"],1)
                         # ,round(summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"t value"],1)
                         # ,round(summary(m5)$coef[c(2:dim(summary(m5)$coef)[1],1),"t value"],1)
                         # ,round(summary(m6)$coef[c(2:dim(summary(m6)$coef)[1],1),"t value"],1)
              )
              #Agrego los radios entre los parámetros manualmente, en el futuro automatizarlo
              ,add.lines = list(c(round(AIC(m1),2),"",round(AIC(m2),2),round(AIC(m3),2),round(AIC(m4),2)
                                  # ,round(AIC(m4),2),round(AIC(m5),2),round(AIC(m6),2)
                                  )
                                ,c("Log-likelihood",round(as.numeric(logLik(m1)),2),round(as.numeric(logLik(m2)),2),round(as.numeric(logLik(m3)),2),round(as.numeric(logLik(m4)),2)
                                   # ,round(as.numeric(logLik(m4)),2),round(as.numeric(logLik(m5)),2),round(as.numeric(logLik(m6)),2)))
                                ))
              #                             ,add.lines=list(as.numeric(summary(fixef(OTPwaiting_model_lmpanel, effect = "individual"))[,"Estimate"])
              #                                             ,names(summary(fixef(OTPwaiting_model_lmpanel, effect = "individual"))[,"Estimate"]))
              ,column.sep.width = "0pt"
              
              ,single.row = TRUE #Test t van en la misma línea del estimador
              # ,covariate.labels=c("Log(corridor length)","Bus Network Length","Metro Network Length","Log(Population Density)","China","2013","China x Metro")
              # ,dep.var.caption = c("Percentage of passengers waiting shorter than the scheduled headway")
              # ,dep.var.labels = ""
              ,dep.var.labels.include = FALSE
              ,notes.align=c("l"),notes.label=c(""),notes.append=FALSE
              # ,font.size ="tiny"
              # ,align=TRUE
              ,no.space =TRUE, report = "vc*s"
              ,ord.intercepts = TRUE
              ,digits = 3
              # ,digits.extra=3
              ,type="html"
              # ,type = "latex"
              # ,keep.stat = "all"
              ,style="default"
              # ,summary=
              ,out="tables/speedBRT_full_categories.doc"
              
)




# b) Subcategory Level -------------------------------------------------------

# - Full models -----------------------------------------------------------

m1 = mlr_speed_full_subcategories
m2 = ol5_speed_full_subcategories
m3 = ol3_speed_full_subcategories
m4 = bl_speed_full_subcategories


b = stargazer(m1,m2,m3,m4
              # ,m4,m5,m6 
              # omit.stat=c("f","theta")
              ,title="Regression coefficients"
              # ,model.names=FALSE,model.numbers=TRUE
              ,coef = list(summary(m1)$coef[c(2:dim(summary(m1)$coef)[1],1),"Estimate"]
                           ,summary(m2)$coef[c(2:dim(summary(m2)$coef)[1],1),"Value"]
                           ,summary(m3)$coef[c(2:dim(summary(m3)$coef)[1],1),"Value"]
                           ,summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"Estimate"]
                           
                           # ,summary(m5)$coef[c(2:dim(summary(m5)$coef)[1],1),"Value"]
                           # ,summary(m6)$coef[c(2:dim(summary(m6)$coef)[1],1),"Value"]
                           
              )
              # ,apply.coef=multiply.by.100
              # , object.names = TRUE,model.numbers = FALSE
              # ,model.names=c("OL5 model","OL3 model","BL Model", "MLR Model")
              ,se = list(round(summary(m1)$coef[c(2:dim(summary(m1)$coef)[1],1),"t value"],1)
                         ,round(summary(m2)$coef[c(2:dim(summary(m2)$coef)[1],1),"t value"],1)
                         ,round(summary(m3)$coef[c(2:dim(summary(m3)$coef)[1],1),"t value"],1)
                         ,round(summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"z value"],1)
                         # ,round(summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"t value"],1)
                         # ,round(summary(m5)$coef[c(2:dim(summary(m5)$coef)[1],1),"t value"],1)
                         # ,round(summary(m6)$coef[c(2:dim(summary(m6)$coef)[1],1),"t value"],1)
              )
              #Agrego los radios entre los parámetros manualmente, en el futuro automatizarlo
              ,add.lines = list(c(round(AIC(m1),2),"",round(AIC(m2),2),round(AIC(m3),2),round(AIC(m4),2)
                                  # ,round(AIC(m4),2),round(AIC(m5),2),round(AIC(m6),2)
              )
              ,c("Log-likelihood",round(as.numeric(logLik(m1)),2),round(as.numeric(logLik(m2)),2),round(as.numeric(logLik(m3)),2),round(as.numeric(logLik(m4)),2)
                 # ,round(as.numeric(logLik(m4)),2),round(as.numeric(logLik(m5)),2),round(as.numeric(logLik(m6)),2)))
              ))
              #                             ,add.lines=list(as.numeric(summary(fixef(OTPwaiting_model_lmpanel, effect = "individual"))[,"Estimate"])
              #                                             ,names(summary(fixef(OTPwaiting_model_lmpanel, effect = "individual"))[,"Estimate"]))
              ,column.sep.width = "0pt"
              
              ,single.row = TRUE #Test t van en la misma línea del estimador
              # ,covariate.labels=c("Log(corridor length)","Bus Network Length","Metro Network Length","Log(Population Density)","China","2013","China x Metro")
              # ,dep.var.caption = c("Percentage of passengers waiting shorter than the scheduled headway")
              # ,dep.var.labels = ""
              ,dep.var.labels.include = FALSE
              ,notes.align=c("l"),notes.label=c(""),notes.append=FALSE
              # ,font.size ="tiny"
              # ,align=TRUE
              ,no.space =TRUE, report = "vc*s"
              ,ord.intercepts = TRUE
              ,digits = 3
              # ,digits.extra=3
              ,type="html"
              # ,type = "latex"
              # ,keep.stat = "all"
              ,style="default"
              # ,summary=
              ,out= "tables/speedBRT_full_subcategories.doc"
              
)



# - Parsimonious Models ---------------------------------------------------

m1 = mlr_speed_parsimonious_subcategories
m2 = ol5_speed_parsimonious_subcategories
m3 = ol3_speed_parsimonious_subcategories
m4 = bl_speed_parsimonious_subcategories



b = stargazer(m1,m2,m3,m4
              # ,m4,m5,m6 
              # omit.stat=c("f","theta")
              ,title="Regression coefficients"
              # ,model.names=FALSE,model.numbers=TRUE
              ,coef = list(summary(m1)$coef[c(2:dim(summary(m1)$coef)[1],1),"Estimate"]
                           ,summary(m2)$coef[c(2:dim(summary(m2)$coef)[1],1),"Value"]
                           ,summary(m3)$coef[c(2:dim(summary(m3)$coef)[1],1),"Value"]
                           ,summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"Estimate"]
                           
                           # ,summary(m5)$coef[c(2:dim(summary(m5)$coef)[1],1),"Value"]
                           # ,summary(m6)$coef[c(2:dim(summary(m6)$coef)[1],1),"Value"]
                           
              )
              # ,apply.coef=multiply.by.100
              # , object.names = TRUE,model.numbers = FALSE
              # ,model.names=c("OL5 model","OL3 model","BL Model", "MLR Model")
              ,se = list(round(summary(m1)$coef[c(2:dim(summary(m1)$coef)[1],1),"t value"],1)
                         ,round(summary(m2)$coef[c(2:dim(summary(m2)$coef)[1],1),"t value"],1)
                         ,round(summary(m3)$coef[c(2:dim(summary(m3)$coef)[1],1),"t value"],1)
                         ,round(summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"z value"],1)
                         # ,round(summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"t value"],1)
                         # ,round(summary(m5)$coef[c(2:dim(summary(m5)$coef)[1],1),"t value"],1)
                         # ,round(summary(m6)$coef[c(2:dim(summary(m6)$coef)[1],1),"t value"],1)
              )
              #Agrego los radios entre los parámetros manualmente, en el futuro automatizarlo
              ,add.lines = list(c(round(AIC(m1),2),"",round(AIC(m2),2),round(AIC(m3),2),round(AIC(m4),2)
                                  # ,round(AIC(m4),2),round(AIC(m5),2),round(AIC(m6),2)
              )
              ,c("Log-likelihood",round(as.numeric(logLik(m1)),2),round(as.numeric(logLik(m2)),2),round(as.numeric(logLik(m3)),2),round(as.numeric(logLik(m4)),2)
                 # ,round(as.numeric(logLik(m4)),2),round(as.numeric(logLik(m5)),2),round(as.numeric(logLik(m6)),2)))
              ))
              #                             ,add.lines=list(as.numeric(summary(fixef(OTPwaiting_model_lmpanel, effect = "individual"))[,"Estimate"])
              #                                             ,names(summary(fixef(OTPwaiting_model_lmpanel, effect = "individual"))[,"Estimate"]))
              ,column.sep.width = "0pt"
              
              ,single.row = TRUE #Test t van en la misma línea del estimador
              # ,covariate.labels=c("Log(corridor length)","Bus Network Length","Metro Network Length","Log(Population Density)","China","2013","China x Metro")
              # ,dep.var.caption = c("Percentage of passengers waiting shorter than the scheduled headway")
              # ,dep.var.labels = ""
              ,dep.var.labels.include = FALSE
              ,notes.align=c("l"),notes.label=c(""),notes.append=FALSE
              # ,font.size ="tiny"
              # ,align=TRUE
              ,no.space =TRUE, report = "vc*s"
              ,ord.intercepts = TRUE
              ,digits = 3
              # ,digits.extra=3
              ,type="html"
              # ,type = "latex"
              # ,keep.stat = "all"
              ,style="default"
              # ,summary=
              ,out="tables/speedBRT_parsimonious_subcategories.doc"
              
)






# MLR ---------------------------------------------------------------------


# OL (5 Levels) -----------------------------------------------------

m1 = itdp_ordinal_2013
m2 = itdp_ordinal_2014
m3 = itdp_ordinal_nochina
m4 = itdp_ordinal_china
m5 = itdp_ordinal_all
m6 = itdp_ordinal_final
summary(m1)

AIC(m2)
# variables.names(m1)
# b = stargazer(m1,m2,m3, omit.stat=c("f")

b = stargazer(m1,m2,m3,m4,m5,m6 
              # omit.stat=c("f","theta")
              ,title="Regression coefficients for fixed effects models compared with the pooled OLS model coefficients"
              # ,model.names=FALSE,model.numbers=TRUE
              ,coef = list(summary(m1)$coef[c(2:dim(summary(m1)$coef)[1],1),"Value"]
                           ,summary(m2)$coef[c(2:dim(summary(m2)$coef)[1],1),"Value"]
                           ,summary(m3)$coef[c(2:dim(summary(m3)$coef)[1],1),"Value"]
                           ,summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"Value"]
                           ,summary(m5)$coef[c(2:dim(summary(m5)$coef)[1],1),"Value"]
                           ,summary(m6)$coef[c(2:dim(summary(m6)$coef)[1],1),"Value"]
                           
              )
              # ,apply.coef=multiply.by.100
              ,model.names=FALSE, object.names = TRUE,model.numbers = FALSE
              # ,model.names=c("OLS model","Fixed Effects Model (Metro Line Effects)"
              # ,"Fixed Effects Model (Time Effects)", "Fixed Effects Model (Twoways)")
              ,se = list(round(summary(m1)$coef[c(2:dim(summary(m1)$coef)[1],1),"t value"],1)
                         ,round(summary(m2)$coef[c(2:dim(summary(m2)$coef)[1],1),"t value"],1)
                         ,round(summary(m3)$coef[c(2:dim(summary(m3)$coef)[1],1),"t value"],1)
                         ,round(summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"t value"],1)
                         ,round(summary(m5)$coef[c(2:dim(summary(m5)$coef)[1],1),"t value"],1)
                         ,round(summary(m6)$coef[c(2:dim(summary(m6)$coef)[1],1),"t value"],1)
              )
              #Agrego los radios entre los parámetros manualmente, en el futuro automatizarlo
              ,add.lines = list(c("AIC",round(AIC(m1),2),round(AIC(m2),2),round(AIC(m3),2),round(AIC(m4),2),round(AIC(m5),2),round(AIC(m6),2))
                                ,c("Log-likelihood",round(as.numeric(logLik(m1)),2),round(as.numeric(logLik(m2)),2),round(as.numeric(logLik(m3)),2),round(as.numeric(logLik(m4)),2),round(as.numeric(logLik(m5)),2),round(as.numeric(logLik(m6)),2)))
              #                             ,add.lines=list(as.numeric(summary(fixef(OTPwaiting_model_lmpanel, effect = "individual"))[,"Estimate"])
              #                                             ,names(summary(fixef(OTPwaiting_model_lmpanel, effect = "individual"))[,"Estimate"]))
              ,column.sep.width = "0pt"
              
              ,single.row = TRUE #Test t van en la misma línea del estimador
              # ,covariate.labels=c("Log(corridor length)","Bus Network Length","Metro Network Length","Log(Population Density)","China","2013","China x Metro")
              # ,dep.var.caption = c("Percentage of passengers waiting shorter than the scheduled headway")
              # ,dep.var.labels = ""
              ,dep.var.labels.include = FALSE
              ,notes.align=c("l"),notes.label=c(""),notes.append=FALSE
              # ,font.size ="tiny"
              # ,align=TRUE
              ,no.space =TRUE, report = "vc*s"
              ,ord.intercepts = TRUE
              ,digits = 3
              # ,digits.extra=3
              ,type="html"
              # ,type = "latex"
              # ,keep.stat = "all"
              ,style="default"
              # ,summary=
              ,out= "tables/BRTChina_Ordered.doc"
              
)












# Regression models BRT China ---------------------------------------------------------------


m1 = itdp_ordinal_2013
m2 = itdp_ordinal_2014
m3 = itdp_ordinal_nochina
m4 = itdp_ordinal_china
m5 = itdp_ordinal_all
m6 = itdp_ordinal_final
summary(m1)

AIC(m2)
# variables.names(m1)
# b = stargazer(m1,m2,m3, omit.stat=c("f")

b = stargazer(m1,m2,m3,m4,m5,m6 
              # omit.stat=c("f","theta")
              ,title="Regression coefficients for fixed effects models compared with the pooled OLS model coefficients"
              # ,model.names=FALSE,model.numbers=TRUE
              ,coef = list(summary(m1)$coef[c(2:dim(summary(m1)$coef)[1],1),"Value"]
                           ,summary(m2)$coef[c(2:dim(summary(m2)$coef)[1],1),"Value"]
                           ,summary(m3)$coef[c(2:dim(summary(m3)$coef)[1],1),"Value"]
                           ,summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"Value"]
                           ,summary(m5)$coef[c(2:dim(summary(m5)$coef)[1],1),"Value"]
                           ,summary(m6)$coef[c(2:dim(summary(m6)$coef)[1],1),"Value"]
                           
              )
              # ,apply.coef=multiply.by.100
              ,model.names=FALSE, object.names = TRUE,model.numbers = FALSE
              # ,model.names=c("OLS model","Fixed Effects Model (Metro Line Effects)"
              # ,"Fixed Effects Model (Time Effects)", "Fixed Effects Model (Twoways)")
              ,se = list(round(summary(m1)$coef[c(2:dim(summary(m1)$coef)[1],1),"t value"],1)
                         ,round(summary(m2)$coef[c(2:dim(summary(m2)$coef)[1],1),"t value"],1)
                         ,round(summary(m3)$coef[c(2:dim(summary(m3)$coef)[1],1),"t value"],1)
                         ,round(summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"t value"],1)
                         ,round(summary(m5)$coef[c(2:dim(summary(m5)$coef)[1],1),"t value"],1)
                         ,round(summary(m6)$coef[c(2:dim(summary(m6)$coef)[1],1),"t value"],1)
              )
              #Agrego los radios entre los parámetros manualmente, en el futuro automatizarlo
              ,add.lines = list(c("AIC",round(AIC(m1),2),round(AIC(m2),2),round(AIC(m3),2),round(AIC(m4),2),round(AIC(m5),2),round(AIC(m6),2))
              ,c("Log-likelihood",round(as.numeric(logLik(m1)),2),round(as.numeric(logLik(m2)),2),round(as.numeric(logLik(m3)),2),round(as.numeric(logLik(m4)),2),round(as.numeric(logLik(m5)),2),round(as.numeric(logLik(m6)),2)))
#                             ,add.lines=list(as.numeric(summary(fixef(OTPwaiting_model_lmpanel, effect = "individual"))[,"Estimate"])
#                                             ,names(summary(fixef(OTPwaiting_model_lmpanel, effect = "individual"))[,"Estimate"]))
              ,column.sep.width = "0pt"
              
              ,single.row = TRUE #Test t van en la misma línea del estimador
              # ,covariate.labels=c("Log(corridor length)","Bus Network Length","Metro Network Length","Log(Population Density)","China","2013","China x Metro")
              # ,dep.var.caption = c("Percentage of passengers waiting shorter than the scheduled headway")
              # ,dep.var.labels = ""
              ,dep.var.labels.include = FALSE
              ,notes.align=c("l"),notes.label=c(""),notes.append=FALSE
              # ,font.size ="tiny"
              # ,align=TRUE
              ,no.space =TRUE, report = "vc*s"
              ,ord.intercepts = TRUE
              ,digits = 3
              # ,digits.extra=3
              ,type="html"
              # ,type = "latex"
              # ,keep.stat = "all"
              ,style="default"
              # ,summary=
              ,out="tables/BRTChina_Ordered.doc"
              
)


# Regression Results ------------------------------------------------------

#Export Results in Latex 

# 1: China 2: No China 3:All


#Models include only one explanatory variable for the score of the ITDP standard

m1 = mlr_itdp_china
m2 = mlr_itdp_nochina
m3 = mlr_itdp_all  

#Models include one explanatory variable for each dimension of the ITDP standard
m4 = itdp_model_china
m5 = itdp_model_nochina
m6 = itdp_model_all

# multiply.by.100 <- function(x) (x * 100) #cambio de unidades a cientos

# Between OLS
b = stargazer(m1,m2,m3,m4,m5,m6, omit.stat=c("f","theta")
              ,title="Regression coefficients for fixed effects models compared with the pooled OLS model coefficients"
              # ,model.names=FALSE,model.numbers=TRUE
              ,coef = list(summary(m1)$coef[c(2:dim(summary(m1)$coef)[1],1),"Estimate"]
                           ,summary(m2)$coef[c(2:dim(summary(m2)$coef)[1],1),"Estimate"]
                           ,summary(m3)$coef[c(2:dim(summary(m3)$coef)[1],1),"Estimate"]
                           ,summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"Estimate"]
                           ,summary(m5)$coef[c(2:dim(summary(m5)$coef)[1],1),"Estimate"]
                           ,summary(m6)$coef[c(2:dim(summary(m6)$coef)[1],1),"Estimate"]
              )
              # ,apply.coef=multiply.by.100
              ,model.names=FALSE, object.names = TRUE,model.numbers = FALSE
              # ,model.names=c("OLS model","Fixed Effects Model (Metro Line Effects)"
              # ,"Fixed Effects Model (Time Effects)", "Fixed Effects Model (Twoways)")
              ,se = list(round(summary(m1)$coef[c(2:dim(summary(m1)$coef)[1],1),"t value"],1)
                         ,round(summary(m2)$coef[c(2:dim(summary(m2)$coef)[1],1),"t value"],1)
                         ,round(summary(m3)$coef[c(2:dim(summary(m3)$coef)[1],1),"t value"],1)
                         ,round(summary(m4)$coef[c(2:dim(summary(m4)$coef)[1],1),"t value"],1)
                         ,round(summary(m5)$coef[c(2:dim(summary(m5)$coef)[1],1),"t value"],1)
                         ,round(summary(m6)$coef[c(2:dim(summary(m6)$coef)[1],1),"t value"],1)
              )
              #Agrego los radios entre los parámetros manualmente, en el futuro automatizarlo
              # ,add.lines = list(c("Ratio","-37.5","-8.75","11.86","-1.04"))
              #               ,add.lines=list(as.numeric(summary(fixef(OTPwaiting_model_lmpanel, effect = "individual"))[,"Estimate"])
              #                               ,names(summary(fixef(OTPwaiting_model_lmpanel, effect = "individual"))[,"Estimate"]))
              ,column.sep.width = "0pt"
              
              ,single.row = TRUE #Test t van en la misma línea del estimador
              #               ,covariate.labels=c("Trips","Lost Trips","Temperature (Fº)","Precipitation (%)","Constant")
              #               ,dep.var.caption = c("Percentage of passengers waiting less than the scheduled headway")
              # ,dep.var.labels = ""
              ,dep.var.labels.include = FALSE
              ,notes.align=c("l"),notes.label=c(""),notes.append=FALSE
              # ,font.size ="tiny"
              # ,align=TRUE
              ,no.space =TRUE, report = "vcs"
              ,digits = 1
              # ,digits.extra=3
              ,type = "html",style="default"
              ,out="/tables/Productivity_Scores_ITDP.doc"
              
)
