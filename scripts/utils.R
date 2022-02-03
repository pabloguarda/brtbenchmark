# FUNCTIONS ---------------------------------------------------------------

# 1) DATA READING ------------------------------------------------------------

# Los datos de excel deben ser guardados como csv windows comma separated y no solo comma separated
# En otro caso el encoding ISO-8859 no funciona
Data.Reader <- function(data.base,sep,format.file,file.encoding){
  
  file.list = list.files(data.base)
  file = file.list[1]
  data = data.frame(c(0))
  format = FALSE
  data.file = data.frame()

  for (i in 1:length(file.list)){
    for(j in 1:str_length(file.list[i])) {
      
      if(substring(file.list[i],j,str_length(file.list[i]))==format.file){
        file = file.list[i]
        format = TRUE
      }
      
    }
    
    if(format==TRUE){
      
      if(format.file=="csv"){
        data.file = read.csv(str_c(data.base,file),sep = sep,stringsAsFactors=FALSE,header=TRUE,fileEncoding = file.encoding,blank.lines.skip = TRUE,row.names=NULL)  
      }
      
      if(format.file=="xls"|format.file=="xlsx"){
        data.file = read.xls(str_c(data.base,file),stringsAsFactors=FALSE,header=TRUE,fileEncoding = file.encoding,blank.lines.skip = TRUE,row.names=NULL)  
      }
      
      combine <- list(data.file,data) #the database can have a different number of variables and this add NA in the data base which does not have the variable
      data= do.call(rbind.fill, combine)
      
    }
  }
  #Elimina lineas sin informacion (solo con nas que no aportan informacion)
  
  
  rm(data.file)
  data = suppressWarnings(remove.vars(data,c("c.0.","row.names","X")))
  row.names(data)=NULL
  data = data[rowSums(is.na(data)) != ncol(data),]
  return(data)
}

#Create two matrix. The first matrix has the values for each variable and system. The second matrix has the year in which the value was taken for that system and the third matrix the source.
BRTData.Reader <- function(data.base,sep,format.file,ids,file.encoding){

  
  #Create un separated database for each file
  file.list = list.files(data.base)
  file = file.list[1]
  data = data.frame(c(0))
  format = FALSE
  data.file = data.frame()
  temp = list()

  for (i in 1:length(file.list)){
    for(j in 1:str_length(file.list[i])) {
      
      if(substring(file.list[i],j,str_length(file.list[i]))==format.file){
        file = file.list[i]
        format = TRUE
      }
      
    }

    if(format==TRUE){
      
      if(format.file=="csv"){
        data.file = read.csv(str_c(data.base,file),sep = sep,stringsAsFactors=FALSE,header=TRUE,fileEncoding = file.encoding,blank.lines.skip = TRUE,row.names=NULL)  
      }
      
      if(format.file=="xls"|format.file=="xlsx"){
        data.file = read.xls(str_c(data.base,file),stringsAsFactors=FALSE,header=TRUE,fileEncoding = file.encoding,blank.lines.skip = TRUE,row.names=NULL)  
      }
    
     #Crea una lista de tres matrices, la primera contiene el valor de la variable para un sistema, la segunda el año donde fue medido el dato, y la tercera la fuente donde se obtuvo el dato
      
      #Columnas que identifican al sistema, corredor o país
      common_ids = names(data.file)[-sapply(1:length(ids),function(i) which(names(data.file)== ids[i]))]
      
      for (j in 1:length(ids)){
        if (i == 1){#Si es el primer archivo que se está leyendo, se crean columnas que identifican al sistema
          temp[[j]] = data.file[,common_ids]  
        }
        #Hago un merge para poner los datos de las otras variables, después de las primeras columnas que identifican al sistema
        temp[[j]] = merge(temp[[j]],data.file[c(common_ids,ids[j])],by=common_ids,all.x=TRUE)   
        names(temp[[j]])[length(temp[[j]])] = file.list[i]
     }
  
#       combine <- list(data.file,data) #the database can have a different number of variables and this add NA in the data base which does not have the variable
#       data= do.call(rbind.fill, combine)
#       
    }
    names(temp) = ids
  }
  
  return(temp)
}


#En la nueva version de R hay un problema para ver las variables que no son data.frame
view <- function(data){
  
  View(as.data.frame(data))
}


rename.group <- function (x, replace) {
  #Example for using the function:
  
#   some.names <- structure(c(itdp$ITDP2013), names=c(itdp$ITDP2014))
#   temp2014 = rename.group(temp2014,replace = some.names)
  
  old_names <- names(x)
  new_names <- unname(replace)[match(old_names, names(replace))]
  setNames(x, ifelse(is.na(new_names), old_names, new_names))
}

# 2) PROCESSING DATASETS ----------------------------------------------------------------

# Combining two vectors to one vector

#NA observations of a variable (var1) from the dataset will be replaced by information from another dataset (var 2)
combine.variables <- function(ids, var1, var2){
  data = data.frame(ids,var1,var2) ; data = subset(data, is.na(data$var1)|data$var1=="")
  var1[is.na(var1)|var1==""] = data$var2 
  #   return(data$var1)
  return(var1)
}

#Fill blank spaces with NA
fill.na <- function(data){ 
  data[data==""] = NA
  return(data)
}

#This allows join all the arrays in a vector with 1 column, and should be intialize with the first term  
rowbind <-function(M){
  A = M[[1]]
  
  for(j in seq(2,length(M))) {
    A = rbind(A,M[[j]])
  }
  return(A) 
}

#Receive a list of models
model.labels <-function(models,data){
  
  
  
  for(i in 1:length(models)){
    
    model.names = variable.names(models[[i]])
    for(j in 1:length(variable.names(models[[i]]))){
      
      for(k in 1:dim(data.evasion)[2]){
        
        if(names(data[k])==  variable.names(models[[i]])[j])
        {
          
          model.names[j] =  Hmisc::label(data[k])[[1]]
        }
      }
      
    }
    
  }
  
  
  return(model.names)
  
}

# Se hace un merge entre el primer arreglo(data1) pero del segundo arreglo(data2) elimina todos los 
# valores de la variable elegida(var.merge) que estÃ¡n duplicados, sino cuando se hace merge se 
# aumenta el tamaÃ±o de la matriz final.
# var.merge siempre debe ser un cÃ³digo

merge.nodup <- function(data1,data2,var.merge){

  temp=data.frame(data2,duplicated(data2[var.merge])); colnames(temp) = c(colnames(data2),"duplicated"); row.names(temp)=NULL
  temp =subset(temp,temp$duplicated==FALSE); temp = remove.vars(temp,c("duplicated"))
  data.merged = merge(data1,temp,by=var.merge,all.x=TRUE)

  return(data.merged)  
}

labelling.variables <- function(data, var_labels){
  labels_temp = as.character(var_labels)
  variables_temp = names(var_labels)
  for(i in 1:dim(data)[2]){
  
    for(j in 1:length(var_labels)){
      
      if(names(data)[i]==variables_temp[j]){
        Hmisc::label(data[[names(data)[i]]]) = labels_temp[j]
      }  
      
    }
    
  }
  return(data)
}

convertLabelledVectorsToNumbers <-function(data){
  varList2014 = c("a1","a2","a3","a4","a5","b1","b2","b3","b4","b5","b6","b7","c1","c2","c3","c4","c5","d1","d2","d3","d4","d5","e1","e2"
             ,"f1","f2","f3","f4","f5","f6","g1","g2","g3","g4","g5","g6","g7","g8")
  
  for(var in 1:length(varList2014)){
    data[,var] = as.numeric(var)
    
  }
    
  return(data)
}


# 3) FORMATS ----------------------------------------------------------

date.id <-function(month.id, day.id, year.id){
  date.id  = paste(month.id,"/",day.id,"/", year.id)
  date.id = str_replace_all(date.id," ","")
  return(date.id)
}

#Short format or long format
day.name <-function(day,long = TRUE){
   
  if(long==TRUE){
    day = factor(day, levels = seq(1,7), labels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  }
  
  else{
    day = factor(day, levels = seq(1,7), labels = c("Mo","Tu","We","Th","Fr","Sa","Su"))
  }
  
  return(day)
  
}

month.name <-function(month,long = TRUE){

  if(long==TRUE){
    month = factor(month, levels = seq(1,12), labels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
  }
  
  else{
    month =factor(month, levels = seq(1,12), labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
  }
  
return(month)
}

#We create this variable to have hour and minutes with two digts
month.id <- function(x){
  month.id = factor(x, levels = seq(1,12), labels = c("01","02","03","04","05","06","07","08","09","10","11","12"))
  month.id = as.character(month.id)
  
  return(month.id)
}
day.id <- function(x){
  day.id = factor(x, levels = seq(1,31), labels = c("01","02","03","04","05","06","07","08","09",seq(10,31)))
  day.id = as.character(day.id)
  return(day.id)
}

#n is 2 if the time format is hour:minute and 3 if the time format is hour:minute:second
time.separate <- function(time,n){
  #Time is separated in two columns by ":", the first part is the hour and the second part is the minute
  #   if(n==2){
  #     time = sapply(1:length(time), function(i) strsplit(time[i],":")[[1]][1:2]) ; 
  #     time = t(time) ; colnames(time) = c("hour","minute")  
  #   }
  #   
  #   if(n==3){
  #     time = sapply(1:length(time), function(i) strsplit(time[i],":")[[1]][1:3]) ; 
  #     time = t(time) ; colnames(time) = c("hour","minute","second")  
  #   }
  time = sapply(1:length(time), function(i) strsplit(time[i],":")[[1]])
  hour = sapply(1:length(time), function(i) time[i][1])
  minu = sapply(1:length(time), function(i) time[i][1])
  time = t(time) ; colnames(time) = c("hour","minute")  
  
  
  # # Give a data.frame with 2 columns(hour and minute)
  return(time)
}
hour.id <- function(x){
  hour.id = factor(x, levels = seq(0,24), labels = c("00","01","02","03","04","05","06","07","08","09",seq(10,24)))  
  hour.id = as.character(hour.id)
  return(hour.id)
}
minute.id <- function(x){
  minute.id = factor(x, levels = seq(0,59), labels = c("00","01","02","03","04","05","06","07","08","09",seq(10,59)))
  minute.id = as.character(minute.id)
  return(minute.id)
}

#Quarter Corresponding to US classifaction (1:Winter, 2: Sptring, 3: Summer, 4: Fall)

quarter.us <- function(date){
  
  
  quarters=data.frame(date); colnames(quarters) = c("date")
  quarters$quarter = 0
  quarters$quarter[(as.Date(date, format = "%m/%d/%Y")-as.Date("12/27/2013", format = "%m/%d/%Y")>0)& 
                    (as.Date(date, format = "%m/%d/%Y")-as.Date("03/21/2014", format = "%m/%d/%Y")<=0)
                  ] = 1
  
  quarters$quarter[(as.Date(quarters$date, format = "%m/%d/%Y")-as.Date("12/27/2013", format = "%m/%d/%Y")>0)& 
                    (as.Date(quarters$date, format = "%m/%d/%Y")-as.Date("06/20/2014", format = "%m/%d/%Y")<=0)
                  ] = 2
  
  quarters$quarter[(as.Date(date, format = "%m/%d/%Y")-as.Date("06/20/2014", format = "%m/%d/%Y")>0)& 
                    (as.Date(quarters$date, format = "%m/%d/%Y")-as.Date("08/29/2014", format = "%m/%d/%Y")<=0)
                  ] = 3
  
  quarters$quarter[(as.Date(date, format = "%m/%d/%Y")-as.Date("08/29/2014", format = "%m/%d/%Y")>0)& 
                    (as.Date(quarters$date, format = "%m/%d/%Y")-as.Date("12/26/2014", format = "%m/%d/%Y")<=0)
                  ] = 4

  return(quarters$quarter)
}
diff.dates <- function(date1,date2){
  
  date=data.frame(date1,date2); colnames(date) = c("date1","date2")
  
  return((as.Date(date$date1, format = "%m/%d/%Y")-as.Date(date2, format = "%m/%d/%Y")))
}

#Split each term of the vector of strings to n parts using the pattern. 
#Then the method generates another vector with the part indexed by n
split.id <- function(data,id,pattern,index){
  #   return(sapply(1:dim(data)[1], function(i) str_split(data[,id],"-")[[1]][index]))
  return(sapply(1:dim(data)[1], function(i) str_split(data[,id][i],pattern)[[1]][index]))
}

#Receive a dataset with strings, remove accents of the whole set of words and replace capital letters)
Var.names <- function(data){
  
  data2 = data
  variables = colnames(data2)
  
  #   #Replace dot symbol to underscores 
  variables = str_replace_all(as.character(variables),as.character(","),"_") #Replace commas using underscores
  variables = str_replace_all(as.character(variables),as.character("[-]"),"_") #Replace % using underscores
  variables = str_replace_all(as.character(variables),as.character("[%]"),"_") #Replace % using underscores
  variables = str_replace_all(as.character(variables),as.character("[$]"),"_") #Replace $ using underscores
  variables = str_replace_all(as.character(variables),as.character("[(]"),"_") #Replace $ using underscores
  variables = str_replace_all(as.character(variables),as.character("[)]"),"_") #Replace $ using underscores
  variables = str_replace_all(as.character(variables),as.character(" "),"_") #Replace Spaces using underscores
  variables = str_replace_all(as.character(variables),as.character("[.]"),"_") #Including parenthesis, otherwise dots are not detected in the strings
  
  
  
  
  #Generate a list with differents numbers of underscores (variables names could contain more than one space )
  underscores = list(c("_"))
  
  for(i in 2:max(str_length(variables))){
    underscores[i] = str_c(as.character(underscores[i-1]),"_")
  }
  
  #If there are more than one underscore, only one is kept
  for(i in 1:length(underscores)){
    variables = str_replace_all(as.character(variables),as.character(underscores[length(underscores)+1-i]),"_")
  }
  
  #Remove underscores at the end of the variable name
  for(i in 1:length(variables)){
    #Last character in the string
    last = str_sub(variables[i],str_length(variables[i]),str_length(variables[i]))
    if(last=="_"){
      variables[i] = str_sub(variables[i],1,str_length(variables[i])-1)
    }
  }
  #Replace accents(and Ã±) in variable names 
  variables = Encoding.UTF8(variables)[,1]
  
  #Variable names with Capital Letters
  variables = toupper(variables)
  
  #Remove _CSV or CSV in the end of the name
  variables = str_replace_all(as.character(variables),as.character("_CSV"),"") #Including parenthesis, otherwise dots are not detected in the strings
  
  colnames(data2)=variables
  
  
  #Remove names of unnecessary variables 
  data2 = suppressWarnings(remove.vars(data2,c("c.0.","row.names","X")))
  
  return(data2)
}

#Replace ony vector with class character in the dataset(data)
Encoding.UTF8 <- function(data){
  
  data2 = as.data.frame(data,stringsAsFactors = FALSE)
  variables =colnames(data2)
  characters = list("á","é","í","ó","ú","Ñ","Á","É","Í","Ó","Ú","Ñ")
  no.accents = list("a","e","i","o","u","n","A","E","I","O","U","N")
  
  for(i in 1:length(variables)){
    
    if(class(data2[,variables[i]])=="character"){
      for(j in 1:length(characters)){
        data2[variables[i]] = str_replace_all(as.character(data2[,variables[i]]),as.character(characters[j]),as.character(no.accents[j]))  
      }
    }
  }
  
  return(data2)
}

# 4) STATISTICS --------------------------------------------------------------

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Un vector con la informacion observada(data.obs) y otro con las predicciones (data.pred).
#Para el test tambien se incluye el numero de grados de libertad  que es igual al nobs 
# menos el numero de predictores(predictors) que incluye el intercepto.

# model = MTT.Binomial.Final, offset: nombre de la variable que se utiliza para el offset.
IRR <-function(model)
{
  est <- cbind(Estimate = coef(model), confint(model))
  irr.matrix= exp(est)
  irr = exp(model$coef) 
  rse <- sqrt(diag(vcovHC(model, type="HC0"))) #Con errores robustos
  ec <- c(irr) 
  rs <- c(rse) 
  se = ec*rs #Errores estandar
  t = summary(model)$coef[c(2:dim(summary(model)$coef)[1],1),"z value"]
  irr.matrix.final = round(cbind(irr.matrix,se=se,t=t,fc= (irr-1)*100),4)
  
  return(irr.matrix.final)
  
  
}

Chi2.glm <- function(data_obs,model) {
  
  variables = remove.vars(model.frame(model),c("row.names")) #Incluye la variable dependiente usada para estimar el modelo
  #Extrae el nombre de la variable dependiente que se va a predecir
  dep.var = colnames(variables)[1]
  obs = dim(data$data_obs)[1] #NÃºmero de observaciones
  predictors = dim(variables)[2] #Como incluye la variable dependiente, se "contabilizarÃ?a el intercepto"
  
  #La probabilidad de que se produzcan diferentes valores del proceso de conteo.
  propPred = as.data.frame(colSums(predprob(model,data$data_obs))/obs) ; propPred=cbind(seq(0,dim(propPred)[1]-1),propPred)
  colnames(propPred)=c("Count","propPred"); propPred$Count = as.numeric(as.character(propPred$Count))
  #   propPred=data.frame(Count= seq(0,length(propPred)[1]-1),propPred)
  
  propObsv = as.data.frame(table(data$data_obs[dep.var])/obs); colnames(propObsv)=c("Count","propObsv")
  propObsv$Count = as.numeric(as.character(propObsv$Count))
  
  # #MÃ¡ximo nÃºmero de evasiones contadas en la base de datos.
  #   Count = data.frame(seq(0,max(as.numeric(as.character(propObsv$Count))))); colnames(Count) = c("Count")
  
  #Maximo nÃºmero de conteos dado por lo observado
  max.obsv =max(as.numeric(as.character(propObsv$Count)))
  max.pred = max(as.numeric(as.character(propPred$Count)))
  Count = data.frame(seq(0,max.obsv))
  colnames(Count) = c("Count")
  
  #Se mezclan las predicciones con lo observado
  temp = merge(Count,propObsv,by="Count",all.x=TRUE); temp=merge(temp,propPred,by="Count",all.x=TRUE)
  #Se agregan ceros en los valores de conteo de evasiÃ³n en donde no hay observaciones
  temp$propObsv[is.na(temp$propObsv)]=0; temp$propPred[is.na(temp$propPred)]=0
  
  temp=cbind(temp,diff=temp$propObsv-temp$propPred)
  
  #Ahora debo agrupar el vector para que no quede ningun slot lleno de 0's en las probabilidades que se predicen sino se indefine el test
  sum=0; index=dim(temp)[1]
  for(i in 1:dim(temp)[1]){
    
    
    if(temp$propPred[dim(temp)[1]+1-i]<=0.01){
      index=dim(temp)[1]+1-i  #Se empieza desde el Ãºltimo elemento del arreglo hacia atrÃ¡s
      sum=sum(temp$propPred[index:dim(temp)[1]])
    }
  }
  #Hago un nuevo vector agregando todos los valores de los conteos posteriores a 0 en los valores observados
  
  temp$propObsv[index] = 1-sum(temp$propObsv[1:(index-1)])
  temp$propPred[index] = 1-sum(temp$propPred[1:(index-1)])
  temp= temp[1:index,]
  temp$diff = temp$propObsv-temp$propPred
  temp$propPred = round(temp$propPred,4) ; temp$propObsv = round(temp$propObsv,4); temp$diff= round(temp$diff,4)
  
  #Test Chi cuadrado por los intervalos
  chitest.int = with(temp,sum((obs*(propObsv-propPred))^2/(obs*propPred)))
  dof.int = dim(temp)[1]-1
  pvalue.int = pchisq(chitest.int,df = dof.int,lower.tail=FALSE)  
  #Valor de tabla Chi cuadrado (El valor grÃ¡fico de chi es como al revÃ©s, si aumento p, mÃ¡s exigente es, pero cambiando la cola queda mÃ¡s intuitivo)
  chitable.int=qchisq(0.05,dof.int,lower.tail=FALSE) #al 95% de confianza
  
  #Test chi comparando predicciÃ³n y valor observado (un valor por cada observaciÃ³n)
  prediction = predict.glm(model,data$data_obs,type="response")
  chitest = sum((data$data_obs[dep.var]-prediction)^2/(prediction))
  dof = obs-1
  pvalue = pchisq(chitest,df = dof,lower.tail=FALSE)
  chitable=qchisq(0.05,dof,lower.tail=FALSE) #al 95% de confianza, a medida que subo es mÃ¡s grande el estadÃ?stico
  
  
  #Mostramos los resultados como porcentajes
  temp$propPred=  temp$propPred*100; temp$propObsv=  temp$propObsv*100; temp$diff=  temp$diff*100
  
  #Root Mean Square Error (RMSE)
  rmse = sqrt(sum((data$data_obs[dep.var]-prediction)^2)/obs)
  
  results=list(table=temp,chitable.int=chitable.int,chitest.int=chitest.int,
               chitable=chitable,chitest=chitest,rmse=rmse, nobs=obs)
  
  return(results)
  #El primer elemento es el valor del test, el segundo el valor de tabla, el tercero el valor p
}

#group_variable: The variable in the dataset to divide the data. 
#test_variable: the variable used to compare the means
#weight_variable: The variable used to weight the data and apply the test 

weighted.mean.test <- function(data,group_variable,group_value,weight_variable=NA,test_variable,alternative){

  data$variable = 0; data$variable[data[group_variable]==group_value] = 1
  data1 = subset(data,variable ==1)
  data2 = subset(data,variable ==0)
  
  
  if(is.na(weight_variable)==TRUE){
    
    data1$weight = 1
    data2$weight = 1
    
    temp = wtd.t.test(x=data1[[test_variable]],
                      y=data2[[test_variable]],
                      weight = data1$weight,
                      weighty = data2$weight,
                      alternative = alternative,samedata=FALSE)
  }
  
  else{

    temp = wtd.t.test(x=data1[[test_variable]],
                      y=data2[[test_variable]],
                      weight= data1[[weight_variable]],
                      weighty=data2[[weight_variable]],
                      alternative = alternative,samedata=FALSE)
  }
  
  if(dim(data2)[1]==1){
    
    temp$additional= rename(temp$additional,c(Mean="Mean.x"))
    temp$additional= rename(temp$additional,c(Alternative="Mean.y"))
    
#     temp$Mean.x = temp$additional["Mean"]
#     temp$Mean.y = temp$additional["Alternative"]
  }
  
  temp = cbind(t(data.frame(temp$additional)),t(data.frame(temp$coefficients))); row.names(temp) = NULL
  
  return(temp)
}

colMax <- function(X) apply(X, 2, max)

# 5) TIME SERIES -------------------------------------------------------------

#Receive a data frame with dates (MDY format) and gives a vector with all days between the maximum and the minimum date 
everyday <- function(dates){
  
  dates = mdy(dates)
  max_date = max(dates)
  min_date = min(dates)
  diff_dates = as.numeric(max_date-min_date)  
  data = data.frame(seq(1,diff_dates)) ; rownames(data)=NULL ; colnames(data)=c("date")
  data$date = NA
  
  for(i in 1:diff_dates)
  {
    data$date[i] = date.id(month.id(month(min_date+days(i))),day.id(day(min_date+days(i))),year(min_date+days(i)))
  }
  
  return(data)
}

# The period variable is defined using the Transantiago Schedule Classification
# Data must have the day of the week, hour and minute when the observation was collected

period <- function(data, schedule){
  
  data$period = NA
  
  #   for( i in 1:dim(data)[1]){
  #     t = with(data,as.numeric(hour[i])*60+as.numeric(minute[i]))
  #       
  #     for(j in 1:dim(schedule)[1]){
  #         t1 = with(schedule,as.numeric(start.hour)[j]*60+as.numeric(start.minute[j]))
  #         t2 = with(schedule,as.numeric(end.hour[j])*60+as.numeric(end.minute[j]))
  #         if((is.na(t1)==FALSE) & (is.na(t2)==FALSE) & (is.na(t)==FALSE) 
  #            & (is.na(data$day.week[i])==FALSE) & (is.na(data$day.week[j])==FALSE)){
  #           
  #           if( (t>=t1) & (t<=t2) & (data$day.week[i] == schedule$day.week[j])) {
  #             data$period[i] = schedule$period[j]
  #             j=dim(schedule)[1]
  #           }
  #         }
  #       }
  #     }
  
  for(i in 1:dim(schedule)[1]){
    data$period[(data$time<=schedule$end.time[i] & data$time>=schedule$start.time[i]) & (schedule$day.week[i] == data$day.week )] = schedule$period[i]  
  }
  
  return(data)
}

#Receive a data.frame in which each variable is the column, and the row are from the most old to the most recent event
#Then, calculate the lag for each one and return this matrix (define by input variable lag)

AR<- function(data.base, lag){
  
  data.base = as.data.frame(data.base)
  data.base.AR = data.base
  
  #Go along the variables(columns)
  for(i in 1:dim(data.base)[2])
  {
    data.base.AR[,i] = matrix(nrow= length(data.base[,i])) #Vector of NAs  
    
    # lag 1, the first term have to be removed but for the moment is going to be taken in 0
    
    for(j in (lag+1):length(data.base[,i]))
    {  
      data.base.AR[j,i] = data.base[j-lag,i]           
    }
    
    data.base.AR[j-1,i]  
  }
  
  #Assign a name for each lag variable .lx where x is the number of lags
  
  names.lags = matrix(nrow=length(colnames(data.base)))
  
  for(i in 1:length(colnames(data.base))){
    names.lags[i]= str_c(colnames(data.base)[i],".l",lag)
    colnames(data.base.AR)[i] = c(names.lags[i])
  }
  
  return(data.base.AR)
}

#Diff: How many differentials, for the moment only one
DR<- function(data.base, diff){
  
  data.base = as.data.frame(data.base)
  data.base.DR = data.base
  
  #Go along the variables(columns)
  for(i in 1:dim(data.base)[2])
  {
    data.base.DR[,i] = matrix(nrow= length(data.base[,i])) #Vector of zeros  
    data.base.DR[,i] = data.base[,i]-AR(data.base[,i],diff)               
  }
  
  #Assign a name for each lag variable .lx where x is the number of lags
  
  names.lags = matrix(nrow=length(colnames(data.base)))
  
  for(i in 1:length(colnames(data.base))){
    names.lags[i]= str_c(colnames(data.base)[i],".d",diff)
    colnames(data.base.DR)[i] = c(names.lags[i])
  }
  
  return(data.base.DR)
  
  
}

#Id is the criteria to aggregate the information by a variable (var.name) using a math function(fun) 
# in the  source data (data1). Then, it is merged in the smaller dataset which has one observation by id(data2).
aggregate.id <- function(data1,data2,id,var.name,fun) {
  
  
  temp = tapply(data1[,var.name],data1[,id],fun)
  temp.m = data.frame(names(temp),as.numeric(temp)); colnames(temp.m) = c(id,var.name)
  data2 = merge(temp.m,data2,by=id)  
  data2[,id] = as.character(data2[,id])
  
  return(data2)
}

# 5) SOCIAL INFLUENCE -----------------------------------------------------

#Receive a vector of routes and stops with theirs ids, and the flows on each stop (Evasions, boarding, exiting, etc)
#and sequentially ordered.
Influence.Matrix <- function(inspection.route, stop.id , stop.flow,interval){
  data = data.frame(inspection.route,stop.id,stop.flow) 
  colnames(data) = c("inspection.route","stop.id","stop.flow")
  
  #Receive a vector of routes and stops using the bus stop ids
  data$stop.id = as.character(data$stop.id) ; data= data[order(data$stop.id),]  
  
  data.route = unique(data) #Select just one row (stop) for each group of stops
  
  
  flow.route = split(data.route, data.route[1]) # Divide the data in one vector for each route and cointains the total boarding in each bus stop. It is necessary save the key of the bus stop 
  
  influence.flow = sapply(1:length(flow.route), function(i) Influence.Ratio(t(flow.route[[i]][3]), interval))
  
  #Create a matrix M where the column i represent the influence considering the next i stops. 
  #Each column is initializated with the first vector calculated and then we put the next columns The first column
  
  
  # (The algorithm fails if the route id is wrong, e.g the departure time is not the same for each inspector)
  M=0
  
  for(i in seq(1,interval)) 
  {
    #This allows join all the arrays in a vector with 1 column, and should be intialize with the first term  
    A = influence.flow[[1]][i,]
    
    for(j in seq(2,length(influence.flow))) {
      A = append(A,influence.flow[[j]][i,])
    }
    
    if(i==1){
      M= A #Matrix is initialized
    }
    else{
      M=cbind(M,A) 
    }
  }
  #Each array has to be merge with the original data because it was used only the unique stop.
  
  #We should merge with the original database using the stop.id "keys"
  stop.ids = unique(data$stop.id) 
  stop.df <- data.frame(stop.ids,M); colnames(stop.df) = c("stop.id")
  M = merge(data, stop.df, by="stop.id")   #by stop.id 
  
  #Now it is going to be calculated the partial influence, in other words, the influence of 
  #the evasion on the next stop i stops, not accumulated. 
  #The index of sapply is because must be given only the matrix with the variables.
  
  a=(dim(data)[2]+1); b=(dim(M)[2]) ; dM= M[,a] #Flow of Passengers at the bus stop 
  for(i in seq(a+1,b))
  {
    dM= cbind(dM,M[,i]-M[,i-1])
  }
  
  dM = data.frame(M[,1],M[,2],M[,3],dM) ; #Bus stop ids(M[,1]) and inspection.route(M[,2]) are added to the matrix dM 
  
  #   row.names(dM)=NULL 
  #Variable Names
  names = array(1:(interval))
  for(i in 1:length(names)){
    names[i] = str_c("e",i) 
  }
  colnames(dM) = c("stop.id","inspection.route","e0",names)
  
  dM$stop.id = as.character(dM$stop.id)
  
  dM= dM[order(dM$stop.id),] #The output is ordered by the bus stop ids.
  
  return(dM)
  # return(flow.route)
}



#Vector of data must be sequentially ordered by bus stop
Influence.Ratio  <- function(data, interval){ 
  
  len = dim(data)[2] 
  #Generate a matrix with the accumulated number of boarding, evading and so on, considering as maximum the length of the vector
  
  matrix.holder= sapply(1:(len-1), function(i) sapply(1:(len-1), function(x) 
    if ((i+x) <= len) {sum(data[(i+1):(i+x)]) }
    else {sum(data[(i+1):(len)])}))
  matrix.holder <- cbind(matrix.holder,0)
  
  #Considering all the vector should be work using the same intervals but there are different length, it will be necessary
  #replicate the last row of the matrix generated before, n times, where n is equal to the difference between the intervals and the length of the route
  
  #Considering the form than operate the last method is necessary do a extra operation when there one or two observations
  if(len==1){matrix.holder = matrix(seq(0,0,length.out=interval))}
  
  
  else{ 
    B = sapply(length(data):interval, function(i) matrix.holder[length(data)-1,])
    matrix.holder = rbind(matrix.holder,t(B))
    
    #Otherwise is necessary remove rows.
    if(interval < len){    
      matrix.holder = matrix.holder[1:interval,]
    }
  }
  
  
  matrix.holder = as.matrix.data.frame(matrix.holder)
  
  return(matrix.holder)
}  





# boarding.stop = list()


# 
# #Creation of one variable for each quantity i of next stop boarding
# boarding.stop= boarding.intervals[][[1]]
# 
# #This allows join all the arrays in a vector with 1 column, and should be intialize with the first term
# 
# total.boarding = boarding.stop[[1]]  
# for(i in seq(2,length(boarding.stop))) {total.boarding = append(total.boarding,boarding.stop[[i]])}
# 
# 
#  A=Influence()

# 6) WEAKNESSES AND STRENGTHS ---------------------------------------------------

#Receive a list of variables to compare between the two groups
weakness.strengths <- function(list_test_variables, data,group_variable,group_value,weight_variable=NA
                               ,alternative,significance){
  
  weighted_means = list()
  temp = data.frame()
  for(i in 1:length(list_test_variables)){
    
    temp1 = round(as.data.frame(weighted.mean.test(test_variable = list_test_variables[i],data=data, group_variable = group_variable, group_value = group_value,weight_variable = weight_variable,alternative=alternative)),3)
    
    variable = as.character(unique(Hmisc::label(data[[list_test_variables[i]]])))
    
    temp1$x_difference = round(with(temp1,Difference/Mean.y),3)*100
    
    temp1 = cbind(id_variable = list_test_variables[i],variable = variable,test = alternative,temp1)

    if(i==1){
      temp = temp1
    }
    else{
      temp = rbind(temp,temp1)  
    }
  }
  
  if(alternative == "two.tailed"){
    
    strengths=subset(temp,p.value<=significance & test == alternative & Difference>0)
    weaknesses=subset(temp,p.value<=significance & test == alternative & Difference<0)
    no_difference = subset(temp,p.value>significance)
    
    strengths = strengths[order(strengths$x_difference,decreasing=TRUE),]
    weaknesses = weaknesses[order(weaknesses$x_difference,decreasing=FALSE),]
    no_difference = no_difference[order(no_difference$x_difference,decreasing=TRUE),]
    
    if(dim(strengths)[1]==0){
      strengths[1,] = NA
    }
    
    if(dim(weaknesses)[1]==0){
      weaknesses[1,] = NA
    }
    
    if(dim(no_difference)[1]==0){
      no_difference[1,] = NA
    }
    
    return(list(strengths = strengths, weaknesses = weaknesses, no_difference = no_difference))
  }
  
  if(alternative == "less"){
    weaknesses=subset(temp,p.value<=significance & test == alternative & Difference<0)
    no_weaknesses = subset(temp,p.value>significance | Difference == 0)
    
    weaknesses = weaknesses[order(weaknesses$x_difference,decreasing=FALSE),]
    no_weaknesses = no_weaknesses[order(no_weaknesses$x_difference,decreasing=TRUE),]
    
    if(dim(weaknesses)[1]==0){
      weaknesses[1,] = NA
    }
    if(dim(no_weaknesses)[1]==0){
      no_weaknesses[1,] = NA
    }
    
    return(list(weaknesses = weaknesses, no_weaknesses = no_weaknesses))  
  }
  
  if(alternative == "greater"){
    strengths=subset(temp,p.value<=significance & test == alternative & Difference>0)
    no_strengths = subset(temp,p.value>significance | Difference == 0)
    
    strengths = strengths[order(strengths$x_difference,decreasing=TRUE),]
    no_strengths =no_strengths[order(no_strengths$x_difference,decreasing=TRUE),]
    
    if(dim(strengths)[1]==0){
      strengths[1,] = NA
    }
    if(dim(no_strengths)[1]==0){
      no_strengths[1,] = NA
    }
    
    return(list(strengths = strengths, no_strengths = no_strengths))  
  }
  
#   if(alternative=="all"){
#     weaknesses_less=subset(temp,p.value<=significance & test == "less" & Difference<0)
#     weaknesses_twotailed=subset(temp,p.value<=significance & test ==  "two.tailed" & Difference<0)
#     weaknesses = merge(weaknesses_less,weaknesses_twotailed,by="id_variable")
#     
#     strengths_less=subset(temp,p.value<=significance & test ==  "less"& Difference>0)
#     strengths_twotailed=subset(temp,p.value<=significance & test == "two.tailed" & Difference>0)
#     strengths = merge(strengths_less,strengths_twotailed,by="id_variable")
#     
#     
#     return(list(strengths = strengths, weaknesses = weaknesses))
#   }
  
}

weakness.strengths.anova <- function(list_test_variables, data,group_variable,group_value,significance){
  
  weighted_means = list()
  temp = data.frame()
  alternative="two.tailed"
  weight_variable = NA
  
  for(i in 1:length(list_test_variables)){
    
    temp1 = round(as.data.frame(weighted.mean.test(test_variable = list_test_variables[i],data=data, group_variable = group_variable, group_value = group_value
                                                   ,weight_variable = weight_variable,alternative=alternative)),3)
    
    variable = as.character(unique(Hmisc::label(data[[list_test_variables[i]]])))
    # temp1 = as.data.frame(temp1)
    temp1$x_difference = round(with(temp1,Difference/Mean.y),3)*100

    temp1 = cbind(id_variable = list_test_variables[i],variable = variable,test = alternative,temp1)
    

    temp1 = temp1[c("id_variable","variable","Difference","Mean.x","Mean.y","x_difference")]
    
    data$y = data[[list_test_variables[i]]]
    data$group_variable = 0
    data$group_variable[data[[group_variable]]==group_value] = 1
    
    temp1$p.value = round(anova(lm(y ~ group_variable,data))$P[1],3)
    
    if(i==1){
      temp = temp1
    }
    else{
      temp = rbind(temp,temp1)  
    }
  }
  
  
    strengths=subset(temp,p.value<=significance & Difference>0)
    weaknesses=subset(temp,p.value<=significance & Difference<0)
    no_difference = subset(temp,p.value>significance | Difference == 0)
    
    strengths = strengths[order(strengths$x_difference,decreasing=TRUE),]
    weaknesses = weaknesses[order(weaknesses$x_difference,decreasing=FALSE),]
    no_difference = no_difference[order(no_difference$x_difference,decreasing=TRUE),]
    
    if(dim(strengths)[1]==0){
      strengths[1,] = NA
    }
    if(dim(weaknesses)[1]==0){
      weaknesses[1,] = NA
    }
    if(dim(no_difference)[1]==0){
      no_difference[1,] = NA
    }
    
    return(list(strengths = strengths, weaknesses = weaknesses, no_difference = no_difference))
  
  
  
}

# 7) CORRIDORS-SYSTEM -----------------------------------------------------

# Transform corridors in system

# Data: Database
# Weight_variable: Variable used for weight
# Var_list: List of variables that will be aggregated. The rest of variables will be kept constant
# Mode: Weighted (weight) or simple average (mean)
# Unit: The unit of analysis use to aggregate (city, country, corridor, system, etc). 
#If more than one unit of analysis is used, the function of receive a vector os strings. The units are uniques ids.


# data = scores
# unit = unit=c("system","city","corridor")
# data$unit
# weight_variable = "corridor_length"
aggregate.indicators <- function(data = data, unit = NA, weight_variable = NA, var_list = var_list){
  
  data = data
  data_raw = data
  
  if(length(unit)==1){
    if(is.na(unit)==TRUE){
      unit="unit"
      data$unit = "1"
    }
    
    else{
      for(i in 1:length(unit)){
        if(i == 1){
          data$unit = paste(data[[unit[i]]])
        }
        else{
          data$unit = paste(data$unit," - ",data[[unit[i]]])  
        }
      }
    }
  }
  
  else{
    for(i in 1:length(unit)){
      if(i == 1){
        data$unit = paste(data[[unit[i]]])
      }
      else{
        data$unit = paste(data$unit," - ",data[[unit[i]]])  
      }
    }
  }
  
  
  if(is.na(weight_variable)==FALSE){
    data$weight_variable = as.numeric(data[[weight_variable]])
  }
  
  else{
    data$weight_variable=1
    weight_variable = "weight_variable"
    
  }
  
  
  temp = data[c(var_list,weight_variable,"unit")]  
  temp$weight_variable = temp[[weight_variable]] 
  for(i in 1:length(var_list)){
    letter = var_list[i]  
    x=temp[[letter]]
    temp[[letter]] = round(as.numeric(with(temp,ave(weight_variable*x,unit,FUN=function(x) sum(x))))/as.numeric(with(temp,ave(weight_variable,unit,FUN=function(x) sum(x)))),2)
  }
  
  
  data[var_list] = temp[var_list]
  # data$unit = data[unit]
  data[[weight_variable]] = as.numeric(ave(as.numeric(data[[weight_variable]]),data[unit],FUN=function(x) sum(x)))
  
  #The unit of aggregation is not NA
  if(unit[1]!="unit"){
    data = unique(data[c(unit,weight_variable,var_list)])
  }
  else{
    data = unique(data[c(weight_variable,var_list)])
  }
  
  data = labelling.variables(data,Hmisc::label(data_raw))
  
  
  return(data)
  
}


# 8) GRAPHICS -------------------------------------------------------------

labels1000K <- function (numbers){
  
  numbers = as.character(numbers)
  for(i in 1:length(numbers)){
      if(str_sub(numbers[i],str_length(numbers[i])-2,str_length(numbers[i]))=="000"){
        numbers[i] = str_c(str_sub(numbers[i],0,str_length(numbers[i])-3),"K")
      }
    if(numbers[i]=="0"){
      numbers[i] = "0K"
    }
    
    
  }
  
  return(numbers)
  
  
}




