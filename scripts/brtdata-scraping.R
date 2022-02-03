
# 1) READING DATA FROM BRTDATA.ORG ---------------------------------------------------

# install.packages("XML")
library("XML")

#http://statistics.berkeley.edu/computing/r-reading-webpages
#https://www.r-bloggers.com/scraping-table-from-any-web-page-with-r-or-cloudstat/

# brtdata = "http://brtdata.org/indicators/systems/gdp_per_capita_us"
# brtdata_table = readHTMLTable(brtdata, header=T, which=1,stringsAsFactors=F)

# Código para obtener toda la lisa de indicadores de la página. 

#Line 428 has all information
# download.file('http://brtdata.org/panorama/systems',destfile = str_c(data.base,"BRTData.org/",today()))
# temp = readLines('http://brtdata.org/panorama/systems')[428] #leyendo desde la web
temp_readlines = readLines(str_c(data.base,"input/brtdata.org/","2016-09-03")) #Leyendo desde el archivo html

# temp = temp_readlines[383] #Description for each indicator 

temp = temp_readlines[428]
# getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
temp1 = str_replace_all(temp,"'",'"')
temp1 = str_replace_all(temp1,'City"s',"City's")
# View(temp2)
temp1 = str_sub(temp1,as.numeric(str_locate(temp1,"\\["))+1,as.numeric(str_locate(temp1,"\\]"))-1)[1]
temp2 = as.data.frame(str_split(temp1,"\\}"))
# View(temp2)

data_temp = NA
for(i in 1:dim(temp2)[1]){
#  i=1
#   temp_link = str_sub(temp2[i,1],str_locate(temp2[i,1],", link")[1]+1,str_length(temp2[i,1])) 
#   temp2[i,1] = str_sub(temp2[i,1],0,str_locate(temp2[i,1],", link")[1]-1)
#   temp_link = str_replace_all(temp_link,'"',"'") 
#   i=297
#   temp2[i,1] = str_replace_all(temp2[i,1],'City"s',"City's")
  
  if(i==1){
    
    data_temp = c(name = str_sub(temp2[i,1],str_locate_all(temp2[i,1],'"')[[1]][c(1,2)][1]+1,str_locate_all(temp2[i,1],'"')[[1]][c(1,2)][2]-1)
                  ,value = str_sub(temp2[i,1],str_locate_all(temp2[i,1],'"')[[1]][c(3,4)][1]+1,str_locate_all(temp2[i,1],'"')[[1]][c(3,4)][2]-1)
                  ,data = str_sub(temp2[i,1],str_locate_all(temp2[i,1],'"')[[1]][c(5,6)][1]+1,str_locate_all(temp2[i,1],'"')[[1]][c(5,6)][2]-1)
                  ,link = str_sub(temp2[i,1],str_locate_all(temp2[i,1],'"')[[1]][c(7,8)][1]+1,str_locate_all(temp2[i,1],'"')[[1]][c(7,8)][2]-1)
    )  
    
  }
  else{
    data_temp_0 = c(name = str_sub(temp2[i,1],str_locate_all(temp2[i,1],'"')[[1]][c(1,2)][1]+1,str_locate_all(temp2[i,1],'"')[[1]][c(1,2)][2]-1)
                  ,value = str_sub(temp2[i,1],str_locate_all(temp2[i,1],'"')[[1]][c(3,4)][1]+1,str_locate_all(temp2[i,1],'"')[[1]][c(3,4)][2]-1)
                  ,data = str_sub(temp2[i,1],str_locate_all(temp2[i,1],'"')[[1]][c(5,6)][1]+1,str_locate_all(temp2[i,1],'"')[[1]][c(5,6)][2]-1)
                  ,link = str_sub(temp2[i,1],str_locate_all(temp2[i,1],'"')[[1]][c(7,8)][1]+1,str_locate_all(temp2[i,1],'"')[[1]][c(7,8)][2]-1)
    )  
    data_temp = rbind(data_temp,data_temp_0)
  }
}

row.names(data_temp) = NULL
data_temp = as.data.frame(data_temp)
#

table_regions = subset(data_temp,data_temp$data=="Region")
table_countries = subset(data_temp,data_temp$data=="Country")
table_cities = subset(data_temp,data_temp$data=="City")
table_indicators_system = subset(data_temp,data_temp$data=="Indicator (System)")
table_indicators_corridor = subset(data_temp,data_temp$data=="Indicator (Corridor)")

#Add a column with the category
category_corridor_indicators = read.csv(str_c(data.base,"input/BRTData.org/","category_corridor_indicators.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
category_system_indicators = read.csv(str_c(data.base,"input/BRTData.org/","category_system_indicators.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")

table_indicators_corridor = merge(table_indicators_corridor,category_corridor_indicators[c("name","category")],by.x="name",by.y="name")
table_indicators_system = merge(table_indicators_system,category_system_indicators[c("name","category")],by.x="name",by.y="name")

# 2) STATISTICS BY INDICATOR ---------------------------------------------

# a) Corridors  ---------------------------------------------------------------

  temp_indicator_info_corridor = data.frame()
  data_indicators_corridors = data.frame()
  
  for(i in 1:dim(table_indicators_corridor)[1]){
  
  # for(i in 1:3){
  table_indicators_corridor$link[i] = as.character(table_indicators_corridor$link[i])
  temp_link = as.character(table_indicators_corridor$link[i])
  #Leyendo información de cada uno de los indicadores. 
  
  temp_indicator = readHTMLTable(temp_link,header=T, which=1,stringsAsFactors=F)
  
  temp_colnames = colnames(temp_indicator)
  temp_indicator$Indicator = as.character(table_indicators_corridor$name[i])
  temp_indicator$Category= as.character(table_indicators_corridor$category[i])
  temp_indicator = temp_indicator[c("Indicator","Category",temp_colnames)]
  
  temp_indicator$Country = as.character(temp_indicator$Country)
  
  temp_indicator$Value = as.character(temp_indicator$Value)
  temp_indicator$Value = fill.na(temp_indicator$Value)
  #Remove commas when they are used to separate thousands in numbers
  
  for(j in 1:dim(temp_indicator)[1]){
    #Detect the position the last comma
    if(is.na(temp_indicator$Value[j])==FALSE & str_detect(temp_indicator$Value[j],",")==TRUE & (temp_indicator$Indicator[j] %in% c("Corridor Name","Station boarding level, corridor") ==FALSE))
       {
      # if(is.na(temp_indicator$Value[j])==FALSE & is.numeric(is.na(temp_indicator$Value[j])==TRUE) & str_detect(temp_indicator$Value[j],",")==TRUE){
      
      temp_digits = str_length(temp_indicator$Value[j])-str_locate_all(temp_indicator$Value[j],",")[[1]][length(str_locate_all(temp_indicator$Value[j],",")[[1]])]
      if(temp_digits==3){
        # temp_indicator$Value[i] = as.numeric(str_replace_all(temp_indicator$Value[i],",",""))
        temp_indicator$Value = as.numeric(str_replace_all(temp_indicator$Value,",",""))
        temp_indicator$Value = as.numeric(temp_indicator$Value)
        break
      }
      #Replace commas with . to represent decimal numbers
      else{
        # temp_indicator$Value[i] = str_replace_all(temp_indicator$Value[i],",",".")  
        temp_indicator$Value = str_replace_all(temp_indicator$Value,",",".")
        temp_indicator$Value = as.numeric(temp_indicator$Value)
        break
      }
    }
  }
  
  temp_indicator$Year = as.numeric(temp_indicator$Year)
  temp_indicator$Source= as.character(temp_indicator$Source)
  temp_indicator$Category= as.character(temp_indicator$Category)
  temp_indicator$Timeliness = year(today())-temp_indicator$Year
  
  data_indicators_corridors = rbind(data_indicators_corridors,temp_indicator)
  
  temp_indicator_info_corridor_0 = data.frame(indicator = unique(as.character(temp_indicator$Indicator))
    ,category = unique(as.character(temp_indicator$Category))
    ,obs = dim(temp_indicator)[1]
    ,nas = dim(subset(temp_indicator,is.na(temp_indicator$Value)==TRUE))[1]
    ,no_nas = dim(subset(temp_indicator,is.na(temp_indicator$Value)==FALSE))[1]
    ,availability = round(100*dim(subset(temp_indicator,is.na(temp_indicator$Value)==FALSE))[1]/dim(temp_indicator)[1],1)
    ,value_mean= round(mean(as.numeric(temp_indicator$Value),na.rm=TRUE),1)
    ,value_sd = round(sqrt(var(as.numeric(temp_indicator$Value),na.rm=TRUE)),1)
    ,value_cv = round(sqrt(var(as.numeric(temp_indicator$Value),na.rm=TRUE))/mean(as.numeric(temp_indicator$Value),na.rm=TRUE),2)
    ,timeliness_mean = round(mean(temp_indicator$Timeliness,na.rm=TRUE),1)
    ,timeliness_sd = round(sqrt(var(temp_indicator$Timeliness,na.rm=TRUE)),1)
    ,timeliness_cv = round(sqrt(var(temp_indicator$Timeliness,na.rm=TRUE))/mean(temp_indicator$Timeliness,na.rm=TRUE),1)
    ,n_sources = length(unique(subset(temp_indicator,is.na(temp_indicator$Value)==FALSE)$Source))
    ,homogeneity = round(dim(subset(temp_indicator,is.na(temp_indicator$Value)==FALSE))[1]/length(unique(subset(temp_indicator,is.na(temp_indicator$Value)==FALSE)$Source)),1) #obs_per_source
    # ,index_homogeneity = round(100*1/length(unique(subset(temp_indicator,is.na(temp_indicator$Value)==FALSE)$Source)),1)
    )
  
  temp_indicator_info_corridor = rbind(temp_indicator_info_corridor,temp_indicator_info_corridor_0)
  }
  
  # View(temp_indicator_info_corridor)
#   View(data_indicators_corridors)
# b) Systems  ---------------------------------------------------------------

temp_indicator_info_system = data.frame()
data_indicators_systems = data.frame()
# i=1
for(i in 1:dim(table_indicators_system)[1]){
  
  # for(i in 1:3){
  
  table_indicators_system$link[i] = as.character(table_indicators_system$link[i])
  temp_link = as.character(table_indicators_system$link[i])
  #Leyendo información de cada uno de los indicadores. 
  
  temp_indicator = readHTMLTable(temp_link,header=T, which=1,stringsAsFactors=F)
  
  temp_colnames = colnames(temp_indicator)
  temp_indicator$Indicator = as.character(table_indicators_system$name[i])
  temp_indicator$Category= as.character(table_indicators_system$category[i])
  temp_indicator = temp_indicator[c("Indicator","Category",temp_colnames)]
  
  temp_indicator$Country = as.character(temp_indicator$Country)
  
  temp_indicator$Value = as.character(temp_indicator$Value)
  temp_indicator$Value = fill.na(temp_indicator$Value)
  #Remove commas when they are used to separate thousands in numbers
  
  for(j in 1:dim(temp_indicator)[1]){
    #Detect the position the last comma
    if(is.na(temp_indicator$Value[j])==FALSE 
       & (temp_indicator$Indicator[j] %in% c("System name","Transit agency","Position of with-flow lanes","Station boarding level","Propulsion, standard buses") ==FALSE) 
       & str_detect(temp_indicator$Value[j],",")==TRUE){
      
      temp_digits = str_length(temp_indicator$Value[j])-str_locate_all(temp_indicator$Value[j],",")[[1]][length(str_locate_all(temp_indicator$Value[j],",")[[1]])]
      if(temp_digits==3){
        # temp_indicator$Value[i] = as.numeric(str_replace_all(temp_indicator$Value[i],",",""))
        temp_indicator$Value = as.numeric(str_replace_all(temp_indicator$Value,",",""))
        temp_indicator$Value = as.numeric(temp_indicator$Value)
        break
      }
      #Replace commas with . to represent decimal numbers
      else{
        # temp_indicator$Value[i] = str_replace_all(temp_indicator$Value[i],",",".")  
        temp_indicator$Value = str_replace_all(temp_indicator$Value,",",".")
        temp_indicator$Value = as.numeric(temp_indicator$Value)
        break
      }
    }
  }
  
  temp_indicator$Source= as.character(temp_indicator$Source)
  temp_indicator$Year = as.numeric(temp_indicator$Year)
  temp_indicator$Timeliness = year(today())-temp_indicator$Year
  
  
  data_indicators_systems = rbind(temp_indicator,data_indicators_systems)
  
  temp_indicator_info_system_0 = data.frame(indicator = unique(as.character(temp_indicator$Indicator))
                                     ,category = unique(as.character(temp_indicator$Category))
                                     ,obs = dim(temp_indicator)[1]
                                     ,nas = dim(subset(temp_indicator,is.na(temp_indicator$Value)==TRUE))[1]
                                     ,no_nas = dim(subset(temp_indicator,is.na(temp_indicator$Value)==FALSE))[1]
                                     ,availability = round(100*dim(subset(temp_indicator,is.na(temp_indicator$Value)==FALSE))[1]/dim(temp_indicator)[1],1)
                                     ,value_mean= round(mean(as.numeric(temp_indicator$Value),na.rm=TRUE),1)
                                     ,value_sd = round(sqrt(var(as.numeric(temp_indicator$Value),na.rm=TRUE)),1)
                                     ,value_cv = round(sqrt(var(as.numeric(temp_indicator$Value),na.rm=TRUE))/mean(as.numeric(temp_indicator$Value),na.rm=TRUE),1)
                                     ,timeliness_mean = round(mean(temp_indicator$Timeliness,na.rm=TRUE),1)
                                     ,timeliness_sd = round(sqrt(var(temp_indicator$Timeliness,na.rm=TRUE)),1)
                                     ,timeliness_cv = round(sqrt(var(temp_indicator$Timeliness,na.rm=TRUE))/mean(temp_indicator$Timeliness,na.rm=TRUE),1)
                                     ,n_sources = length(unique(subset(temp_indicator,is.na(temp_indicator$Value)==FALSE)$Source))
                                     ,homogeneity = round(dim(subset(temp_indicator,is.na(temp_indicator$Value)==FALSE))[1]/length(unique(subset(temp_indicator,is.na(temp_indicator$Value)==FALSE)$Source)),1) #obs_per_source
                                     # ,index_homogeneity = round(100*1/length(unique(subset(temp_indicator,is.na(temp_indicator$Value)==FALSE)$Source)),1)
  )
  temp_indicator_info_system = rbind(temp_indicator_info_system,temp_indicator_info_system_0)
}

View(temp_indicator_info_system)

# 3) STATISTICS BY REGION ----------------------------------------------------
# 1) Corridors ---------------------------------------------------------------

#Observations by country
country_obs = as.data.frame(table(data_indicators_corridors$Country)); colnames(country_obs) = c("country","obs")
data_indicators_corridors$Country = as.character(data_indicators_corridors$Country)

#Missing Data 
data_indicators_corridors$nas = 0
data_indicators_corridors$nas[is.na(data_indicators_corridors$Value)==TRUE] = 1
temp = as.data.frame(tapply(data_indicators_corridors$nas,data_indicators_corridors$Country,FUN=sum)); colnames(temp) = c("nas")
country_nas = data.frame(country = row.names(temp),nas = temp$nas)
# country_no_nas = data.frame(country = row.names(temp),nas = temp$no_nas)

#Timeliness
temp = as.data.frame(tapply(subset(data_indicators_corridors,is.na(Year)==FALSE)$Year,subset(data_indicators_corridors,is.na(Year)==FALSE)$Country,FUN=mean)); colnames(temp) = c("average_year")
country_timeliness = data.frame(country = row.names(temp),timeliness = round(year(today())-temp$average_year,1))
row.names(country_timeliness)= NULL

#Homogeneity
temp = tapply(subset(data_indicators_corridors,is.na(Year)==FALSE)$Source,subset(data_indicators_corridors,is.na(Year)==FALSE)$Country,FUN=unique)
country_n_sources = data.frame()
country_homogeneity = data.frame()
for(i in 1:dim(temp)[1]){
  country_n_sources_0 = data.frame(country = as.character(names(temp[i])),n_sources = as.numeric(dim(as.data.frame(temp[i]))[1]))
  country_homogeneity_0 = data.frame(country = as.character(names(temp[i])),homogeneity = round(100*1/as.numeric(dim(as.data.frame(temp[i]))[1]),1))
  country_n_sources = rbind(country_n_sources,country_n_sources_0)
  country_homogeneity = rbind(country_homogeneity,country_homogeneity_0)
}

#Merging
corridors_statistics_country = country_obs
corridors_statistics_country = merge(corridors_statistics_country,country_nas,by="country") #NAS
corridors_statistics_country$no_nas = with(corridors_statistics_country,obs-nas) #NO NAS
corridors_statistics_country$availability = round(with(corridors_statistics_country,1-nas/obs)*100,1) #Availability
corridors_statistics_country = merge(corridors_statistics_country,country_timeliness,by="country") #Timeliness
corridors_statistics_country = merge(corridors_statistics_country,country_homogeneity,by="country") #homogeneity
corridors_statistics_country$homogeneity = round(with(corridors_statistics_country,homogeneity*no_nas/100),1)
# 2) Systems -----------------------------------------------------------------

#Observations by country
country_obs = as.data.frame(table(data_indicators_systems$Country)); colnames(country_obs) = c("country","obs")
data_indicators_systems$Country = as.character(data_indicators_systems$Country)

#Missing Data 
data_indicators_systems$nas = 0
data_indicators_systems$nas[is.na(data_indicators_systems$Value)==TRUE] = 1
temp = as.data.frame(tapply(data_indicators_systems$nas,data_indicators_systems$Country,FUN=sum)); colnames(temp) = c("nas")
country_nas = data.frame(country = row.names(temp),nas = temp$nas)

#Timeliness
temp = as.data.frame(tapply(subset(data_indicators_systems,is.na(Year)==FALSE)$Year,subset(data_indicators_systems,is.na(Year)==FALSE)$Country,FUN=mean)); colnames(temp) = c("average_year")
country_timeliness = data.frame(country = row.names(temp),timeliness = round(year(today())-temp$average_year,1))
row.names(country_timeliness)= NULL

#Homogeneity
temp = tapply(subset(data_indicators_systems,is.na(Year)==FALSE)$Source,subset(data_indicators_systems,is.na(Year)==FALSE)$Country,FUN=unique)
country_n_sources = data.frame()
country_homogeneity = data.frame()
for(i in 1:dim(temp)[1]){
  country_n_sources_0 = data.frame(country = as.character(names(temp[i])),n_sources = as.numeric(dim(as.data.frame(temp[i]))[1]))
  country_homogeneity_0 = data.frame(country = as.character(names(temp[i])),homogeneity = round(100*1/as.numeric(dim(as.data.frame(temp[i]))[1]),1))
  country_n_sources = rbind(country_n_sources,country_n_sources_0)
  country_homogeneity = rbind(country_homogeneity,country_homogeneity_0)
}

#Merging
systems_statistics_country = country_obs
systems_statistics_country = merge(systems_statistics_country,country_nas,by="country") #NAS
systems_statistics_country$no_nas = with(systems_statistics_country,obs-nas) #No NAS
systems_statistics_country$availability = round(with(systems_statistics_country,1-nas/obs)*100,1) #Availability
systems_statistics_country = merge(systems_statistics_country,country_timeliness,by="country") #Timeliness
systems_statistics_country = merge(systems_statistics_country,country_homogeneity,by="country") #homogeneity
corridors_statistics_country$homogeneity = round(with(systems_statistics_country,homogeneity*no_nas/100),1)
View(systems_statistics_country)

# 4) STATISTICS BY INDICATOR'S CATEGORY -------------------------------------

# a) Corridors ------------------------------------------------------------


# table_indicators_corridor = merge(table_indicators_corridor,category_corridor_indicators[c("name","category")],by="name")

#Observations by category
category_obs = as.data.frame(table(data_indicators_corridors$Category)); colnames(category_obs) = c("category","obs")
# data_indicators_corridors$category = as.character(data_indicators_corridors$Category)

#Missing Data 
data_indicators_corridors$nas = 0
data_indicators_corridors$nas[is.na(data_indicators_corridors$Value)==TRUE] = 1
temp = as.data.frame(tapply(data_indicators_corridors$nas,data_indicators_corridors$Category,FUN=sum)); colnames(temp) = c("nas")
category_nas = data.frame(category = row.names(temp),nas = temp$nas)

#Timeliness
temp = as.data.frame(tapply(subset(data_indicators_corridors,is.na(Year)==FALSE)$Year,subset(data_indicators_corridors,is.na(Year)==FALSE)$Category,FUN=mean)); colnames(temp) = c("average_year")
category_timeliness = data.frame(category = row.names(temp),timeliness = round(year(today())-temp$average_year,1))
row.names(category_timeliness)= NULL

#Homogeneity
temp = tapply(subset(data_indicators_corridors,is.na(Year)==FALSE)$Source,subset(data_indicators_corridors,is.na(Year)==FALSE)$Category,FUN=unique)
category_n_sources = data.frame()
category_homogeneity = data.frame()
for(i in 1:dim(temp)[1]){
  category_n_sources_0 = data.frame(category = as.character(names(temp[i])),n_sources = as.numeric(dim(as.data.frame(temp[i]))[1]))
  category_homogeneity_0 = data.frame(category = as.character(names(temp[i])),homogeneity = round(100*1/as.numeric(dim(as.data.frame(temp[i]))[1]),1))
  category_n_sources = rbind(category_n_sources,category_n_sources_0)
  category_homogeneity = rbind(category_homogeneity,category_homogeneity_0)
}

#Merging
corridors_statistics_category = category_obs
corridors_statistics_category = merge(corridors_statistics_category,category_nas,by="category") #NAS
corridors_statistics_category$no_nas = with(corridors_statistics_category,obs-nas) #No NAS
corridors_statistics_category$availability = round(with(corridors_statistics_category,1-nas/obs)*100,1) #Availability
corridors_statistics_category = merge(corridors_statistics_category,category_timeliness,by="category") #Timeliness
corridors_statistics_category = merge(corridors_statistics_category,category_homogeneity,by="category") 
corridors_statistics_category$homogeneity = round(with(corridors_statistics_category,homogeneity*no_nas/100),1)
# b) System ---------------------------------------------------------------

#Observations by category
category_obs = as.data.frame(table(data_indicators_systems$Category)); colnames(category_obs) = c("category","obs")
# data_indicators_systems$category = as.character(data_indicators_systems$Category)

#Missing Data 
data_indicators_systems$nas = 0
data_indicators_systems$nas[is.na(data_indicators_systems$Value)==TRUE] = 1
temp = as.data.frame(tapply(data_indicators_systems$nas,data_indicators_systems$Category,FUN=sum)); colnames(temp) = c("nas")
category_nas = data.frame(category = row.names(temp),nas = temp$nas)

#Timeliness
temp = as.data.frame(tapply(subset(data_indicators_systems,is.na(Year)==FALSE)$Year,subset(data_indicators_systems,is.na(Year)==FALSE)$Category,FUN=mean)); colnames(temp) = c("average_year")
category_timeliness = data.frame(category = row.names(temp),timeliness = round(year(today())-temp$average_year,1))
row.names(category_timeliness)= NULL

#Homogeneity
temp = tapply(subset(data_indicators_systems,is.na(Year)==FALSE)$Source,subset(data_indicators_systems,is.na(Year)==FALSE)$Category,FUN=unique)
category_n_sources = data.frame()
category_homogeneity = data.frame()
for(i in 1:dim(temp)[1]){
  category_n_sources_0 = data.frame(category = as.character(names(temp[i])),n_sources = as.numeric(dim(as.data.frame(temp[i]))[1]))
  category_homogeneity_0 = data.frame(category = as.character(names(temp[i])),homogeneity = round(100*1/as.numeric(dim(as.data.frame(temp[i]))[1]),1))
  category_n_sources = rbind(category_n_sources,category_n_sources_0)
  category_homogeneity = rbind(category_homogeneity,category_homogeneity_0)
}

#Merging
systems_statistics_category = category_obs
systems_statistics_category = merge(systems_statistics_category,category_nas,by="category") #NAS
systems_statistics_category$no_nas = with(systems_statistics_category,obs-nas) #No NAS
systems_statistics_category$availability = round(with(systems_statistics_category,1-nas/obs)*100,1) #Availability
systems_statistics_category = merge(systems_statistics_category,category_timeliness,by="category") #Timeliness
systems_statistics_category = merge(systems_statistics_category,category_homogeneity,by="category") #homogeneity
systems_statistics_category$homogeneity = round(with(systems_statistics_category,homogeneity*no_nas/100),1) #homogeneity

# 5) EXPORT ------------------------------------------------------------------

# a) Corridors ---------------------------------------------------------------
write.csv(table_indicators_corridor,file = str_c(data.base,"/output/table_indicators_corridor.csv"),row.names = FALSE)
write.csv(table_indicators_system,file = str_c(data.base,"/output/table_indicators_system.csv"),row.names = FALSE)
write.csv(temp_indicator_info_corridor,file = str_c(data.base,"/output/corridors_statistics_indicators.csv"),row.names = FALSE)

write.csv(corridors_statistics_country,file = str_c(data.base,"/output/corridors_statistics_country.csv"),row.names = FALSE)
write.csv(corridors_statistics_category,file = str_c(data.base,"/output/corridors_statistics_category.csv"),row.names = FALSE)
# b) Systems -----------------------------------------------------------------
write.csv(data_indicators_systems,file = str_c(data.base,"/output/table_indicators_corridor.csv"),row.names = FALSE)
write.csv(data_indicators_systems,file = str_c(data.base,"/output/nalysis_system_indicators_corridor.csv"),row.names = FALSE)
write.csv(temp_indicator_info_system,file = str_c(data.base,"/output/systems_statistics_indicators.csv"),row.names = FALSE)

write.csv(systems_statistics_country,file = str_c(data.base,"/output/systems_statistics_country.csv"),row.names = FALSE)
write.csv(systems_statistics_category,file = str_c(data.base,"/output/systems_statistics_category.csv"),row.names = FALSE)







