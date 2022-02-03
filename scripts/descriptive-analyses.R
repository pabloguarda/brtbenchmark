
# DESCRIPTIVE ANALYSYS AND GRAPHS----------------------------------------------------

# 1) Benchmarking ---------------------------------------------------------

# - Sample description ---------------------------------------------------

#Lista de todos los paises que fueron medidos en los dos periodos 
country_list = unique(rbind(data.frame(country = unique(scores2013$country)),data.frame(country = unique(scores2014$country))))

#Lista de los paises que fueron medidos in each year
country_year_list = rbind(cbind(country = country_list,year = "2013"), cbind(country = country_list,year = "2014"))

#List of all systems
system_list = unique(rbind(data.frame(system = unique(scores2013$system)),data.frame(system = unique(scores2014$system))))

#List of all cities
city_list = unique(rbind(data.frame(city = unique(scores2013$city)),data.frame(city = unique(scores2014$city))))

#List of all corridors
corridor_list = unique(rbind(data.frame(corridor_name = unique(scores2013$corridor)),data.frame(corridor_name = unique(scores2014$corridor))))


# 
# #Total length of corridors by country and year
# list_temp = c("country","corridor_length")
# 
# #2013
# corridorslength2013_country= aggregate(scores_table2013["corridor_length"],
#                                    by=list(country = scores_table2013$country)
#                                    ,FUN=function(x) round(sum(x),1))
# 
# temp = merge(corridorslength2013_country,
#       aggregate(scores_table2013["corridor_length"],by=list(country = scores_table2013$country),FUN=function(x) round(length(x),1)),by = "country"
# )
# 
# corridorslength2013_country =rename(corridorslength2013_country,c(corridor_length="corridors_length"))
# corridorslength2013_country$proportion = round(100*corridorslength2013_country$corridors_length/sum(corridorslength2013_country$corridors_length),1)
# 
# #2014
# corridorslength2014_country= aggregate(scores_table2014["corridor_length"],
#                                       by=list(country = scores_table2014$country)
#                                       ,FUN=function(x) round(sum(x),1))
# 
# corridorslength2014_country =rename(corridorslength2014_country,c(corridor_length="corridors_length"))
# corridorslength2014_country$proportion = round(100*corridorslength2014_country$corridors_length/sum(corridorslength2014_country$corridors_length),1)
# 
# #Total length of corridors, number of corridors adding 2013 and 2014
# 
# temp = rbind(corridorslength2013_country,corridorslength2014_country)
# 
# corridorslength_country= aggregate(temp$corridors_length,
#                                        by=list(country = temp$country)
#                                        ,FUN=function(x) round(sum(x),1))
# corridorslength_country =rename(corridorslength_country,c(x="corridors_length"))
# corridorslength_country$proportion = round(100*corridorslength_country$corridors_length/sum(corridorslength_country$corridors_length),1)
# 
# 
# corridorslength2013_country
# corridorslength2014_country
# corridorslength_country


# temp = c(country,corridors_length)
# 
# corridorslength2014 =c(country,corridors_length)
# - Global Context ----------------------------------------------------------

var_list = c("year","country","city","corridor_length","a","b","c","d","e","f","g","total_score") 

scores_table2013 = scores2013[var_list]
scores_table2014 = scores2014[var_list]
Hmisc::label(scores_table2013)
#Puntajes promedio por país en cada item evlauado por ITDP en 2013
scores2013_mean_country= aggregate(scores_table2013[-which(names(scores_table2013)=="country")],
          by=list(country = scores_table2013$country)
          ,FUN=function(x) round(mean(x),1))

scores2014_mean_country= aggregate(scores_table2014[-which(names(scores_table2014)=="country")],
                                   by=list(country = scores_table2014$country)
                                   ,FUN=function(x) round(mean(x),1))

#Solo los puntajes totales (no por item)
total_scores2013_mean_country= aggregate(scores_table2013[c("total_score")],
                                   by=list(country = scores_table2013$country)
                                   ,FUN=function(x) round(mean(x),1))


# total_scores2013_mean_country =rename(total_scores2013_mean_country,c(total_score="score"))

total_scores2014_mean_country= aggregate(scores_table2014[c("total_score")],
                                   by=list(country = scores_table2014$country)
                                   ,FUN=function(x) round(mean(x),1))

# total_scores2014_mean_country =rename(total_scores2014_mean_country,c(total_score="total_score"))

table_total_scores = merge(total_scores2013_mean_country,total_scores2014_mean_country,by="country",all=TRUE)
colnames(table_total_scores) = c("country","score2013","score2014")

table_total_scores$delta = with(table_total_scores,score2014-score2013)

View(table_total_scores)


temp1 = merge(subset(country_year_list,year=="2013"), total_scores2013_mean_country, by = c("country"),all=TRUE)
temp2 = merge(subset(country_year_list,year=="2014"), total_scores2014_mean_country, by = c("country"),all=TRUE)

table_total_scores = rbind(temp1,temp2)

table_total_scores = table_total_scores[order(as.character(table_total_scores$country)),]

# View(table_total_scores)
# - Comparisons between China and other countries by year -----------------------------------------------------------

scores_table2013_China = subset(scores_table2013,country == "China")
scores_table2014_China = subset(scores_table2014,country == "China")

scores_table2013_noChina = subset(scores_table2013,country != "China")
scores_table2014_noChina = subset(scores_table2014,country != "China")


#Average values in china by item evaluated

#2013
var_list = c("a","b","c","d","e","f","g","total_score","corridor_length")

table0 = Hmisc::label(scores_table2013)[var_list]

#Simple average
table1 = t(data.frame(lapply(as.list(scores_table2013_China)[var_list],mean))) 
#Weighted average by the corridor length
table1wav = t(data.frame(lapply(as.list(scores_table2013_China)[var_list],FUN=function(x) sum(x*scores_table2013_China$corridor_length/sum(scores_table2013_China$corridor_length)))))

table2 = t(data.frame(lapply(as.list(scores_table2013_noChina)[var_list],mean)))
table2wav = t(data.frame(lapply(as.list(scores_table2013_noChina)[var_list],FUN=function(x) sum(x*scores_table2013_noChina$corridor_length/sum(scores_table2013_noChina$corridor_length)))))

china_world2013 = as.data.frame(cbind(indicator = table0, china = round(table1,1),world = round(table2,1),diff = round((table1-table2)/table2*100,1)))
china_world2013 = china_world2013[-which(row.names(china_world2013)=="corridor_length"),]
colnames(china_world2013) = c("indicator2013","average_china","average_world","average_diff")

#Weighted average
china_world2013_wav = as.data.frame(cbind(indicator = table0, china = round(table1,1),world = round(table2wav,1),diff = round((table1wav-table2wav)/table2wav*100,1)))
china_world2013_wav = china_world2013_wav[-which(row.names(china_world2013_wav)=="corridor_length"),]
colnames(china_world2013_wav) = c("indicator2013","weightav_china","weightav_world","weightav_diff")


#2014
var_list = c("a","b","c","d","e","f","g","total_score","corridor_length")

table0 = Hmisc::label(scores_table2014)[var_list]

#Simple average
table1 = t(data.frame(lapply(as.list(scores_table2014_China)[var_list],mean)))
#Weighted average by the corridor length
table1wav = t(data.frame(lapply(as.list(scores_table2014_China)[var_list],FUN=function(x) sum(x*scores_table2014_China$corridor_length/sum(scores_table2014_China$corridor_length)))))

table2 = t(data.frame(lapply(as.list(scores_table2014_noChina)[var_list],mean)))
table2wav = t(data.frame(lapply(as.list(scores_table2014_noChina)[var_list],FUN=function(x) sum(x*scores_table2014_noChina$corridor_length/sum(scores_table2014_noChina$corridor_length)))))

china_world2014 = as.data.frame(cbind(indicator = table0, china = round(table1,1),world = round(table2,1),diff = round((table1-table2)/table2*100,1)))
china_world2014 = china_world2014[-which(row.names(china_world2014)=="corridor_length"),]
colnames(china_world2014) = c("indicator2014","average_china","average_world","average_diff")

#Weighted average
china_world2014_wav = as.data.frame(cbind(indicator = table0, china = round(table1,1),world = round(table2wav,1),diff = round((table1wav-table2wav)/table2wav*100,1)))
china_world2014_wav = china_world2014_wav[-which(row.names(china_world2014_wav)=="corridor_length"),]
colnames(china_world2014_wav) = c("indicator2014","weightav_china","weightav_world","weightav_diff")
# - China context -----------------------------------------------------------

#Chinese cities. 

dim(unique(scores_table2013_China[c("city")]))
dim(unique(scores_table2014_China[c("city")]))
scores_table2014_China

dim(scores_table2013_China)
dim(scores_table2014_China)
# - Differences in scores by item (mixing data from 2013 and 2014) -------------------------------------------

# temp_list = c("average_china","average_world","average_diff")
temp_list = c("average_diff")

#Simple average
data.frame(indicator2014=china_world2014$indicator2014,diff2013 = china_world2013$average_diff, diff2014 = china_world2014$average_diff)

#Weighted average
data.frame(indicator2014_wav=china_world2014_wav$indicator2014,diff2013 = china_world2013_wav$weightav_diff, diff2014 = china_world2014_wav$weightav_diff)


china_world2013[temp_list]
# - Comparison by City ---------------------------------------------------

temp =data.frame(city=row.names(as.data.frame(tapply(scores$corridor_length,scores$city,sum))),
                 corridor_length = as.numeric(tapply(scores$corridor_length,scores$city,sum)))
                 
# merge(data.frame(city=unique(scores$city)),scores[c("country","city")],by="city",all.x=TRUE)

temp = merge(table_wav_scores,temp,by="city")
temp = merge(temp,unique(scores[c("country","city")]),by="city")
temp = temp[,c(length(temp),seq(0,length(temp)-1))] #

#Transform score to percentage

temp[c("a","b","c","d","e","f","g")] = round(temp[c("a","b","c","d","e","f","g")],1)
# temp$score = round(temp$score/100,2)
# temp$total_score = round(temp$total_score/100,2)
temp = temp[order(str_c(temp$country,temp$city)),]
temp = temp[c("country","city","corridor_length","a","b","c","d","e","f","g","score","total_score")]
suppressWarnings(write.csv(Var.names(Encoding.UTF8(temp)),file = str_c(data.base,"/output/scores-itdp-cities.csv"),row.names = FALSE,fileEncoding = "ISO-8859-1"))
# - Comparison by Country ---------------------------------------------------

temp =data.frame(country=row.names(as.data.frame(tapply(scores$corridor_length,scores$country,sum))))

# merge(data.frame(city=unique(scores$city)),scores[c("country","city")],by="city",all.x=TRUE)

temp = merge(table_wav_scores_country,temp,by="country")
temp = merge(temp,unique(scores[c("country")]),by="country")
temp = temp[,c(length(temp),seq(0,length(temp)-1))] #

#Transform score to percentage

temp[c("a","b","c","d","e","f","g")] = round(temp[c("a","b","c","d","e","f","g")],1)
# temp$score = round(temp$score/100,2)
# temp$total_score = round(temp$total_score/100,2)
temp = temp[order(str_c(temp$country)),]
temp = temp[c("country","corridor_length","a","b","c","d","e","f","g","score","total_score")]

#The last row represents the statistics for all the BRT corridors in the world 
# temp = rbind(temp,append(data.frame(country="World",corridor_length = sum(scores$corridor_length)),table_wav_scores_world))

suppressWarnings(write.csv(Var.names(Encoding.UTF8(temp)),file = str_c(data.base,"/output/scores-itdp-countries.csv"),row.names = FALSE,fileEncoding = "ISO-8859-1"))
# - World Indicators -----------------------------------------------------
# - Specific indicators -----------------------------------------------------
# - Productivity BRTdata ----------------------------------------------------

#System

#China
mean(with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$productivity)==FALSE),productivity))
#Rest of the world
mean(with(subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$productivity)==FALSE),productivity))

brt_systems$ones = 1

mean_productivity_length = with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$productivity)==FALSE),wtd.mean(productivity,system_length))
sd_productivity_length = sqrt(with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$productivity)==FALSE),wtd.var(productivity,system_length)))
mean_productivity_demand = with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$productivity)==FALSE),wtd.mean(productivity,demand))
sd_productivity_demand = sqrt(with(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$productivity)==FALSE),wtd.var(productivity,demand)))

#SYSTEM
#Weighting by demand
wtd.t.test(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$productivity)==FALSE)$productivity,
           subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$productivity)==FALSE)$productivity,
           weight=subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$productivity)==FALSE)$demand,
           weighty=subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$productivity)==FALSE)$demand)

#Weighting by system length
wtd.t.test(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$productivity)==FALSE)$productivity,
           subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$productivity)==FALSE)$productivity,
           weight=subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$productivity)==FALSE)$system_length,
           weighty=subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$productivity)==FALSE)$system_length)

#Weighting equally
wtd.t.test(subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$productivity)==FALSE)$productivity,
           subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$productivity)==FALSE)$productivity,
           weight=subset(brt_systems,brt_systems$country=="China" & is.na(brt_systems$productivity)==FALSE)$one,
           weighty=subset(brt_systems,brt_systems$country!="China" & is.na(brt_systems$productivity)==FALSE)$one,
           )



# 2) Metro -------------------------------------------------------------------

# Metro and BRT corridors Length by year ------------------------------------------------------------

km_brt_year = subset(brt_corridors,corridor_length>0 & year_corridor_commenced>0)
km_brt_year= cbind(km_brt_year,corridor_length_year = ave(km_brt_year$corridor_length,km_brt_year$year_corridor_commenced,FUN = sum))
km_brt_year = unique(km_brt_year[c("year_corridor_commenced","corridor_length_year")])
km_brt_year$year_corridor_commenced = as.numeric(km_brt_year$year_corridor_commenced)
class(km_brt_year$year_corridor_commenced)

km_metro_year = subset(metro,metro_length>0 & year_metro_commenced>0)
km_metro_year= cbind(km_metro_year,metro_length_year = ave(km_metro_year$metro_length,km_metro_year$year_metro_commenced,FUN = sum))
km_metro_year = unique(km_metro_year[c("year_metro_commenced","metro_length_year")])
km_metro_year$year_metro_commenced = as.numeric(km_metro_year$year_metro_commenced)

km_year = data.frame(year=seq(min(min(brt_corridors$year_corridor_commenced[is.na(brt_corridors$year_corridor_commenced)==FALSE]),
        min(metro$year_metro_commenced[is.na(metro$year_metro_commenced)==FALSE])),
    max(max(brt_corridors$year_corridor_commenced[is.na(brt_corridors$year_corridor_commenced)==FALSE]),
        max(metro$year_metro_commenced[is.na(metro$year_metro_commenced)==FALSE]))
))
km_year$year = as.numeric(km_year$year)

km_year = merge(km_year,km_brt_year,by.x="year",by.y="year_corridor_commenced",all.x=TRUE)
km_year = merge(km_year,km_metro_year,by.x="year",by.y="year_metro_commenced",all.x=TRUE)
km_year[is.na(km_year)] = 0

save(km_year,file=str_c(data.base,"/output/Km_year.Rdata"))
write.csv(km_year,file = str_c(data.base,"/output/Km_year.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

#China

km_brt_year_china = subset(brt_corridors,corridor_length>0 & year_corridor_commenced>0 & country=="China")
km_brt_year_china= cbind(km_brt_year_china,corridor_length_year = ave(km_brt_year_china$corridor_length,km_brt_year_china$year_corridor_commenced,FUN = sum))
km_brt_year_china = unique(km_brt_year_china[c("year_corridor_commenced","corridor_length_year")])
km_brt_year_china$year_corridor_commenced = as.numeric(km_brt_year_china$year_corridor_commenced)

km_metro_year_china = subset(metro,metro_length>0 & year_metro_commenced>0 & country =="China")
km_metro_year_china= cbind(km_metro_year_china,metro_length_year = ave(km_metro_year_china$metro_length,km_metro_year_china$year_metro_commenced,FUN = sum))
km_metro_year_china = unique(km_metro_year_china[c("year_metro_commenced","metro_length_year")])
km_metro_year_china$year_metro_commenced = as.numeric(km_metro_year_china$year_metro_commenced)


km_year_china = data.frame(year=seq(min(min(brt_corridors$year_corridor_commenced[is.na(brt_corridors$year_corridor_commenced)==FALSE & brt_corridors$country == "China"]),
                                  min(metro$year_metro_commenced[is.na(metro$year_metro_commenced)==FALSE& metro$country == "China"])),
                              max(max(brt_corridors$year_corridor_commenced[is.na(brt_corridors$year_corridor_commenced)==FALSE& brt_corridors$country == "China"]),
                                  max(metro$year_metro_commenced[is.na(metro$year_metro_commenced)==FALSE& metro$country == "China"] ))
))
km_year_china$year = as.numeric(km_year_china$year)

km_year_china = merge(km_year_china,km_brt_year_china,by.x="year",by.y="year_corridor_commenced",all.x=TRUE)
km_year_china = merge(km_year_china,km_metro_year_china,by.x="year",by.y="year_metro_commenced",all.x=TRUE)
km_year_china[is.na(km_year_china)] = 0

scores2013 =rename(scores2013,c(QUALITY_OF_SERVICE_PASSENGER_INFORMATION_SYSTEMS="e"))

save(km_year_china,file=str_c(data.base,"/output/Km_year_China.Rdata"))
write.csv(km_year_china,file = str_c(data.base,"output/Km_year_China.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")


km_year_china = rename(km_year_china,c(corridor_length_year="corridor_length_year_china"))
km_year_china = rename(km_year_china,c(metro_length_year="metro_length_year_china"))


#Joining the two databases

km_year = merge(km_year,km_year_china,by="year",all.x=TRUE)
# km_year[is.na(km_year)==TRUE] = 0

#Proportion Metro in China
km_year_china$proportion_metro = with(km_year_china,metro_length_year_/(corridor_length_year+metro_length_year))

save(km_year,file= str_c(data.base,"/output/Km_year.Rdata"))
write.csv(km_year,file = str_c(data.base,"output/Km_year.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")



# 2) Extras ---------------------------------------------------------------

brt_systems$fare_ratio = round(with(brt_systems,bus_fare/(gdp/365))*100,4)

# View(data.frame(1:dim(brt_systems)[1],brt_systems[order(brt_systems$fare_ratio,decreasing = TRUE),])))




# 3) BRTData.ORG ----------------------------------------------------------


# Total length of bus priority corridors ----------------------------------
sum(brt_corridors$da,na.rm = TRUE)

# Daily Demand in the World -----------------------------------------------

sum(brt_corridors$corridor_length,na.rm = TRUE)

# Number of cities with BRT -----------------------------------------------
length(unique(brt_corridors$city))





# Length of Bus Priority Corridors per country ----------------------------

temp = subset(brt_corridors,is.na(corridor_length)==FALSE)

# View(tapply(temp$corridor_length,temp$country,sum))

#Porcentaje para los cuales no hay información 
temp_na = subset(brt_corridors,is.na(corridor_length)==TRUE)
tapply(temp_na$corridor_length,temp_na$country,FUN=function(x) length(x))






