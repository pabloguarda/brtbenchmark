
# TABLEAU -----------------------------------------------------------------

# - BRT systems -----------------------------------------------

brt_systems1 = brt_systems[c("continent","country","city","demand","system_length","productivity","speed","station_spacing")]
save(brt_systems1,file=str_c(data.base,"output/system_analysis.Rdata"))
write.csv(brt_systems1,file = str_c(data.base,"output/system_analysys.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

# ITDP --------------------------------------------------------------------
# write.csv(Var.names(Encoding.UTF8(temp)),file = str_c(project,"/Databases/scoresITDP.csv"),row.names = FALSE,fileEncoding = "ISO-8859-1")

# Scores ------------------------------------------------------------------
save(scores,file=str_c(data.base,"output/scoresITDP.Rdata"))
write.csv(scores,file = str_c(data.base,"output/scoresITDP.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

#Data from 2014
write.csv(scores2014,file = str_c(data.base,"output/scores2014ITDP.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")



# Metro -------------------------------------------------------------------
save(metro,file=str_c(data.base,"output/Metro.Rdata"))
write.csv(metro,file = str_c(data.base,"output/Metro.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

# Strengths and Weaknesses ------------------------------------------------

letters = c("a","b","c","d","e","f","g","score","total_score")
# letters = c("a","b","c","d","e","f","g")
list_test_variables = c(itdp$ITDP2014,letters)

group_variable = "country"
group_value= "China"
# weight_variable = "corridor_length"
weight_variable =NA
alternative = "two.tailed"

significance=0.05

# - Per City ----------------------------------------------------------------
data = itdp_cities
unit=c("country","city","system")
# - Per Corridor ------------------------------------------------------------

# data = subset(scores,corridor_length<=40)
data =scores
unit=c("country","city","system","corridor")
# - Per corridor and year -------------------------------------------------

strengths_weaknesses_China_year$dimension = NA
strengths_weaknesses_China_year$dimension[strengths_weaknesses_China_year$id %in% c("a","b","c","d","e","f","g")] = "Category"
strengths_weaknesses_China_year$dimension[strengths_weaknesses_China_year$id %in% c("a","b","c","d","e","f","g") == FALSE] = "Sub-Category"
strengths_weaknesses_China_year$dimension[strengths_weaknesses_China_year$id %in% c("total_score","score")] = "Net Score"
# strengths_weaknesses_China_year$dimension[strengths_weaknesses_China_year$id %in% itdp$ITDP2014[str_count(itdp$ITDP2014,"g")==1]] = "Point Deductions"

strengths_weaknesses_China_year = merge(strengths_weaknesses_China_year,itdp2014[c("id","item")],by.x="id_variable",by.y ="id")
strengths_weaknesses_China_year= rename(strengths_weaknesses_China_year,c(item="category"))

# strengths_weaknesses_China_year$dimension[strengths_weaknesses_China_year$dimension=="Point Deductions"] = "Net Score"

strengths_weaknesses_China_year = merge(strengths_weaknesses_China_year,strengths_weaknesses,all.x=TRUE)

save(strengths_weaknesses_China_year,file=str_c(data.base,"output/strengths_weaknesses_China_year.Rdata"))
write.csv(strengths_weaknesses_China,file = str_c(data.base,"output/strengths_weaknesses_China_year.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

# Methods -----------------------------------------------------------------
# - T-sample test Output Tableau ---------------------------------------------------------

list_strengths = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                    ,alternative,significance)$strengths

list_weaknesses = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                     ,alternative,significance)$weaknesses


list_nodifference = weakness.strengths(list_test_variables, data,group_variable,group_value,weight_variable
                                       ,alternative,significance)$no_difference

strengths_weaknesses_China = rbind(data.frame(type="strengths",list_strengths),data.frame(type="weaknesses",list_weaknesses),data.frame(type="no_difference",list_nodifference))
# - Anova test Output Tableau-------------------------------------------------------------------

list_strengths = weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$strengths
list_weaknesses= weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$weaknesses
list_nodifference = weakness.strengths.anova(list_test_variables, data,group_variable,group_value,significance=significance)$no_difference

strengths_weaknesses_China = rbind(data.frame(type="strengths",list_strengths),data.frame(type="weaknesses",list_weaknesses),data.frame(type="no_difference",list_nodifference))

# Final Routine -----------------------------------------------------------

strengths_weaknesses_China$dimension = NA
strengths_weaknesses_China$dimension[strengths_weaknesses_China$id %in% letters] = "Category"
strengths_weaknesses_China$dimension[strengths_weaknesses_China$id %in% itdp$ITDP2014] = "Sub-Category"
strengths_weaknesses_China$dimension[strengths_weaknesses_China$id %in% c("total_score","score")] = "Net Score"
strengths_weaknesses_China$dimension[strengths_weaknesses_China$id %in% itdp$ITDP2014[str_count(itdp$ITDP2014,"g")==1]] = "Point Deductions"

strengths_weaknesses_China = merge(strengths_weaknesses_China,itdp2014[c("id","item")],by.x="id_variable",by.y ="id")
strengths_weaknesses_China= rename(strengths_weaknesses_China,c(item="category"))

save(strengths_weaknesses_China,file=str_c(data.base,"output/strengths_weaknesses_China.Rdata"))
write.csv(strengths_weaknesses_China,file = str_c(data.base,"output/strengths_weaknesses_China.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

# Trade off analysis ------------------------------------------------------



# dim(subset(strengths_weaknesses_China_year,year=="2013-2014"))

# ITDP Cities -------------------------------------------------

save(itdp_cities,file=str_c(data.base,"output/ITDPcities.Rdata"))


# Scores by cities --------------------------------------------------------

temp = itdp_cities[c("country","city","corridor_length","a","b","c","d","e","f","g","score","total_score")] ; temp = temp[order(temp$country),]
temp[c("a","b","c","d","e","f","g","score","total_score")] = round(temp[c("a","b","c","d","e","f","g","score","total_score")],1)
write.csv(Var.names(Encoding.UTF8(temp)),file = str_c(data.base,"output/scoresbycities.csv"),row.names = FALSE,fileEncoding = "ISO-8859-1")


# Scores by country -------------------------------------------------------

temp = itdp_countries[c("country","corridor_length","a","b","c","d","e","f","g","score","total_score")] ; temp = temp[order(temp$country),]
temp[c("a","b","c","d","e","f","g","score","total_score")] = round(temp[c("a","b","c","d","e","f","g","score","total_score")],1)
write.csv(Var.names(Encoding.UTF8(temp)),file = str_c(data.base,"output/coresbycountries.csv"),row.names = FALSE,fileEncoding = "ISO-8859-1")















