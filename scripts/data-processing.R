# a) Data Reading and Writing------------------------------------------------------------
# - ITDP ------------------------------------------------------------------

#Name of the indicators defined by ITDP in 2013 and 2014
itdp2013 = read.csv(file = str_c(data.base,"input/itdp/2013/indicators2013.csv"),stringsAsFactors=FALSE, encoding="UTF-8")
write.csv(Var.names(Encoding.UTF8(itdp2013)),file = str_c(data.base,"output/itdp2013.csv"),row.names = FALSE)

itdp2014 = read.csv(file = str_c(data.base,"input/itdp/2014/indicators2014.csv"),stringsAsFactors=FALSE, encoding="UTF-8")
write.csv(Var.names(Encoding.UTF8(itdp2014)),file = str_c(data.base,"output/itdp2014.csv"),row.names = FALSE)

#Dictionary to compare the indicators between 2013 and 2014
itdp = read.csv(file = str_c(data.base,"input/itdp/itdp-indicators.csv"),stringsAsFactors=FALSE, encoding="UTF-8")
write.csv(Var.names(Encoding.UTF8(itdp)),file = str_c(data.base,"output/itdp.csv"),row.names = FALSE)

#Value of the indicators defined by ITDP in 2013 and 2014
scores2013 = read.csv(file = str_c(data.base,"input/itdp/2013/scores2013.csv"),stringsAsFactors=FALSE, encoding="UTF-8")
write.csv(Var.names(Encoding.UTF8(scores2013)),file = str_c(data.base,"output/scores2013.csv"),row.names = FALSE,fileEncoding = "UTF-8")

scores2014 = read.csv(file = str_c(data.base,"input/itdp/2014/scores2014.csv"),stringsAsFactors=FALSE, encoding="UTF-8")
write.csv(Var.names(Encoding.UTF8(scores2014)),file = str_c(data.base,"output/scores2014.csv"),row.names = FALSE,fileEncoding = "UTF-8")
# - BRTdata.org -------------------------------------------------------------

#a) By System

#Data
brt_systems = BRTData.Reader(data.base=str_c(data.base,"input/brtdata/system-level/"),sep =",",format="csv",ids=c("Value","Year","Source"),file.encoding = "ISO-8859-1")$Value
write.csv(Var.names(Encoding.UTF8(brt_systems)),file = str_c(data.base,"output/brt_systems.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")
#Sources
brt_systems_source = BRTData.Reader(data.base=str_c(data.base,"input/brtdata/system-level/"),sep =",",format="csv",ids=c("Value","Year","Source"),file.encoding = "ISO-8859-1")$Source
write.csv(Var.names(Encoding.UTF8(brt_systems_source)),file = str_c(data.base,"output/brt_systems_source.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")
#Year linked to the data sources
brt_systems_year = BRTData.Reader(data.base=str_c(data.base,"input/brtdata/system-level/"),sep =",",format="csv",ids=c("Value","Year","Source"),file.encoding = "ISO-8859-1")$Year
write.csv(Var.names(Encoding.UTF8(brt_systems_year)),file = str_c(data.base,"output/brt_systems_year.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

#b) By Corridor

#Data
brt_corridors = BRTData.Reader(data.base=str_c(data.base,"input/brtdata/corridor-level/"),sep =",",format.file ="csv",ids=c("Value","Year","Source"),file.encoding = "ISO-8859-1")$Value
write.csv(Var.names(Encoding.UTF8(brt_corridors)),file = str_c(data.base,"output/brt_corridors.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")
#Sources
brt_corridors_source = BRTData.Reader(data.base=str_c(data.base,"input/brtdata/corridor-level/"),sep =",",format="csv",ids=c("Value","Year","Source"),file.encoding = "ISO-8859-1")$Source
write.csv(Var.names(Encoding.UTF8(brt_corridors_source)),file = str_c(data.base,"output/brt_corridors_source.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")
#Year linked to the data sources
brt_corridors_year = BRTData.Reader(data.base=str_c(data.base,"input/brtdata/corridor-level/"),sep =",",format="csv",ids=c("Value","Year","Source"),file.encoding = "ISO-8859-1")$Year
write.csv(Var.names(Encoding.UTF8(brt_corridors_year)),file = str_c(data.base,"output/brt_corridors_year.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

# #Information by country
# countries = Data.Reader(str_c(data.base,"Country Information/"),sep =",",format="csv")
# write.csv(Var.names(Encoding.UTF8(countries)),file = str_c(data.base,"countries.csv"))

# - ITDP and BRTData --------------------------------------------------------

#Data collected manually (June 2017)
itdp_brtdata_2017 = read.csv(file = str_c(data.base,"input/itdp-brtdata/june-2017.csv"),stringsAsFactors=FALSE, encoding="ISO-8859-1")
write.csv(Var.names(Encoding.UTF8(itdp_brtdata_2017)),file = str_c(data.base,"output/itdp_brtdata_2017.csv"),row.names = FALSE)

# - City--------------------------------------------------------------------
# - Metro -----------------------------------------------------------------
metros = read.csv(file = str_c(data.base,"input/metro/metros-worldwide.csv"),stringsAsFactors=FALSE, encoding="ISO-8859-1")
write.csv(Var.names(Encoding.UTF8(metros)),file = str_c(data.base,"output/metros-worldwide.csv"),row.names = FALSE, fileEncoding = "UTF-8")


# b) Backing Up and Reading Data ---------------------------------------------

data.itdp2013 = read.csv(paste(data.base,"output/itdp2013.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.itdp2014 = read.csv(paste(data.base,"output/itdp2014.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.itdp = read.csv(paste(data.base,"output/itdp.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.scores2013 = read.csv(paste(data.base,"output/scores2013.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.scores2014 = read.csv(paste(data.base,"output/scores2014.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.itdp_brtdata_2017 = read.csv(paste(data.base,"output/itdp_brtdata_2017.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.brt_systems = read.csv(paste(data.base,"output/brt_systems.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.brt_corridors= read.csv(paste(data.base,"output/brt_corridors.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
data.metro= read.csv(paste(data.base,"output/metros-worldwide.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")

# c) Reading exported datasets ---------------------------------------------------------
itdp2013 = data.itdp2013 ; itdp2014 = data.itdp2014 ; itdp = data.itdp
scores2013 = data.scores2013 ; scores2014 = data.scores2014 
brt_systems = data.brt_systems; brt_corridors = data.brt_corridors; metro = data.metro
itdp_brtdata_2017 = data.itdp_brtdata_2017

