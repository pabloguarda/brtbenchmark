###############################################################################################################
##################### VARIABLES NAMES AND TYPES ###############################################################
###############################################################################################################
# 1) ITDP Scores 2013 -----------------------------------------------------------------

#Remove extra variables that calculate BRT Basics
scores2013 = scores2013[-which(names(scores2013)=="BRT_BASICS_MINIMUM_NEEDED_18")]

# a) Items -------------------------------------------------------------------

#Variables
itdp2013 =rename(itdp2013,c(VARIABLE="variable")); Hmisc::label(itdp2013$variable) = "Variable"
itdp2013 =rename(itdp2013,c(ID="id")); Hmisc::label(itdp2013$id) = "Id"
itdp2013 =rename(itdp2013,c(ITEM="item")); Hmisc::label(itdp2013$item) = "Item"
itdp2013 =rename(itdp2013,c(MAX_SCORE="max_score")); Hmisc::label(itdp2013$max_score) = "Max Score"
itdp2013 =rename(itdp2013,c(NOTES="notes")); Hmisc::label(itdp2013$notes) = "Notes"

# - BRT Basics (a) -------------------------------------------------------------

#Overall score in this item
scores2013 = rename(scores2013,c(BRT_BASICS_MINIMUM_SCORE_OF_18_POINTS_NEEDED="a"))
scores2013$a = as.numeric(scores2013$a)
Hmisc::label(scores2013$a) = "BRT Basics"

#Busway Alignment (a1)
scores2013 =rename(scores2013,c(BUSWAY_ALIGNMENT="a1"))
scores2013$a1 = as.numeric(scores2013$a1)
Hmisc::label(scores2013$a1) = "Busway alignment"

#Dedicated right of way (a2)
scores2013 =rename(scores2013,c(DEDICATED_RIGHT_OF_WAY="a2")) 
scores2013$a2 = as.numeric(scores2013$a2)
Hmisc::label(scores2013$a2) = "Dedicated right of way"
#Off-board fare collection (a3)
scores2013 =rename(scores2013,c(OFF_BOARD_FARE_COLLECTION="a3"))
scores2013$a3 = as.numeric(scores2013$a3)
Hmisc::label(scores2013$a3) = "Off-board fare collection"
#Intersection treatments (a4)
scores2013 =rename(scores2013,c(INTERSECTION_TREATMENTS="a4"))
scores2013$a4 = as.numeric(scores2013$a4)
Hmisc::label(scores2013$a4) = "Intersection treatments"
#Platform-level boarding (a5)
scores2013 =rename(scores2013,c(PLATFORM_LEVEL_BOARDING="a5"))
scores2013$a5 = as.numeric(scores2013$a5)
Hmisc::label(scores2013$a5) = "Platform-level boarding"
# - Service Planning (b) -------------------------------------------------------------

#Overall score in this item
scores2013 =rename(scores2013,c(SERVICE_PLANNING="b"))
scores2013$b = as.numeric(scores2013$b)
Hmisc::label(scores2013$b) = "Service Planning"

#Multiple routes (b1)
scores2013 = rename(scores2013,c(MULTIPLE_ROUTES="b1"))
scores2013$b1 = as.numeric(scores2013$b1)
Hmisc::label(scores2013$b1) = "Multiple routes"

# Peak frequency (b2)
scores2013 =rename(scores2013,c(PEAK_FREQUENCY="b2")) 
scores2013$b2 = as.numeric(scores2013$b2)
Hmisc::label(scores2013$b2) = "Peak frequency"

# Off-peak frequency (b3)
scores2013 =rename(scores2013,c(OFF_PEAK_FREQUENCY="b3")) 
scores2013$b3 = as.numeric(scores2013$b3)
Hmisc::label(scores2013$b3) = "Off-peak frequency"

# Express, limited, and local services (b4)
scores2013 =rename(scores2013,c(EXPRESS_LIMITED_AND_LOCAL_SERVICES="b4")) 
scores2013$b4 = as.numeric(scores2013$b4)
Hmisc::label(scores2013$b4) = "Express, limited, and local services"

# Control center (b5)
scores2013 =rename(scores2013,c(CONTROL_CENTER="b5")) 
scores2013$b5 = as.numeric(scores2013$b5)
Hmisc::label(scores2013$b5) = "Control center"

# Located In top ten corridors (b6)
scores2013 =rename(scores2013,c(LOCATED_IN_TOP_TEN_CORRIDORS="b6")) 
scores2013$b6 = as.numeric(scores2013$b6)
Hmisc::label(scores2013$b6) = "Located in top ten corridors"

# Demand Profile (b7)
scores2013 =rename(scores2013,c(DEMAND_PROFILE="b7")) 
scores2013$b7 = as.numeric(scores2013$b7)
Hmisc::label(scores2013$b7) = "Demand Profile"

# Hours of operations (b8)
scores2013 =rename(scores2013,c(HOURS_OF_OPERATIONS="b8")) 
scores2013$b8 = as.numeric(scores2013$b8)
Hmisc::label(scores2013$b8) = "Hours of operations"

# Multi-corridor network (b9)
scores2013 =rename(scores2013,c(MULTI_CORRIDOR_NETWORK="b9")) 
scores2013$b9 = as.numeric(scores2013$b9)
Hmisc::label(scores2013$b9) = "Multi-corridor network"
# - Infrastructure (c) -------------------------------------------------------------

#Overall score in this item
scores2013 =rename(scores2013,c(INFRASTRUCTURE="c"))
scores2013$c = as.numeric(scores2013$c)
Hmisc::label(scores2013$c) = "Infrastructure"

#Passing lanes at station (c1)
scores2013 =rename(scores2013,c(PASSING_LANES_AT_STATIONS="c1"))
scores2013$c1 = as.numeric(scores2013$c1)
Hmisc::label(scores2013$c1) = "Passing lanes at stations"

#Minimizing bus emissions (c2)
scores2013 =rename(scores2013,c(MINIMIZING_BUS_EMISSIONS="c2"))
scores2013$c2 = as.numeric(scores2013$c2)
Hmisc::label(scores2013$c2) = "Minimizing bus emissions"

#Stations set back from intersections (c3)
scores2013 =rename(scores2013,c(STATIONS_SET_BACK_FROM_INTERSECTIONS="c3"))
scores2013$c3 = as.numeric(scores2013$c3)
Hmisc::label(scores2013$c3) = "Stations set back from intersections"

#Center stations (c4)
scores2013 =rename(scores2013,c(CENTER_STATIONS="c4"))
scores2013$c4 = as.numeric(scores2013$c4)
Hmisc::label(scores2013$c4) = "Center stations"

#Pavement quality (c5)
scores2013 =rename(scores2013,c(PAVEMENT_QUALITY="c5"))
scores2013$c5 = as.numeric(scores2013$c5)
Hmisc::label(scores2013$c5) = "Pavement quality"
# - Station Design and Station-bus Interface (d) -------------------------------------------------------------

#Overall score in this item
scores2013 =rename(scores2013,c(STATION_DESIGN_AND_STATION_BUS_INTERFACE="d"))
scores2013$d = as.numeric(scores2013$d)
Hmisc::label(scores2013$d) = "Station Design and Station-bus Interface"

#Distances between stations (d1)
scores2013 =rename(scores2013,c(DISTANCES_BETWEEN_STATIONS="d1"))
scores2013$d1 = as.numeric(scores2013$d1)
Hmisc::label(scores2013$d1) = "Distances between stations"

#Safe and comfortable stations (d2)
scores2013 =rename(scores2013,c(SAFE_AND_COMFORTABLE_STATIONS="d2"))
scores2013$d2 = as.numeric(scores2013$d2)
Hmisc::label(scores2013$d2) = "Safe and comfortable stations"

#Number of doors on bus (d3)
scores2013 =rename(scores2013,c(NUMBER_OF_DOORS_ON_BUS="d3"))
scores2013$d3 = as.numeric(scores2013$d3)
Hmisc::label(scores2013$d3) = "Number of doors on bus"

#Docking bays and sub-stops (d4)
scores2013 =rename(scores2013,c(DOCKING_BAYS_AND_SUB_STOPS="d4"))
scores2013$d4 = as.numeric(scores2013$d4)
Hmisc::label(scores2013$d4) = "Docking bays and sub-stops"

#Sliding doors in BRT stations (d5)
scores2013 =rename(scores2013,c(SLIDING_DOORS_IN_BRT_STATIONS="d5"))
scores2013$d5 = as.numeric(scores2013$d5)
Hmisc::label(scores2013$d5) = "Sliding doors in BRT stations"
# - Quality of Service & Passenger Information Systems (e) -------------------------------------------------------------

#Overall score in this item
scores2013 =rename(scores2013,c(QUALITY_OF_SERVICE_PASSENGER_INFORMATION_SYSTEMS="e"))
scores2013$e = as.numeric(scores2013$e)
Hmisc::label(scores2013$e) = "Quality of Service & Passenger Information Systems"

#Branding (e1)
scores2013 =rename(scores2013,c(BRANDING="e1"))
scores2013$e1 = as.numeric(scores2013$e1)
Hmisc::label(scores2013$e1) = "Branding"

#Passenger information (e2)
scores2013 =rename(scores2013,c(PASSENGER_INFORMATION="e2"))
scores2013$e2 = as.numeric(scores2013$e2)
Hmisc::label(scores2013$e2) = "Passenger information"
# - Integration and Access (f) -------------------------------------------------------------

#Overall score in this item
scores2013 =rename(scores2013,c(INTEGRATION_AND_ACCESS="f"))
scores2013$f = as.numeric(scores2013$f)
Hmisc::label(scores2013$f) = "Integration and Access"

#Universal access (f1)
scores2013 =rename(scores2013,c(UNIVERSAL_ACCESS="f1"))
scores2013$f1 = as.numeric(scores2013$f1)
Hmisc::label(scores2013$f1) = "Universal access"

#Integration with other public transport (f2)
scores2013 =rename(scores2013,c(INTEGRATION_WITH_OTHER_PUBLIC_TRANSPORT="f2"))
scores2013$f2 = as.numeric(scores2013$f2)
Hmisc::label(scores2013$f2) = "Integration with other public transport"

#Pedestrian access (f3)
scores2013 =rename(scores2013,c(PEDESTRIAN_ACCESS="f3"))
scores2013$f3 = as.numeric(scores2013$f3)
Hmisc::label(scores2013$f3) = "Pedestrian access"

#Secure bicycle parking (f4)
scores2013 =rename(scores2013,c(SECURE_BICYCLE_PARKING="f4"))
scores2013$f4 = as.numeric(scores2013$f4)
Hmisc::label(scores2013$f4) = "Secure bicycle parking"

#Bicycle lanes (f5)
scores2013 =rename(scores2013,c(BICYCLE_LANES="f5"))
scores2013$f5 = as.numeric(scores2013$f5)
Hmisc::label(scores2013$f5) = "Bicycle lanes"

#Bicycle-sharing integration (f6)
scores2013 =rename(scores2013,c(BICYCLE_SHARING_INTEGRATION="f6"))
scores2013$f6 = as.numeric(scores2013$f6)
Hmisc::label(scores2013$f6) = "Bicycle-sharing integration"
# - Point Deductions (g) -------------------------------------------------------------

#Overall score in this item
scores2013 =rename(scores2013,c(POINT_DEDUCTIONS="g"))
scores2013$g = -as.numeric(scores2013$g)
Hmisc::label(scores2013$g) = "Point Deductions"

#Universal access (g1)
scores2013 =rename(scores2013,c(COMMERCIAL_SPEEDS="g1"))
scores2013$g1 = -as.numeric(scores2013$g1)
Hmisc::label(scores2013$g1) = "Commercial Speeds"

#Peak passengers per hour per direction (pphpd) below 1,000 (g2)
scores2013 =rename(scores2013,c(PEAK_PASSENGERS_PER_HOUR_PER_DIRECTION_PPHPD_BELOW_1_000="g2"))
scores2013$g2 = -as.numeric(scores2013$g2)
Hmisc::label(scores2013$g2) = "Peak passengers per hour per direction (pphpd) below 1,000"

#Lack of enforcement of right-of-way (g3)
scores2013 =rename(scores2013,c(LACK_OF_ENFORCEMENT_OF_RIGHT_OF_WAY="g3"))
scores2013$g3 = -as.numeric(scores2013$g3)
Hmisc::label(scores2013$g3) = "Lack of enforcement of right-of-way"

#Significant gap between bus floor and station platform (g4)
scores2013 =rename(scores2013,c(SIGNIFICANT_GAP_BETWEEN_BUS_FLOOR_AND_STATION_PLATFORM="g4"))
scores2013$g4 = -as.numeric(scores2013$g4)
Hmisc::label(scores2013$g4) = "Significant gap between bus floor and station platform"

#Overcrowding (g5)
scores2013 =rename(scores2013,c(OVERCROWDING="g5"))
scores2013$g5 = -as.numeric(scores2013$g5)
Hmisc::label(scores2013$g5) = "Overcrowding"

#Poorly-maintained Busway, Buses, Stations and Technology Systems (g6)
scores2013 =rename(scores2013,c(POORLY_MAINTAINED_BUSWAY_BUSES_STATIONS_AND_TECHNOLOGY_SYSTEMS="g6"))
scores2013$g6 = -as.numeric(scores2013$g6)
Hmisc::label(scores2013$g6) = "Poorly-maintained Busway, Buses, Stations and Technology Systems"


# b) General Information ----------------------------------------------------
# - Year --------------------------------------------------------------------
scores2013 =rename(scores2013,c(YEAR="year")) 
scores2013$year = as.integer(scores2013$year)
Hmisc::label(scores2013$year) = "Year"
# - Country --------------------------------------------------------------------
scores2013 =rename(scores2013,c(COUNTRY="country")); #Hmisc::label(scores2013$country) = "Country"
# - City --------------------------------------------------------------------
scores2013 =rename(scores2013,c(CITY="city")); #Hmisc::label(scores2013$city) = "City"
# - System --------------------------------------------------------------------
scores2013 =rename(scores2013,c(SYSTEM="system")); #Hmisc::label(scores2013$system) = "System"
# - Corridor Name --------------------------------------------------------------------
scores2013 =rename(scores2013,c(CORRIDOR="corridor")); #Hmisc::label(scores2013$corridor) = "Corridor name"
# - Corridor Length --------------------------------------------------------------------
scores2013 =rename(scores2013,c(CORRIDOR_LENGTH_KM="corridor_length")); #Hmisc::label(scores2013$corridor_length) = "Corridor length"
# - Score --------------------------------------------------------------------

#Without discounting point deductions (initial score)
scores2013 =rename(scores2013,c(TOTAL_100="score")); #Hmisc::label(scores2013$score) = "Score"

#Discounting point deductions
scores2013 =rename(scores2013,c(TOTAL_SCORE="total_score")); #Hmisc::label(scores2013$total_score) = "Total score"
# - Classification --------------------------------------------------------
scores2013 =rename(scores2013,c(CLASSIFICATION="classification"))
scores2013$classification = capitalize(tolower(scores2013$classification))
scores2013$classification[scores2013$classification=="Basic brt"] = "Basic"
Hmisc::label(scores2013$classification) = "Classification"

# 2) ITDP Scores 2014 -----------------------------------------------------------------

#Remove an additional variable in the database where BRT Basics is shown again
scores2014 = scores2014[-which(names(scores2014)=="BRT_BASICS_MINIMUM_NEEDED_20")]

# a) Items ----------------------------------------------------------------

#Variables
itdp2014 =rename(itdp2014,c(VARIABLE="variable")); #Hmisc::label(itdp2014$variable) = "Variable"
itdp2014 =rename(itdp2014,c(ID="id")); #Hmisc::label(itdp2014$id) = "Id"
itdp2014 =rename(itdp2014,c(ITEM="item")); #Hmisc::label(itdp2014$item) = "Item"
itdp2014 =rename(itdp2014,c(MAX_SCORE="max_score")); #Hmisc::label(itdp2014$max_score) = "Max Score"
itdp2014 =rename(itdp2014,c(NOTES="notes")); #Hmisc::label(itdp2014$notes) = "Notes"

# - BRT Basics (a) ------------------------------------------------------------

#Overall score in this item
scores2014 =rename(scores2014,c(BRT_BASICS_MINIMUM_SCORE_OF_20_POINTS_NEEDED="a"))
scores2014$a = as.numeric(scores2014$a)
Hmisc::label(scores2014$a) = "BRT Basics"

# Dedicated right-of-way - Minimum 4 points (a1)
scores2014 =rename(scores2014,c(DEDICATED_RIGHT_OF_WAY_MINIMUM_4_POINTS="a1"))
Hmisc::label(scores2014$a1) = "Dedicated right-of-way"

# Busway alignment - Minimum 4 points (a2)
scores2014 =rename(scores2014,c(BUSWAY_ALIGNMENT_MINIMUM_4_POINTS="a2")); #Hmisc::label(scores2014$a2) = "Busway alignment"

#Tier Configuration
scores2014$tierConfig = NA
scores2014$tierConfig[scores2014$a1 >= 6] = 1
scores2014$tierConfig[scores2014$a1 >= 3 & scores2014$a1 < 6] = 2
scores2014$tierConfig[scores2014$a1 >= 1  & scores2014$a1 < 3] = 3
scores2014$tierConfig[scores2014$a1 == 0] = 0

# Off-board fare collection (a3)
scores2014 =rename(scores2014,c(OFF_BOARD_FARE_COLLECTION="a3")); #Hmisc::label(scores2014$a3) = "Off-board fare collection"

# Intersection treatments (a4)
scores2014 =rename(scores2014,c(INTERSECTION_TREATMENTS="a4")); #Hmisc::label(scores2014$a4) = "Intersection treatments"

# Platform-level boarding (a5)
scores2014 =rename(scores2014,c(PLATFORM_LEVEL_BOARDING="a5")); #Hmisc::label(scores2014$a5) = "Platform-level boarding"
# - Service Planning (b) ----------------------------------------------------

#Overall score in this item
scores2014 =rename(scores2014,c(SERVICE_PLANNING="b"))
scores2014$b = as.numeric(scores2014$b)
Hmisc::label(scores2014$b) = "Service Planning"

# Multiple routes (b1)
scores2014 =rename(scores2014,c(MULTIPLE_ROUTES="b1")); 
scores2014$b1 = as.numeric(scores2014$b1)
Hmisc::label(scores2014$b1) = "Multiple routes"

# Express, limited, and local services (b2)
scores2014 =rename(scores2014,c(EXPRESS_LIMITED_AND_LOCAL_SERVICES="b2"))
scores2014$b2 = as.numeric(scores2014$b2)
Hmisc::label(scores2014$b2) = "Express, limited, and local services"

# Control center (b3)
scores2014 =rename(scores2014,c(CONTROL_CENTER="b3"))
scores2014$b3 = as.numeric(scores2014$b3)
Hmisc::label(scores2014$b3) = "Control center"

# Located In top ten corridors (b4)
scores2014 =rename(scores2014,c(LOCATED_IN_TOP_TEN_CORRIDORS="b4"))
scores2014$b4 = as.numeric(scores2014$b4)
Hmisc::label(scores2014$b4) = "Located In top ten corridors"

# Demand Profile (b5)
scores2014 =rename(scores2014,c(DEMAND_PROFILE="b5"))
scores2014$b5 = as.numeric(scores2014$b5)
Hmisc::label(scores2014$b5) = "Demand Profile"

# Hours of operations (b6)
scores2014 =rename(scores2014,c(HOURS_OF_OPERATIONS="b6"))
scores2014$b6 = as.numeric(scores2014$b6)
Hmisc::label(scores2014$b6) = "Hours of operations"

# Multi-corridor network (b7)
scores2014 =rename(scores2014,c(MULTI_CORRIDOR_NETWORK="b7"))
scores2014$b7 = as.numeric(scores2014$b7)
Hmisc::label(scores2014$b7) = "Multi-corridor network"
# - Infrastructure (c) ----------------------------------------------------

#Overall score in this item
scores2014 =rename(scores2014,c(INFRASTRUCTURE="c"))
scores2014$c = as.numeric(scores2014$c)
Hmisc::label(scores2014$c) = "Infrastructure"

# Passing lanes at stations (c1)
scores2014 =rename(scores2014,c(PASSING_LANES_AT_STATIONS="c1"))
scores2014$c1 = as.numeric(scores2014$c1)
Hmisc::label(scores2014$c1) = "Passing lanes at stations"

# Minimizing bus emissions (c2)
scores2014 =rename(scores2014,c(MINIMIZING_BUS_EMISSIONS="c2"))
scores2014$c2 = as.numeric(scores2014$c2)
Hmisc::label(scores2014$c2) = "Minimizing bus emissions"

# Stations set back from intersections (c3)
scores2014 =rename(scores2014,c(STATIONS_SET_BACK_FROM_INTERSECTIONS="c3"))
scores2014$c3 = as.numeric(scores2014$c3)
Hmisc::label(scores2014$c3) = "Stations set back from intersections"

# Center stations (c4)
scores2014 =rename(scores2014,c(CENTER_STATIONS="c4"))
scores2014$c4 = as.numeric(scores2014$c4)
Hmisc::label(scores2014$c4) = "Center stations"

# Pavement quality (c5)
scores2014 =rename(scores2014,c(PAVEMENT_QUALITY="c5"))
scores2014$c5 = as.numeric(scores2014$c5)
Hmisc::label(scores2014$c5) = "Pavement quality"
# - Station Design and Station-bus Interface (d) --------------------------

#Overall score in this item
scores2014 =rename(scores2014,c(STATION_DESIGN_AND_STATION_BUS_INTERFACE="d"))
scores2014$d = as.numeric(scores2014$d)
Hmisc::label(scores2014$d) = "Station Design and Station-bus Interface"

#Distances between stations (d1)
scores2014 =rename(scores2014,c(DISTANCES_BETWEEN_STATIONS="d1"))
scores2014$d1 = as.numeric(scores2014$d1)
Hmisc::label(scores2014$d1) = "Distances between stations"

#Safe and comfortable stations (d2)
scores2014 =rename(scores2014,c(SAFE_AND_COMFORTABLE_STATIONS="d2"))
scores2014$d2 = as.numeric(scores2014$d2)
Hmisc::label(scores2014$d2) = "Safe and comfortable stations"

#Number of doors on bus (d3)
scores2014 =rename(scores2014,c(NUMBER_OF_DOORS_ON_BUS="d3"))
scores2014$d3 = as.numeric(scores2014$d3)
Hmisc::label(scores2014$d3) = "Number of doors on bus"

#Docking bays and sub-stops (d4)
scores2014 =rename(scores2014,c(DOCKING_BAYS_AND_SUB_STOPS="d4"))
scores2014$d4 = as.numeric(scores2014$d4)
Hmisc::label(scores2014$d4) = "Docking bays and sub-stops"

#Sliding doors in BRT stations (d5)
scores2014 =rename(scores2014,c(SLIDING_DOORS_IN_BRT_STATIONS="d5"))
scores2014$d5 = as.numeric(scores2014$d5)
Hmisc::label(scores2014$d5) = "Sliding doors in BRT stations"
# - Communications and Marketing (e) --------------------------------------

#Overall score in this item
scores2014 =rename(scores2014,c(COMMUNICATIONS_AND_MARKETING="e"))
scores2014$e = as.numeric(scores2014$e)
Hmisc::label(scores2014$e) = "Communications and Marketing"

#Branding (e1)
scores2014 =rename(scores2014,c(BRANDING="e1"))
scores2014$e1 = as.numeric(scores2014$e1)
Hmisc::label(scores2014$e1) = "Branding"

#Passenger information (e2)
scores2014 =rename(scores2014,c(PASSENGER_INFORMATION="e2"))
scores2014$e2 = as.numeric(scores2014$e2)
Hmisc::label(scores2014$e2) = "Passenger information"
# - Integration and Access (f) --------------------------------------------

#Overall score in this item
scores2014 =rename(scores2014,c(INTEGRATION_AND_ACCESS="f"))
scores2014$f = as.numeric(scores2014$f)
Hmisc::label(scores2014$f) = "Integration and Access"

#Universal access (f1)
scores2014 =rename(scores2014,c(UNIVERSAL_ACCESS="f1"))
scores2014$f1 = as.numeric(scores2014$f1)
Hmisc::label(scores2014$f1) = "Universal access"

#Integration with other public transport (f2)
scores2014 =rename(scores2014,c(INTEGRATION_WITH_OTHER_PUBLIC_TRANSPORT="f2"))
scores2014$f2 = as.numeric(scores2014$f2)
Hmisc::label(scores2014$f2) = "Integration with other public transport"

#Pedestrian access (f3)
scores2014 =rename(scores2014,c(PEDESTRIAN_ACCESS="f3"))
scores2014$f3 = as.numeric(scores2014$f3)
Hmisc::label(scores2014$f3) = "Pedestrian access"

#Secure bicycle parking (f4)
scores2014 =rename(scores2014,c(SECURE_BICYCLE_PARKING="f4"))
scores2014$f4 = as.numeric(scores2014$f4)
Hmisc::label(scores2014$f4) = "Secure bicycle parking"

#Bicycle lanes (f5)
scores2014 =rename(scores2014,c(BICYCLE_LANES="f5"))
scores2014$f5 = as.numeric(scores2014$f5)
Hmisc::label(scores2014$f5) = "Bicycle lanes"

#Bicycle-sharing integration (f6)
scores2014 =rename(scores2014,c(BICYCLE_SHARING_INTEGRATION="f6"))
scores2014$f6 = as.numeric(scores2014$f6)
Hmisc::label(scores2014$f6) = "Bicycle-sharing integration"
# - Point Deductions (g) --------------------------------------------------

# Overall score in this item
scores2014 =rename(scores2014,c(POINT_DEDUCTIONS="g"))
scores2014$g = -as.numeric(scores2014$g)
Hmisc::label(scores2014$g) = "Point Deductions"

#Commercial Speeds (g1)
scores2014 =rename(scores2014,c(COMMERCIAL_SPEEDS="g1"))
scores2014$g1 = -as.numeric(scores2014$g1)
Hmisc::label(scores2014$g1) = "Commercial speeds"

#Peak passengers per hour per direction (pphpd) below 1,000 (g2)
scores2014 =rename(scores2014,c(PEAK_PASSENGERS_PER_HOUR_PER_DIRECTION_PPHPD_BELOW_1_000="g2"))
scores2014$g2 = -as.numeric(scores2014$g2)
Hmisc::label(scores2014$g2) = "Peak passengers per hour per direction (pphpd) below 1,000"

#Lack of enforcement of right-of-way (g3)
scores2014 =rename(scores2014,c(LACK_OF_ENFORCEMENT_OF_RIGHT_OF_WAY="g3"))
scores2014$g3 = -as.numeric(scores2014$g3)
Hmisc::label(scores2014$g3) = "Lack of enforcement of right-of-way"

#Significant gap between bus floor and station platform (g4)
scores2014 =rename(scores2014,c(SIGNIFICANT_GAP_BETWEEN_BUS_FLOOR_AND_STATION_PLATFORM="g4"))
scores2014$g4 = -as.numeric(scores2014$g4)
Hmisc::label(scores2014$g4) = "Significant gap between bus floor and station platform"

#Overcrowding (g5)
scores2014 =rename(scores2014,c(OVERCROWDING="g5"))
scores2014$g5 = -as.numeric(scores2014$g5)
Hmisc::label(scores2014$g5) = "Overcrowding"

#Peak frequency (g6)
scores2014 =rename(scores2014,c(PEAK_FREQUENCY="g6"))
scores2014$g6 = -as.numeric(scores2014$g6)
Hmisc::label(scores2014$g6) = "Peak frequency"

#Off-peak frequency (g7)
scores2014 =rename(scores2014,c(OFF_PEAK_FREQUENCY="g7"))
scores2014$g7 = -as.numeric(scores2014$g7)
Hmisc::label(scores2014$g7) = "Off-peak frequency"

#Poorly-maintained Busway, Buses, Stations and Technology Systems (g8)
scores2014 =rename(scores2014,c(POORLY_MAINTAINED_BUSWAY_BUSES_STATIONS_AND_TECHNOLOGY_SYSTEMS="g8"))
scores2014$g8 = -as.numeric(scores2014$g8)
Hmisc::label(scores2014$g8) = "Poorly-maintained Busway, Buses, Stations and Technology Systems"

# b) General Information --------------------------------------------------
# - Year --------------------------------------------------------------------
scores2014 =rename(scores2014,c(YEAR="year"))
scores2014$year = as.integer(scores2014$year)
Hmisc::label(scores2014$year) = "Year"
# - Country --------------------------------------------------------------------
scores2014 =rename(scores2014,c(COUNTRY="country")); #Hmisc::label(scores2014$country) = "Country"
# - City --------------------------------------------------------------------
scores2014 =rename(scores2014,c(CITY="city")); #Hmisc::label(scores2014$city) = "City"
# - System --------------------------------------------------------------------
scores2014 =rename(scores2014,c(SYSTEM="system")); #Hmisc::label(scores2014$system) = "System"

#Correction of a small difference in the name of the Transjakarta System in Indonesia
scores2014$system[scores2014$system=="Transjakarta"] = "TransJakarta"

# - Corridor Name --------------------------------------------------------------------
scores2014 =rename(scores2014,c(CORRIDOR="corridor")); #Hmisc::label(scores2014$corridor) = "Corridor name"
# - Corridor Length --------------------------------------------------------------------
scores2014 =rename(scores2014,c(CORRIDOR_LENGTH_KM="corridor_length")); #Hmisc::label(scores2014$corridor_length) = "Corridor length"
# - Score --------------------------------------------------------------------

#Without discounting point deductions (initial score)
scores2014 =rename(scores2014,c(TOTAL_100="score")); #Hmisc::label(scores2014$score) = "Score"

#Discounting point deductions
scores2014 =rename(scores2014,c(TOTAL_SCORE="total_score")); #Hmisc::label(scores2014$total_score) = "Total Score"
# - Classification --------------------------------------------------------
scores2014 =rename(scores2014,c(CLASSIFICATION="classification"))
scores2014$classification = capitalize(tolower(scores2014$classification))
scores2014$classification[scores2014$classification=="Basic brt"] = "Basic"
Hmisc::label(scores2014$classification) = "Classification"
# - Comments ----------------------------------------------------------------
scores2014 =rename(scores2014,c(SUGGESTIONS_FOR_QUICK_IMPROVEMENTS_1="comment1")); #Hmisc::label(scores2014$comment1) = "Suggestions for Quick Improvements 1"
scores2014 =rename(scores2014,c(SUGGESTIONS_FOR_QUICK_IMPROVEMENTS_2="comment2")); #Hmisc::label(scores2014$comment2) = "Suggestions for Quick Improvements 2"
scores2014 =rename(scores2014,c(SUGGESTIONS_FOR_QUICK_IMPROVEMENTS_3="comment3")); #Hmisc::label(scores2014$comment3) = "Suggestions for Quick Improvements 3"



# 3) BRTdata.org ----------------------------------------------------------

# a) System Information  --------------------------------------------------
# - Continent ---------------------------------------------------------------
brt_systems =rename(brt_systems,c(REGION="continent")); #Hmisc::label(brt_systems$continent) = "Continent"
# - Country --------------------------------------------------------------------
brt_systems =rename(brt_systems,c(COUNTRY="country")); #Hmisc::label(brt_systems$country) = "Country"
# - City --------------------------------------------------------------------
brt_systems =rename(brt_systems,c(CITY="city")); #Hmisc::label(brt_systems$city) = "City"
# - Corridor name --------------------------------------------------------------------
brt_systems =rename(brt_systems,c(SYSTEM_NAME="system")); #Hmisc::label(brt_systems$system) = "System"
# - Corridor length -------------------------------------------------------
brt_systems =rename(brt_systems,c(SYSTEM_LENGTH_KM="system_length")); #Hmisc::label(brt_systems$system_length) = "System Length (km)"
# - Population ---------------------------------------------------------------------
brt_systems =rename(brt_systems,c(POPULATION_CITY="population")); #Hmisc::label(brt_systems$population) = "Population in the city"
brt_systems$population = as.numeric(brt_systems$population)
# - Population Density ----------------------------------------------------
brt_systems =rename(brt_systems,c(POPULATION_DENSITY_METROPOLITAN_AREA="pop_density")); #Hmisc::label(brt_systems$pop_density) = "Population Density Metropolitan Area [hab/km2]"
brt_systems$pop_density = as.numeric(brt_systems$pop_density)
# - GDP ---------------------------------------------------------------------
brt_systems =rename(brt_systems,c(GDP_PER_CAPITA_US="gdp")); #Hmisc::label(brt_systems$gdp) = "GDP per capita (US$)"
brt_systems$gdp = as.numeric(brt_systems$gdp)
# - Bus Fare ---------------------------------------------------------------------
brt_systems =rename(brt_systems,c(STANDARD_FARE_US="bus_fare"))
brt_systems$bus_fare = as.numeric(brt_systems$bus_fare)
#Hmisc::label(brt_systems$bus_fare) = "Standard Bus Fare (US$)"
# - Modal split in public transport ---------------------------------------------------------------------
brt_systems =rename(brt_systems,c(MODAL_SPLIT_PUBLIC_TRANSPORT="bus_split")); #Hmisc::label(brt_systems$bus_split) = "Modal Split in Public Transport"
# - Modal split in private transport ---------------------------------------------------------------------
brt_systems =rename(brt_systems,c(MODAL_SPLIT_PRIVATE_TRANSPORT="car_split")); #Hmisc::label(brt_systems$car_split) = "Modal Split in Private Transport"
# - System Name --------------------------------------------------------------------
# systems =rename(systems,c(SYSTEM_NAME="system")); Hmisc::label(systems$system) = "System"
# - Year System Commenced ----------------------------------------------------------
# Year when operations were inaugurated.
brt_systems =rename(brt_systems,c(YEAR_SYSTEM_COMMENCED="year_system_commenced"))
brt_systems$year_system_commenced = as.numeric(brt_systems$year_system_commenced)
Hmisc::label(brt_systems$year_system_commenced) = "Year System Commenced"
# - Daily Demand ----------------------------------------------------------
brt_systems =rename(brt_systems,c(DAILY_DEMAND_PASSENGERS_PER_DAY="demand"))
brt_systems$demand = as.numeric(brt_systems$demand)
Hmisc::label(brt_systems$demand) = "Demand [pax/day]"
# - Station Spacing -------------------------------------------------------
# Average distance between stations on the corridor. Data in meters.
brt_systems =rename(brt_systems,c(STATION_SPACING_M="station_spacing")); 
brt_systems$station_spacing= as.numeric(brt_systems$station_spacing)
Hmisc::label(brt_systems$station_spacing) = "Station Spacing corridor [m]"
# - Operating speed ---------------------------------------------------------
#Average speed of buses in the system, including the dwell time at stations. Data in km/h.
brt_systems =rename(brt_systems,c(OPERATING_SPEED="speed"))
brt_systems$speed = as.numeric(brt_systems$speed)
Hmisc::label(brt_systems$speed) = "Operating Speed System [km/hr]"
# - Peak Frequency --------------------------------------------------------
# Average number of buses per hour serving the segment with highest passenger boardings during peak hour and in the peak direction.
brt_systems =rename(brt_systems,c(PEAK_FREQUENCY_BUSES_PER_HOUR="peak_frequency"))
brt_systems$peak_frequency = as.numeric(brt_systems$peak_frequency)
Hmisc::label(brt_systems$peak_frequency) = "Peak of Bus frequency [buses/hour]"
# - Peak Load of Passengers --------------------------------------------------------
# Maximum number of passengers aboard buses per hour per direction along the most heavily loaded segment. Note it is different from the maximum quantity of passengers boarding per direction during the peak hour.
brt_systems =rename(brt_systems,c(PEAK_LOAD_PASSENGERS_PER_HOUR_PER_DIRECTION="peak_load_pax"))
brt_systems$peak_load_pax = as.numeric(brt_systems$peak_load_pax)
Hmisc::label(brt_systems$peak_load_pax) = "Peak Load of Passengers [pax/hour]"
# - Annual Fare Revenues ----------------------------------------------------

# An estimate of the total amount of farebox revenue collected each year calculated based on the average user fare and annual passenger trips. Data in current US dollars.
brt_systems =rename(brt_systems,c(ANNUAL_FARE_REVENUE_US_MILLIONS="annual_revenues"))
brt_systems$annual_revenues = as.numeric(brt_systems$annual_revenues)
Hmisc::label(brt_systems$annual_revenues) = "Annual Fare Revenues [US$/year]"



# b) Corridor Information  --------------------------------------------------
# - Continent ---------------------------------------------------------------
brt_corridors =rename(brt_corridors,c(REGION="continent")); #Hmisc::label(brt_corridors$continent) = "Continent"
# - Country --------------------------------------------------------------------
brt_corridors =rename(brt_corridors,c(COUNTRY="country")); #Hmisc::label(brt_corridors$country) = "Country"
# - City --------------------------------------------------------------------
brt_corridors =rename(brt_corridors,c(CITY="city")); #Hmisc::label(brt_corridors$city) = "City"
# - Corridor name --------------------------------------------------------------------
brt_corridors =rename(brt_corridors,c(CORRIDOR="corridor")); #Hmisc::label(brt_corridors$corridor) = "Corridor"
# - Corridor length -------------------------------------------------------
brt_corridors =rename(brt_corridors,c(CORRIDOR_LENGTH_KM="corridor_length")); #Hmisc::label(brt_corridors$corridor_length) = "Corridor Length (km)"
# - Year Corridor Commenced ----------------------------------------------------------
brt_corridors =rename(brt_corridors,c(YEAR_CORRIDOR_COMMENCED="year_corridor_commenced"))
brt_corridors$year_corridor_commenced = as.numeric(brt_corridors$year_corridor_commenced)
Hmisc::label(brt_corridors$year_corridor_commenced) = "Year Corridor Commenced"
# - Operating speed ---------------------------------------------------------
brt_corridors =rename(brt_corridors,c(OPERATING_SPEED_CORRIDOR="speed"))
brt_corridors$speed = as.numeric(brt_corridors$speed)
Hmisc::label(brt_corridors$speed) = "Operating Speed Corridor [km/hr]"
# - Total cost per kilometer --------------------------------------------------
brt_corridors =rename(brt_corridors,c(TOTAL_COST_PER_KILOMETER_CORRIDOR_US_MILLION_PER_KM="cost_per_km"))
brt_corridors$cost_per_km = as.numeric(brt_corridors$cost_per_km)
Hmisc::label(brt_corridors$cost_per_km) = "Total Cost Per Kilometer of Corridor[US$MM per km]"
# - Peak Load --------------------------------------------------
# Maximum number of passengers aboard a bus per hour per direction along the most heavily loaded segment. Note it is different from the maximum quantity 
# of passengers boarding per direction during the peak hour.
brt_corridors =rename(brt_corridors,c(PEAK_LOAD_CORRIDOR_PASSENGERS_PER_HOUR_PER_DIRECTION="peak_load"))
brt_corridors$peak_load = as.numeric(brt_corridors$peak_load)
Hmisc::label(brt_corridors$peak_load) = "Peak Load [passengers/hour]"
# - Peak Frequency --------------------------------------------------------
# Average number of buses per hour along the corridor segment with highest passenger boardings during peak hour and in the peak direction.
brt_corridors =rename(brt_corridors,c(PEAK_FREQUENCY_CORRIDOR_BUSES_PER_HOUR_PER_DIRECTION="peak_frequency"))
brt_corridors$peak_frequency = as.numeric(brt_corridors$peak_frequency)
Hmisc::label(brt_corridors$peak_frequency) = "Bus frequency [buses/hour]"
# - Physically Integrated Feeders -----------------------------------------
brt_corridors =rename(brt_corridors,c(PHYSICALLY_INTEGRATED_FEEDERS="integrated_feeders"))
brt_corridors$integrated_feeders = as.character(brt_corridors$integrated_feeders)
Hmisc::label(brt_corridors$integrated_feeders) = "Physically Integrated Feeders"
# - Grade-separated intersections -----------------------------------------
brt_corridors =rename(brt_corridors,c(GRADE_SEPARATED_INTERSECTIONS_CORRIDOR="separated_intersections"))
brt_corridors$separated_intersections = as.character(brt_corridors$separated_intersections)
Hmisc::label(brt_corridors$separated_intersections) = "Grade-separated intersections"
# - Daily Demand ----------------------------------------------------------
brt_corridors =rename(brt_corridors,c(DAILY_DEMAND_CORRIDOR_PASSENGERS_PER_DAY="demand"))
brt_corridors$demand = as.numeric(brt_corridors$demand)
Hmisc::label(brt_corridors$demand) = "Demand [pax/day]"
# - Station Spacing -------------------------------------------------------
# Average distance between stations on the corridor. Data in meters.
brt_corridors =rename(brt_corridors,c(STATION_SPACING_CORRIDOR_M="station_spacing")); 
brt_corridors$station_spacing= as.numeric(brt_corridors$station_spacing)
Hmisc::label(brt_corridors$station_spacing) = "Station Spacing corridor [m]"
# - Pre-board fare collection -----------------------------------
# Whether or not passengers on the corridor pay or validate their fares before boarding the buses.
brt_corridors =rename(brt_corridors,c(PRE_BOARD_FARE_COLLECTION_CORRIDOR="offboard_payment"))
brt_corridors$offboard_payment = as.character(brt_corridors$offboard_payment)
brt_corridors$offboard_payment[brt_corridors$offboard_payment==""] = NA
Hmisc::label(brt_corridors$offboard_payment) = "Existence of Pre-board Fare Collection"
# - Length of Segregated Lanes ----------------------------------------------

# Length of segregated with-flow lanes on the corridor. These are physically separated (i.e. by paint, curbs, barriers, grade separation) 
# from other traffic, but with at-grade crossings for vehicles and pedestrians at intersections. Data in kilometers.
brt_corridors =rename(brt_corridors,c(LENGTH_OF_SEGREGATED_LANES_CORRIDOR_KM="segregated_lanes_length"))
brt_corridors$segregated_lanes_length = as.numeric(brt_corridors$segregated_lanes_length)
Hmisc::label(brt_corridors$segregated_lanes_length) = "Length of segregated lanes in the corridor [km]"
# - Length of Exclusive Lanes -----------------------------------------------

# Length of physically separated facilities for two-direction bus travel at all times (i.e. no traffic signals along the length or intersections with other traffic and pedestrians) 
# on the corridor. Data in kilometers.
brt_corridors =rename(brt_corridors,c(LENGTH_OF_EXCLUSIVE_LANES_CORRIDOR_KM="exclusive_lanes_length"))
brt_corridors$exclusive_lanes_length = as.numeric(brt_corridors$exclusive_lanes_length)
Hmisc::label(brt_corridors$exclusive_lanes_length) = "Length of exclusive lanes in the corridor [km]"
# - Length of segregated counterflow lanes --------------------------------
# Length of segregated counterflow segments (i.e. where buses operate in the opposite direction of the mixed traffic) along the corridor. Data in kilometers.
brt_corridors =rename(brt_corridors,c(LENGTH_OF_SEGREGATED_COUNTERFLOW_LANES_CORRIDOR_KM="segregated_counterflow_lanes_length"))
brt_corridors$segregated_counterflow_lanes_length = as.numeric(brt_corridors$segregated_counterflow_lanes_length)
Hmisc::label(brt_corridors$segregated_counterflow_lanes_length) = "Length of segregated counterflow lanes in the corridor [km]"



# 4) BRT and ITDP Data manually collected ---------------------------------

itdp_brtdata_2017 = rename(itdp_brtdata_2017,c(CORRIDOR="corridor")); Hmisc::label(itdp_brtdata_2017$corridor) = "Corridor"
itdp_brtdata_2017 =rename(itdp_brtdata_2017,c(SYSTEM="system")); Hmisc::label(itdp_brtdata_2017$system) = "System"
itdp_brtdata_2017 =rename(itdp_brtdata_2017,c(POPULATION_DENSITY_METROPOLITAN_AREA="pop_density_2017")); Hmisc::label(itdp_brtdata_2017$pop_density_2017) = "Population density, metropolitan area"
itdp_brtdata_2017 =rename(itdp_brtdata_2017,c(PEAK_LOAD_CORRIDOR_PASSENGERS_PER_HOUR_PER_DIRECTION="peak_load_2017")); Hmisc::label(itdp_brtdata_2017$peak_load_2017) = "Peak load, corridor (passengers per hour per direction)"
itdp_brtdata_2017 =rename(itdp_brtdata_2017,c(SPEED_KPH_ITDP="speed_ITDP_2017")); Hmisc::label(itdp_brtdata_2017$speed_ITDP_2017) = "Speed (kph) ITDP"
itdp_brtdata_2017 =rename(itdp_brtdata_2017,c(SPEED_KPH_BRTDATA="speed_BRTData_2017")); Hmisc::label(itdp_brtdata_2017$speed_BRTData_2017) = "Speed (kph) BRTdata"
itdp_brtdata_2017 =rename(itdp_brtdata_2017,c(DAILY_DEMAND_CORRIDOR_PPD="demand_BRTData_2017")); Hmisc::label(itdp_brtdata_2017$demand_BRTData_2017) = "Daily demand corridor (ppd)"
itdp_brtdata_2017 =rename(itdp_brtdata_2017,c(CORRIDOR_LENGTH_KM_BRTDATA="corridor_length_BRTData_2017")); Hmisc::label(itdp_brtdata_2017$corridor_length_BRTData_2017) = "Corridor length (km) BRTdata"

itdp_brtdata_2017 =rename(itdp_brtdata_2017,c(YEAR="year")) 
itdp_brtdata_2017$year = as.integer(itdp_brtdata_2017$year)
Hmisc::label(itdp_brtdata_2017$year) = "Year"

# 5) City information -----------------------------------------------------
# - City -----------------------------------------------------------------
# countries =rename(countries,c(COUNTRY="country")); Hmisc::label(countries$country) = "Country"
#Hay diferencias en el nombre de las ciudades utilizadas por ITDP. Se hará una conversión de los nombres utilizando un archivo que cree

temp_cities = read.csv(paste(data.base,"input/itdp-brtdata/conversion-dictionaries/cities-names.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
brt_corridors = rename(brt_corridors,c(city="city_BRTData"))
brt_corridors = merge(brt_corridors,temp_cities[c("city","city_BRTData")],by="city_BRTData",all.x=TRUE)
brt_corridors$city[is.na(brt_corridors$city)] = brt_corridors$city_BRTData[is.na(brt_corridors$city)]
brt_systems = rename(brt_systems,c(city="city_BRTData"))
brt_systems = merge(brt_systems,temp_cities[c("city","city_BRTData")],by="city_BRTData",all.x=TRUE)
brt_systems$city[is.na(brt_systems$city)] = brt_systems$city_BRTData[is.na(brt_systems$city)]

scores2013 = rename(scores2013,c(city="city_ITDP"))
scores2013 = merge(scores2013,temp_cities[c("city","city_ITDP")],by="city_ITDP",all.x=TRUE)
scores2014 = rename(scores2014,c(city="city_ITDP"))
scores2014 = merge(scores2014,temp_cities[c("city","city_ITDP")],by="city_ITDP",all.x=TRUE)


# 6) Country information -----------------------------------------------------
# - Country -----------------------------------------------------------------
# countries =rename(countries,c(COUNTRY="country")); Hmisc::label(countries$country) = "Country"
# list_countries = unique(brt_corridors$country)[order(unique(brt_corridors$country))]
# write.csv(Var.names(Encoding.UTF8(list_countries)),file = str_c(data.base,"countries.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

#Hay diferencias en el nombre de los paises utilizados por ITDP. Se hará una conversión de los nombres utilizando un archivo que cree
temp_countries = read.csv(paste(data.base,"input/itdp-brtdata/conversion-dictionaries/countries-names.csv",sep=''),stringsAsFactors=FALSE,fileEncoding = "ISO-8859-1")
brt_corridors = rename(brt_corridors,c(country="country_BRTData"))
brt_corridors = merge(brt_corridors,temp_countries[c("country","country_BRTData")],by="country_BRTData",all.x=TRUE)
brt_corridors$country[is.na(brt_corridors$country)] = brt_corridors$country_BRTData[is.na(brt_corridors$country)]
brt_systems = rename(brt_systems,c(country="country_BRTData"))
brt_systems = merge(brt_systems,temp_countries[c("country","country_BRTData")],by="country_BRTData",all.x=TRUE)
brt_systems$country[is.na(brt_systems$country)] = brt_systems$country_BRTData[is.na(brt_systems$country)]


scores2013 = rename(scores2013,c(country="country_ITDP"))
scores2013 = merge(scores2013,temp_countries[c("country","country_ITDP")],by="country_ITDP",all.x=TRUE)
Hmisc::label(scores2013$country) = "Country"; Hmisc::label(scores2013$city) = "City"

scores2014 = rename(scores2014,c(country="country_ITDP"))
scores2014 = merge(scores2014,temp_countries[c("country","country_ITDP")],by="country_ITDP",all.x=TRUE)
Hmisc::label(scores2014$country) = "Country"; Hmisc::label(scores2014$city) = "City"


# 7) Metro ----------------------------------------------------------------
# - Continent ---------------------------------------------------------------
metro =rename(metro,c(CONTINENT="continent")); Hmisc::label(metro$continent) = "Continent"
# - Country --------------------------------------------------------------------
metro =rename(metro,c(COUNTRY="country")); Hmisc::label(metro$country) = "Country"
# - City --------------------------------------------------------------------
metro =rename(metro,c(CITY="city")); Hmisc::label(metro$city) = "City"
# - Year Metro Commenced ----------------------------------------------------------

metro =rename(metro,c(OPENING="date_metro_commenced")); Hmisc::label(metro$date_metro_commenced) = "Date Metro Commenced"
#In some cases only the year (and the month) is typed
metro$year_metro_commenced = NA

for (i in 1:length(str_split(metro$date_metro_commenced," "))){
  if(length(str_split(metro$date_metro_commenced," ")[[i]])==3){
    metro$year_metro_commenced[i] = str_split(metro$date_metro_commenced," ")[[i]][3]
  }
  if(length(str_split(metro$date_metro_commenced," ")[[i]])==2){
    metro$year_metro_commenced[i] = str_split(metro$date_metro_commenced," ")[[i]][2]
  }
  if(length(str_split(metro$date_metro_commenced," ")[[i]])==1){
    metro$year_metro_commenced[i] = str_split(metro$date_metro_commenced," ")[[i]][1]
  }
}
metro$year_metro_commenced = as.numeric(metro$year_metro_commenced)
# - Metro Network Length ----------------------------------------------------

metro =rename(metro,c(NETWORK_LENGTH="metro_length")); Hmisc::label(metro$metro_length) = "Network Length (km)"
metro$metro_length = str_replace_all(metro$metro_length," km",""); metro$metro_length = as.numeric(metro$metro_length)


# 8) Merging Databases ----------------------------------------------------

# - ITDP & Country information from BRTdata.org --------------------------

#GDP Country
temp = unique(with(brt_systems,data.frame(gdp,country))); temp$country = as.character(temp$country)
temp = subset(temp,!is.na(temp$gdp))
if(length(unique(temp$country))==dim(temp)[1]){
  scores2013 = merge(scores2013,temp,by="country",all.x=TRUE)
  scores2014 = merge(scores2014,temp,by="country",all.x=TRUE)
}

#Continent
temp = unique(with(brt_systems,data.frame(continent,country))); temp$country = as.character(temp$country)

if(length(unique(temp$country))==dim(temp)[1]){
  scores2013 = merge(scores2013,temp,by="country",all.x=TRUE)
  scores2014 = merge(scores2014,temp,by="country",all.x=TRUE)
  scores2013$continent[scores2013$country=="South Korea"]="Asia"
  scores2014$continent[scores2014$country=="South Korea"]="Asia"
}
# - ITDP & City information from BRTdata.org --------------------------

# temp = merge(scores2014,countries[c("country","gdp")],by = "country")

scores2013 = merge(scores2013,unique(with(brt_systems,data.frame(pop_density,city))),by="city",all.x=TRUE)
scores2014 = merge(scores2014,unique(with(brt_systems,data.frame(pop_density,city))),by="city",all.x=TRUE)

#KM BRT at the city of the corridor
brt_km = subset(brt_corridors,is.na(corridor_length)==FALSE)
brt_km_city= aggregate(brt_km$corridor_length,by=list(city = brt_km$city),FUN=function(x) round(sum(x),1))
brt_km_city =rename(brt_km_city,c(x="corridors_city_length"))

scores2013 = merge(scores2013,brt_km_city,by="city",all.x=TRUE)
scores2014 = merge(scores2014,brt_km_city,by="city",all.x=TRUE)



#Population
temp = unique(with(brt_systems,data.frame(population,city))); temp$city = as.character(temp$city)
scores2013 = merge(scores2013,temp,by="city",all.x=TRUE)
scores2014 = merge(scores2014,temp,by="city",all.x=TRUE)
# - ITDP & System information from BRTdata.org --------------------------

#Solo hay un sistema por cada ciudad, entonces es equivalente hacer el merge por ciudad o sistema 
#Bus split
temp = unique(with(brt_systems,data.frame(bus_split,city))); temp$city = as.character(temp$city)
scores2013 = merge(scores2013,temp,by="city",all.x=TRUE)
scores2014 = merge(scores2014,temp,by="city",all.x=TRUE)
# - ITDP & Corridor information from BRTdata.org --------------------------
# scores2013 = merge(scores2013,brt_corridors[c("corridor","demand")],by = "corridor",all.x=TRUE)
# scores2014 = merge(scores2014,brt_corridors[c("corridor","demand")],by = "corridor",all.x=TRUE)
# - Adding System information in Corridor database from BRTdata.org --------------------------
brt_corridors = merge(brt_corridors,brt_systems[c("city","population","pop_density","bus_split","car_split","bus_fare","gdp","year_system_commenced")],by= "city")
# - ITDP and BRTdata manually collected -------------------------------------

temp = itdp_brtdata_2017[c("corridor","system","pop_density_2017","peak_load_2017","speed_ITDP_2017","speed_BRTData_2017","demand_BRTData_2017","corridor_length_BRTData_2017")]

scores2013 = merge(scores2013,temp,by=c("corridor","system"),all.x=TRUE)
scores2014 = merge(scores2014,temp,by=c("corridor","system"),all.x=TRUE)

scores2014$corridor_length = as.numeric(scores2014$corridor_length)
scores2014$corridor_length_BRTData_2017 = as.numeric(scores2014$corridor_length_BRTData_2017)

scores2014$demand_BRTData_2017 = str_replace_all(scores2014$demand_BRTData_2017,",","")
scores2014$demand_BRTData_2017 = as.numeric(scores2014$demand_BRTData_2017)

#Peak Load
scores2014$peak_load_2017 = str_replace_all(scores2014$peak_load_2017,",","")
scores2014$peak_load_2017 = as.numeric(scores2014$peak_load_2017)


scores2014$productivity_BRTData_2017 = round(with(scores2014,demand_BRTData_2017/corridor_length_BRTData_2017))
scores2014$speed_BRTData_2017 = as.numeric(scores2014$speed_BRTData_2017)
scores2014$pop_density_2017 = as.numeric(scores2014$pop_density_2017)

# Metro and BRTdata -------------------------------------------------------
brt_corridors = merge(brt_corridors,metro[c("city","metro_length")],by="city",all.x = TRUE)

#If there is no data for a given city, then the metro line does not exist

brt_corridors$metro_length[is.na(brt_corridors$metro_length)] = 0 

# ITDP & Metro ------------------------------------------------------------

scores2013 = merge(scores2013,metro[c("city","metro_length")],by="city",all.x = TRUE)
scores2013$metro_length[is.na(scores2013$metro_length)] = 0
scores2014 = merge(scores2014,metro[c("city","metro_length")],by="city",all.x = TRUE)
scores2014$metro_length[is.na(scores2014$metro_length)] = 0





###############################################################################################################
#####################  FEATURE ENGINEERING ####################################################################
###############################################################################################################
# 1) BRTdata -----------------------------------------------------------------

# a) BRT_corridors -----------------------------------------------------------

# save(brt_corridors,file=str_c(project,"/Databases/BRTdata_corridors.Rdata"))
# write.csv(brt_corridors,file = str_c(project,"/Databases/BRTdata_corridors.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

# Maturity ----------------------------------------------------------------

brt_corridors$system_maturity = with(brt_corridors,year_corridor_commenced-year_system_commenced)
# Productivity ------------------------------------------------------------
brt_corridors$productivity = with(brt_corridors,demand/corridor_length)



# b) BRT systems -----------------------------------------------------------
 
# Productivity ------------------------------------------------------------
brt_systems$productivity = round(with(brt_systems,demand/system_length),1)
Hmisc::label(brt_systems$productivity) = "Productivity (pax/km)"
# Revenue Productivity ----------------------------------------------------
brt_systems$revenue_productivity = round(with(brt_systems,annual_revenues/system_length),1)

# c) BRT cities -----------------------------------------------------------

# 2) ITDP ---------------------------------------------------------------------
# ITDP Score ------------------------------------------------------------------

scores2013$net_score = with(scores2013,a+b+c+d+e+f-abs(g))
scores2014$net_score = with(scores2014,a+b+c+d+e+f-abs(g))
# Merging ITDP Scores from 2013 & 2014 ------------------------------------------------------

temp2013 = scores2013
temp2014 = scores2014

#Cálculo Porcentual de los puntjes en cada item 

#Los ítems generales se mantuvieron casi idénticos (cambio el nombre del e)
#No obstante, cambiaron los puntajes totales por item asi que para que sean comparables
# ,se normalizaran calculando un valor porcentual respecto al maximo del item 

#2013
letters_temp = c("a","b","c","d","e","f","g")
letters_temp2013 = c(itdp$ITDP2013)

for (i in 1:length(letters_temp2013)){
  for(j in 1:dim(temp2013)[1]){
    # if(letters_temp2013[i] %in% c(letters_temp) == FALSE){
      max_score = subset(itdp2013,id==letters_temp2013[i])$max_score
      temp2013[[letters_temp2013[i]]][j] = as.numeric(round(as.numeric(temp2013[[letters_temp2013[i]]][j]/as.numeric(max_score))*100,1))
      
    # }
      
  }
}

#2014
letters_temp = c("a","b","c","d","e","f","g")
letters_temp2014 = c(itdp$ITDP2014)

for (i in 1:length(letters_temp2014)){
  for(j in 1:dim(temp2014)[1]){
    max_score = subset(itdp2014,id==letters_temp2014[i])$max_score
    temp2014[[letters_temp2014[i]]][j] = as.numeric(round(as.numeric(temp2014[[letters_temp2014[i]]][j]/as.numeric(max_score))*100,1))
    
  }
}

#Las variables de 2013 se transforman a nombres equivalentes con los datos de 2014. 
some.names <- structure(c(itdp$ITDP2014), names=c(itdp$ITDP2013))
temp2013 = rename.group(temp2013,replace = some.names)
temp2013$year = 2013; temp2014$year = 2014
variables_temp = intersect(colnames(temp2014),colnames(temp2013))
temp2013 = temp2013[variables_temp]
temp2014 =temp2014[variables_temp]

scores= rbind(temp2014,temp2013)
scores$id = seq(1,dim(scores)[1])

#Se pone un nombre standard
# Aggregate ITDP Scores by City using weigthed average -------------------------------------------

#By year and city
var_list_all = c("a","b","c","d","e","f","g","score","total_score",itdp$ITDP2014)
# View(aggregate.indicators(data=scores, var_list = var_list_all, unit=c("year","city"),weight_variable="corridor_length"))

# table_wav_scores_country = aggregate.indicators(data=scores, var_list, unit=c("country"),weight_variable="corridor_length")

#Weighted Average by year and city
table_wav_year = scores
var_list = c("a","b","c","d","e","f","g","score","total_score")
for(i in 1:length(var_list)){
  letter = var_list[i]  
  x=table_wav_year[[letter]]
  table_wav_year[[letter]] = round(as.numeric(with(table_wav_year,ave(corridor_length*x,country,city,year,FUN=function(x) sum(x))))/as.numeric(with(table_wav_year,ave(corridor_length,country,city,year,FUN=function(x) sum(x)))),2)
}


table_wav = scores
var_list = c("a","b","c","d","e","f","g","score","total_score")
for(i in 1:length(var_list)){
  letter = var_list[i]  
  x=table_wav[[letter]]
  table_wav[[letter]] = round(as.numeric(with(table_wav,ave(corridor_length*x,country,city,FUN=function(x) sum(x))))/as.numeric(with(table_wav,ave(corridor_length,country,city,FUN=function(x) sum(x)))),2)
}

table_wav_scores = unique(table_wav[c("city",var_list)])
# Aggregate ITDP Scores by Country using weigthed average -------------------------------------------

#Weighted Average
var_list = c("a","b","c","d","e","f","g","score","total_score")
table_wav_scores_country = aggregate.indicators(data=scores, var_list, unit=c("country"),weight_variable="corridor_length")
# Aggregate ITDP Scores in the world using weigthed average -------------------------------------------

#Weighted Average
variable_list = c("a","b","c","d","e","f","g","score","total_score")
table_wav_scores_world = aggregate.indicators(data=scores, var_list=variable_list,weight_variable="corridor_length")                                                       
                                                       

# Countries ITDP ----------------------------------------------------------

var_country_list = c("score","total_score",itdp$ITDP2014)

# weight_variable = "corridor_length"
weight_variable = NA

itdp_countries = aggregate.indicators(data = scores,weight_variable = weight_variable,var_list = var_country_list,unit=c("country"))
itdp_countries = merge(itdp_countries,aggregate.indicators(data = scores,weight_variable = "corridor_length",var_list = var_country_list,unit=c("country"))[c("country","corridor_length")],by="country")
# Cities ITDP -------------------------------------------------------------
var_city_list = c("score","total_score",itdp$ITDP2014)

# weight_variable = "corridor_length"
weight_variable = NA

itdp_cities = aggregate.indicators(data = scores,weight_variable = weight_variable,var_list = var_city_list,unit=c("country","city"))
itdp_cities = merge(itdp_cities,aggregate.indicators(data = scores,weight_variable = "corridor_length",var_list = var_city_list,unit=c("country","city"))[c("city","corridor_length")],by="city")


# View(itdp_cities)
# Systems ITDP ------------------------------------------------------------
var_system_list = c("score","total_score",itdp$ITDP2014)
itdp_systems = aggregate.indicators(data = scores,weight_variable = "corridor_length",var_list = var_system_list,unit=c("country","city","system"))
# Corridors ITDP ------------------------------------------------------------
#It is necessary to know how many corridors (and their length) covers the system evaluated.

# 3) ITDP & BRTdata ------------------------------------------------

# Score -------------------------------------------------------------------
# Maturity ----------------------------------------------------------------

#Algorithm that calculates how many kilometers were built before the system was built 

# area = c(1)
# area1 = c("a")
# area2 = c(area,area1)
# 
# scores2013$gdp
# Public or Private Administration ----------------------------------------
# Bus modal share ---------------------------------------------------------
# Performance indexes from BRTData to ITDP corridors  --------------------------------------

temp = table_wav_year
temp_list = c("id","year","country","city","corridor","a","b","c","d","e","f","g")
temp = temp[temp_list]    
colnames(temp) = c("id","year","country","city","corridor","x_a","x_b","x_c","x_d","x_e","x_f","x_g")
# Hmisc::label(temp) = Hmisc::label(scores)[temp_list]
temp = merge(scores,temp[c("id","x_a","x_b","x_c","x_d","x_e","x_f","x_g")],by = "id",all.x = TRUE)

Hmisc::label(temp$x_a) =  str_c(as.character(Hmisc::label(temp$a))," [0-1]")
Hmisc::label(temp$x_b) =  str_c(as.character(Hmisc::label(temp$b))," [0-1]")
Hmisc::label(temp$x_c) =  str_c(as.character(Hmisc::label(temp$c))," [0-1]")
Hmisc::label(temp$x_d) =  str_c(as.character(Hmisc::label(temp$d))," [0-1]")
Hmisc::label(temp$x_e) =  str_c(as.character(Hmisc::label(temp$e))," [0-1]")
Hmisc::label(temp$x_f) =  str_c(as.character(Hmisc::label(temp$f))," [0-1]")
Hmisc::label(temp$x_g) =  str_c(as.character(Hmisc::label(temp$g))," [0-1]")

#Merge productivity of BRT systems

temp = merge(temp,brt_systems[c("city","productivity","demand","system_length","speed","year_system_commenced","peak_frequency","peak_load_pax")],by="city",all.x=TRUE)
itdp_cities = merge(itdp_cities,brt_systems[c("city","productivity","demand","system_length","speed","year_system_commenced","pop_density","peak_frequency","peak_load_pax")],by="city",all.x=TRUE)

save(itdp_cities,file=str_c(data.base,"output/itdp-cities.Rdata"))
write.csv(itdp_cities,file = str_c(data.base,"output/itdp-cities.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

if(dim(scores)[1] == dim(temp)[1]){
  scores = scores[order(scores$id),] ; temp = temp[order(temp$id),]
  scores = merge(scores,temp)
}

save(scores,file=str_c(data.base,"output/scores-itdp.Rdata"))
write.csv(scores,file = str_c(data.base,"output/scores-itdp.csv"),row.names = FALSE, fileEncoding = "ISO-8859-1")

# Merging By corridor name --------------------------------------------------------

#Se eliminan los corredores sin nombre 
temp = subset(brt_corridors,is.na(corridor)==FALSE & corridor!="")

letters_list = c("corridor","city","corridor_length","total_score","score","a","b","c","d","e","f","g")

#El merge debería ser por nombre de corredor y de sistema, por ahora usaremos la ciudad, suponiendo que en la ciudad no puede haber dos corredores iguales
# brt_corridors = merge(temp[c("corridor","city","speed","population","pop_density","cost_per_km")],scores,by = c("city","corridor"),all.x = TRUE)
brt_corridors = merge(brt_corridors,table_wav_scores,by.x = c("city"),by.y =c("city"),all.x = TRUE)

brt_corridors$classification = NA
brt_corridors$classification[with(brt_corridors,total_score<=55)]="Basic"
brt_corridors$classification[with(brt_corridors,total_score>55 & total_score<=69)]="Bronze"
brt_corridors$classification[with(brt_corridors,total_score>69 & total_score<=84)]="Silver"
brt_corridors$classification[with(brt_corridors,total_score>84)]="Gold"
brt_corridors$classification = as.factor(brt_corridors$classification)

brt_corridors$classification = factor(brt_corridors$classification, levels = c("Basic","Bronze","Silver","Gold"), labels = c("Basic","Bronze","Silver","Gold"))
save(brt_corridors,file=str_c(data.base,"output/BRTdata_ITDP_corridors.Rdata"))

# Metro -------------------------------------------------------------------

# temp = merge(scores,metro[c("city","metro_length")],by="city",all.x=TRUE)

scores$metro = 0
scores$metro[scores$metro_length>0] = 1




# 4) Speed Model Using ITDP Data


# Creation of new dataset -------------------------------------------------

#We will only use data from 2014 for the analyses

speedITDP = scores2014 
speedITDP$speed = speedITDP$g1

# SpeedLevels (For Ordinal Models) ----------------------------------------

#5 levels (Ordinal Logit)
speedITDP$speed5 = NA
speedITDP$speed5[speedITDP$speed==10] = 1
speedITDP$speed5[speedITDP$speed==6] = 2
speedITDP$speed5[speedITDP$speed==3] = 3
speedITDP$speed5[speedITDP$speed==1] = 4
speedITDP$speed5[speedITDP$speed==0] = 5
speedITDP$speed5 = as.factor(speedITDP$speed5)

#3 levels (Ordinal Logit)
speedITDP$speed3 = NA
speedITDP$speed3[speedITDP$speed>5] = 1 #speedITDP$g1[speedITDP$g1==10 || speedITDP$g1==6] = 1
speedITDP$speed3[speedITDP$speed<=5 & speedITDP$speed>0] = 2
speedITDP$speed3[speedITDP$speed==0] = 3
speedITDP$speed3 = as.factor(speedITDP$speed3)

#2 levels (Binary logit)

speedITDP$speed2 = NA
speedITDP$speed2[speedITDP$speed>0] = 0
speedITDP$speed2[speedITDP$speed==0] = 1

# Affluence (Control Variable) --------------------------------------------

speedITDP$highAffluence =0
speedITDP$highAffluence[speedITDP$b4>0 & speedITDP$b5>0] = 1


# Stop Spacing ------------------------------------------------------------

speedITDP$largeStopSpacing = 0
speedITDP$largeStopSpacing[speedITDP$d1>0] = 1



# Dedicated-right-of-way --------------------------------------------------

speedITDP$dedicatedRightOfWay = speedITDP$a1


# Intersection Treatments -------------------------------------------------

speedITDP$intersectionTreatments = speedITDP$a4


# All door boarding -------------------------------------------------------
speedITDP$allDoorBoarding = speedITDP$d3


