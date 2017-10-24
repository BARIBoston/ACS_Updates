########################################################################################################

# Creating Census Tracts Data File for Boston Research Map: ACS 2011-2015

########################################################################################################

# This file collects downloads and manipulates necessary data sets to create 
# the Census-based files for the Boston Research Map. In its form below, it 
# is written for 2009-2013 ACS data.
# NOTE: Only the endyr command in line 24 needs to be changed for 
# other 5-year estimates. 
# See line 69 for the code for 2005-09 with the accompanying 2000 census geographies.

## CODEBOOK NOTE: The Median Gross Rent is top-coded at 2001.
## CODEBOOK NOTE: The Median Home Value is top-coded at 1000001.

########################################################################################################

#install.packages("acs")
require(acs)
#install.packages("stringer")
require(stringr)

# install API key **(just need to do this once)**
api.key.install("829a6baee8d09366819cd05f131b47aeaf232576")  

# specify the end year and the rest does not need to change, 
# this is for 5-year estimates; for shorter spans, change "span = 5"
endyr <- 2015

# define the geographies you want data for: here for all tracts/blocks groups 
# in all counties in MA
ma.tracts = geo.make(state="MA", county="*", tract="*", check=T)

############################################################################
### CONSTRUCT DATA SET AND EXTRACT TOTAL POPULATION FROM ACS FOR MASSACHUSETTS CENSUS TRACTS
# Total Population
TotalPop <- acs.fetch(endyear = endyr, span = 5, geography=ma.tracts, variable = "B01003_001", col.names=c("TotalPop"))
head(TotalPop)

# Extract numerical CT_ID_10s to build data set
geography(TotalPop)$county.fix<-str_pad(geography(TotalPop)$county, 3, pad = "0")
geography(TotalPop)$tract.fix<-str_pad(geography(TotalPop)$tract, 6, pad = "0")
geography(TotalPop)$CT_ID_10 <- paste(geography(TotalPop)$state, geography(TotalPop)$county.fix, geography(TotalPop)$tract.fix, sep="")
sum(nchar(geography(TotalPop)$CT_ID_10)!=11)==0 # must be TRUE

# Create full data set from CT_IDs and Total Population
acs.tr <- data.frame(CT_ID_10 = geography(TotalPop)$CT_ID_10, TotalPop = estimate(TotalPop), row.names=NULL)
head(acs.tr)
summary(acs.tr)
nrow(acs.tr) # 1478, (1366 for 2005-2009)

####
# Construct all variables, organized by category
####

###
# Sex, Density, and Age
###

# DENSITY
# Load 2010 census tract land area data from Boston Data Portal (should be in same folder as this R-Script)
land <- read.table("G:/BARI/BARI Work/ACS/Gaz_tracts_national.txt", head=T)
land.ma<-land[land$USPS=="MA",c("GEOID","ALAND")] #ALAND here is in square meters
land.ma$ALAND10_sqmi <- land.ma$ALAND/(2.59e+6)
land.ma$ALAND <- NULL

# Merge data, create PopDen variable, remove "ALAND" (land area) column
acs.tr <- merge(acs.tr, land.ma, by.x="CT_ID_10", by.y="GEOID", all.x=T)
acs.tr$PopDen <- acs.tr$TotalPop/acs.tr$ALAND10_sqmi
acs.tr$ALAND10_sqmi <- NULL

####################################################################################
# ### 2005-09 ACS with 2000 census geographies ###
# # Total Population
# TotalPop <- acs.fetch(endyear = endyr, span = 5, geography=ma.tracts, 
#                       variable = "B01003_001", col.names=c("TotalPop"))
# head(TotalPop)
# 
# # Extract numerical CT_ID_00s to build data set
# geography(TotalPop)$county.fix<-str_pad(geography(TotalPop)$county, 3, pad = "0")
# geography(TotalPop)$tract.fix<-str_pad(geography(TotalPop)$tract, 6, pad = "0")
# geography(TotalPop)$CT_ID_00 <- paste(geography(TotalPop)$state, geography(TotalPop)$county.fix, 
#                                       geography(TotalPop)$tract.fix, sep="")
# sum(nchar(geography(TotalPop)$CT_ID_00)!=11)==0 # must be TRUE
# 
# # Create full data set from CT_IDs and Total Population
# acs.tr <- data.frame(CT_ID_00 = geography(TotalPop)$CT_ID_00, 
#                      TotalPop = estimate(TotalPop), 
#                      row.names=NULL)
# head(acs.tr)
# summary(acs.tr)
# nrow(acs.tr) # 1366 for 2005-2009
# 
# # density
# land00 <- read.table("~/Desktop/Work_BARI/2016/Census/Data/Gaz_tracts_national2000.txt")
# land00.ma <- land00[grep("^MA", land00$V1),c("V1","V4")] #ALAND here is in square meters
# land00.ma$CT_ID_00 <- gsub("MA","",land00.ma$V1)
# land00.ma$ALAND00_sqmi <- land00.ma$V4/(2.59e+6)
# land00.ma <- land00.ma[,c("CT_ID_00", "ALAND00_sqmi")]
# # Merge data, create PopDen variable, remove "ALAND" (land area) column
# acs.tr <- merge(acs.tr, land00.ma, by="CT_ID_00", all.x=T)
# head(acs.tr)
# acs.tr$PopDen <- acs.tr$TotalPop/acs.tr$ALAND00_sqmi
# acs.tr$ALAND00_sqmi <- NULL


# SEX
Sex <- estimate(acs.fetch(endyear = endyr, span = 5, geography = ma.tracts, variable = c("B01001_026","B01001_002")))
acs.tr$SexRatio <- Sex[,1]/Sex[,2]

# AGE
AgeU18 <-   estimate(acs.fetch(endyear = endyr, span = 5, geography = ma.tracts, variable = c("B06001_002", "B06001_003")))
acs.tr$AgeU18 <- (apply(AgeU18, 1, FUN=sum))/acs.tr$TotalPop

Age1834 <-   estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, variable = c("B06001_004", "B06001_005")))
acs.tr$Age1834 <- (apply(Age1834, 1, FUN=sum))/acs.tr$TotalPop

Age3564 <-   estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, variable = c("B06001_006", "B06001_007", "B06001_008", "B06001_009","B06001_010")))
acs.tr$Age3564 <- (apply(Age3564, 1, FUN=sum))/acs.tr$TotalPop

AgeO65 <-   estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, variable = c("B06001_011", "B06001_012")))
acs.tr$AgeO65 <- (apply(AgeO65, 1, FUN=sum))/acs.tr$TotalPop


###
# Racial and Ethnic Composition
###

acs.tr$ForBorn <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, variable = "B05002_013", col.names = "ForBorn"))[,1]/acs.tr$TotalPop
acs.tr$White <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts,variable = "B03002_003"))[,1]/acs.tr$TotalPop
acs.tr$Black <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts,variable = "B03002_004", col.names = "Black"))[,1]/acs.tr$TotalPop
acs.tr$Asian <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts,variable = "B03002_006", col.names = "Asian"))[,1]/acs.tr$TotalPop
acs.tr$Hispanic <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts,variable = "B03002_012", col.names = "Hispanic"))[,1]/acs.tr$TotalPop
acs.tr$TwoOrMore <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts,variable = "B03002_009", col.names = "TwoOrMore"))[,1]/acs.tr$TotalPop
acs.tr$EthHet <-  1-(acs.tr$White^2 + acs.tr$Hispanic^2 + acs.tr$Black^2 + acs.tr$Asian^2)


###
# Economic Characteristics 
###

acs.tr$MedHouseIncome  <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, variable = "B19013_001"))[,1] 

pubassist <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, variable = c("B19057_002","B19057_001")))
acs.tr$PubAssist  <- pubassist[,1]/pubassist[,2]

acs.tr$GINI  <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, variable = "B19083_001"))[,1] 

fampov <- estimate(acs.fetch(endyear = endyr, span = 5, geography = ma.tracts, variable = c("B17019_002","B17019_001")))
acs.tr$FamPovPer  <- fampov[,1]/fampov[,2]

unemp <- estimate(acs.fetch(endyear = endyr, span = 5, geography = ma.tracts, variable = c("B23025_005","B23025_003")))
acs.tr$UnempRate  <- unemp[,1]/unemp[,2]

# # FOR 2006-2010 because later tables not yet unavailable
# colnames(unemp0610)
# unemp0610 <- estimate(acs.fetch(endyear = endyr, span = 5, geography = ma.tracts,
#                             table.name = "B23001", col.names="pretty"))
# 
# # colnames(unemp0610)[grep("In labor force: Civilian:$|65 to 69 years: In labor force:$|0 to 74 years: In labor force:$|75 years and over: In labor force:$", colnames(unemp0610))]
# # colnames(unemp0610)[grep("Civilian: Unemployed$|In labor force: Unemployed$", colnames(unemp0610))]
# 
# acs.tr$labforce <- apply(unemp0610[,colnames(unemp0610)[grep("In labor force: Civilian:$|65 to 69 years: In labor force:$|0 to 74 years: In labor force:$|75 years and over: In labor force:$", colnames(unemp0610))]], 1, FUN=sum)
# acs.tr$UnempRate  <- apply(unemp0610[,colnames(unemp0610)[grep("Civilian: Unemployed$|In labor force: Unemployed$", colnames(unemp0610))]], 1, FUN=sum) / acs.tr$labforce
# acs.tr$labforce <- NULL

# # FOR 2005-2009 because later tables not yet unavailable
# unemp0509 <- estimate(acs.fetch(endyear = endyr, span = 5, geography = ma.tracts,
#                             table.name = "B23001", col.names="pretty"))
# 
# # colnames(unemp0509)[grep("In labor force: Civilian: $|65 to 69 years: In labor force: $|0 to 74 years: In labor force: $|75 years and over: In labor force: $", colnames(unemp0509))]
# # colnames(unemp0509)[grep("Civilian: Unemployed $|In labor force: Unemployed $", colnames(unemp0509))]
# 
# acs.tr$labforce <- apply(unemp0509[,colnames(unemp0509)[grep("In labor force: Civilian: $|65 to 69 years: In labor force: $|0 to 74 years: In labor force: $|75 years and over: In labor force: $", colnames(unemp0509))]], 1, FUN=sum)
# acs.tr$UnempRate  <- apply(unemp0509[,colnames(unemp0509)[grep("Civilian: Unemployed $|In labor force: Unemployed $", colnames(unemp0509))]], 1, FUN=sum) / acs.tr $labforce
# acs.tr$labforce <- NULL


###
# Family and Household Characteristics 
###

family <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, variable = c("B11011_012","B11011_002","B11011_001"))) #female-headed, family households, and total households

acs.tr$TotalHouseH <- family[,3]
acs.tr$FamHousePer  <- family[,2]/family[,3]
acs.tr$FemHeadPer <- family[,1]/family[,2]

samesex <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, variable = c("B11009_002","B11009_003","B11009_005"))) #total unmarried partner households, both male households, and both female households

acs.tr$SameSexCoupPer <- (samesex[,2]+samesex[,3])/samesex[,1]

acs.tr$GrandHeadPer <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, variable = "B10051_002"))[,1]/family[,2]

###  
# Educational Attainment Characteristics
###
edu <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, table.number = "B15002", col.names = "pretty"))
colnames(edu)

acs.tr$LessThanHS <- apply(edu[,c(3:10,20:27)], 1, FUN=sum)/edu[,1]
acs.tr$HSGrad <- apply(edu[,c(11,28)], 1, FUN=sum)/edu[,1]
acs.tr$SomeColl <- apply(edu[,c(12:14,29:31)], 1, FUN=sum)/edu[,1]
acs.tr$Bach <- apply(edu[,c(15,32)], 1, FUN=sum)/edu[,1]
acs.tr$Master <- apply(edu[,c(16,33)], 1, FUN=sum)/edu[,1]
acs.tr$Prof <- apply(edu[,c(17,34)], 1, FUN=sum)/edu[,1]
acs.tr$Doc <- apply(edu[,c(18,35)], 1, FUN=sum)/edu[,1]


###
# Transportation to work
###

commute.time <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, table.number = "B08303", col.names = "pretty"))
colnames(commute.time)

acs.tr$CommuteLess10 <- apply(commute.time[,c(2:3)], 1, FUN=sum) / commute.time[,1]
acs.tr$Commute1030 <- apply(commute.time[,c(4:7)], 1, FUN=sum) / commute.time[,1]
acs.tr$Commute3060 <- apply(commute.time[,c(8:11)], 1, FUN=sum) / commute.time[,1]
acs.tr$Commute6090 <- commute.time[,c(12)] / commute.time[,1]
acs.tr$CommuteOver90 <- commute.time[,c(13)] / commute.time[,1]


commute.type<- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, table.number = "B08301", col.names = "pretty"))
colnames(commute.type)

acs.tr$ByCar <- commute.type[,2] / commute.type[,1]
acs.tr$ByPubTrans <- commute.type[,10] / commute.type[,1]
acs.tr$ByBike <- commute.type[,18] / commute.type[,1]
acs.tr$ByWalk <- commute.type[,19] / commute.type[,1]


###
# Housing Characteristics
### 

occstatus<- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, table.number = "B25002", col.names = "pretty"))
typeocc <-  estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, table.number = "B25009", col.names = "pretty"))

# occupany status percentages
acs.tr$TotalHouseUnits <- occstatus[,1]
acs.tr$VacantUnitPer <- occstatus[,3]/occstatus[,1]
acs.tr$RentersPer <- typeocc[,10]/typeocc[,1]
acs.tr$HomeOwnPer <- typeocc[,2]/typeocc[,1]

# NOTE: Median Gross Rent is top-coded at 2001, Median Home Value top-coded at 1000001
acs.tr$MedGrossRent <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, variable = "B25064_001"))[,1]
acs.tr$MedHomeVal <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, variable = "B25077_001"))[,1]
acs.tr$MedYrBuiltRaw <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.tracts, variable = "B25035_001"))[,1]
acs.tr$MedYrBuilt <- cut(acs.tr$MedYrBuiltRaw, breaks=c(1900,1940,1950,1960,1970,1980,1990,2000,2010, Inf),labels= c("Prior to 1940","1940 to 1949", "1950 to 1959", "1960 to 1969", "1970 to 1979","1980 to 1989", "1990 to 1999", "2000 to 2009", "2010 or Later"), right=F)

#####################################################################
#### EXPORT FINAL DATA SET
#####################################################################
head(acs.tr)
summary(acs.tr)


write.csv(acs.tr, file = "NAME YOUR LOCATION", row.names=F)

#####################################################################
#### Creating Shapefiles for Census Tracts
#####################################################################

#install.packages("rgdal")
library(rgdal)

setwd("G:/BARI/BARI Work/ACS")
mass.tr <- readOGR(".", "CENSUS2010TRACTS_POLY")
acs1115.tr <- acs.tr

##mass0913.tr <- merge(mass.tr, acs0913.tr, by.x = "GEOID10", by.y="CT_ID_10", all.x=TRUE, all.y = TRUE)
##mass1014.tr <- merge(mass.tr, acs1014.tr, by.x = "GEOID10", by.y="CT_ID_10", all.x=TRUE, all.y = TRUE)
mass1115.tr <- merge(mass.tr, acs1115.tr, by.x = "GEOID10", by.y="CT_ID_10", all.x=TRUE, all.y = TRUE)

write.csv(mass1115.tr, file = "NAME YOUR LOCATION", row.names=F)

summary(mass1115.tr)
summary(acs1115.tr)


##writeOGR(mass0913.tr, ".", "ACS0913_TRACT_SHAPE", driver = "ESRI Shapefile")
##writeOGR(mass1014.tr, ".", "ACS1014_TRACT_SHAPE", driver = "ESRI Shapefile")
writeOGR(mass1115.tr, ".", "ACS1115_TRACT_SHAPE", driver = "ESRI Shapefile")


