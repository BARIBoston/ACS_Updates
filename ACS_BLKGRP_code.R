########################################################################################################

# Creating Census Block Groups Data File for Boston Research Map: ACS 2011-2015

########################################################################################################

# This file collects downloads and manipulates necessary data sets to create the Census-based files
# for Boston Research Map. In its form below, it is written for 2009-2013 ACS data.
# NOTE: Only the endyr command in line 24 needs to be changed for other 5-year estimates. 
# See line 80 for the code for 2005-09 with the accompanying 2000 census geographies.
## CODEBOOK NOTE: The Median Gross Rent is top-coded at 2001.
## CODEBOOK NOTE: The Median Home Value is top-coded at 1000001.

########################################################################################################
install.packages("acs")
require(acs)
require(foreign)
require(stringr)

# install API key **(just need to do this once)**
api.key.install("829a6baee8d09366819cd05f131b47aeaf232576")  

# specify the end year and the rest does not need to change, this is for 5-year estimates; for shorter spans, change "span = 5"
endyr <- 2015

# define the geographies you want data for: here for all blocks groups in all counties (n=14) in MA; have to specify all counties explicitly because of the Census API
ma.blkgrps = geo.make(state="MA", county=c("Barnstable", "Berkshire", "Bristol", "Dukes", "Essex", "Franklin", "Hampden","Hampshire","Middlesex", "Nantucket", "Norfolk", "Plymouth", "Suffolk", "Worcester"), tract="*", block.group = "*", check=T)


# How to get all county names
ma.counties = geo.make(state="MA", county="*", check=T)
TotalPop.counties <- acs.fetch(endyear = endyr, span = 5, geography=ma.counties, variable = "B01003_001", col.names=c("TotalPop"))
geography(TotalPop.counties)

############################################################################
### CONSTRUCT DATA SET AND EXTRACT TOTAL POPULATION FROM ACS FOR MASSACHUSETTS CENSUS BLOCKS
### Notice that you've already created all the variables you need: "endyear", "geography", etc.

# Total Population
TotalPop <- acs.fetch(endyear = endyr, span = 5, geography=ma.blkgrps, variable = "B01003_001", col.names=c("TotalPop"))

# Extract numerical BG_ID_10s (block group ID numbers from 2010) to build data set
geography(TotalPop)$county.fix<-str_pad(geography(TotalPop)$county, 3, pad = "0")
geography(TotalPop)$tract.fix<-str_pad(geography(TotalPop)$tract, 6, pad = "0")
geography(TotalPop)$BG_ID_10 <- paste(geography(TotalPop)$state, geography(TotalPop)$county.fix, geography(TotalPop)$tract.fix, geography(TotalPop)$blockgroup, sep="")
sum(nchar(geography(TotalPop)$BG_ID_10)!=12)==0 # must be TRUE

# Create full data set from CT_IDs and Total Population
acs.bg <- data.frame(BG_ID_10 = geography(TotalPop)$BG_ID_10, TotalPop = estimate(TotalPop), row.names=NULL)
head(acs.bg) 
nrow(acs.bg) # 4985 (5050 for 2005-2009)

#####
# Construct all variables, organized by category
######

###
# Sex, Density, and Age
###

# DENSITY - taking the 2010 total area (in meters squared)
# Load 2010 census tract land area data from Dataverse
## land.bg <- read.dbf("~/Desktop/Work_BARI/2016/Census/Data/CENSUS2010BLOCKGROUPS_POLY.dbf")
## test.land.bg <- read.dbf("D:/BARI/BARI Work/Census Block Groups.dbf")
land.bg <- read.dbf("G:/BARI/BARI Work/Census Block Groups.dbf")
land.bg <- land.bg[,c("GEOID10", "ALAND10")]
land.bg$ALAND10_sqmi <- land.bg$ALAND10/(2.59e+6)
land.bg$ALAND10 <- NULL
summary(land.bg)

# Merge data, create PopDen variable, remove "ALAND" (land area) column
acs.bg <- merge(acs.bg, land.bg, by.x="BG_ID_10", by.y="GEOID10", all.x=T)
acs.bg$PopDen <- acs.bg$TotalPop/acs.bg$ALAND10_sqmi
acs.bg$ALAND10_sqmi <- NULL

###################################################################################
###
## Census 2000 for 2005-2009
# # ###
# # Total Population
# TotalPop <- acs.fetch(endyear = endyr, span = 5, geography=ma.blkgrps, 
#                       variable = "B01003_001", col.names=c("TotalPop"))
# 
# # Extract numerical BG_ID_10s to build data set
# geography(TotalPop)$county.fix<-str_pad(geography(TotalPop)$county, 3, pad = "0")
# geography(TotalPop)$tract.fix<-str_pad(geography(TotalPop)$tract, 6, pad = "0")
# geography(TotalPop)$BG_ID_00 <- paste(geography(TotalPop)$state, geography(TotalPop)$county.fix, 
#                                       geography(TotalPop)$tract.fix, geography(TotalPop)$blockgroup, sep="")
# sum(nchar(geography(TotalPop)$BG_ID_00)!=12)==0 # must be TRUE
# 
# # Create full data set from CT_IDs and Total Population
# acs.bg <- data.frame(BG_ID_00 = geography(TotalPop)$BG_ID_00, 
#                      TotalPop = estimate(TotalPop), 
#                      row.names=NULL)
# head(acs.bg)
# nrow(acs.bg) # 5050 for 2005-2009
# land00.bg <- read.dbf("~/Desktop/Work_BARI/2016/Census/Data/census2000blockgroups_poly.dbf")
# head(land00.bg)
# land00.bg <- land00.bg[,c("BG_ID", "DRY_SQMI")]
# 
# # Merge data, create PopDen variable, remove "ALAND" (land area) column
# acs.bg <- merge(acs.bg, land00.bg, by.x="BG_ID_00", by.y="BG_ID", all.x=T)
# acs.bg$PopDen <- acs.bg$TotalPop/acs.bg$DRY_SQMI
# acs.bg$DRY_SQMI <- NULL


# SEX
Sex <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, variable = c("B01001_026","B01001_002")))
acs.bg$SexRatio <- Sex[,1]/Sex[,2]

# AGE
Age <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, table.number  = "B01001", col.names= "pretty"))
colnames(Age)

acs.bg$AgeU18 <- (apply(Age[,c(3:6, 27:30)], 1, FUN=sum))/acs.bg$TotalPop
acs.bg$Age1834 <- (apply(Age[,c(7:12, 31:36)], 1, FUN=sum))/acs.bg$TotalPop
acs.bg$Age3564 <- (apply(Age[,c(13:19, 37:43)], 1, FUN=sum))/acs.bg$TotalPop
acs.bg$AgeO65 <- (apply(Age[,c(20:25, 44:49)], 1, FUN=sum))/acs.bg$TotalPop


###
# Racial and Ethnic Composition
###

acs.bg$White <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, variable = "B03002_003"))[,1]/acs.bg$TotalPop
acs.bg$Black <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, variable = "B03002_004", col.names = "Black"))[,1]/acs.bg$TotalPop
acs.bg$Asian <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, variable = "B03002_006", col.names = "Asian"))[,1]/acs.bg$TotalPop
acs.bg$Hispanic <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, variable = "B03002_012", col.names = "Hispanic"))[,1]/acs.bg$TotalPop
acs.bg$TwoOrMore <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, variable = "B03002_009", col.names = "TwoOrMore"))[,1]/acs.bg$TotalPop
acs.bg$EthHet <-  1-(acs.bg$White^2 + acs.bg$Hispanic^2 + acs.bg$Black^2 + acs.bg$Asian^2)


###
# Economic Characteristics 
###
acs.bg$MedHouseIncome  <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, variable = "B19013_001"))[,1] 

pubassist <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, variable = c("B19057_002","B19057_001")))
acs.bg$PubAssist  <- pubassist[,1]/pubassist[,2]

fampov <- estimate(acs.fetch(endyear = endyr, span = 5, geography = ma.blkgrps, table.number = c("B17010"), col.names= "pretty"))
acs.bg$FamPovPer  <- fampov[,2]/fampov[,1]

# Unemployment Rate below not available for 2005-09, 2006-10
unemp <- estimate(acs.fetch(endyear = endyr, span = 5, geography = ma.blkgrps, variable = c("B23025_005","B23025_003")))
acs.bg$UnempRate  <- unemp[,1]/unemp[,2]


###
# Family and Household Characteristics 
###

family <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, table.number = c("B11001"), col.names = "pretty")) 
colnames(family)

acs.bg$TotalHouseH <- family[,1]
acs.bg$FamHousePer  <- family[,2]/family[,1]
acs.bg$FemHeadPer <- family[,6]/family[,2]

###  
# Educational Attainment Characteristics
###

edu <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, table.number = "B15002", col.names = "pretty"))
colnames(edu)

acs.bg$LessThanHS <- apply(edu[,c(3:10,20:27)], 1, FUN=sum)/edu[,1]
acs.bg$HSGrad <- apply(edu[,c(11,28)], 1, FUN=sum)/edu[,1]
acs.bg$SomeColl <- apply(edu[,c(12:14,29:31)], 1, FUN=sum)/edu[,1]
acs.bg$Bach <- apply(edu[,c(15,32)], 1, FUN=sum)/edu[,1]
acs.bg$Master <- apply(edu[,c(16,33)], 1, FUN=sum)/edu[,1]
acs.bg$Prof <- apply(edu[,c(17,34)], 1, FUN=sum)/edu[,1]
acs.bg$Doc <- apply(edu[,c(18,35)], 1, FUN=sum)/edu[,1]


###
# Transportation to work
###

commute.time <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, table.number = "B08303", col.names = "pretty"))
colnames(commute.time)

acs.bg$CommuteLess10 <- apply(commute.time[,c(2:3)], 1, FUN=sum) / commute.time[,1]
acs.bg$Commute1030 <- apply(commute.time[,c(4:7)], 1, FUN=sum) / commute.time[,1]
acs.bg$Commute3060 <- apply(commute.time[,c(8:11)], 1, FUN=sum) / commute.time[,1]
acs.bg$Commute6090 <- commute.time[,c(12)] / commute.time[,1]
acs.bg$CommuteOver90 <- commute.time[,c(13)] / commute.time[,1]


commute.type<- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, table.number = "B08301", col.names = "pretty"))
colnames(commute.type)

acs.bg$ByCar <- commute.type[,2] / commute.type[,1]
acs.bg$ByPubTrans <- commute.type[,10] / commute.type[,1]
acs.bg$ByBike <- commute.type[,18] / commute.type[,1]
acs.bg$ByWalk <- commute.type[,19] / commute.type[,1]


###
# Housing Characteristics
### 

occstatus<- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, table.number = "B25002", col.names = "pretty"))
typeocc <-  estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, table.number = "B25009", col.names = "pretty"))

# occupany status percentages
acs.bg$TotalHouseUnits <- occstatus[,1]
acs.bg$VacantUnitPer <- occstatus[,3]/occstatus[,1]
acs.bg$RentersPer <- typeocc[,10]/typeocc[,1]
acs.bg$HomeOwnPer <- typeocc[,2]/typeocc[,1]

# NOTE: Median Gross Rent is top-coded at 2001, Median Home Value is top-coded at 1000001
acs.bg$MedGrossRent <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, variable = "B25064_001"))[,1]
acs.bg$MedHomeVal <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, variable = "B25077_001"))[,1]
acs.bg$MedYrBuiltRaw <- estimate(acs.fetch(endyear = endyr, span = 5,geography = ma.blkgrps, variable = "B25035_001"))[,1]
acs.bg$MedYrBuilt <- cut(acs.bg$MedYrBuiltRaw, breaks=c(1900,1940,1950,1960,1970,1980,1990,2000,2010, Inf), labels= c("Prior to 1940","1940 to 1949", "1950 to 1959", "1960 to 1969", "1970 to 1979", "1980 to 1989", "1990 to 1999", "2000 to 2009", "2010 or Later"), right=F)


#####################################################################
#### EXPORT FINAL DATA SET
#####################################################################
head(acs.bg)
summary(acs.bg)

write.csv(acs.bg, file = "NAME YOUR LOCATION", row.names=F)