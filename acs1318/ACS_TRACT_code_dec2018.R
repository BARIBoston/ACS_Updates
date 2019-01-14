+########################################################################################################

# Creating Census Tracts Data File for Boston Research Map: ACS 5 Year Estimates

########################################################################################################

# This file collects downloads and manipulates necessary data sets to create 
#   the Census-based files for the Boston Research Map. In its form below, it 
#   is written for automation of every 5 year estimate of ACS data.
#   The only command that will need to change is the "endyr" variable to specify the most current
#   year of data available (line 34) 

# DATA NOTE: The only external data needed for this update is 
#   the "National Census Tracts Gazetteer File" for Census Tracts.
#   It can be found at (https://www.census.gov/geo/maps-data/data/gazetteer2010.html) as of 2017.

# CODEBOOK NOTE: The Median Gross Rent is top-coded at 2001.
# CODEBOOK NOTE: The Median Home Value is top-coded at 1000001.

########################################################################################################
library(stringr)
library(tidycensus)
library(tidyverse)
library(tigris)
options(tigris_class = "sf",tigris_use_cache = T)

# install API key **(just need to do this once)**
#   If the key below doesn't work, you'll need to get a new Census API key: 
#   (https://api.census.gov/data/key_signup.html) 
census_api_key("93b29625c4103b58817881d60225988fd7f00c3d",install=T)

# Specify the end year and the rest does not need to change, 
#   this is for 5-year estimates; for shorter spans, change "span = 5"
endyr <- 2017

# define the geographies you want data for: here for all tracts/blocks groups 
#   in all counties in MA
# ma.tracts = geo.make(state="MA", county="*", tract="*", check=T)

################################################################################################
### CONSTRUCT DATA SET AND EXTRACT TOTAL POPULATION FROM ACS FOR MASSACHUSETTS CENSUS TRACTS ###
################################################################################################

# check variables:
varlist <- load_variables(year = endyr,dataset = "acs5")
# View(varlist)

### TOTAL POPULATION ###
dat <- tidycensus::get_acs(geography = "tract",
																survey="acs5",
																state="MA",
																year = endyr,
																output = "wide",
																variables = "B01003_001", # Total Pop
																keep_geo_vars = T
)

# Extract numerical CT_ID_10s to build data set
dat <- dat %>%
	rename(CT_ID_10 = GEOID,
				 TotalPop = B01003_001E) %>%
	select(CT_ID_10, NAME, TotalPop)
sum(nchar(dat$CT_ID_10)!=11)==0 # check: must be TRUE


##################################################
#                                                #
#   CONSTRUCTING ACS VARIABLES - BY CATEGORY     #
#                                                #
##################################################

# The ACS uses a series of numbered variables that detail demographic and economic characteristics
# of census tracts and block groups. A useful website for verifying variable names can be found at:
# (https://www.socialexplorer.com/data/ACS2016_5yr/metadata/?ds=ACS16_5yr) as of December 2017

# The name in the working directory to access BARI's shared drive will need to change

#-----------------------#
# Sex, Density, and Age #
#-----------------------#

### DENSITY ###
land <- tigris::tracts(state="MA",year = endyr)
land <- land %>%
	rename(CT_ID_10 = GEOID) %>%
	select(CT_ID_10,ALAND) %>% #ALAND here is in square meters
	mutate(ALAND10_sqmi = ALAND/(2.59e+6)) # convert to sqare miles


#   Merge data, create PopDen variable, remove "ALAND" (land area) column
dat$ALAND10_sqmi <- land$ALAND10_sqmi[match(dat$CT_ID_10, land$CT_ID_10)]
dat <- dat %>%
	mutate(PopDen = TotalPop/ALAND10_sqmi) %>%
	select(-ALAND10_sqmi) # drop old var



### SEX ###
Sex <- tidycensus::get_acs(geography = "tract",
													 survey="acs5",
													 state="MA",
													 year = endyr,
													 output = "wide",
													 variables = c("B01001_026","B01001_002"), #Sex by Age - Female and Sex by Age- Male
													 keep_geo_vars = T
) %>%
	mutate(SexRatio = B01001_026E/B01001_002E) %>%
	select(GEOID,SexRatio)
dat <- left_join(dat,Sex,by=c("CT_ID_10" = "GEOID"))

### AGE ###
Age <- tidycensus::get_acs(geography = "tract",
													 survey="acs5",
													 state="MA",
													 year = endyr,
													 output = "wide",
													 variables = c("B06001_002", "B06001_003", # Under 5 years; Between 5 to 17
													 							"B06001_004", "B06001_005", # 18 to 24 years; 25 to 34 years
													 							"B06001_006", "B06001_007", # 35 to 44 years; 45 to 54 years
													 							"B06001_008", "B06001_009", # 55 to 59 years; 60 to 61 years
													 							"B06001_010", # 61 to 64 years
													 							"B06001_011", "B06001_012" # 65 to 74 years; 75 years and older
													 ), 
													 keep_geo_vars = T
) %>%
	mutate(AgeU18 = B06001_002E + B06001_003E, # combining variables to make aggregate categories
				 Age1834 = B06001_004E + B06001_005E,
				 Age3564 = B06001_006E + B06001_007E + B06001_008E + B06001_009E + B06001_010E,
				 AgeO65 = B06001_011E + B06001_012E
	) %>%
	select(GEOID,AgeU18,Age1834,Age3564,AgeO65)

dat <- left_join(dat,Age,by=c("CT_ID_10" = "GEOID")) %>%
	mutate(AgeU18 = AgeU18/TotalPop,
				 Age1834 = Age1834/TotalPop,
				 Age3564 = Age3564/TotalPop,
				 AgeO65 = AgeO65/TotalPop
				 )

#-------------------------------#
# Racial and Ethnic Composition #
#-------------------------------#
Race <- tidycensus::get_acs(geography = "tract",
																survey="acs5",
																state="MA",
																year = endyr,
																output = "wide",
																variables = c("B05002_013", # foreign born
																							"B03002_003", # white alone
																							"B03002_004", # black or African American alone
																							"B03002_006", # Asian alone
																							"B03002_012", # Hispanic or Latino alone
																							"B03002_009" # Two or more races
																), 
																keep_geo_vars = T
) %>%
	rename(ForBorn = B05002_013E,
				 White = B03002_003E,
				 Black = B03002_004E,
				 Asian = B03002_006E,
				 Hispanic = B03002_012E,
				 TwoOrMore = B03002_009E
	) %>%
	mutate(EthHet = 1-(White^2 + Hispanic^2 + Black^2 + Asian^2)) %>%
	select(GEOID,ForBorn,White, Black, Asian, Hispanic, TwoOrMore, EthHet)

dat <- left_join(dat,Race,by=c("CT_ID_10" = "GEOID")) %>%
	mutate(ForBorn = ForBorn/TotalPop,
				 White = White/TotalPop,
				 Black = Black/TotalPop,
				 Asian = Asian/TotalPop,
				 Hispanic = Hispanic/TotalPop,
				 TwoOrMore = TwoOrMore/TotalPop
	)

# Checking the dataset to see if columns were created
colnames(dat)

#--------------------------#
# Economic Characteristics # 
#--------------------------#

Econ <- tidycensus::get_acs(geography = "tract",
														survey="acs5",
														state="MA",
														year = endyr,
														output = "wide",
														variables = c("B19013_001", # Median Household Income in the Past 12 Months
																					"B19057_002", # households with public assistance
																					"B19057_001", # total households
																					"B19083_001", # GINI index of income inequality
																					"B17019_002", # Income in the Past 12 Months Below Poverty Level
																					"B17019_001", # Total Families
																					"B23025_005", # Number of Unemployed individuals 16 years and over
																					"B23025_003" # Number of civilian Labor Force 16 years and over
														), 
														keep_geo_vars = T
) %>%
	rename(MedHouseIncome = B19013_001E,
				 GINI = B19083_001E
	) %>%
	mutate(PubAssist = B19057_002E/B19057_001E,
				 FamPovPer = B17019_002E/B17019_001E,
				 UnempRate = B23025_005E/B23025_003E) %>%
	select(GEOID,MedHouseIncome,PubAssist,GINI,FamPovPer,UnempRate)

dat <- left_join(dat,Econ,by=c("CT_ID_10" = "GEOID")) 

#--------------------------------------#
# Family and Household Characteristics # 
#--------------------------------------#
Family <- tidycensus::get_acs(geography = "tract",
														survey="acs5",
														state="MA",
														year = endyr,
														output = "wide",
														variables = c("B11011_012", # Female Hoseholder, No Husband Present
																					"B11011_002", # Family Households
																					"B11011_001", # Total number of households
																					"B11009_002", # Unmarried-Partner Households
																					"B11009_003", # Male Householder and Male Partner
																					"B11009_005", # Female Householder and Female Partner
																					"B10051_002"  #  Grandparent Responsible For Own Grandchildren Under 18 Years
														), 
														keep_geo_vars = T
) %>%
	rename(TotalHouseH = B11011_001E
	) %>%
	mutate(FamHousePer = B11011_002E/TotalHouseH,
				 FemHeadPer = B11011_012E/TotalHouseH,
				 SameSexCoupPer = (B11009_003E + B11009_005E)/B11009_002E,
				 GrandHeadPer = B10051_002E/TotalHouseH) %>%
	select(GEOID,TotalHouseH,FamHousePer,FemHeadPer,SameSexCoupPer,GrandHeadPer)

dat <- left_join(dat,Family,by=c("CT_ID_10" = "GEOID")) 


#----------------------------------------# 
# Educational Attainment Characteristics #
#----------------------------------------#

Educ <- tidycensus::get_acs(geography = "tract",
															survey="acs5",
															state="MA",
															year = endyr,
															output = "wide",
															variables = c("B15002_001", # variables for Sex by Edu Attainment for Population 25 yrs or Over
																						"B15002_003","B15002_004","B15002_005",
																						"B15002_006","B15002_007","B15002_008","B15002_009","B15002_010",
																						"B15002_020","B15002_021","B15002_022","B15002_023","B15002_024",
																						"B15002_025","B15002_026","B15002_027",
																						"B15002_011","B15002_028",
																						"B15002_012","B15002_013","B15002_014",
																						"B15002_029","B15002_030","B15002_031",
																						"B15002_015","B15002_032",
																						"B15002_016","B15002_033",
																						"B15002_017","B15002_034",
																						"B15002_018","B15002_035"
															), 
															keep_geo_vars = T
) %>%
	rename(
	) %>%
	mutate(LessThanHS = (B15002_003E+B15002_004E+B15002_005E+B15002_006E+B15002_007E+B15002_008E+B15002_009E+B15002_010E+B15002_020E+B15002_021E+B15002_022E+B15002_023E+B15002_024E+B15002_025E+B15002_026E+B15002_027E)/B15002_001E,
				 HSGrad = (B15002_011E + B15002_028E)/B15002_001E,
				 SomeColl = (B15002_012E + B15002_013E + B15002_014E + B15002_029E + B15002_030E + B15002_031E)/B15002_001E,
				 Bach = (B15002_015E + B15002_032E)/B15002_001E,
				 Master = (B15002_016E + B15002_033E)/B15002_001E,
				 Prof = (B15002_017E + B15002_034E)/B15002_001E,
				 Doc = (B15002_018E + B15002_035E)/B15002_001E
				 ) %>%
	select(GEOID,LessThanHS,HSGrad,SomeColl,Bach,Master,Prof,Doc)

dat <- left_join(dat,Educ,by=c("CT_ID_10" = "GEOID")) 

colnames(dat)


#------------------------#
# Transportation to work #
#------------------------#
Commute <- tidycensus::get_acs(geography = "tract",
														survey="acs5",
														state="MA",
														year = endyr,
														output = "wide",
														variables = c("B08303_001", # COMMUTE TIME denominoator
																					"B08303_003", # Travel Time to Work, various combos
																					"B08303_002",
																					"B08303_007","B08303_006","B08303_005",
																					"B08303_004",
																					"B08303_011","B08303_010","B08303_009",
																					"B08303_008",
																					"B08303_012",
																					"B08303_013",
																					"B08301_001", # COMMUTE TYPE denom
																					"B08301_002", # By Car, Truck or Van
																					"B08301_010", # By Public Transportation (Excluding Taxicab)															
																					"B08301_018", # By Bicycle
																					"B08301_019" # By Walking
														), 
														keep_geo_vars = T
) %>%
	mutate(CommuteLess10 = (B08303_003E + B08303_002E)/B08303_001E,
				 Commute1030 = (B08303_007E + B08303_006E + B08303_005E + B08303_004E)/B08303_001E,
				 Commute3060 = (B08303_011E + B08303_010E + B08303_009E + B08303_008E)/B08303_001E,
				 Commute6090 = B08303_012E/B08303_001E,
				 CommuteOver90 = B08303_013E/B08303_001E,
				 ByAuto = B08301_002E/B08301_001E,
				 ByPubTrans = B08301_010E/B08301_001E,
				 ByBike = B08301_018E/B08301_001E,
				 ByWalk = B08301_019E/B08301_001E
				 	) %>%
	select(GEOID,CommuteLess10,Commute1030,Commute3060,Commute6090,CommuteOver90,ByAuto,ByPubTrans,ByBike,ByWalk)

dat <- left_join(dat,Commute,by=c("CT_ID_10" = "GEOID")) 


#-------------------------#
# Housing Characteristics #
#-------------------------#

Housing <- tidycensus::get_acs(geography = "tract",
															 survey="acs5",
															 state="MA",
															 year = endyr,
															 output = "wide",
															 variables = c(
															 	"B25002_001", # Total Housing Units Occupancy Status
															 	"B25002_003", # Total Vacant Housing Units
															 	"B25009_001", # Total Occupied Housing Units
															 	"B25009_002", # Total Owner Occupied Housing Units
															 	"B25009_010", # Total Renter Occupied Housing Units
															 	"B25064_001", # Median Gross Rent
															 	# NOTE: Median Gross Rent is top-coded at 2001, Median Home Value top-coded at 1000001
															 	"B25077_001", # Median Home Value
															 	"B25035_001", # Median Year Structure Built
															 	"B25039_001", # Median Year Householder Moved Into Unit by Tenure
															 	"B25039_002",
															 	"B25039_003"
															 ), 
															 keep_geo_vars = T
) %>%
	rename(TotalHouseUnits = B25002_001E,
				 MedGrossRent = B25064_001E,
				 MedHomeVal = B25077_001E,
				 MedYrBuiltRaw = B25035_001E,
				 MedYrMovedInraw = B25039_001E,
				 MedYrRentMovedIn = B25039_003E,
				 MedYrOwnMovedIn = B25039_002E
				 ) %>%
	mutate(VacantUnitPer = B25002_003E/TotalHouseUnits,
				 RentersPer = B25009_010E/B25009_001E,
				 HomeOwnPer = B25009_002E/B25009_001E,
				 MedYrBuilt = cut(MedYrBuiltRaw, 
				 									breaks=c(1900,1940,1950,1960,1970,1980,1990,2000,2010, Inf),
				 									labels= c("Prior to 1940","1940 to 1949", "1950 to 1959", 
				 														"1960 to 1969", "1970 to 1979","1980 to 1989", 
				 														"1990 to 1999", "2000 to 2009", "2010 or Later"), right=F)
	) %>%
	select(GEOID,TotalHouseUnits,VacantUnitPer,RentersPer,HomeOwnPer,
				 MedGrossRent,MedHomeVal,MedYrBuiltRaw,MedYrBuilt,MedYrMovedInraw,MedYrRentMovedIn,MedYrOwnMovedIn)

dat <- left_join(dat,Housing,by=c("CT_ID_10" = "GEOID")) 



#------------------------------------------------------------------------#
# Accounting for "-66666" value in a few categories. Labeling them as NA #
#------------------------------------------------------------------------#
summary(dat)
dat$MedHouseIncome[dat$MedHouseIncome < 0] <-NA
dat$GINI[dat$GINI < 0]<-NA
dat$MedGrossRent[dat$MedGrossRent < 0] <-NA
dat$MedHomeVal[dat$MedHomeVal < 0]<-NA
dat$MedYrBuiltRaw[dat$MedYrBuiltRaw < 0]<-NA
dat$MedYrMovedInraw[dat$MedYrMovedInraw < 0] <- NA
dat$MedYrRentMovedIn[dat$MedYrRentMovedIn < 0] <- NA
dat$MedYrOwnMovedIn[dat$MedYrOwnMovedIn < 0] <-NA

summary(dat)

###################################################################
#               ADDING MUNICIPALITIES TO DATA SET                 #
###################################################################
# Data on the cities and towns of Massachusetts can be found in shapefile format at 
# (https://docs.digital.mass.gov/dataset/massgis-data-datalayers-2010-us-census). Using
# GIS we joined this shapefile with 2010 Census Tracts to attribute census tracts to
# towns and cities. Unfortunately, census tracts can span over town borders or include 
# multiple towns. We attributed a centroid to each census tract and calculated where the 
# majority of the tract's land area resided. If less than 50% of a tract resides in one
# municipalty, then the tract is attributed to multiple towns.

matractstowns <- read_csv("../Data/MA Towns Shapefiles/maTractsToTowns.csv")
colnames(matractstowns)

matowns <- matractstowns %>%
	select(TRACTCE10, GEOID10, AREA_ACRES, TOWN_ID,TOWN, FIPS_STCO, COUNTY, AREA_ACR_1) %>%
	mutate(GEOID10 = as.character(GEOID10))

dat <- left_join(dat, matowns, by = c("CT_ID_10" = "GEOID10"))
dat <- dat %>%
	mutate(MAtown = ifelse(AREA_ACRES <= (1+AREA_ACR_1), TOWN,"Multiple Municipalities"))

colnames(dat)
dat <- dat%>%
	select(-MedYrOwnMovedIn,-TRACTCE10)

###################################################################
#                   EXPORT FINAL DATA SET                         #
###################################################################
head(dat)
summary(dat)

#   This will write the csv to the BARI Google Shared Drive
write.csv(dat, file = "../Data/ACS_1317_TRACT.csv", row.names=F)

#####################################################################
#               Creating Shapefiles for Census Tracts               #
#####################################################################

mass.tr <- tigris::tracts(state = "MA",year = endyr)

mass.tr <- mass.tr %>%
	rename(CT_ID_10 = GEOID) %>%
	select(CT_ID_10)
mass1317.tr <- left_join(mass.tr, dat, by=c("CT_ID_10"="CT_ID_10"))

# library(rgdal)
# writeOGR(sf::as_Spatial(mass1317.tr), "../Data/ACS1317 Tracts Shapefiles", "ACS1317_TRACT_SHAPE",overwrite_layer = T, driver = "ESRI Shapefile")

sf::write_sf(mass1317.tr, "../Data/ACS1317 Tracts Shapefiles", "ACS1317_TRACT_SHAPE", driver = "ESRI Shapefile")
