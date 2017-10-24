install.packages("rgdal")
library(rgdal)

setwd("G:/BARI/BARI Work/ACS")
mass.tr <- readOGR(".", "CENSUS2010TRACTS_POLY")
mass1115.tr <- readOGR(".", "ACS_1115_TRACT") 

mass0913.tr <- merge(mass.tr, acs0913.tr, by.x = "GEOID10", by.y="CT_ID_10", all.x=TRUE, all.y = TRUE)
mass1014.tr <- merge(mass.tr, acs1014.tr, by.x = "GEOID10", by.y="CT_ID_10", all.x=TRUE, all.y = TRUE)
mass1115.tr <- merge(mass.tr, acs1115.tr, by.x = "GEOID10", by.y="CT_ID_10", all.x=TRUE, all.y = TRUE)

summary(mass0913.tr)
summary(acs0913.tr)

writeOGR(mass0913.tr, ".", "ACS0913_TRACT_SHAPE", driver = "ESRI Shapefile")
writeOGR(mass1014.tr, ".", "ACS1014_TRACT_SHAPE", driver = "ESRI Shapefile")