### List of ACS Variables
# NOT CREATED FOR BARI
# fOUND ONLINE AS REFERENCE

# A list of default variables to fetch
# must be one name one code
#    TotalPop = "B01003_001",
#    Female = "B01001_026",
#    Male = "B01001_002",
#    AgeU5 = "B06001_002",
#    Age517 = "B06001_003",
#    Age1824 = "B06001_004",
#    Age2534 = "B06001_005",
#    Age3544 = "B06001_006",
#    Age4554 = "B06001_007",
#    Age5559 = "B06001_008",
#    Age6061 = "B06001_009",
#    Age6264 = "B06001_010",
#    Age6574 = "B06001_011",
#    Age75P = "B06001_012",
#    BornInState = "B06001_013",
#    BornInOtherState = "B06001_025",
#    NativeBornOutOfUS = "B06001_037",
#    ForeignBorn = "B05002_013",
#    White = "B03002_003",
#    Black = "B03002_004",
#    Asian = "B03002_006",
#    Hispanic = "B03002_012",
#    TwoOrMore = "B03002_009", # two or more races
#    MedHouseIncome = "B19013_001",
#    GINI = "B19083_001",
#    PubAssistYes = "B19057_002",  # household with public assistantship
#    PubAssistTotal = "B19057_001",
#    PovertyTotalHousehold = "B17019_002",
#    BelowPoverty = "B17019_002",
#    BelowPovertyOccupied = "B17019_003",  # owner occupied
#    BelowPovertyRenter = "B17019_004",  # owner occupied
#    LaborTotal = "B23025_003",  # civilian in labor force
#    LaborUnemp = "B23025_005",
#    HouseholdTotal = "B11011_001", 
#    MaleHead = "B11011_011",  # male householder, no wife
#    FemaleHead = "B11011_012",  # female householder, no husband
#    Nonfamily = "B11011_016", # nonfamily households
#    GrandWithChildTotal = "B10050_001",
#    GrandHead = "B10051_002"


MutateAcs <- function(dat) {
    # Convert abs numbers to percetages
    mutate(dat,
           Female = Female / TotalPop,
           Male = Male / TotalPop,
           SexRatio = Female / Male,
           AgeU5 = AgeU5 / TotalPop,
           Age517 = Age517 / TotalPop,
           Age1824 = Age1824 / TotalPop,
           Age2534 = Age2534 / TotalPop,
           Age3544 = Age3544 / TotalPop,
           Age4554 = Age4554 / TotalPop,
           Age5559 = Age5559 / TotalPop,
           Age6064 = (Age6061 + Age6264) / TotalPop,
           Age6574 = Age6574 / TotalPop,
           Age75P = Age75P / TotalPop,
           BornInState = BornInState / TotalPop,
           BornInOtherState = BornInOtherState / TotalPop,
           NativeBornOutOfUS = NativeBornOutOfUS / TotalPop,
           ForeignBorn = ForeignBorn / TotalPop,
           White = White / TotalPop,
           Black = Black / TotalPop,
           Asian = Asian / TotalPop,
           Hispanic = Hispanic / TotalPop,
           TwoOrMore = TwoOrMore / TotalPop,
           EthHet =  1 - (White^2 + Hispanic^2 + Black^2 + Asian^2),
           MedHouseIncome = MedHouseIncome,
           MaleHead = MaleHead / HouseholdTotal,
           FemaleHead = FemaleHead / HouseholdTotal,
           Nonfamily = Nonfamily / HouseholdTotal,
           GrandHead = GrandHead / GrandWithChildTotal,
           PubAssist = PubAssistYes / PubAssistTotal,
           UnempRate = LaborUnemp / LaborTotal,
           GINI = GINI
    )
}

FetchACS <- function(endyr = 2015,
                     yrspan = 5,
                     variables = kDefaultVars,
                     state = "MA",
                     geo.group = "tract",
                     raw.numbers = FALSE,
                     savefile = TRUE) {
    
# Generage census indicators for given period
    # Return: a data frame of the indicators
    
    fetch.one <- function(geo.filter) {
        # if fail, return an NA row
        dat <- as.data.frame(t(variables))
        dat[,] <- 0
        tryCatch({
            dat <- acs.fetch(
            endyear = endyr,
            span = yrspan,
            geography = geo.filter,
            variable = variables,
            col.names = names(variables)
            )
        }, error = function(e) e)
        dat
    }
    
    if (geo.group == "tract") {
        geo.filter = geo.make(state = state, county="*", tract = "*")
        dat <- fetch.one(geo.filter)
        geo.m <- geography(dat)
        CT_ID_10 <- geo.m$state * 1e9 + geo.m$county * 1e6 + as.integer(geo.m$tract)
        CT_ID_10 <- str_pad(CT_ID_10, 11, pad = "0")
                dat %<>% estimate() %>% as.data.frame()
                dat <- cbind(NAME = row.names(dat), CT_ID_10, dat)
                row.names(dat) <- NULL
            } else {
        f.state <- state
        zips <- filter(zipcode, state == f.state) %>% .$zip
        # zips <- zips[1:4]
        # dat <- data.frame()
        # results <- lapply(zips, function(zip) {
        #   fetch.one(geo.make(zip.code = zip))
        # })
        # dat <- rbind(dat, do.call(rbind, results))
        # dat <- cbind(zip = zips, dat)
        dat <- acs.fetch(
            endyear = endyr,
            span = yrspan,
            geography = geo.make(zip.code = paste0(zips, collapse = ",")),
            variable = variables,
            col.names = names(variables)
        )
        # convert to a data frame
        dat %<>% estimate() %>% as.data.frame()
        # get zip codes from row names 
        zip <- str_match(row.names(dat), "[0-9]{5}")
        # put zip code in a new column
        dat <- cbind(zip, dat)
    }
    
    if (savefile) {
        # startyr <- endyr - span
        # filename <- sprintf("ACS_%s_%s.csv", startyr, endyr)
        # write.csv(dat, file = filename, row.names = F)
    }
    if (raw.numbers) {
        return(dat)
    }
    MutateAcs(dat)
}

# `aci` is ACS indicators
aci.zip.orig <- FetchACS(geo.group = "zip", raw.numbers = TRUE)
aci.zip <- MutateAcs(aci.zip.orig)
aci.tract <- FetchACS(geo.group = "tract")