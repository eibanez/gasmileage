# fetch.R: Functions to fetch data from the fueleconomy.gov website

# Install and load the XML library
install.packages("XML")
require("XML")

# Used to clean strings
trim <- function(string) {
  gsub('^[[:space:]]+', '',
  gsub('[[:space:]]+$', '', string))
}

getMakeName <- function(shortmake) {
  long <- switch(shortmake,
    ASC = "ASC Incorporated",
    Alfa = "Alfa Romeo",
    ASC = "ASC Incorporated",
    Alfa = "Alfa Romeo",
    American = "American Motors Corporation",
    Aston = "Aston Martin",
    Autokraft = "Autokraft Limited",
    Austin = "Austin Rover",
    Aurora = "Aurora Cars Ltd",
    Bill = "Bill Dovell Motor Car Company",
    Bitter = "Bitter Gmbh and Co. Kg",
    CCC = "CCC Engineering",
    CX = "CX Automotive",
    Consulier = "Consulier Industries Inc",
    Dabryan = "Dabryan Coach Builders Inc",
    E. = "E. P. Dutton, Inc.",
    Environmental = "Environmental Rsch and Devp Corp",
    Evans = "Evans Automobiles",
    Excalibur = "Excalibur Autos",
    Federal = "Federal Coach",
    General = "General Motors",
    Grumman = "Grumman Olson",
    Isis = "Isis Imports Ltd",
    Import = "Import Trade Services",
    JBA = "JBA Motorcars, Inc.",
    J.K. = "J.K. Motors",
    Laforza = "Laforza Automobile Inc",
    Lambda = "Lambda Control Systems",
    Land = "Land Rover",
    London = "London Coach Co Inc",
    Mcevoy = "Mcevoy Motors",
    Panoz = "Panoz Auto-Development",
    Panther = "Panther Car Company Limited",
    PAS = "PAS, Inc",
    "PAS," = "PAS, Inc",
    Red = "Red Shift Ltd.",
    Roush = "Roush Performance",
    Ruf = "Ruf Automobile Gmbh",
    Saleen = "Saleen Performance",
    Tecstar = "Tecstar, LP",
    "Tecstar," = "Tecstar, LP",
    Texas = "Texas Coach Company",
    TVR = "TVR Engineering Ltd",
    Vixen = "Vixen Motor Company",
    Volga = "Volga Associated Automobile",
    Wallace = "Wallace Environmental")
  if (is.null(long)) {
    getMakeName <- shortmake
  } else {
    getMakeName <- long
  }
}

# Get data from the end of tables
getDataBottom <- function(tds,num) {
  tmp <- sapply(xmlChildren(tds[[length(tds)-num]]), xmlValue)
  getDataBottom <- trim(as.character(tmp))
}

# Read mpg information for the corresponding cell
getMpgCell <- function(cell, offset=0) {
  temp <- sapply(xmlChildren(cell), xmlValue)
  fuel <- temp[1+offset]
  mpg <- temp[3+offset]
  mpgcity <- gsub("City", "", temp[7+offset])
  mpghwy <- gsub("Highway", "", temp[9+offset])
  getMpgCell <- cbind(mpg, mpgcity, mpghwy, fuel)
}

# Given the 'id' of a vehicle, return the data fetched
getVehicleData <- function(id) {
  url <- paste("http://www.fueleconomy.gov/feg/noframes/", id, ".shtml", sep="")
  count <- 0
  doc.read <- FALSE  
  while (!doc.read && count < 5) {  # Keep trying until done at least 5 times
    try(this.fails, silent = TRUE)
    try(doc <- htmlParse(url, error = function(...){}))
    errmsg <- strsplit(geterrmessage(), "/noframes/")[[1]][2]
    if (!is.na(errmsg)) { count <- count +1 } else { doc.read <- TRUE }
  }
  
  getVehicleData <- NULL
  
  # Data successfully fetched
  if (doc.read) {
    # Get year, make and model
    caption <- getNodeSet(doc, "//caption")
    name <- sapply(xmlChildren(caption[[1]]), xmlValue)
    name <- as.character(name)
    name2 <- strsplit(name, " ")
    year <- name2[[1]][1]
    make <- getMakeName(name2[[1]][2])
    name <- gsub(paste(year,make), "", name)
    name <- trim(name)
    
    # Get cylinders, engine size, drive and speeds
    tds <- getNodeSet(doc, "//td")
    temp <- sapply(xmlChildren(tds[[1]]), xmlValue)
    if (length(temp)==1) temp <- sapply(xmlChildren(tds[[2]]), xmlValue)
    val_eng <- strsplit(trim(temp[1]), ", ")$text
    valves <- gsub(" cyl", "", val_eng[1])
    engine <- gsub(" L", "", val_eng[2])
    trans <- trim(temp[3])
    fuel <- strsplit(trim(temp[5]), " or ")$text
    
    # Get values in the last table
    len <- length(tds)
    class <- getDataBottom(tds, 8)
    drive <- getDataBottom(tds, 7)
    guzzler <- getDataBottom(tds, 6)
    turbo <- getDataBottom(tds, 5)
    super <- getDataBottom(tds, 4)
    passenger <- gsub("ft", "", getDataBottom(tds, 3)[1])
    cargo <- gsub("ft", "", getDataBottom(tds, 2)[1])
    
    model <- cbind(year, make, name)
    tech <- cbind(class, engine, valves, trans, drive, guzzler, turbo, super, passenger, cargo)
    
    # Read fuel economy values for all fuels
    if (length(fuel)==1) {
      mpgs <- getMpgCell(tds[[3]])
      special <- sapply(xmlChildren(tds[[2]]), xmlValue)[2]
      getVehicleData <- cbind(id, model, mpgs, tech, special)
    } else {
      mpgs <- rbind(getMpgCell(tds[[4]]), getMpgCell(tds[[5]], 1))
      special <- sapply(xmlChildren(tds[[3]]), xmlValue)[2]
      getVehicleData <- cbind(rbind(id,paste(id, "b")),
                              rbind(model, model),
                              mpgs,
                              rbind(tech, tech),
                              rbind(special, special))
    }
  }
}

# Use the following to test the fetching mechanism
test0 <- getVehicleData(1)      # First entry
test1 <- getVehicleData(29302)  # Flex-fuel vehicle
test2 <- getVehicleData(19302)  # Hybrid

# Save the time at which the fetching began
time_start <- date()

# Check all possible id's and store results
r <- NULL
for (i in 1:30000) r <- rbind(r, getVehicleData(i))

# Save the time when done fetching
time_finish <- date()

# Store raw data into a file
write.csv(r, "bulkmpg.csv", row.names = FALSE)
