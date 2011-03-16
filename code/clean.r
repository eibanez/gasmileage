# clean.r: Read the data that was fetched and clean data for later use

mpg <- read.csv("bulkmpg.csv")
head(mpg)
summary(mpg)


# Standardize fuel entry
summary(mpg$fuel)
fixFuel <- function(entry) {
  switch(entry,
    Reg = "Regular",
    Pre = "Premium",
    Mid = "Midgrade",
    Com = "CNG",
    Die = "Diesel",
    Ele = "Electricity",
    entry)
}
mpg$fuel <- as.character(lapply(strtrim(mpg$fuel, 3), fixFuel))


# Dismiss entries without engine or cylinder info
summary(mpg$engine)
summary(mpg$valves)
mpg <- mpg[!is.na(mpg$engine) & !is.na(mpg$valves), ]


# Conversion to binary
mpg$guzzler <- ifelse(mpg$guzzler=="yes", 1, 0)
mpg$turbo <- ifelse(mpg$turbo=="yes", 1, 0)
mpg$super <- ifelse(mpg$super=="yes", 1, 0)


# Extract transmission and number of speeds
summary(mpg$trans)
mpg <- mpg[mpg$trans!="", ]
temp <- gsub("Automatic ?\\(?", "", mpg$trans)
temp <- gsub("Manual ?\\(?", "", temp)
temp <- gsub("Auto ?\\(?", "", temp)
temp <- gsub(" Doubled", "", temp)
temp <- gsub("-?spd", "", temp)
temp <- gsub("[)ACML ]", "", temp)
first_tmp <- strtrim(temp, 1)
mpg$variable <- ifelse(first_tmp == "v" | first_tmp == "V", 1, 0)
mpg$nospeeds <- ifelse(temp == "None", 1, 0)
temp[(mpg$variable + mpg$nospeeds) > 0] <- "0"

SimpleTrans <- function(letter) {
  switch(letter, A="Automatic", M="Manual", N="None")
}
mpg$trans <- lapply(strtrim(mpg$trans, 1), SimpleTrans)
mpg$trans <- as.character(mpg$trans)
mpg$trans[strtrim(temp, 1) == "S"] <- "Auto. Shift"

temp <- gsub("S", "", temp)
mpg$speeds <- factor(temp)


# Make uniform drive variables
summary(mpg$drive)
mpg <- mpg[mpg$drive != "" | mpg$nospeeds == 1, ]
SimpleDrive <- function(letter) {
  switch(letter, F="Front", "2"="Front", R="Rear", "A"="All", "4"="All", "None")
}
mpg$drive <- lapply(strtrim(mpg$drive, 1), SimpleDrive)
mpg$drive <- as.character(mpg$drive)


# Fix passenger and cargo volume
mpg$passenger <- ifelse(mpg$passenger=="Not Available", NA, mpg$passenger)
mpg$cargo <- ifelse(mpg$cargo=="Not Available", NA, mpg$cargo)


# Fix class
summary(mpg$class)
temp <- gsub(" ?-? ?/?[0-9][wW][dD]", "", mpg$class)
temp <- gsub(" Cars", "", temp)
temp <- gsub(" Wagons", "", temp)
temp <- gsub(" Trucks", "", temp)
temp <- gsub("Standard", "Std.", temp)
temp <- gsub("Midsize-", "", temp)
temp[strtrim(temp,7) == "Special"] <- "Special"
temp[strtrim(temp,4) == "Vans"] <- "Vans"
temp[strtrim(temp,5) == "Sport"] <- "SUVs"
mpg$class <- factor(temp)

write.csv(mpg, "mpg.csv")

