
# Load data
rawData <- read.csv("data/NFA 2018.csv")


# Add ISO-2 country codes to data set
countryCodes <- read.csv("data/country-codes.csv", na.strings = "-----")
countryCodes <- countryCodes[, c("alpha.2", "alpha.3")]

rawData <- merge(rawData, countryCodes, by.x = "ISO.alpha.3.code", by.y = "alpha.3",
                 sort = FALSE, all.x = TRUE)
rawData <- rawData[order(rawData$country, rawData$year, rawData$record), ]
