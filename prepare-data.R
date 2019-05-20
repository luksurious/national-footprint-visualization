
# Do some pre-filtering for the biocapacity
totalBiocapPerCountry <- rawData[rawData$record == "BiocapTotGHA",]
capitaBiocapPerCountry <- rawData[rawData$record == "BiocapPerCap",]

totalBiocapContinent <- aggregate(
  cbind(
    crop_land,
    grazing_land,
    forest_land,
    fishing_ground,
    built_up_land,
    population,
    total
  ) ~ year + UN_region,
  totalBiocapPerCountry,
  sum
)

capitaBiocapContinent <- aggregate(
  cbind(
    crop_land,
    grazing_land,
    forest_land,
    fishing_ground,
    built_up_land,
    population,
    total
  ) ~ year + UN_region,
  capitaBiocapPerCountry,
  sum
)

# Footprint data
totalFootprintPerCountry <- rawData[rawData$record == "EFConsTotGHA",]
capitaFootprintPerCountry <- rawData[rawData$record == "EFConsPerCap",]

totalFootprintContinent <- aggregate(
  cbind(
    crop_land,
    grazing_land,
    forest_land,
    fishing_ground,
    built_up_land,
    population,
    carbon,
    total
  ) ~ year + UN_region,
  totalFootprintPerCountry,
  sum
)

capitaFootprintContinent <- aggregate(
  cbind(
    crop_land,
    grazing_land,
    forest_land,
    fishing_ground,
    built_up_land,
    population,
    carbon,
    total
  ) ~ year + UN_region,
  capitaFootprintPerCountry,
  sum
)

# Function to select data depending on input
selectBiocapData <- function (regionType,
                              country,
                              region,
                              dataType,
                              years) {
  return(selectData('Biocap', regionType, country, region, dataType, years))
}

selectFootprintData <- function (regionType,
                              country,
                              region,
                              dataType,
                              years) {
  return(selectData('Footprint', regionType, country, region, dataType, years))
}

selectData <-
  function(record,
           regionType,
           country,
           region,
           dataType,
           years) {
    if (regionType == 'Countries') {
      if (dataType == 'Per person') {
        if (record == 'Biocap') {
          data <- capitaBiocapPerCountry
        } else {
          data <- capitaFootprintPerCountry
        }
      } else {
        if (record == 'Biocap') {
          data <- totalBiocapPerCountry
        } else {
          data <- totalFootprintPerCountry
        }
      }
      region <- country
    } else {
      if (record == 'Biocap') {
        data <- totalBiocapContinent
      } else {
        data <- totalFootprintContinent
      }
    }
    
    
    names(data)[names(data) == "UN_region"] <- "region"
    names(data)[names(data) == "country"] <- "region"
    
    data <- data[data$region == region
                 & data$year >= years[1]
                 & data$year <= years[2],]
    
    # per continent data per capita is not provided, we need to roughly calculate it
    # because of the large numbers, the precision is not perfect
    if (dataType == 'Per person' && regionType == 'Continents') {
      data$crop_land <- data$crop_land / data$population
      data$grazing_land = data$grazing_land / data$population
      data$forest_land = data$forest_land / data$population
      data$fishing_ground = data$fishing_ground / data$population
      data$built_up_land = data$built_up_land / data$population
      data$total = data$total / data$population
    }
    
    return(data)
  }