

# Do some pre-filtering for the biocapacity
totalBiocapPerCountry <- rawData[rawData$record == "BiocapTotGHA",]
capitaBiocapPerCountry <-
  rawData[rawData$record == "BiocapPerCap",]

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

# Function to select data depending on input
selectBiocapData <-
  function(regionType,
           country,
           region,
           dataType,
           years) {
    if (regionType == 'Countries') {
      if (dataType == 'Per person') {
        data <- capitaBiocapPerCountry
      } else {
        data <- totalBiocapPerCountry
      }
      region <- country
    } else {
      data <- totalBiocapContinent
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