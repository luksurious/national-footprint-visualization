
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
      if (dataType == 'Per person') {
        # only World has given data per capita, other regions must be calculated
        if (region == 'World') {
          if (record == 'Biocap') {
            data <- capitaBiocapContinent
          } else {
            data <- capitaFootprintContinent
          }
        } else {
          if (record == 'Biocap') {
            data <- totalBiocapContinent
          } else {
            data <- totalFootprintContinent
          }
        }
      } else {
        if (record == 'Biocap') {
          data <- totalBiocapContinent
        } else {
          data <- totalFootprintContinent
        }
      }
    }
    
    
    names(data)[names(data) == "UN_region"] <- "region"
    names(data)[names(data) == "country"] <- "region"
    
    data <- data[data$region == region
                 & data$year >= years[1]
                 & data$year <= years[2],]
    
    # per continent data per capita is not provided, we need to roughly calculate it
    # because of the large numbers, the precision is not perfect
    if (dataType == 'Per person' && regionType == 'Continents' && region != 'World') {
      data$crop_land <- data$crop_land / data$population
      data$grazing_land = data$grazing_land / data$population
      data$forest_land = data$forest_land / data$population
      data$fishing_ground = data$fishing_ground / data$population
      data$built_up_land = data$built_up_land / data$population
      data$total = data$total / data$population
      
      if (record == 'Footprint') {
        data$carbon = data$carbon / data$population
      }
    }
    
    return(data)
  }

deficitData <- function (regionType, dataType) {
  columns <- c("total", "year")
  if (dataType == 'Total') {
    if (regionType == 'Countries') {
      footprint <- totalFootprintPerCountry
      biocapacity <- totalBiocapPerCountry
      
      columns <- append(columns, c("country", "ISO.alpha.3.code"))
      matchCol <- "country"
    } else {
      footprint <- totalFootprintContinent
      biocapacity <- totalBiocapContinent
      
      columns <- append(columns, c("UN_region"))
      matchCol <- "UN_region"
    }
  } else {
    if (regionType == 'Countries') {
      footprint <- capitaFootprintPerCountry
      biocapacity <- capitaBiocapPerCountry
      
      columns <- append(columns, c("country", "ISO.alpha.3.code"))
      matchCol <- "country"
    } else {
      footprint <- capitaFootprintContinent
      biocapacity <- capitaBiocapContinent
      
      columns <- append(columns, c("UN_region"))
      matchCol <- "UN_region"
    }
  }
  
  cur_data <- merge(footprint[, columns],
                    biocapacity[, columns],
                    by = c(matchCol, "year"))
  cur_data <-
    within(cur_data, diff <- total.y - total.x)
  
  cur_data <- cur_data[order(cur_data$year), ]
  
  #names(cur_data)[names(cur_data) == "UN_region"] <- "region"
  #names(cur_data)[names(cur_data) == "country"] <- "region"
  

  return(cur_data)
}
