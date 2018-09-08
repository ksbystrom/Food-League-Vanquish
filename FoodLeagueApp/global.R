library(dplyr)

bikemaps <- read.csv("data/Bikemaps(collision).csv")

cleantable <- bikemaps %>%
  select(
    Type = i_type,
    With = incident_with,
    Date = date,
    Details = details,
    Riding = riding_on,
    Terrain = terrain,
    Direction = direction,    
    Turning = turning,
    Age = age,
    Sex = sex,
    Longitude = longitude,
    Latitude = latitude,
    X = X,
    Y = Y
  )