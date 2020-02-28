map_fish<-function(tidy_fish, year){
  require(tidyverse)
  require(rnaturalearth)
  require(rnaturalearthdata)
  require(rgeos)
  year_fish <- tidy_fish %>%
    filter(Year == year)

  worldmap <- ne_countries(scale = "medium", returnclass = "sf")
  # FIXIT include as part of documentation that FAOSTAT data should inclue UN codes so this can be used to match with world map polygon databases


  # CLEAN NAMES:
  # Which names don't match
  food_countries<-unique(food_consumption$country)
  food_countries[!food_countries %in% worldmap$name]
}
