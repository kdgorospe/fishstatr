map_production<-function(tidy_fish, year, fish_var){
  require(tidyverse)
  require(rnaturalearth)
  require(rnaturalearthdata)
  require(rgeos)

  worldmap <- ne_countries(scale = "medium", returnclass = "sf")
  # FIXIT include as part of documentation that FAOSTAT data should inclue 3ALPHA ISO code to match with polygon database in rnaturalearth
  # For matching countries, use 3ALPHA ISO code because FAO data has separate reports for Taiwan; if matching by 3alpha UN code, Taiwan would be dropped

  year_fish <- tidy_fish %>%
    filter(Year == year)

  # CLEAN NAMES:
  # Which names don't match
  food_countries<-unique(food_consumption$country)
  food_countries[!food_countries %in% worldmap$name]
}
