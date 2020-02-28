#geo_level can be country, region, continent
#fish_var can be quantity or value
#fish_type can be total, family, order, isscaap, or <scientific name of species>

map_production<-function(tidy_fish,
                         year_start,
                         year_end=NA,
                         fish_var="quantity",
                         fish_type="total",
                         geo_level="country"){
  require(dplyr)
  require(rnaturalearth)
  require(rnaturalearthdata)
  require(rgeos)

  worldmap <- ne_countries(scale = "medium", returnclass = "sf")
  # note use iso_a3 code to keep Taiwan separate from China, since Taiwan has separate FAO reporting data
  # other examples?
  # better to merge with finer scale now and aggregate later, otherwise separate reports are lost with the merge
  # need to include this in instructions for people downloading data from FishStatJ

  # Filter years
  if (is.na(year_end)){
    all_years <- year_start
  } else {
    all_years <- seq(from = year_start, to = year_end, by = 1)
  }
  year_fish <- tidy_fish %>%
    filter(year == all_years) %>%
    droplevels()

  # Match column name to geo_level parameter
  # In tmp_fish there are more levels in iso3 than in "country"; for now, use iso3 as the reporting level
  # for future use: un_a3 in worldmap == "country" column in tmp_fish == code UN-M49 in FishStatJ (3 digit country ID number)
  if (geo_level == "country"){
    geography <- "country_iso3_code"
  }
  if (geo_level == "continent"){
    geography<-"continent_group"
  }
  if (geo_level == "region"){
    geography<-"georegion_group"
  }

  # FIXIT - probably don't want to produce all 48 levels of isscaap groups, 416 levels of taxa "family", or 104 levels of "order"
  # Match column name to fish_type parameter
  if (fish_type == "isscaap"){
    taxa <- "isscaap_group"
  } else if (fish_type == "family"){
    taxa <- "family"
  } else { # Filter specific species
    year_fish <- year_fish %>%
      filter(species_scientific_name == fish_type) #FIXIT need to test if this works
  }

  # Now aggregate: If total fish is desired:
  if (fish_type == "total"){
    year_geo_taxa_fish<-year_fish %>%
      group_by(get(geography)) %>%
      summarize(quant_sum = sum(get(fish_var)))
  } else { # If grouped fish is desired:
    year_geo_taxa_fish<-year_fish %>%
      group_by(get(geography), get(taxa)) %>%
      summarize(quant_sum = sum(get(fish_var)))
  }








  # CLEAN NAMES:
  # Which names don't match
  food_countries<-unique(food_consumption$country)
  food_countries[!food_countries %in% worldmap$name]
}
