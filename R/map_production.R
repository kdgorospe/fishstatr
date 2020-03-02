#geo_level can be country, region, continent
#fish_var can be quantity or value
#fish_level can be total, family, order, isscaap, or species

# Testing fuction:
#tidy_fish<-tmp_fish
#year_start=2013
#year_end=NA
#fish_var="quantity"
#fish_level="total"
#fish_type=NA
#geo_level="country"
#fish_unit="kg"

map_production<-function(tidy_fish,
                         year_start,
                         year_end=NA,
                         fish_var="quantity",
                         fish_level="total",
                         fish_type=NA,
                         geo_level="country"){
  require(dplyr)
  require(rnaturalearth)
  require(rnaturalearthdata)
  require(rgeos)
  require(ggplot2)
  require(sf)

  worldmap <- ne_countries(scale = "medium", returnclass = "sf")
  # note use iso_a3 code to keep Taiwan separate from China, since Taiwan has separate FAO reporting data
  # other examples?
  # i.e., better to merge with finer scale now and aggregate later, otherwise separate reports are lost with the merge
  # need to include this in instructions for people downloading data from FishStatJ

  # Filter correct units
  tidy_fish<-tidy_fish %>%
    filter(unit == fish_unit)

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
    merge_col <- "iso_a3"
  }
  if (geo_level == "continent"){
    geography<-"continent_group"
    merge_col <- "region_un"
  }
  if (geo_level == "region"){
    geography<-"georegion_group"
    # FIXIT - need to check, no equivalent for this merge???
  }

  # FIXIT - probably don't want to produce all 48 levels of isscaap groups, 416 levels of taxa "family", or 104 levels of "order"
  # FIXIT - instead, default should be "Total", otherwise map specified isscaap, family, or species
  # Match column name to fish_level parameter
  if (fish_level == "isscaap"){
    taxa <- "isscaap_group"
  } else if (fish_level == "family"){
    taxa <- "family"
  } else if (fish_level == "species") { # Filter specific species
    taxa <- "species_scientific_names"
  }

  # Now aggregate: If total fish is desired:
  if (fish_level == "total"){
    year_geo_taxa_fish<-year_fish %>%
      group_by(get(geography)) %>%
      summarize(fish_sum = sum(get(fish_var))) %>%
      rename(!!geography := 'get(geography)') # using !! and := tells dplyr to rename based on the expression of geography
  } else { # If grouped fish is desired:
    year_geo_taxa_fish<-year_fish %>%
      filter(get(taxa) == fish_type) %>% # FIXIT need to test if this works
      group_by(get(geography)) %>%
      summarize(fish_sum = sum(get(fish_var)))
  }

  worldmap[[merge_col]] <- as.factor(worldmap[[merge_col]])
  worldmap_nomatch <- levels(worldmap[[merge_col]])[!levels(worldmap[[merge_col]]) %in% levels(year_geo_taxa_fish[[geography]])]
  fao_nomatch <- levels(year_geo_taxa_fish[[geography]])[!levels(year_geo_taxa_fish[[geography]]) %in% levels(worldmap[[merge_col]])]
  sort(unique(year_fish$country_name_en[year_fish$country_iso3_code %in% fao_nomatch]))
  # Countries in Worldmap but not in FAO data: "ALA" "ATA" "ESH" "GGY" "HMD" "JEY" "SGS" "VAT"
  # Countries in FAO but not Worldmap:"ANT" "BES" "CSK" "EAZ" "GIB" "GLP" "GUF" "MTQ" "MYT" "REU" "SCG" "SJM" "SUN" "TKL" "TUV" "YUG"
  #[1] Bonaire/S.Eustatius/Saba Channel Islands          Czechoslovakia
  #[4] French Guiana            Gibraltar                Guadeloupe
  #[7] Martinique               Mayotte                  Netherlands Antilles
  #[10] Other nei                Réunion                  Serbia and Montenegro
  #[13] Sudan (former)           Svalbard and Jan Mayen   Tokelau
  #[16] Tuvalu                   Un. Sov. Soc. Rep.       Yugoslavia SFR
  #[19] Zanzibar

  combined <- sort(union(levels(worldmap[[merge_col]]), levels(year_geo_taxa_fish[[geography]])))
  levels(worldmap[[merge_col]])<-combined
  levels(year_geo_taxa_fish[[geography]])<-combined
  firstname <- geography
  join_cols <- merge_col
  names(join_cols) <- firstname

  mapdat <- year_geo_taxa_fish %>%
    left_join(worldmap, join_cols) %>%
    arrange(desc(fish_sum))

  # Plot map
  allplots<-
    theme_void()+
    theme(axis.text.x = element_blank(),
          legend.title=element_text(size=30),
          legend.text=element_text(size=25),
          title = element_text(size=40))



  p1<-ggplot()+
    geom_sf(data=mapdat, aes(fill=fish_sum, geometry=geometry))+ # When working with tibbles, Need to specify geometry column manually
    labs(title="Production")+
    allplots

  png(filename="plot.png", width=1800, height=1200)
  print(p1)
  dev.off()

}
