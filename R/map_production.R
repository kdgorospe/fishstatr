#geo_level can be country, region, continent
#fish_var can be quantity or value
#fish_level can be total, family, order, isscaap, or species

# Testing fuction:
tidy_fish<-tmp_fish
year_start=2013
year_end=NA
fish_var="quantity"
fish_level="total"
fish_type=NA
geo_level="country"
fish_unit="kg"
combine_sources=FALSE

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
  # note use FAO's iso_a3 code to keep Taiwan separate from China, since Taiwan has separate FAO reporting data
  # other examples?
  # i.e., better to merge with finer scale now and aggregate later, otherwise separate reports are lost with the merge
  # need to provide instructions for people using FishStatJ to include this as a data field before downloading the dataset
  # worldmap has 235 iso_n3 and 235 iso_a3 codes
  # FAO has 247 country (aka iso_n3) and 260 iso3 alpha codes - finest scale appears to be FAO's iso3 alpha codes

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
  } else if (geo_level == "continent"){
    geography<-"continent_group"
    merge_col <- "region_un"
  } else if (geo_level == "region"){
    geography<-"georegion_group"
    # FIXIT - need to check, no equivalent for this merge???
  }

  # If, fish_level is not "total", then specify the column "taxa", and use this column to filter for specific fish_type
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
    year_geo_taxa_fish <- year_fish %>%
      group_by(get(geography), source_name_en) %>%
      summarize(fish_sum = sum(get(fish_var))) %>%
      rename(!!geography := 'get(geography)') # using !! and := tells dplyr to rename based on the expression of geography
  } else { # If grouped fish is desired:
    year_geo_taxa_fish <- year_fish %>%
      filter(get(taxa) == fish_type) %>% # FIXIT need to test if this works
      group_by(get(geography)) %>%
      summarize(fish_sum = sum(get(fish_var)))
  }

  if (combine_sources == TRUE){
    year_geo_taxa_fish <- year_geo_taxa_fish %>%
      group_by(get(geography)) %>%
      summarize(fish_sum = sum(get_fish_var))
  }

  # Convert fao[[geography]] to character before merge to suppress warning message
  year_geo_taxa_fish[[geography]] <- as.character(year_geo_taxa_fish[[geography]])
  #worldmap[[merge_col]] <- as.factor(worldmap[[merge_col]])
  #worldmap_nomatch <- levels(worldmap[[merge_col]])[!levels(worldmap[[merge_col]]) %in% levels(year_geo_taxa_fish[[geography]])]
  #fao_nomatch <- levels(year_geo_taxa_fish[[geography]])[!levels(year_geo_taxa_fish[[geography]]) %in% levels(worldmap[[merge_col]])]
  #sort(unique(year_fish$country_name_en[year_fish$country_iso3_code %in% fao_nomatch]))
  # Countries in Worldmap but not in FAO data: "ALA" "ATA" "ESH" "GGY" "HMD" "JEY" "SGS" "VAT"
  # Countries in FAO but not Worldmap:"ANT" "BES" "CSK" "EAZ" "GIB" "GLP" "GUF" "MTQ" "MYT" "REU" "SCG" "SJM" "SUN" "TKL" "TUV" "YUG"
  #[1] Bonaire/S.Eustatius/Saba Channel Islands          Czechoslovakia
  #[4] French Guiana            Gibraltar                Guadeloupe
  #[7] Martinique               Mayotte                  Netherlands Antilles
  #[10] Other nei                Réunion                  Serbia and Montenegro
  #[13] Sudan (former)           Svalbard and Jan Mayen   Tokelau
  #[16] Tuvalu                   Un. Sov. Soc. Rep.       Yugoslavia SFR
  #[19] Zanzibar

  #combined <- sort(union(levels(worldmap[[merge_col]]), levels(year_geo_taxa_fish[[geography]])))
  #levels(worldmap[[merge_col]])<-combined
  #levels(year_geo_taxa_fish[[geography]])<-combined
  firstname <- geography
  join_cols <- merge_col
  names(join_cols) <- firstname

  # Plot map
  allplots<-
    theme_void()+
    theme(axis.text.x = element_blank(),
          legend.title = element_text(size=30),
          legend.text = element_text(size=25),
          title = element_text(size=40))

  if (combine_sources == TRUE){
    mapdat <- year_geo_taxa_fish %>%
      full_join(worldmap, join_cols) %>%
      arrange(desc(fish_sum))

    # Plot single map of combined total production from all sources
    p1<-ggplot()+
      geom_sf(data=mapdat, aes(fill=fish_sum, geometry=geometry))+ # When working with tibbles, Need to specify geometry column manually
      labs(title="Total Production - All Sources Combined")+
      allplots

    png(filename = "plot_TotalProduction-AllSources.png", width=1800, height=1200)
    print(p1)
    dev.off()
  } else if (combine_sources == FALSE){
    # Plot separate maps for each production source
    for (i in 1:nlevels(year_geo_taxa_fish$source_name_en)){
      sourcedat<-year_geo_taxa_fish %>%
        filter(source_name_en == levels(year_geo_taxa_fish$source_name_en)[i]) %>%
        full_join(worldmap, join_cols) %>%
        arrange(desc(fish_sum))

      p1<-ggplot()+
        geom_sf(data=sourcedat, aes(fill=fish_sum, geometry=geometry))+ # When working with tibbles, Need to specify geometry column manually
        labs(title = levels(year_geo_taxa_fish$source_name_en)[i])+
        allplots

      nextfile <- paste("plot_", levels(year_geo_taxa_fish$source_name_en)[i], ".png", sep="")
      png(filename=nextfile, width=1800, height=1200)
      print(p1)
      dev.off()
    }
  }

}
