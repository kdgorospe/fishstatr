#geo_level can be country, region, continent
#fish_var can be quantity or value
#fish_level can be total, family, order, isscaap, or species

# Testing fuction:
tidy_fish<-tmp_fish
year_start=1995
year_end=2000
fish_var="quantity" # can also be number
fish_level="total"; # can also be isscaap_group, family, or species_scientific_name, but must match column name
fish_name=NA # must be found within fish_level
geo_level="country"
fish_unit="t" # can also be "no"; used for whales, seals, walruses, crocodiles/alligators
combine_sources=FALSE
output_path=("C:\\Users\\kdgor\\Documents")

## Other options:
combine_sources=TRUE
fish_level="species_scientific_name"
fish_name="Scarus forsteni"

map_production<-function(tidy_fish,
                         year_start,
                         year_end=NA,
                         fish_var="quantity",
                         fish_level="total",
                         fish_name=NA,
                         fish_unit="t",
                         geo_level="country",
                         combine_sources=FALSE,
                         output_path=NA){
  require(dplyr)
  require(rnaturalearth)
  require(rnaturalearthdata)
  require(rgeos)
  require(ggplot2)
  require(sf)

  if (is.na(output_path)) {
    outdir<-getwd()
    setwd(outdir)
  } else {
    setwd(output_path)
  }

  worldmap <- ne_countries(scale = "medium", returnclass = "sf")
  # note use FAO's iso_a3 (reporting countries and territories)
  # This is finer-scale than country: example, Taiwan separate from China; Guam, PR separate from USA
  # i.e., work with finer scale now, can decide to aggregate later
  # need to provide instructions for people using FishStatJ to include this as a data field before downloading the dataset
  # worldmap has 235 iso_n3 and 235 iso_a3 codes
  # FAO has 247 country (aka iso_n3) and 260 iso3 alpha codes - finest scale appears to be FAO's iso3 alpha codes

  # Filter correct units
  dat_fish<-tidy_fish %>%
    filter(unit == fish_unit)

  # Filter years
  if (is.na(year_end)){
    all_years <- year_start
  } else {
    all_years <- seq(from = year_start, to = year_end, by = 1)
  }
  year_fish <- dat_fish %>%
    filter(year %in% all_years)

  # Match column name to geo_level parameter
  # In tmp_fish there are more levels in iso3 than in "country"; for now, use iso3 as the reporting level
  # other notes: un_a3 in worldmap == "country" in tmp_fish == code in FishStatJ (3 digit UNM49 country ID number)
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

  # NOTE: in some cases, countries explicitly report "zero" for certain groups of fish and/or sources, while others are NA (likely also zero?)
  # i.e., FIXIT: Ask JG/FAO, is there a difference between countries that explicitly report zeroes vs. NAs?
  # For now, remove all entries where quantity=0 (otherwise, countries are colored in as 0 as opposed to being NA)
  year_fish <- year_fish %>%
    filter(get(fish_var) != 0)

  # Now aggregate:
  # If production sources should be combined:
  # If, fish_level is not "total", then code will filter for specified fish_name in specified fish_level
  if (combine_sources == TRUE & fish_level == "total"){
    year_geo_taxa_fish <- year_fish %>%
      group_by(get(geography), year) %>%
      summarize(fish_sum = sum(get(fish_var))) %>%
      rename(!!geography := 'get(geography)') %>%
      droplevels()
  } else if (combine_sources == TRUE & fish_level != "total"){
    if (fish_name %in% year_fish[[fish_level]]){ # Can the fish be found in the dataset?
      year_geo_taxa_fish <- year_fish %>%
        filter(get(fish_level) == fish_name) %>%
        group_by(get(geography), year) %>%
        summarize(fish_sum = sum(get(fish_var))) %>%
        rename(!!geography := 'get(geography)') %>%
        droplevels()
    } else {
      nofish<-paste("No", tolower(fish_name), "production in specified year(s)")
      stop(nofish)
      }
  }



  # If production sources (capture vs various types of aquaculture) should be kept separate
  # If, fish_level is not "total", then code will filter for specified fish_name in specified fish_level
    if (combine_sources == FALSE & fish_level == "total"){
    year_geo_taxa_fish <- year_fish %>%
      group_by(year, get(geography), source_name_en) %>%
      summarize(fish_sum = sum(get(fish_var))) %>%
      rename(!!geography := 'get(geography)') %>% # using !! and := tells dplyr to rename based on the expression of geography
      droplevels()
  } else if (combine_sources == FALSE & fish_level != "total"){ # If grouped fish is desired:
    if (fish_name %in% year_fish[[fish_level]])
    year_geo_taxa_fish <- year_fish %>%
      filter(get(fish_level) == fish_name) %>%
      group_by(year, get(geography), source_name_en) %>%
      summarize(fish_sum = sum(get(fish_var))) %>%
      rename(!!geography := 'get(geography)') %>%
      droplevels()
    else {
      nofish<-paste("No", tolower(fish_name), "production in specified year(s)")
      stop(nofish)
    }
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

  # Map setup:
  allplots<-
    theme_void()+
    theme(axis.text.x = element_blank(),
          legend.title = element_text(size = 30),
          legend.text = element_text(size = 25),
          title = element_text(size = 40))
  png_width <- 1800
  png_height <- 1200

  # Plot map
  # FIXIT need to build in warnings / errors for when specified fish_name is produced in some but not all years
  # If sources should be combined...
  if (combine_sources == TRUE){
    # First find maximum value for fish_sum and use this to standardize legend of all maps in time series
    max_scale<-max(year_geo_taxa_fish[["fish_sum"]], na.rm = TRUE)
    # max_scale<-signif(max_fish, digits = 1) # This creates NA's on the map if max_scale ends up being less than max value
    # Loop through each year, and plot single map of combined total production from all sources
    for (y in 1:length(all_years)){
      mapdat <- year_geo_taxa_fish %>%
        filter(year==all_years[y]) %>%
        full_join(worldmap, join_cols) %>%
        arrange(desc(fish_sum))
      if (is.na(fish_name)){
        total_title <- paste("Global total seafood production in", all_years[y], sep = " ")
      } else {
        total_title <- paste("Global total seafood production of", tolower(fish_name), "in", all_years[y], sep=" ")
      }
      p1<-ggplot()+
        geom_sf(data=mapdat, aes(fill=fish_sum, geometry=geometry))+ # When working with tibbles, Need to specify geometry column manually
        labs(title=total_title)+
        scale_fill_continuous(limits=c(0, max_scale), name = fish_unit)+
        allplots
      if (is.na(fish_name)){
        totalfile <- paste("plot_TotalProduction-AllSources-", all_years[y], ".png", sep = "")
      } else {
        totalfile <- paste("plot_TotalProduction-AllSources-", fish_name, "-", all_years[y], ".png", sep="")
      }
      png(filename = totalfile, width=png_width, height=png_height)
      print(p1)
      dev.off()
    }

    # BUT if production sources should be kept separate
  } else if (combine_sources == FALSE){
    # Loop and plot separate maps for each production source and each year
    for (i in 1:nlevels(year_geo_taxa_fish$source_name_en)){
      # First get the production source
      sourcedat<-year_geo_taxa_fish %>%
        filter(source_name_en == levels(year_geo_taxa_fish$source_name_en)[i])
      # First find maximum value for fish_sum (within a production source) and use this to standardize legend of all maps in time series
      max_scale<-max(sourcedat[["fish_sum"]], na.rm = TRUE)
      # Then loop through sourcedat and make plot for each year:
      for (y in 1:length(all_years)){
        mapdat <- sourcedat %>%
          filter(year==all_years[y]) %>%
          full_join(worldmap, join_cols) %>%
          arrange(desc(fish_sum))
        if (is.na(fish_name)){
          next_title <- paste("Global", tolower(levels(year_geo_taxa_fish$source_name_en)[i]), "in", all_years[y], sep=" ")
        } else {
          next_title <- paste("Global", tolower(levels(year_geo_taxa_fish$source_name_en)[i]), "of", tolower(fish_name), "in", all_years[y], sep=" ")
        }
        p1<-ggplot()+
          geom_sf(data=mapdat, aes(fill=fish_sum, geometry=geometry))+ # When working with tibbles, Need to specify geometry column manually
          labs(title = next_title)+
          scale_fill_continuous(limits=c(0, max_scale), name = fish_unit)+
          allplots
        if (is.na(fish_name)){
          nextfile <- paste("plot_", levels(year_geo_taxa_fish$source_name_en)[i], "-", all_years[y], ".png", sep="")
        } else {
          nextfile <- paste("plot_", levels(year_geo_taxa_fish$source_name_en)[i], "-", fish_name, "-", all_years[y], ".png", sep="")
        }
        png(filename=nextfile, width=png_width, height=png_height)
        print(p1)
        dev.off()
      }
    }
  }
}
