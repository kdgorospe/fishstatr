# Plot all geo_levels (country or continent) for range of years
# Or plot time series for given geo_name(s) within geo_level

# Testing fuction:
tidy_fish<-tmp_fish
year_start=2005
year_end=NA
fish_level="total"; # can also be isscaap_group, family, or species_scientific_name, but must match column name
fish_name=NA # if fish_level specified, must be a name found within fish_level
plot_as_time_series=FALSE
geo_level="country"
geo_name=NA
fish_unit="t" # can also be "no"; used for whales, seals, walruses, crocodiles/alligators
output_path=("~/")
fish_var="quantity"
# NOTE: remove option to combine_sources=TRUE/FALSE since this function will produce stacked barplots
# NOTE: fish_var should always be quantity, i.e., name of fish column to be plotted, leave in as a parameter for now? in case it's possible to use this function for other data sources


graph_production<-function(tidy_fish,
                           year_start,
                           year_end=NA,
                           fish_var="quantity",
                           fish_level="total",
                           fish_name=NA,
                           fish_unit="t",
                           geo_level="country",
                           combine_sources=FALSE,
                           output_path="~/"){
  require(dplyr)
  require(ggplot2)

  if (is.na(output_path)) {
    outdir<-getwd()
    setwd(outdir)
  } else {
    setwd(output_path)
  }

  # FAO data country column (N = 247)
  # FAO data country_iso3_code column (N = 260)
  # ISO_a3 is finer-scale than country: example, Taiwan separate from China; Guam, PR separate from USA
  # Use ISO_a3 for summarizing data


  # Deal with countries that have no iso3 alpha codes:
  # For Sudan, country column (aka iso numeric code) changed from 736 to 729 after year 2011
  # Fill in iso3 alpha = SUD for iso3 numeric 736
  dat_fish <- tidy_fish %>%
    mutate(country_iso3_code = replace(country_iso3_code, country==736, "SDN")) %>%
    # For Channel Islands, drop for now: some of the channel islands (Jersey, Isle of Man, Guernsey) have their own alpha codes but they're all lumped together in FAO data as iso_n3=830
    filter(country!=830) %>%
    # Drop iso_n3=896 (used for "Areas not elsewhere specified", aka country ID not known?)
    filter(country!=896) %>%
    # Filter for desired units
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
  } else if (geo_level == "continent"){
    geography<-"continent_group"
  } else if (geo_level == "region"){
    geography<-"georegion_group"
  }

  # NOTE: in some cases, countries explicitly report "zero" for certain groups of fish and/or sources, while others are NA (likely also zero?)
  # i.e., FIXIT: Ask JG/FAO, is there a difference between countries that explicitly report zeroes vs. NAs?
  # For now, remove all entries where quantity=0 (otherwise, countries are colored in as 0 as opposed to being NA)
  year_fish <- year_fish %>%
    filter(get(fish_var) != 0)


  if (fish_level == "total"){
    year_geo_taxa_fish <- year_fish %>%
      group_by(year, get(geography), source_name_en) %>%
      summarize(fish_sum = sum(get(fish_var))) %>%
      rename(!!geography := 'get(geography)') %>% # using !! and := tells dplyr to rename based on the expression of geography
      droplevels()
  } else if (fish_level != "total"){ # If grouped fish is desired: # FIXIT - still need to test this portion
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

  # Plot set up
  allplots <- theme_void()+
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 30),
          legend.text = element_text(size = 25))
  plot_width <- 10
  plot_height <- 6

  # arrange for data checking
  year_geo_taxa_fish <- year_geo_taxa_fish %>%
    arrange(desc(fish_sum))
  # FIXIT - eventually remove LOG or consider splitting into multiple graphs with different scales

  # arrange factor levels (and order of plotting) by the SUM of fish_sum by country (i.e., total production combined across all sources)
  # reorder(get(geography), fish_sum, sum)
  p <- ggplot(year_geo_taxa_fish) +
    geom_bar(aes(x = reorder(get(geography), log(fish_sum), sum), y = log(fish_sum), fill = source_name_en), position="stack", stat = "identity") +
    scale_fill_viridis_d(name = "Production Source") +
    allplots
    #coord_polar()

  if (fish_level=="total"){
    nextfile = paste("plot_bar_Production_per_", geo_level, ".png", sep="")
  }

  p + ggsave(filename = nextfile, width = plot_width, height = plot_height, units = "in")


}
