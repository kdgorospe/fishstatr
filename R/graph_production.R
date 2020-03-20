# Plot all geo_levels (country or continent) for range of years
# Or plot time series for given geo_name(s) within geo_level

# Testing fuction:
tidy_fish<-tmp_fish
year_start=2005
year_end=NA # FIXIT - still need to add and time series component
fish_level="total"; # can also be isscaap_group, family, or species_scientific_name, but must match column name
fish_name=NA # if fish_level specified, must be a name found within fish_level
# FIX IT - still need to test when fish_name is specified; also need to add flexibility for when dataset does not include all countries
# FIX IT - clean up plotting code whereby it makes ONE plot, but if this is too big it makes one big plot followed by a number of subplots (how to make this flexible?)
plot_as_time_series=FALSE
geo_level="country"
geo_name=NA
fish_unit="t" # can also be "no"; used for whales, seals, walruses, crocodiles/alligators
output_path=("~/")
fish_var="quantity"
combine_aquaculture=FALSE
incl_CHN=TRUE
# NOTE: removed option combine_sources=TRUE/FALSE since this function will produce stacked barplots
# NOTE: fish_var should always be quantity, i.e., name of fish column to be plotted, leave in as a parameter for now? in case it's possible to use this function for other data sources


#ls()[!(ls() %in% c("tmp_fish", "rebuild_fish"))]
#will give the elements excluding 'keepThis' and 'andThis'.  Thus

#rm(list = ls()[!(ls() %in% c('keepThis','andThis'))])

graph_production<-function(tidy_fish,
                           year_start,
                           year_end=NA,
                           fish_var="quantity",
                           fish_level="total",
                           fish_name=NA,
                           fish_unit="t",
                           geo_level="country",
                           output_path="~/"){
  require(dplyr)
  require(ggplot2)
  require(stringr)

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

  # FILTERING and CLEANING:
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
    filter(unit == fish_unit) %>%
    # NOTE: in some cases, countries explicitly report "zero" for certain groups of fish and/or sources, while others are NA (likely also zero?)
    # i.e., FIX IT: Ask JG/FAO, is there a difference between countries that explicitly report zeroes vs. NAs?
    # For now, remove all entries where quantity=0 (otherwise, countries are colored in as 0 as opposed to being NA)
    filter(get(fish_var) != 0)

  # Combine aquaculture sources
  if (combine_aquaculture==TRUE) {
    dat_fish <- dat_fish %>%
      mutate(source_name_en = as.character(source_name_en)) %>%
      mutate(source_name_en = replace(source_name_en, str_detect(source_name_en, "Aquaculture"), "Aquaculture production")) %>%
      mutate(source_name_en = as.factor(source_name_en))
  }



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






  if (fish_level == "total"){
    year_geo_taxa_fish <- year_fish %>%
      group_by(year, get(geography), source_name_en) %>%
      summarize(fish_sum = sum(get(fish_var))) %>%
      rename(!!geography := 'get(geography)') %>% # using !! and := tells dplyr to rename based on the expression of geography
      droplevels()
  } else if (fish_level != "total"){ # If grouped fish is desired:
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


  # NOTE - ln(fish_sum) produces negative values for countries < 1 ton
  # Adjust all countries < 1 to 1 (just for total plot)
  # OR instead of ln(fish_sum) just remove CHN
  if (incl_CHN==FALSE) {
    year_geo_taxa_fish <- year_geo_taxa_fish %>%
      #mutate(fish_sum = replace(fish_sum, fish_sum < 1, 1)) %>%
      filter(country_iso3_code!="CHN") %>%
      droplevels()
  }

  # arrange for data checking
  year_geo_taxa_fish <- year_geo_taxa_fish %>%
    arrange(desc(fish_sum))

  # Set up plotting theme
  plot_theme <- theme(legend.position = "bottom",
                      legend.title = element_text(size = 16),
                      legend.text = element_text(size = 14),
                      panel.border = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_line(),
                      axis.title.y = element_text(size = 16),
                      axis.text.y = element_text(size = 14),
                      axis.title.x = element_text(size = 16),)
  plot_width <- 12
  plot_height <- 6



  # Determine order of countries by sum of fish_sum and use this to:
  # (1) get vline in plot of all countries
  # (2) create plots of subsets of countries
  country_order<-with(year_geo_taxa_fish, reorder(get(geography), desc(fish_sum), sum))
  country_order<-levels(country_order)
  start_split <- floor(seq(from = 1, to = length(country_order), length(country_order)/3))
  end_split <- c(start_split[-1]-1, length(country_order))

  # First plot all countries together
  # arrange factor levels (and order of plotting) by the SUM of fish_sum by country (i.e., total production combined across all sources)
  # reorder(get(geography), fish_sum, sum)
  p <- ggplot(year_geo_taxa_fish) +
    geom_bar(aes(x = reorder(get(geography), desc(fish_sum), sum), y = fish_sum, fill = source_name_en), position="stack", stat = "identity") +
    scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
                       #labels = function(x) format(x, scientific = TRUE)) +
    scale_fill_viridis_d(name = "Production Source",
                         guide = guide_legend(nrow = 2)) +
    geom_vline(xintercept = end_split[1], linetype = "dotted") +
    geom_vline(xintercept = end_split[2], linetype = "dotted") +
    labs(y = "tonnes", x = "countries") +
    plot_theme +
    theme(axis.text.x=element_blank(),
    axis.ticks.x=element_blank())

  if (fish_level=="total"){
    nextfile = paste("plot_bar_Production_per_", geo_level, ".png", sep="")
  }

  p + ggsave(filename = nextfile, width = plot_width, height = plot_height, units = "in")

  # Plot subsets of countries
  # arrange factor levels (and order of plotting) by the SUM of fish_sum by country (i.e., total production combined across all sources)
  # reorder(get(geography), fish_sum, sum)
  for (i in 1:length(start_split)) {
    # get subsets of country_list
    country_list <- country_order[start_split[i]:end_split[i]]
    sub_plot_dat <- year_geo_taxa_fish %>%
      filter(country_iso3_code %in% country_list)
    p <- ggplot(sub_plot_dat) +
      geom_bar(aes(x = reorder(get(geography), desc(fish_sum), sum), y = fish_sum, fill = source_name_en), position="stack", stat = "identity") +
      scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
      scale_fill_viridis_d(name = "Production Source",
                           guide = guide_legend(nrow = 2)) +
      labs(y = "tonnes", x = "countries") +
      plot_theme +
      theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5))




    if (fish_level=="total"){
      nextfile = paste("plot_bar_Production_per_", geo_level, "_", i, ".png", sep="")
    }

    p + ggsave(filename = nextfile, width = plot_width, height = plot_height, units = "in")

  }


}
