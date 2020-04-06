# Function for visualizing the proportion of each species' production in terms of aquaculture vs wild capture:
# If one year provided, produces:
# 1 - bubbleplot for all species and countries with point size scaled to proportion produced by wild capture
# 2 - plot for visualizing how the cutoff value for ignoring alternative production source affects the quantity of mislabelled production sources
# FIX IT - calculate proportion aquaculture instead of proportion capture (FAO SOFIA report frames it in terms of aquaculture production, increasing globally through time)

# If range of years is provided, produces plots for visualizing species changes through time in terms of the proportion produced by wild capture
# FIx IT - all titles show sd cutoff, but should actually be sd RANGE (0 to 0.1, 0.1 to 0.2, 0.2 to 0.3 etc)

tidy_fish=tmp_fish
year_start=2012
year_end=NA
#year_start=2000
#year_end=2005
fish_var="quantity"
fish_level="species_scientific_name"
fish_name="all"
fish_unit="t"
geo_level="country"
geo_name="all"
output_path="~/"
combine_aquaculture=TRUE
incl_CHN=TRUE
percent_capture=TRUE


ts_production_by_source <- function(tidy_fish,
                                    year_start,
                                    year_end=NA,
                                    fish_var="quantity",
                                    fish_level="total",
                                    fish_name=NA,
                                    fish_unit="t",
                                    country,
                                    output_path=NA,
                                    combine_aquaculture=TRUE,
                                    incl_CHN=TRUE) {

  require(dplyr)
  require(ggplot2)
  require(stringr)



  if (is.na(output_path)) {
    outdir<-getwd()
    setwd(outdir)
  } else {
    setwd(output_path)
  }


  # UNIVERSAL FILTERING and CLEANING: MOVE THIS INTO ANOTHER FUNCTION?
  # Deal with countries that have no iso3 alpha codes:
  # For Sudan, country column (aka iso numeric code) changed from 736 to 729 after year 2011
  # Fill in iso3 alpha = SUD for iso3 numeric 736
  # FIX IT - this should be saved as a function in a separate .R file
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


  # Filter years
  if (is.na(year_end)){
    all_years <- year_start
  } else {
    all_years <- seq(from = year_start, to = year_end, by = 1)
  }
  dat_fish <- dat_fish %>%
    filter(year %in% all_years)

  # If desired, combine aquaculture sources
  if (combine_aquaculture==TRUE) {
    dat_fish <- dat_fish %>%
      mutate(source_name_en = as.character(source_name_en)) %>%
      mutate(source_name_en = replace(source_name_en, str_detect(source_name_en, "Aquaculture"), "Aquaculture production")) %>%
      mutate(source_name_en = as.factor(source_name_en)) %>%
      droplevels()
  }

  # Match column name to geo_level parameter
  # In tmp_fish there are more levels in iso3 than in "country"; for now, use iso3 as the reporting level
  # other notes: un_a3 in worldmap == "country" in tmp_fish == code in FishStatJ (3 digit UNM49 country ID number)
  if (geo_level == "country" | geo_name == "all"){
    geography <- "country_iso3_code"
  } else if (geo_level == "continent"){
    geography<-"continent_group"
  } else if (geo_level == "region"){
    geography<-"georegion_group"
  }

  # AGGREGATE:
  if(fish_name=="all" & geo_name=="all"){
    year_geo_taxa_fish <- dat_fish %>%
      group_by(year, get(geography), get(fish_level), source_name_en) %>%
      summarize(fish_sum = sum(get(fish_var))) %>%
      rename(!!geography := 'get(geography)') %>% # using !! and := tells dplyr to rename based on the expression of geography
      rename(!!fish_level := 'get(fish_level)') %>%
      droplevels()
  }

  # NOTE: right_join(by = get(geography)) does not work, need to supply a named column
  first_geo <- geography
  join_geo <- geography
  names(join_geo) <- first_geo

  first_fish <- fish_level
  join_fish <- fish_level
  names(join_fish) <- fish_level

  # if desired, calculate proportions
  if(percent_capture==TRUE){
    # year_geo_taxa_totals sums production (aquaculture + capture) within year, country, taxa
    year_geo_taxa_totals <- year_geo_taxa_fish %>%
      group_by(year, get(geography), get(fish_level)) %>%
      #group_by(year, get(geography)) %>%
      #group_by(year) %>%
      summarize(fish_all_sources = sum(fish_sum)) %>%
      rename(!!geography := 'get(geography)') %>% # using !! and := tells dplyr to rename based on the expression of geography
      rename(!!fish_level := 'get(fish_level)')

    # year_geo_taxa_capture only keeps capture production, drops all aquaculture production
    year_geo_taxa_capture<-year_geo_taxa_fish %>%
      filter(source_name_en=="Capture production")

    # year_geo_taxa_prop: match fish_sum column of just capture data (i.e., total amt of capture) back to totals data
    # rows with no matching capture data are filled in as NAs and replaced with zeroes - i.e., years where only aquaculture was produced
    year_geo_taxa_prop <- year_geo_taxa_totals %>%
      left_join(year_geo_taxa_capture, by=c("year", join_geo, join_fish)) %>%
      rename(fish_capture = fish_sum) %>%
      mutate(fish_capture = replace(fish_capture, is.na(fish_capture), 0)) %>%
      mutate(fish_plot = fish_capture / fish_all_sources) %>%
      arrange(desc(fish_plot, get(geography), get(fish_level))) %>%
      select(-source_name_en)

    plot_dat <- year_geo_taxa_prop
  }


  # if only one year provided, plot scatterplot - snapshot
  ## FIX IT - check all axis labels
  ## FIX IT - consider making "mislabelled" plot based on relative quantities (% of total production that year)
  if (is.na(year_end)) {
    # Set up plotting theme
    plot_theme <- theme(panel.border = element_blank(),
                        panel.background = element_blank(),
                        legend.position = "bottom",
                        legend.title = element_text(size = 16),
                        legend.text = element_text(size = 14),
                        legend.key = element_rect(fill=NA),
                        axis.line.y = element_line(),
                        axis.line.x = element_blank(),
                        axis.title.y = element_text(size = 16),
                        axis.text.y = element_text(size = 14),
                        axis.title.x = element_text(size = 16),)

    p <- ggplot(data = plot_dat, aes(y = fish_plot, x = get(geography))) +
      geom_point(alpha = 0.2, color = "purple", aes(size = fish_all_sources)) +
      #geom_jitter(alpha = 0.2, color = "purple", width = 10, aes(size = fish_all_sources)) +
      #geom_violin() +
      scale_y_continuous(expand = expand_scale(mult = c(0.05, 0.05))) +
      geom_hline(yintercept = 0) +
      labs(y = "proportion wild capture", x = geo_level) +
      scale_size_continuous(breaks = signif(seq(0, max(plot_dat$fish_all_sources), length.out = 6), digits = 2),
                            guide = guide_legend(title = "total production (wild + aquaculture)", nrow=2),
                            labels = function(x) format(x, scientific = TRUE),
                            range = c(1,10)) +
      plot_theme +
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    p

    # Try to determine a cutoff value for when to assign something as fully capture vs aquaculture
    #source_seq <- c(seq(0.01, 0.1, by = 0.01),0.15, 0.2, 0.25, 0.5, 0.75)
    source_seq <- seq(0.01, 0.5, by = 0.01)
    swept_source_dat <- NA
    for (i in 1:length(source_seq)){
      source_cutoff = source_seq[i]

      swept_aqua <- plot_dat %>%
        # by definition, 0s and 1s will be fully assigned to aqua vs capture
        filter(fish_plot < 1 & fish_plot > 0) %>%
        # by definition, anything greater than or equal to 1-cutoff will be assigned fully to capture
        filter(fish_plot >= (1-source_cutoff)) %>%
        mutate(missing_aqua = fish_all_sources - fish_capture)

      swept_capture <- plot_dat %>%
        # by definition, 0s and 1s will be fully assigned to aqua vs capture
        filter(fish_plot < 1 & fish_plot > 0) %>%
        # by definition, anything less than or equal to 0+source_cutoff will be assigned fully to aquaculture
        filter(fish_plot <= (0+source_cutoff)) %>%
        mutate(missing_capture = fish_capture)

      missing_source_i <- sum(swept_aqua$missing_aqua) + sum(swept_capture$missing_capture)

      next_i <- c(source_cutoff, missing_source_i, i)

      swept_source_dat <- as.data.frame(rbind(swept_source_dat, next_i))
    }

    names(swept_source_dat) <- c("source_cutoff", "missing_source_quantity", "i")
    swept_source_dat <- swept_source_dat[-1,]
    rownames(swept_source_dat) <- seq(1, dim(swept_source_dat)[1], 1)



    swept_plot <- ggplot(data = swept_source_dat, aes(y = missing_source_quantity, x = source_cutoff)) +
      geom_line() +
      labs(x = "source cutoff", y = "quantity (t) of mislabelled source")

    swept_plot


  }


  # if time series provided, plot lines
  if (!is.na(year_end)) {
    # for time series, filter out species where
    sd_seq = c(seq(0, 0.4, 0.1))

    for(i in 2:length(sd_seq)){
      sd_cutoff = sd_seq[i]
      sd_start = sd_seq[i-1]
      sd_plot <- plot_dat %>%
        group_by(get(geography), get(fish_level)) %>%
        summarise(var_prop = var(fish_plot, na.rm = TRUE),
                  sd_prop = sd(fish_plot, na.rm = TRUE),
                  mean_prop = mean(fish_plot, na.rm = TRUE),
                  all_year_total = sum(fish_all_sources)) %>%
        #filter(mean_prop!=0 & mean_prop!=1) %>%
        arrange(desc(sd_prop)) %>%
        filter(sd_prop >= sd_start & sd_prop <= sd_cutoff) %>%
        rename(!!geography := 'get(geography)') %>% # using !! and := tells dplyr to rename based on the expression of geography
        rename(!!fish_level := 'get(fish_level)')

      # Does variation in production source biased towards capture vs aquaculture?
      # i.e., is the following plot pretty symmetrical
      sd1 <- ggplot(sd_plot, aes(x = mean_prop, y = sd_prop))+
        #geom_point() +
        geom_jitter(height = 0.025, width = 0.015) +
        xlim(-0.1, 1.1) +
        ylim(-0.1, 1.1) +
        #scale_x_continuous(expand = expand_scale(mult = c(0, 0.1))) +
        #scale_y_continuous(expand = expand_scale(mult = c(0, 0.1))) +
        labs(title = paste("sd cutoff =", sd_cutoff, sep=" "),
             x = "mean across years (proportion from capture fishery)",
             y = "st dev across years (proportion from capture fishery)")

      sd1_file <- paste("plot_sd_cutoff_", sd_cutoff, "_sd_vs_mean_proportion_captured.png", sep="")

      sd1 + ggsave(filename = sd1_file, width = 5.5, height = 4.25, units = "in")

      size_scale <- max(plot_dat$fish_all_sources)
      # Does variation in production source biased by total production?
      sd2 <- ggplot(sd_plot, aes(x = all_year_total, y = sd_prop))+
        geom_point() +
        #geom_jitter(height = 0.05, width = 0.05) +
        #ylim(-0.1, 1.1) +
        #xlim(-0.1, size_scale*0.1)+
        labs(title = paste("sd cutoff =", sd_cutoff, sep=" "),
             x = "production (t)",
             y = "st dev across years (proportion from capture fishery)")

      sd2_file <- paste("plot_sd_cutoff_", sd_cutoff, "_sd_vs_total_production.png", sep="")

      sd2 + ggsave(filename = sd2_file, width = 5.5, height = 4.25, units = "in")


      # merge sd_plot (filtered by cutoff SD) back with plot_dat, i.e., only keep data above SD cutoff
      plot_years <- sd_plot %>%
        left_join(plot_dat, by = c(join_geo, join_fish))


      ts <- ggplot(data = plot_years, aes(y = fish_plot,
                                          x = year,
                                          group = interaction(get(geography), get(fish_level)))) +
        geom_line(size = 0.1) +
        geom_point(aes(size = fish_all_sources)) +
        scale_size_continuous("points",
                              guide = guide_legend(title = "total production (wild + aquaculture)"),
                              labels = function(x) format(x, scientific = TRUE)) +
        labs(title = paste("sd cutoff =", sd_cutoff, sep=" "),
             x = "production (t)",
             y = "st dev across years (proportion from capture fishery)") +
        theme(legend.position = "bottom")


      ts_file <- paste("plot_sd_cutoff_", sd_cutoff, "_proportion_from_capture_time_series.png", sep = "")

      ts + ggsave(filename = ts_file, width = 5.5, height = 4.25, units = "in")
    }

  }


}
