tidy_fish=tmp_fish

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
}
