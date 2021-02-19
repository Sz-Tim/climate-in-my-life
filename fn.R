# Climate In My Life
# Shiny App
# Helper functions
# Tim Szewczyk


load_climate_data <- function(plotStart="1850-01-01", 
                              url_base="http://berkeleyearth.lbl.gov/auto/Global/") {
  library(tidyverse)
  
  mo <- map(c("Land_and_Ocean_complete.txt", "Complete_TAVG_complete.txt"),
            ~read_table(paste0(url_base, .x), 
                        comment="%",
                        col_names=c("Year", "Month", 
                                    "AnomMo", "UncertMo",
                                    "Anom1y", "Uncert1y", 
                                    "Anom5y", "Uncert5y", 
                                    "Anom10y", "Uncert10y",
                                    "Anom20y", "Uncert20y")) %>%
              filter(!is.na(Year)) %>%
              mutate(Date=lubridate::ymd(paste(Year, Month, sep="-"), 
                                         truncated=1)) %>%
              filter(Date >= plotStart)) %>%
    setNames(c("globe", "land"))
  yr <- map(mo, ~.x %>% group_by(Year) %>%
              summarise(UncertMo=sd(AnomMo)/sqrt(10),
                        AnomMo=mean(AnomMo)) %>% 
              mutate(YrStart=lubridate::ymd(paste(Year, "01", "01", sep="-")),
                     YrEnd=lubridate::ymd(paste(Year, "12", "31", sep="-"))))
  dec <- map(mo, ~.x %>% 
               mutate(Decade=Year %/% 10 * 10) %>% group_by(Decade) %>%
               summarise(UncertMo=sd(AnomMo)/sqrt(120),
                         AnomMo=mean(AnomMo)) %>% 
               mutate(decadeStart=lubridate::ymd(paste(Decade, "01", "01", sep="-")),
                      decadeEnd=lubridate::ymd(paste(Decade+9, "12", "31", sep="-"))))
  
  return(list(mo=mo, yr=yr, dec=dec))
}