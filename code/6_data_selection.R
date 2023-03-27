nab_with_taxa_df <- read_rds("~/spore_phenology/data/nab_with_taxa.rds")

# station_year combination, measurements > 18, years > 5
station_year_df <- nab_with_taxa_df %>%
  filter(family == "Total") %>%
  mutate(year = format(date, "%Y") %>% as.integer()) %>%
  group_by(location, id, year) %>%
  # summarise(nobservation=n()) %>%
  mutate(nobservation = n()) %>%
  filter(nobservation >= 18) %>%
  ungroup() %>%
  group_by(location, id) %>%
  # summarise(nyear=length(unique(year))) %>%
  mutate(nyear = length(unique(year))) %>%
  filter(nyear >= 5) %>%
  # summarise(nyear=length(unique(year))) %>%
  ungroup()

# linear interpolation
data_insert_df <- station_year_df %>%
  dplyr::select(location, id, year, date, count) %>%
  group_by(location, id) %>%
  padr::pad(start_val = as.Date("2007-01-01"), end_val = as.Date("2019-12-31")) %>%
  mutate(year = format(date, "%Y") %>% as.integer(), count_insert = na.approx(count, maxgap = 7)) %>%
  ungroup()
# #test the maxgap
# data_insert_df<-station_year_df %>%
#   dplyr::select(location,id,year,date,count) %>%
#   group_by(location,id) %>%
#   pad(start_val= as.Date("2007-01-01"),end_val = as.Date("2019-12-31"))%>%
#   mutate(year=format(date, "%Y") %>% as.integer(),count_insert = na.approx(count, maxgap=14)) %>%
#   ungroup()

# Whittaker smooth
# Function for smoothing, because ptw::whit1 does not take NA in the time series
whitfun <- function(x, lambda) {
  max_id <- 0
  done <- F
  while (!done) {
    min_id <- min(which(!is.na(x[(max_id + 1):length(x)]))) + (max_id) # first number that is not NA
    if (min_id == Inf) { # all numbers are NA
      done <- T # consider this ts done
    } else {
      max_id <- min(which(is.na(x[min_id:length(x)]))) - 1 + (min_id - 1) # last number in the first consecutive non-NA segment
      if (max_id == Inf) {
        max_id <- length(x) # last non-NA segment is at the end of the whole ts
        done <- T # consider this ts done
      }
      x[min_id:max_id] <- ptw::whit1(x[min_id:max_id], lambda) # whitman smoothing for this non-NA segment
    }
  }
  return(x)
}
data_smooth_df <- data_insert_df %>%
  dplyr::select(location, id, year, date, count_insert) %>%
  group_by(location, id) %>%
  mutate(count_smooth = whitfun(count_insert, lambda = 1800)) %>%
  ungroup() %>%
  group_by(location, id, year) %>%
  mutate(doy = format(date, "%j") %>% as.integer(), month = format(date, "%b")) %>%
  ungroup()
write_rds(data_smooth_df, "~/spore_phenology/data/data_smooth.rds")

# internal consistency
# ggplot(data=data_smooth_df %>% filter(id==18),aes(x=date, y=log(count_smooth+1)))+
#   geom_line()+
#   facet_wrap(.~location*id, ncol=6)
ggplot(data = data_smooth_df, aes(x = date, y = log(count_smooth + 1))) +
  geom_point(size = 0.01) +
  facet_wrap(. ~ location * id, ncol = 6)
# ggplot(data=data_smooth_df %>% filter(id==5),aes(x=date, y=log(count_smooth+1)))+
#   scale_x_date(date_breaks = "1 year")+
#   geom_path()+
#   facet_wrap(.~location*id, ncol=6)


# determine time window
# histogram
ggplot(data = data_smooth_df %>%
  filter(!is.na(count_smooth))) +
  geom_histogram(aes(x = doy))
# #doy
# ggplot(data=data_smooth_df %>%
#          filter(!is.na(count_smooth)),
#        aes(x=interaction(id, year), y=doy,group=interaction(id, year), col=as.factor(id))
#        )+
#   geom_point(alpha=0.2)
# ggplot(data=data_smooth_df %>%
#          group_by(location,id,year) %>%
#          filter(!is.na(count_smooth)),
#        aes(x=doy, y=log(count_smooth+1),group=interaction(id, year))
#        )+
#   geom_path(alpha=0.2)

# trend of measurements (%)
# #whole year
# ggplot(data=data_smooth_df %>%
#          group_by(location,id,year) %>%
#          filter(!is.na(count_smooth)) %>%
#          summarise(observ_percent=n()/365),
#        aes(x=year,y=observ_percent)
# )+
#   geom_point()+
#   geom_smooth(method="lm")+
#   ylim(0,1)+
#   facet_wrap(.~location*id, ncol=6)+
#   stat_cor(method="pearson", label.y = 500)
# time window (Apr-Sep)
ggplot(
  data = data_smooth_df %>%
    group_by(location, id, year) %>%
    filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) %>%
    filter(!is.na(count_smooth)) %>%
    summarise(observ_percent = n() / 214),
  aes(x = year, y = observ_percent)
) +
  geom_point() +
  geom_smooth(method = "lm") +
  ylim(0, 1) +
  facet_wrap(. ~ location * id, ncol = 6) +
  stat_cor(method = "pearson", label.y = 500)


# #trend of measurements
# ggplot(data=station_year_df %>%
#          group_by(location, id, year) %>%
#          summarise(nobservation=n()),
#        aes(x=year, y=nobservation)
#        )+
#   geom_point()+
#   geom_smooth(method="lm")+
#   scale_x_continuous(breaks=seq(2007,2019,1))+
#   facet_wrap(.~location*id, ncol=6)+
#   stat_cor(method="pearson", label.y = 500)
#
# #internal consistency
# p_load(pracma)
# ggplot(data=station_year_df %>%
#          group_by(location,id) %>%
#          mutate(count_whittaker=whittaker(y=count,lambda=1600)),
#        aes(x=date, y=log(count_whittaker+1))
#        )+
#   geom_line()+
#   facet_wrap(.~location*id, ncol=6)
