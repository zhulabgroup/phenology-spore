df_total_ts<- df %>% 
  select( location, date, family,count) %>% 
  group_by(location, family) %>% 
  tidyr::complete(date = seq(min(date),max(date), by="1 day")) %>% 
  arrange(date, location)%>% 
    mutate(count=case_when(count>=5~count)) # set low values to 0

p_ts <- ggplot(data=df_total_ts %>% 
                 filter(location %in% site_list)%>% 
                 filter(family=="Total") ) +
  geom_line(aes(x=date, y=count))  +
  scale_y_continuous(trans = scales::log_trans(), 
                     breaks = scales::trans_breaks("log", function(x) exp(x)),
                     labels = scales::trans_format("log", scales::math_format(e^.x)))+
  ggtitle("Total Spore Counts (all counts are per cubic meter of air)")+
  ylab("log (count)")+
  theme_classic() +
  facet_wrap(.~location, ncol=2, scales = "free_y")
