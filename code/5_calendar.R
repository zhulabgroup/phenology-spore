labelfunc_x <- function(x) {
  origin <- as.Date("2021-01-01")
  format(origin + x, format = "%b")
}

p_calen<-nab_with_taxa_df %>%
  filter(location %in% site_list) %>%
  filter(family == "Total") %>%
  mutate(doy = format(date, "%j") %>% as.integer()) %>%
  mutate(year = format(date, "%Y") %>% as.integer()) %>%
  # filter(year %in% year_list) %>% 
  group_by(taxa, location, doy) %>%
  summarise(count=mean(count)) %>% 
  ungroup() %>% 
  group_by(taxa, location) %>% 
  mutate(count_st=(count-min(count, na.rm = T))/(max(count, na.rm = T)-min(count, na.rm = T))) %>%
  ungroup() %>% 
  ggplot() +
  geom_tile(aes(x = doy, y = location,  fill = count_st), alpha=1) +
  ylab ("")+
  xlab ("")+
  theme_classic()+
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 2)) +
  scale_x_continuous(labels=labelfunc_x)+
  theme(axis.line.y = element_blank(),
        # axis.text.y = element_blank(),
        axis.ticks.y = element_blank())+
  # theme(strip.text.y= element_text(angle = 0))+
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"))+
  scale_fill_gradient(low = "light yellow", high = "dark green",na.value="white"#,
                      # breaks=(c(0,1, 100,  10000, 1000000)+1) %>% log(10),
                      # labels=c(0,1, 100,  10000, 1000000),
                      # name=expression(Spore~concentration~(grains / m^3))
  )+
  guides(fill=F)