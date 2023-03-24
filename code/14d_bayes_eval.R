df_params<-read_rds(str_c(.path$out_bayes_model, taxa, "/", "params.rds"))
df_pheno_model_all_years_new<-read_rds(str_c(.path$out_bayes_model, taxa, "/", "fit.rds"))

p_fitline<-ggplot(df_pheno_model_all_years_new #%>% 
                  # filter(location %in% site_list)
)+
  geom_point(aes(x=doy+60, y=count, col=year,group=year),alpha=0.1)+
  geom_line(aes(x=doy+60, y=y_pred, col=year,group=year), lwd=1) +
  # ggtitle("Fitted phenology curves of Cladosporiaceae spores")+
  theme_classic()+
  scale_color_viridis_c()+
  facet_wrap(.~year, scale="free_y", ncol=3)+
  # facet_grid(row=vars(id), col=vars(year), scale="free_y")+
  guides(col="none")+
  scale_y_continuous(trans = scales::log_trans(), 
                     breaks = scales::trans_breaks("log", function(x) exp(x)),
                     labels = scales::trans_format("log", scales::math_format(e^.x)))+
  scale_x_continuous(labels=labelfunc_x)+
  # ggtitle("Phenology curves of Cladosporiaceae spores")+
  ylab(expression(Spore~concentration~(grains / m^3)))+
  xlab("Time of year")
p_fitline

# cairo_pdf("./nab/output/figures/4 bayes_fit.pdf", width=6, height=6)
# print (p_bayes)
# dev.off()

p_corr<-ggplot(df_pheno_model_all_years_new %>% 
                 filter(location %in% site_list))+
  geom_point(aes(x=count, y = y_pred, group=year, col=year), alpha=0.1)+
  geom_abline(intercept = 0, slope=1, col="red", lty=2)+
  scale_x_continuous(trans = scales::log_trans(), 
                     breaks = scales::trans_breaks("log", function(x) exp(x)),
                     labels = scales::trans_format("log", scales::math_format(e^.x)))+
  scale_y_continuous(trans = scales::log_trans(), 
                     breaks = scales::trans_breaks("log", function(x) exp(x)),
                     labels = scales::trans_format("log", scales::math_format(e^.x)))+
  xlab(expression(Observed~spore~concentration~(grains / m^3)))+
  ylab(expression(Fitted~spore~concentration~(grains / m^3)))+
  theme_classic()+
  facet_wrap(.~location,scale="free",ncol = 3)+
  scale_color_viridis_c()+
  guides(col="none")

# cairo_pdf("./nab/output/figures/5 bayes_corr.pdf", width=6, height=5)
# print (p_corr)
# dev.off()