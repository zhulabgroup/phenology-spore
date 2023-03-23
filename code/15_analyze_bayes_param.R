p_param_year<-ggplot(df_params)+
  geom_line(aes(x=year, y=value, col=level_site, group=level_site))+
  geom_point(aes(x=year, y=value, col=level_site, group=level_site))+
  theme_classic()+
  guides(col=F)+
  facet_wrap(.~param, scales = "free_y")

df_params_t<-df_params %>% 
  mutate(t_value=case_when(param=="mn"~(value-mn_min)/(mn_max-mn_min),
                           param=="mx"~(value-mx_min)/(mx_max-mx_min),
                           param=="sos"~(value-sos_min)/(sos_max-sos_min),
                           param=="eos"~(value-eos_min)/(eos_max-eos_min),
                           param=="rsp"~(value-rsp_min)/(rsp_max-rsp_min),
                           param=="rau"~(value-rau_min)/(rau_max-rau_min),
                           param=="rsu"~(value-rsu_min)/(rsu_max-rsu_min),
  )) %>% 
  mutate(param=factor(param, levels=c("mn", "mx", "sos", "eos", "rsp", "rau", "rsu")))

param_list<-unique(df_params$param)
lm_list<-vector(mode="list")
for (paramoi in param_list) {
  lmoi<-lm(logit(t_value)~mat+tap, data=df_params_t %>% filter(param==paramoi)) %>% summary()
  lm_list[[paramoi]]<-data.frame(param=paramoi,
                                 mat_p=lmoi$coefficients["mat",4],
                                 tap_p=lmoi$coefficients["tap",4]
                                 )
}
glm_df<-bind_rows(lm_list) %>% 
  mutate(param=factor(param, levels=c("mn", "mx", "sos", "eos", "rsp", "rau", "rsu")))

df_params_sig<-df_params_t %>%
  left_join(glm_df, by="param")

p_mat<-ggplot( )+
  geom_point( data=df_params_t, aes(x=mat, y=t_value, col=level_site, group=level_site))+
  geom_smooth( data=df_params_sig %>% filter(mat_p<0.05),
  aes(x=mat, y=t_value),method="glm", method.args = list(family = "quasibinomial"))+
  xlab("Mean annual temperature (°C)")+
  ylab("Scaled parameter value")+
  theme_classic()+
  facet_wrap(.~param, scales = "free_y", ncol=1)+
  guides(col="none")

p_tap<-ggplot(df_params)+
  geom_point(data=df_params_t,aes(x=tap, y=t_value, col=level_site, group=level_site))+
  geom_smooth(data=df_params_sig %>% filter(tap_p<0.05),aes(x=tap, y=t_value),method="glm", method.args = list(family = "quasibinomial"))+
  xlab("Total annual precipitation (mm)")+
  ylab("Scaled parameter value")+
  theme_classic()+
  facet_wrap(.~param, scales = "free_y", ncol=1)+
  guides(col="none")

# p_mvp<-ggplot(df_params)+
#   geom_point(aes(x=mvp, y=t_value, col=level_site, group=level_site))+
#   geom_smooth(aes(x=mvp, y=t_value),method="glm", method.args = list(family = "quasibinomial"))+
#   xlab("Mean vapor pressure (??)")+
#   ylab("Scaled parameter value")+
#   theme_classic()+
#   facet_wrap(.~param, scales = "free_y", ncol=1)+
#   guides(col=F)

# cairo_pdf("./nab/output/figures/6 pheno_climate.pdf", width=6, height=9)
# grid.arrange(annotate_figure(p_mat, fig.lab = "A"),
#              annotate_figure(p_tap, fig.lab = "B"),
#              nrow=1
# )
# dev.off()

glm_outputlist<-list (
  summary(lm(logit(t_value)~mat+tap, data=df_params_t %>% filter(param=="mn"))),
  summary(lm(logit(t_value)~mat+tap, data=df_params_t %>% filter(param=="mx"))),
  summary(lm(logit(t_value)~mat+tap, data=df_params_t %>% filter(param=="sos"))),
  summary(lm(logit(t_value)~mat+tap, data=df_params_t %>% filter(param=="eos"))),
  summary(lm(logit(t_value)~mat+tap, data=df_params_t %>% filter(param=="rsp"))),
  summary(lm(logit(t_value)~mat+tap, data=df_params_t %>% filter(param=="rau"))),
  summary(lm(logit(t_value)~mat+tap, data=df_params_t %>% filter(param=="rsu")))
)
