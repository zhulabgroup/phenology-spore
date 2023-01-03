P_fore<-as.matrix(1:nrow(coord_df))

######
steps<-60

t_complete<-which(!is.na(rollsum((x[1,,1]), max(unlist(lags)), align = "right",fill=NA)))
if (length(which(date_list==middate))>0) {
  t_in<-seq(max(unlist(lags)), which(date_list==middate-1), by=1)
} else {
  t_in<-seq(max(unlist(lags)), length(date_list), by=1)
}
if(length(intersect(t_complete,t_in))>=3) {
  fore_start_in<-sample(intersect(t_complete,t_in), 3)
} else {
  fore_start_in<-c()
}
if (length(which(date_list==middate))>0) {
  t_out<-seq(which(date_list==middate), which(date_list==ts %>% drop_na(tmean) %>% pull(date) %>% max())-steps, by=1)
  if(length(intersect(t_complete,t_out))>=3) {
    fore_start_out<-sample(intersect(t_complete,t_out), 3)
  } else {
    fore_start_out<-c()
  }
} else {
  fore_start_out<-c()
}

fore_start_list<-c(fore_start_in, fore_start_out) %>% 
    sort()

combine_df_fore_list<-vector(mode="list", length=length(fore_start_list))


for (trial in 1:length(fore_start_list)) {
  fore_start<-fore_start_list[trial]
  
  
  D_fore <- date_list[(fore_start + 1):(fore_start + steps)] %>% 
    as.character() %>% 
    as.matrix()
  
  xnew <- x
  Sigmanew <- Sigma
  xnew[,(fore_start + 1):length(date_list),1]<-NA
  Sigmanew[,(fore_start + 1):length(date_list),1]<-NA
  
  step_groups<-split(seq(1:steps) ,ceiling(seq(1:steps) / 1))
  
  Y_fore<-Var_fore<-matrix(NA, nrow=nrow(P_fore), ncol = steps )      
  for (g in 1:length(step_groups)) {
    t<-step_groups[[g]]
    res<-PrepareEmbedding(xnew,start=fore_start+min(t),end=fore_start+max(t), focalsites = P_fore, lags=lags, neighbors=neighbors,vars=vars, distMat = distMat)
    newX<-res$X
    newP<-res$P
    newD<-res$D
    
    missing_id<-which(rowSums(is.na(newX))!=0)
    if (length(missing_id)>0) {
      newX<-newX[-missing_id,,drop=F]
      newP<-newP[-missing_id,,drop=F]
      newD<-newD[-missing_id,,drop=F]
      t<-t[-missing_id]
    }
    
    if (nrow(newX)>0) {
      res<-PrepareEmbedding(Sigmanew,start=fore_start+min(t),end=fore_start+max(t), focalsites = P_fore, lags=lags, neighbors=neighbors,vars=vars, distMat = distMat)
      newSigma<-res$X
      if (length(missing_id)>0) {
        newSigma<-newSigma[-missing_id,,drop=F]
      }
      
      Y_fore_all<-
        foreach (i=1:num_part,
                 .export = c( 
                   "xnew","Sigmanew",
                   "GPSDM"
                 ),
                 .packages = c("tidyverse","RhpcBLASctl")
        ) %dopar% {
          blas_set_num_threads(1)
          omp_set_num_threads(1)
          particle_pick <- pars[, i, drop = F]
          res <- GPSDM(pars = particle_pick, distMat = distMat, basisX = X_basis, basisP = P_basis,basisD = D_basis, basisY = Y_basis, newX = newX, newP = newP,newD = newD, newSigma=newSigma, mode = c("predict"))
          cbind(res$mt, res$Ct)
        }
      
      logweights<-log_p_valid-max(log_p_valid)
      for(s in 1:nrow(P_fore)) {
        means<-variances<-c()
        for (i in 1:num_part) {
          means<-cbind(means,Y_fore_all[[i]][newP==s,1])
          variances<-cbind(variances,Y_fore_all[[i]][newP==s,2])
        }
        res_df<-bind_rows(as.data.frame(means) %>% mutate(stat="means") %>% mutate(t=row_number()) ,
                          as.data.frame(variances) %>% mutate(stat="variances") %>% mutate(t=row_number()))%>% 
          gather(key="particle", value="value",-t,-stat) %>% 
          spread(key="stat", value="value") %>% 
          group_by(t) %>% 
          summarise(weighted_mean=weighted.mean(means,w=exp(logweights)),
                    weighted_variance=weighted.mean(variances, w=exp(logweights))+Hmisc::wtd.var(means,weights=exp(logweights), normwt = F, method = "ML"))
        
        #store prediction
        Y_fore[P_fore[s,1],t]<-res_df$weighted_mean
        Var_fore[P_fore[s,1],t]<-res_df$weighted_variance
        
        xnew[P_fore[s,1],(fore_start+t),1]<-res_df$weighted_mean
        Sigmanew[P_fore[s,1],(fore_start+t),1]<-res_df$weighted_variance
      }
    }
    
    print(paste0(trial,", ",g))
  }
  
  
  
  fore_df<-as.data.frame(Y_fore) %>% 
    `names<-`(D_fore) %>% 
    mutate(site=P_fore) %>% 
    gather(key="date", value="value",-site) %>% 
    mutate(date=as.Date(date))
  var_fore_df<-as.data.frame(Var_fore) %>% 
    `names<-`(D_fore) %>% 
    mutate(site=P_fore) %>% 
    gather(key="date", value="value",-site) %>% 
    mutate(date=as.Date(date))
  
  path_analyses<-paste0(path_sub,"analyses")
  dir.create(path_analyses,recursive = T)
  write_csv(fore_df,paste0(path_analyses,"/fore_trial ",trial,".csv"))
  write_csv(var_fore_df,paste0(path_analyses,"/var_fore_trial ",trial,".csv"))
  
  fore_df<-left_join(fore_df,var_fore_df, by=c("site", "date")) %>% 
    dplyr::rename(value=value.x, variance=value.y) %>% 
    mutate(lower=value-1.96*sqrt(variance),
           upper=value+1.96*sqrt(variance))
  
  fore_df_ori<-fore_df %>% 
    left_join(df_upper_lower[[1]], by="site",suffix = c("", ".scale"))%>% 
    mutate(value=(value+0.5)*range+lower.scale,
           upper=(upper+0.5)*range+lower.scale,
           lower=(lower+0.5)*range+lower.scale) %>% 
    mutate(horizon=row_number())
  
  combine_df_fore_list[[trial]]<-ts %>% 
    left_join(fore_df_ori %>% dplyr::select(date,site, value, lower, upper, horizon), by=c("date","site")) %>% 
    mutate(pred_error=value-pollen) %>% 
    left_join(climatology, by=c("site_id", "date")) %>% 
    mutate(start_date=date_list[fore_start])%>% 
    mutate(cat=case_when(start_date < middate~ "in-sample",
                         TRUE ~ "out-of-sample"))
  
  # ggplot(combine_df_fore_list[[trial]])+
  #   geom_line(aes(x=date, y=pollen))+
  #   geom_line(aes(x=date, y=value), col="blue")+
  #   geom_line(aes(x=date, y=climatology_mean), col="dark green")+
  #   theme_classic()
}

combine_df_fore<-bind_rows(combine_df_fore_list)


