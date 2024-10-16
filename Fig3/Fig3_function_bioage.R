
rcs_value = function(variable, data, gender){
  sample = data %>% filter(A2 == gender) %>% dplyr::select(A4, all_of(variable)) %>% na.omit() 
  
  fit.all.in.smoothdata<-function(data,variable){
    nknot_vec <- c(50,60,70,80)
    #nknot <- 4
    fit <- lm(as.formula(paste("`",variable,"`","~rcs(A4,nknot_vec)",sep = "")),data = data)
    return(data.frame(A4 = data$A4, predict(fit, interval = "confidence")))
  }
  
  outcome <- fit.all.in.smoothdata(sample, variable) %>% distinct(A4,.keep_all = TRUE) %>% arrange(A4)
  return(data.frame(A4 = outcome$A4, avg_value = outcome$fit))
}

avg_value = function(variable, data, gender){
  sample = data %>% filter(A2 == gender) %>% dplyr::select(A4, variable) %>% na.omit() %>%
    group_by(A4) %>% summarise(avg_value = mean(get(variable)))
  sample$avg_value = as.double(sample$avg_value)
  return(sample)
}
seg_cpdetect_n = function(variable, data, gender, n, type = 's'){
  if(type == 's'){
    sample = rcs_value(variable, data, gender)
  }else{
    sample = avg_value(variable, data, gender)  # type = 'o'
  }
  
  out.lm = glm(avg_value ~ A4, data = sample)
  o = segmented(out.lm, npsi = n) 
  sample$fit <- o$fitted.values
  sample$avg_value = o$fitted.values
  res <- predict(o,se.fit=T)
  sample$lwr = res$fit-20*res$se.fit
  sample$upr = res$fit+20*res$se.fit
  o_sum <- summary(o)
  o_sum.coef <- o_sum$coefficients
  o_se <- o_sum.coef[c(2,3),2]
  
  psi = rep(0, n)
  slp = rep(0, n + 1)
  signif = rep('***', n)
  a = round(o$psi[,2],0)
  b = round(o$coefficients[2:(n + 2)],4)
  for(i in 1:n){
    psi[i] = a[i]
    slp[i] = b[i]
    # todo: 边缘位置处理
    # abs_v = abs(sample[psi[i] + 5 - 24, 2] - if_else(psi[i] -5 > 89, sample[65, 2], sample[psi[i] - 5 - 24, 2]))
    # abs_v = abs(sample[psi[i] + 5 - 24, 2] - sample[psi[i] - 5 - 24, 2])
    # var_v = var(sample[(psi[i] - 5 - 24):(psi[i] - 1 - 24), 2], na.rm = T) + var(sample[(psi[i] + 1 - 24):(psi[i] + 5 - 24), 2], na.rm = T)
    #if(abs_v < var_v){signif[i] = ''}
  }
  slp[n + 1] = b[n + 1]
  psi = data.frame(psi = psi, value = sample[psi - 24, 2])
  return(list(sample = sample, psi = psi, slp = slp, signif = signif,se=o_se))
}



plot_cp = function(variable, input_data, n1, n2, xlab = "年龄", ylab = "记忆总得分",ypos_1,ypos_2,ypos_3,ypos_4){
  
  data = input_data %>% select(A2, A4, variable)
  
  # original data + segmented
  sample4_1 = seg_cpdetect_n(variable, data, "男", n1, type = 's')
  sample4_2 = seg_cpdetect_n(variable, data, "女", n2, type = 's')
  sample4 = list(male = sample4_1, female = sample4_2)
  p4 = plot_cpd_data(sample4, variable, xlab, ylab,ypos_1,ypos_2,ypos_3,ypos_4)
  
  # # smoothing data + segmented
  # sample6_1 = seg_cpdetect_n(variable, data, 1, n1, type = 's')
  # sample6_2 = seg_cpdetect_n(variable, data, 2, n2, type = 's')
  # sample6 = list(male = sample6_1, female = sample6_2)
  # p6 = plot_cpd_data(sample6, variable, xlab, ylab)
  # 
  # # p = ggarrange(p4, p6, nrow = 2, ncol = 1)
  return(p4)
  
}


plot_cpd_data = function(sample, variable, xlab = "年龄", ylab = "记忆总得分",ypos_1,ypos_2,ypos_3,ypos_4){
  sample_m <- sample$male$sample %>% mutate(Gender="Male")
  sample_f <- sample$female$sample %>% mutate(Gender="Female")
  sample_all <- rbind(sample_m,sample_f)
  sample_all$Gender = factor(sample_all$Gender,levels=c("Male","Female"))
  p = ggplot(sample_all, aes(x=A4,y=avg_value,group=Gender)) + 
    geom_line(aes(color=Gender),size = 0.8) +
    scale_color_manual(values = c("#3B4992","#EE0000"))+
    #    geom_line(data = sample$female$sample, aes(A4, avg_value), color = "#EE0000", size = 0.8) + 
    labs(x = xlab, y = ylab) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme_bw() + 
    theme(panel.grid = element_blank()) +
    theme(legend.title = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(color="black"),
          axis.text = element_text(size=12),
          axis.title = element_text(size=12),
          legend.text = element_text(size=12))+
    geom_vline(xintercept = sample$male$psi$psi, size = 0.3, linetype = 'dashed', color = "#3B4992") + 
    geom_vline(xintercept = sample$female$psi$psi, size = 0.3, linetype = 'dashed', color = "#EE0000") +
    ylim(0,100)+
    scale_x_continuous(breaks=seq(20,90,10))+
    #    geom_text() + 
    #annotate("text", label = paste(sample$male$psi$psi, sample$male$signif), x = sample$male$psi$psi, y = sample$male$psi$value, size = 3, color = "#3B4992") +
    #annotate("text", label = paste(sample$female$psi$psi, sample$female$signif), x = sample$female$psi$psi, y = sample$female$psi$value, size = 2, color = "#EE0000") +
    annotate("text", label = sample$male$psi$psi, x = sample$male$psi$psi, y = 0, size = 4, color = "#3B4992") +
    annotate("text", label = sample$female$psi$psi, x = sample$female$psi$psi, y = 2, size = 4, color = "#EE0000") +
    annotate("text", label = sample$male$slp[1], x = 40, y = ypos_1, size = 4, colour = "#3B4992") +
    annotate("text", label = sample$male$slp[2], x = 75, y = ypos_2, size = 4, colour = "#3B4992") +
    annotate("text", label = sample$female$slp[1], x = 40, y = ypos_3, size = 4, colour = "#EE0000") +
    annotate("text", label = sample$female$slp[2], x = 75, y = ypos_4, size = 4, colour = "#EE0000")+
    guides(color=guide_legend(title="Gender"))+
    theme(legend.position = c(0.85,0.85),
          legend.key.size = unit(0.6,"cm"),
          legend.text = element_text(size=8))+
    theme(axis.line.x.top = element_blank(),
          axis.line.y.right = element_blank())
  return(p)
}
