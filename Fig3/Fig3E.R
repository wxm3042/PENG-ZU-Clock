source("~/Rcode/function_bioage.R")
data_new <- read.csv("data_new.csv")
library(ggplot2)
library(rms)
library(haven)
library(tidyr)
box::use(dplyr[...])
library(segmented)
library(labelled)
library(cowplot)

## 截点分析

sample4_1 = seg_cpdetect_n("score", data_new%>%filter(type=="Normal"), "男", 1, type = 's')
sample4_2 = seg_cpdetect_n("score", data_new%>%filter(type=="Slowdown"), "男", 1, type = 's')
sample4_3 = seg_cpdetect_n("score", data_new%>%filter(type=="Accelerated"), "男", 1, type = 's')
sample4_4 = seg_cpdetect_n("score", data_new%>%filter(type=="Normal"), "女", 1, type = 's')
sample4_5 = seg_cpdetect_n("score", data_new%>%filter(type=="Slowdown"), "女", 1, type = 's')
sample4_6 = seg_cpdetect_n("score", data_new%>%filter(type=="Accelerated"), "女", 1, type = 's')


sample_normal <- list(sample4_1,sample4_4)
age_ls <- sapply(sample_normal,function(x){x$psi$psi})

a <- matrix(rep(0,12),nrow=4)

a[1,1] <- sample4_1$sample$fit[which(sample4_1$sample$A4==age_ls[1])]
a[1,2] <-sample4_2$sample$fit[which(sample4_2$sample$A4==age_ls[1])]
a[1,3] <-(sample4_3$sample$fit[which(sample4_3$sample$A4==age_ls[1]-1)]+ sample4_3$sample$fit[which(sample4_3$sample$A4==age_ls[1]+1)])/2
#a[1,3] <-sample4_3$sample$fit[which(sample4_3$sample$A4==age_ls[1])]
a[2,1] <-sample4_4$sample$fit[which(sample4_4$sample$A4==age_ls[2])]
a[2,2] <-sample4_5$sample$fit[which(sample4_5$sample$A4==age_ls[2])]
a[2,3] <-sample4_6$sample$fit[which(sample4_6$sample$A4==age_ls[2])]


a[3,1] <-sample4_1$sample$fit[which(sample4_1$sample$A4==80)]
a[3,2] <-sample4_2$sample$fit[which(sample4_2$sample$A4==80)]
a[3,3] <-sample4_3$sample$fit[which(sample4_3$sample$A4==80)]

a[4,1] <-sample4_4$sample$fit[which(sample4_4$sample$A4==80)]
a[4,2] <-sample4_5$sample$fit[which(sample4_5$sample$A4==80)]
a[4,3] <-sample4_6$sample$fit[which(sample4_6$sample$A4==80)]

score_report <- data.frame(Gender = c("男","女","男","女"),
                           AgeType = c("Normal截点(57)","Normal截点(61)","年老截点(80)","年老截点(80)"),
                           Slowdown = a[,2],
                           Normal = a[,1],
                           Accelerated = a[,3])
write.csv(score_report, file = "score_report.csv")

#图

p2<-plot_cp("score",data_new%>%filter(type=="Normal"),1,1,ylab="Intrinsic Capacity Score",xlab = "Age",83,60,73,40)
ggsave(plot=p2, filename = "Fig_Slowdown.pdf", width = 17, height = 10, units = "cm", dpi = 600)
p1<-plot_cp("score",data_new%>%filter(type=="Slowdown"),1,1,ylab="Intrinsic Capacity Score",xlab = "Age",83,67,73,40)
ggsave(plot=p1, filename = "Fig_Normal.pdf", width = 17, height = 10, units = "cm", dpi = 600)
p3<-plot_cp("score",data_new%>%filter(type=="Accelerated"),1,1,ylab="Intrinsic Capacity Score",xlab = "Age",79,60,67,35)
ggsave(plot=p3, filename = "Fig_Acc.pdf", width = 17, height = 10, units = "cm", dpi = 600)
