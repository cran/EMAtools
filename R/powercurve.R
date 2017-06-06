#' Create power curves for EMA data
#'
#' This allows you to estimate power to detect an effect at three standard effect sizes (d = 0.2, 0.5, and 0.8). It uses the smpsize_lmm function from sjstats to generate data for the curves and ggplot2 to plot them.
#' @param NumbPart Total number of participants (i.e., level-2 unit)
#' @param NumbResp Total max number of responses per participant (e.g., number of days * number of responses per day). You can either enter this OR enter number of days and number of responses per day manually. If all are entered, it will default to NumbResp.
#' @param days Maximum number of days in study.
#' @param respday Maximum number of responses per day.
#' @param Est_ICC Estimated model ICC. Defaults to .5, but you should use a priori information from prior studies.
#' @return A ggplot object that displays power curves at three effect sizes (d=.2,.5,.8).
#' @keywords power analysis
#' @examples
#' \dontrun{ema.powercurve(NumbPart=80,days=30,respday = 3)}
#'  \dontrun{ema.powercurve(NumbPart=80,NumbResp=200)}



ema.powercurve=function(NumbPart,NumbResp,days,respday,Est_ICC=.5){

  if(missing(NumbResp)) {
    NumbResp<-days*respday
  } else {
    NumbResp<-NumbResp
  }


### initate matricies ####
eff8a<-NULL;eff2a<-NULL;eff5a<-NULL

#### functions for power curves ####

for (PWR in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99)){
  eff8<-(sjstats::smpsize_lmm(eff.size = 0.8, power = PWR, sig.level = 0.05, k = NumbPart, icc = Est_ICC))
  eff8a<-as.data.frame(rbind(eff8a,eff8$`Subjects per Cluster`))
}

for (PWR in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99)){
  eff5<-(sjstats::smpsize_lmm(eff.size = 0.5, power = PWR, sig.level = 0.05, k = NumbPart, icc = Est_ICC))
  eff5a<-as.data.frame(rbind(eff5a,eff5$`Subjects per Cluster`))
}



for (PWR in c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99)){
  eff2<-(sjstats::smpsize_lmm(eff.size = 0.2, power = PWR, sig.level = 0.05, k = NumbPart, icc = Est_ICC))
  eff2a<-as.data.frame(rbind(eff2a,eff2$`Subjects per Cluster`))
}


for (Add99 in c(10,20,30,40,50,60,70)){
  eff2a<-as.data.frame(rbind(eff2a,(eff2$`Subjects per Cluster`+Add99)))
  eff5a<-as.data.frame(rbind(eff5a,(eff5$`Subjects per Cluster`+Add99)))
  eff8a<-as.data.frame(rbind(eff8a,(eff8$`Subjects per Cluster`+Add99)))
}

### merging curves ###
power<-rbind(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99,0.99,0.99,0.99,0.99,0.99,0.99,0.99)

### creating response rate lines

NumbRespColumn<-rbind(c(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),(NumbResp*.50),
(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),(NumbResp*.75),
(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1),(NumbResp*1))


LabResp<-(cbind(c("50%","50%","50%","50%","50%","50%","50%","50%","50%","50%","50%","50%","50%","50%","50%","50%","50%",
                 "75%","75%","75%","75%","75%","75%","75%","75%","75%","75%","75%","75%","75%","75%","75%","75%","75%",
                 "100%","100%","100%","100%","100%","100%","100%","100%","100%","100%","100%","100%","100%","100%","100%","100%","100%")))

comp_final<-as.data.frame(cbind((rbind(power,power,power)),NumbRespColumn,LabResp))
colnames(comp_final)<-c("power","NumbRespColumn","Response_Rate")


lg<-data.frame(cbind(power,eff8a,"Large (d=0.8)"));colnames(lg)<-c("Power","Resp","Effect_Size")
md<-data.frame(cbind(power,eff5a,"Medium (d=0.5)"));colnames(md)<-c("Power","Resp","Effect_Size")
sm<-data.frame(cbind(power,eff2a,"Small (d=0.2)"));colnames(sm)<-c("Power","Resp","Effect_Size")

eff_final<-rbind(lg,md,sm)



#### create ggplot ###

xlab_chart <- paste("Responses per participant (n =",NumbPart,"participants)" )


PowerPlot1<-ggplot2::ggplot()+ geom_line(aes(x = Resp,y = Power,color=Effect_Size),size=1, data=eff_final)+
xlab(xlab_chart) + ylab("Power (1-beta)") +
  scale_x_continuous(limits = c(0,(round((NumbResp+40),-1))),breaks =seq(0, (round(NumbResp+40,-1)), by=20))+
  scale_y_continuous(breaks=c(0.1,0.4,0.6,0.8,1.00), limits=c(0.1,1.00))+
  geom_vline(xintercept=(NumbResp*.50),color="grey65", linetype = 3)+geom_vline(xintercept=(NumbResp*.75),color="grey65", linetype = 2)+
  geom_vline(xintercept=(NumbResp),color="grey65", linetype = 1)+
  geom_line(aes(x = as.numeric(NumbRespColumn), y = as.numeric(power), linetype=Response_Rate), data=comp_final,color="grey65")+
  theme_classic()
return(PowerPlot1)
}
