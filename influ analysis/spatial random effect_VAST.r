 #------------------------------------------------------------------------------
 # Authors:Jhen Hsu, Yi-Jay Chang, Nicholas D.Ducharme-Barth
 # Date:05/10/2022
 # caculating the influence of spatial random effect in VAST
 # I followed the influnence formula from Bentley et al.(2012)
 # The influence analysis is showed by using Pacific saury fishery data
 #------------------------------------------------------------------------------
 library(PBSmapping)

 # loding the report, input data and spatial list data from VAST
 load(".\\Optimized_data.RData") # report
 load(".\\Data_Geostat.RData")   # input data
 load(".\\Spatial_List.RData")   # spatial list

 # loading the model output
 Coefficient = Optimized_data$Opt$par
 Report = Optimized_data$Report
 TmbData = Optimized_data$TmbData
 ParHat = Optimized_data$ParHat
 knot_data = spatial_list$loc_x
  
 Opt = Optimized_data $ Opt
 Sdreport = Opt[["SD"]]
 SD = TMB::summary.sdreport(Sdreport)
 aa = row.names(SD)

 # loading the observed data with corrsponding knot
 Data_Geostat = Data_Geostat
 DATA = Data_Geostat[,c(1,2,9)]

 # coefficient spatial effect
 omega = Report$Omega2_sc
 str(omega)
 omega_sd = Coefficient["L_omega2_z"]
 knot_id = as.numeric(levels(factor(DATA$knot_i)))

 DATA2 = NULL
 for (i in knot_id){
 tempdata = subset(DATA,DATA$knot_i==i)
 tempdata$omega <- omega[i]
 DATA2 = rbind(DATA2,tempdata)
 }
 
 DATA2$beta_omega <- 1*DATA2$omega
 DATA2$beta_omega_LL <- DATA2$beta_omega-1.96*omega_sd
 DATA2$beta_omega_UL <- DATA2$beta_omega+1.96*omega_sd

 DATA2$rho_omega <- DATA2$beta_omega-mean(DATA2$beta_omega)
 DATA2$rho_omega_LL <- DATA2$beta_omega-mean(DATA2$beta_omega_LL)
 DATA2$rho_omega_UL <- DATA2$beta_omega-mean(DATA2$beta_omega_UL)

 DATA2$exp_rho_omega <- exp(DATA2$rho_omega)
 DATA2$exp_rho_omega_LL <- exp(DATA2$rho_omega_LL)
 DATA2$exp_rho_omega_UL <- exp(DATA2$rho_omega_UL)

 DATA2 <- DATA2[order(DATA2$exp_rho_omega),]

 # normalized coefficient table
 coeffs = DATA2[order(DATA2$exp_rho_omega),]
 coeffs2_mean = aggregate(coeffs$exp_rho_omega,by=list(coeffs$knot_i),FUN=mean)
 coeffs2_UL = aggregate(coeffs$ exp_rho_omega_UL,by=list(coeffs$knot_i),FUN=mean)
 coeffs2_LL = aggregate(coeffs$ exp_rho_omega_LL,by=list(coeffs$knot_i),FUN=mean)

 coeffs2 <- data.frame(cbind(coeffs2_mean,coeffs2_UL$x,coeffs2_LL$x))

 names(coeffs2) <- c("knot_id","std_coef","coef_LL","coef_UL")
 coeffs2 = coeffs2[order(coeffs2$knot_id),]

 # knot location
 xydata = data.frame(X=knot_data[,1],Y=knot_data[,2])
 names(xydata) <-c("X", "Y")
 attr(xydata, "projection") <-"UTM"
 attr(xydata, "zone") <- 56
 dataLL = convUL(xydata)
 dataLL[1:10,]
 dataLL$knot_id <- c(1:100)

 coeffs3 = merge(coeffs2,dataLL)
 names(coeffs3) <- c("knot_id","std_coef","coef_LL","coef_UL","Lon","Lat")
 coeffs3 = coeffs3[order(coeffs3$std_coef),]
 plot(dataLL$X,dataLL$Y)
 points(coeffs3$Lon,coeffs3$Lat,pch=19)

#-----------------------------------------------------------------------------------
 # group the knot data for clear understanding
 exp_rho_knot2 <- aggregate(coeffs3$std_coef,by=list(coeffs3$knot_gr),FUN=mean)
 exp_rho_knot2_LL2 <- aggregate(coeffs3$coef_LL ,by=list(coeffs3$knot_gr),FUN=mean)
 exp_rho_knot2_UL2 <- aggregate(coeffs3$coef_UL,by=list(coeffs3$knot_gr),FUN=mean)

 RESULT <- cbind(exp_rho_knot2,exp_rho_knot2_LL2$x,exp_rho_knot2_UL2$x)
 names(RESULT) <- c("knot_group","stand_coef","stand_coef_LL","stand_coef_UL")

 dev.new(height=4,width=6)

 plot(RESULT$knot_group,RESULT$stand_coef,pch=19,cex=1,las=1,xlab="Knot_group",ylab="Coefficient",
      ylim=c(0,3),xaxt="n")
 axis(side=1,at=c(1:20),labels=c(1:20))
 abline(h=1,lty=2,col="black")
 arrows(x0= RESULT$knot_group , y0=RESULT$stand_coef_LL , x1=RESULT$knot_group, y1=RESULT$stand_coef_UL ,
        col="black", lwd=1.5,code=3,angle=90, length=0.1)

 # influence plot
 delta_y_area = aggregate(DATA2$exp_rho_omega,by=list(DATA2$Year),FUN=mean)
 delta_y_area2 = aggregate(DATA2$rho_omega,by=list(DATA2$Year),FUN=mean)
 names(delta_y_area) <- c("Year","exp_delta_y_area")

 # plotting influence plot
 dev.new()
 plot(delta_y_area$Year,delta_y_area$exp_delta_y_area,type="o",pch=19,col="black",
      ylab="Influence",xlab="Year",las=1,ylim=c(0.8,1.4),cex=1.5)
 abline(h=1,lty=2,col="black")

 # data distribution
 DATA3 = NULL

 for (k in c(1:20)){
 id_knot <- subset(RESULT,RESULT$knot_group==k)
 tempdata = subset(DATA2,DATA2$knot_i==id_knot[1,1])
 tempdata2 = subset(DATA2,DATA2$knot_i==id_knot[2,1])
 tempresult = rbind(tempdata,tempdata2)
 tempresult$knot_group <- k
 DATA3 = rbind(DATA3,tempresult)
 }
 
 distrs = aggregate(DATA3[,1],by=list(DATA3$Year,DATA3$knot_group),FUN=length)
 names(distrs) = c('term','focus','count')
 distrs2 = merge(distrs,aggregate(list(total=distrs$count),list(focus=distrs$focus),sum))
 distrs2$prop = with(distrs2,count/total)

 # plotting data distribution
 dev.new()
 plot(x=c(1:20),y=c(1997:2016),type="n",ylim=c(1997,2019),xlab="knot",ylab="Year",las=1,xaxt="n",yaxt="n")
 abline(h=seq(1997,2019,by=3),col="grey",lty=2)
 points(as.integer(distrs2$focus),as.integer(distrs2$term),cex=sqrt(distrs2$prop)*6)
 axis(side=1,at=c(1:20),labels=c(RESULT2$knot_group))
 axis(side=2,at=seq(1997,2019,by=3),labels=seq(1997,2019,by=3),las=1)


