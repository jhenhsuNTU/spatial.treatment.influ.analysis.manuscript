 #------------------------------------------------------------------------------
 # Authors:Jhen Hsu, Yi-Jay Chang, Nicholas D.Ducharme-Barth
 # Date:05/10/2022
 # caculating the influence of spatio-temporal random effect in VAST
 # I followed the influnence formula from Bentley et al.(2012)
 # The influence analysis is showed by using Pacific saury fishery data
 #------------------------------------------------------------------------------
 library(scales)
 library(dplyr)

 # loding the report, input data and spatial list data from VAST
 load(".\\Optimized_data.RData") # report
 load(".\\Data_Geostat.RData")   # input data
 load(".\\Spatial_List.RData")   # spatial list

 # loading the model output
 Coefficient = Optimized_data$Opt$par
 Report = Optimized_data$Report
 TmbData = Optimized_data$TmbData
 ParHat = Optimized_data$ParHat

 # loading the observed data with corrsponding knot
 Data_Geostat = Data_Geostat
 DATA = Data_Geostat[,c(1,2,8,9)]

 # coefficient spatio-temporal effect
 epsilon = Report$Epsilon2_sct
 str(epsilon)
 epsilon2 = epsilon[,1,]

 yr_id = data.frame(ID=c(1:23),Year=c(1997:2019))

 DATA2 = NULL
 for (i in c(1997:2019)){
 tempdata = subset(DATA,DATA$Year==i)
 tempdata2 = subset(yr_id,yr_id$Year==i)
 tempdata$yr_id <- tempdata2$ID
 DATA2 = rbind(DATA2,tempdata)
 }

 ss = cbind(as.numeric(DATA2$knot_i),as.numeric(DATA2$yr_id))
 sp_coef = epsilon2[ss]
 DATA2$sp_coef =  sp_coef

 DATA2$beta_epsilon <- 1*DATA2$sp_coef
 DATA2$rho_epsilon <- DATA2$beta_epsilon-mean(DATA2$beta_epsilon)
 DATA2$exp_rho_epsilon <- exp(DATA2$rho_epsilon)
 DATA2 <- DATA2[order(DATA2$exp_rho_epsilon),]

 # combine Year and Area
 for(i in 1:nrow(DATA2)){
 DATA2$sp_id[i] <- paste(DATA2$Year[i],DATA2$knot_i[i],sep=":")
 }

 DATA2 <- DATA2[order(DATA2$ exp_rho_epsilon),]
 length(DATA2[,1])
 DATA2[1:10,]
 boxplot(DATA2$exp_rho_epsilon~DATA2$knot_i ,las=1,col="darkgrey",outline=F,ylim=c(0.2,3))

 yy = boxplot(DATA2$exp_rho_epsilon~DATA2$knot_i ,las=1,col="darkgrey",outline=F,ylim=c(0.2,3))
 med_coef2 = data.frame(med = yy$`stats`[3,],knot= c(1:100))
 med_coef2 <- med_coef2[order(med_coef2$ med),]

 RESULT = NULL
 for (i in c(1:100)){
 id = med_coef2 [i,2]
 tempresult <- subset(DATA2, DATA2$knot_i==id)
 tempresult$id <- i
 RESULT = rbind(tempresult,RESULT)
 }

 boxplot(RESULT$exp_rho_epsilon~RESULT$id,las=1,col="darkgrey",outline=F,ylim=c(0.2,3))

 RESULT$knot_group <- as.numeric(cut(RESULT$id, seq(0,100,5)))
 boxplot(RESULT$exp_rho_epsilon~RESULT$knot_group,las=1,col="darkgrey",outline=F,ylim=c(0.2,3))

 RESULT2 = aggregate(RESULT$exp_rho_epsilon,by=list(RESULT$Year,RESULT$knot_group),FUN=mean)
 names(RESULT2) <- c("Year","knot_group","coeff")
 # assign color
 cols <-  brewer.pal(9, "Reds")
 colfunc <- colorRampPalette(cols)
 COLs = colfunc(23)
 cols <- as.character(cut(RESULT2$Year, breaks = 23, labels = c(COLs)))
 RESULT2$COLS <- cols

 dev.new(height=6,width=8)
 boxplot(RESULT$exp_rho_epsilon~RESULT$knot_group,las=1,col="darkgrey",outline=F,ylim=c(0.2,3))
 stripchart(RESULT2$ coeff ~ RESULT2$knot_group, vertical = T, method = "jitter",
            pch = 21, add = TRUE,bg=alpha(c(RESULT2$COLS),0.9),cex=0.85)
 abline(h=1,lty=2,col="black")

 # influence plot
 delta_y_ee = aggregate(RESULT$rho_epsilon,by=list(RESULT$Year),FUN=mean)
 delta_y_s = aggregate(RESULT$exp_rho_epsilon,by=list(RESULT$Year),FUN=mean)
 names(delta_y_s) <- c("Year","exp_rho_epsilon")

 # plotting influence plot
 dev.new(height=4,width=6)
 plot(delta_y_s$Year,delta_y_s$exp_rho_epsilon,type="o",pch=19,col="black",
      ylab="Influence",xlab="Year",las=1,ylim=c(min(delta_y_s$exp_rho_epsilon),max(delta_y_s$exp_rho_epsilon)),
      cex=1.5)
 abline(h=1,lty=2,col="black")

 # data distribution
 distrs = aggregate(RESULT[,1],by=list(RESULT$Year,RESULT$knot_group),FUN=length)
 names(distrs) = c('term','focus','count')
 distrs = merge(distrs,aggregate(list(total=distrs$count),list(focus=distrs$focus),sum))
 distrs$prop = with(distrs,count/total)

 # plotting data distribution
 dev.new(height=6,width=10)
 plot(x=c(1:20),y=c(1:20),type="n",ylim=c(1997,2019),xlab="Knot",ylab="Year",las=1,xaxt="n",yaxt="n",cex.lab=1.2)
 abline(h=seq(1997,2019,by=3),col="grey",lty=2)
 points(as.integer(distrs$focus),as.integer(distrs$term),cex=sqrt(distrs$prop)*6)
 axis(side=1,at=c(1:20),labels=c(1:20),cex.axis=1.2)
 axis(side=2,at=seq(1997,2019,by=3),labels=seq(1997,2019,by=3),las=1,cex.axis=1.2)

