 #------------------------------------------------------------------------------
 # Authors:Jhen Hsu, Yi-Jay Chang, Nicholas D.Ducharme-Barth
 # Date:05/10/2022
 # caculating the influence of year x spatial interaction random effect in Ad hoc GLMM
 # same calculation process for other area stratification approaches (Binary, and Spatial clustering)
 # I followed the influnence formula from Bentley et al.(2012)
 #------------------------------------------------------------------------------

 # loading the Pacific saury fishery data
 Ori_DATA <- read.csv(".//fishery_data.csv",header=T,sep=",")
 Ori_DATA2 <- Ori_DATA

 # data for glmm model
 glmmdata <- within(
 Ori_DATA2,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area)
 v_id <- factor(ID)
 }
 )

 # GLMM model
 library(lme4)
 library(lmerTest)
 CPUE_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)

 # Influence plot
 # extract the estimates and s.e. of each coef
 se <- sqrt(diag(vcov(CPUE_model)))
 tab <- cbind(Est = fixef(CPUE_model), LL = fixef(CPUE_model)-1.96*se, UL = fixef(CPUE_model)+1.96*se)
 rand_int = ranef(CPUE_model)

 # interaction effect
 ya_int = rand_int$`Year:Area`
 ya_n = length(ya_int[,1])
 ya_int$id = rownames(ya_int[,0])
 ya_int$id2 = c(1:ya_n)

 names(ya_int) <- c("coef","id")


 # combine Year and Area
 for(i in 1:nrow(Ori_DATA2)){
 Ori_DATA2$YA[i] <- paste(Ori_DATA2$Year[i],Ori_DATA2$Area[i],sep=":")
 }

 DATA2 = NULL
 for (i in c(1:ya_n)){
 tempdata <- subset(Ori_DATA2, Ori_DATA2$YA==ya_int$id[i])
 tempdata$id2 <- i
 DATA2 <- rbind(DATA2,tempdata)
 }
 
 DATA3 <- data.frame(Year = DATA2$Year,
                     Area = DATA2$Area, #----------->
                     YA =   DATA2$YA,
                     YA_id  = DATA2$id2,
                     beta_ya = NA)

 DATA4 = NULL
 for (i in c(1:ya_n)){
 tempdata = subset(DATA3,DATA3$YA_id == i)
 tempdata$ beta_ya <- ya_int$coef[i]
 DATA4 = rbind(DATA4,tempdata)
 }

 DATA4$beta_ya2 <- 1* DATA4$beta_ya
 DATA4$rho_ya <- DATA4$beta_ya2-mean(DATA4$beta_ya2)
 DATA4$exp_rho_ya <- exp(DATA4$rho_ya)

 # normalized coefficient table
 RESULT2 = NULL
 for (i in c(1:max(DATA4$YA_id))){
 nor_coef = subset(DATA4,DATA4$YA_id==i)
 tempdata = nor_coef[1,]
 RESULT2 = rbind(RESULT2,tempdata)
 }
 
 # assign color
 library(RColorBrewer)
 cols <-  brewer.pal(9, "Reds")
 colfunc <- colorRampPalette(cols)
 COLs = colfunc(23)
 plot(x=23,y=23,xlim=c(1,23),ylim=c(1,23)
 points(x=c(1:23),y=c(1:23),pch=19,col=c(COLs))

 cols <- as.character(cut(RESULT2$Year, breaks = 23, labels = c(COLs)))
 RESULT2$COLS = cols

 yy = boxplot(RESULT2$exp_rho_ya~RESULT2$Area ,las=1,col="darkgrey",outline=F,ylim=c(0.2,3))
 med_coef2 = data.frame(med = yy$`stats`[3,],area= c(1:4))
 med_coef2 <- med_coef2[order(med_coef2$ med),]

 # 3, 2, 1, 4
 area1_id = which(RESULT2$Area==3)
 a = RESULT2[area1_id,]
 a$area_id <- 1

 area2_id = which(RESULT2$Area==2)
 b = RESULT2[area2_id,]
 b$area_id <- 2

 area3_id = which(RESULT2$Area==1)
 c = RESULT2[area3_id,]
 c$area_id <- 3

 area4_id = which(RESULT2$Area==4)
 d = RESULT2[area4_id,]
 d$area_id <- 4

 RESULT3 = rbind(a,b,c,d)

 boxplot(RESULT3$exp_rho_ya~RESULT3$area_id,las=1,col="darkgrey",outline=F,ylim=c(0.4,2.5),
         names=c("Area3","Area2","Area1","Area4"),xaxt="n")

 # for clearly understanding the results
 stripchart(RESULT3$exp_rho_ya ~ RESULT3$area_id, vertical = T, method = "jitter",
            pch = 21, add = TRUE,bg=c(RESULT3$COLS),cex=1.5)
 abline(h=1,lty=2,col="black")

#-----------------------------------------------------------
 # influence plot
 delta_y_int = aggregate(DATA4$ exp_rho_ya,by=list(DATA4$Year),FUN=mean)
 names(delta_y_int) <- c("Year","exp_delta_y_int")

 # plotting influence plot
 dev.new()
 plot(delta_y_int$Year,delta_y_int$exp_delta_y_int,type="o",pch=19,col="black",
      ylab="Influence",xlab="Year",las=1,ylim=c(0.5,2),cex=1.5)
 abline(h=1,lty=2,col="black")

 # data distribution
 distrs = aggregate(DATA4[,1],by=list(DATA4$Year,DATA4$Area),FUN=length)
 names(distrs) = c('term','focus','count')
 distrs = merge(distrs,aggregate(list(total=distrs$count),list(focus=distrs$focus),sum))
 distrs$prop = with(distrs,count/total)

 area_id = c(3,2,1,4)
 
 distrs2 = NULL
 for (i in c(1:4)){
 tempdata = subset(distrs,distrs$focus==i)
 tempdata$focus2 <- area_id[i]
 distrs2 = rbind(distrs2,tempdata)
 }

 # plotting data distribution
 dev.new()
 plot(x=c(1:100),y=c(1997:2096),type="n",ylim=c(1997,2019),xlab="Vessel",ylab="Year",las=1,xaxt="n",yaxt="n",xlim=c(1,4))
 abline(h=seq(1997,2019,by=3),col="grey",lty=2)
 points(as.integer(distrs2$focus2),as.integer(distrs2$term),cex=sqrt(distrs2$prop)*12)
 axis(side=1,at=c(1:4),labels=c("Area3","Area2","Area1","Area4"))
 axis(side=2,at=seq(1997,2019,by=3),labels=seq(1997,2019,by=3),las=1)

      
