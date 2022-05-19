 #------------------------------------------------------------------------------
 # Author:Jhen Hsu
 # Date:05/10/2022
 # binary recursive approach
 #------------------------------------------------------------------------------

 # loading fishery data
 setwd("C://Users//user//Dropbox//MS_STD CPUE//02_GLM glm tree//no_month//")
 DATA = read.csv("C://Users//user//Dropbox//MS_STD CPUE//DATA//saurydata_1997_2019.csv",header=T,sep=",")
 DATA2 = subset(DATA,DATA$total_catch>0 & DATA$SST>0 & DATA$Month>=8 &DATA$Month<=11 )

 # determining the spatial resolution, here is 0.25 degree
 # beginning of latitude

 #------------------------------------------------------------------------------
 # Node 1 ; 2 areas
 #------------------------------------------------------------------------------
 lon_range = factor(DATA2$Lon0.25)
 lat_range = factor(DATA2$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL

 # beginning of longitude
 for (i in c(2:length(lon_index))){

 sel_index = which(DATA2$Lon0.25<=lon_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_2 <- 1

 retain_data = DATA2[-c(sel_index),]
 retain_data$Area_2 <- 2

 DATA4 = rbind(sel_data,retain_data)
 
 # prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_2)
 v_id <- factor(ID)  }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))

 temp_BIC = print(BIC(GLMM_model))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lon_index[1:length(lon_index)-1])
 names(AIC_record) <- c("AIC","longitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lon_index[1:length(lon_index)-1])
 names(BIC_record) <- c("BIC","longitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.1.Long = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])

#-------------------------------------------------------------------------------
 # Node 1 ; 2 areas
#-------------------------------------------------------------------------------
 lon_range = factor(DATA2$Lon0.25)
 lat_range = factor(DATA2$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of latitude
 for (i in c(2:length(lat_index))){

 sel_index = which(DATA2$Lat0.25<=lat_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_2 <- 1

 retain_data = DATA2[-c(sel_index),]
 retain_data$Area_2 <- 2

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_2)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lat_index[1:length(lat_index)-1])
 names(AIC_record) <- c("AIC","latitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lat_index[1:length(lat_index)-1])
 names(BIC_record) <- c("BIC","latitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.1.Lat = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])

 cut.1_result = data.frame(cbind(AIC=c(cut.1.Long$AIC,cut.1.Lat$AIC),
                                 BIC=c(cut.1.Long$BIC,cut.1.Lat$BIC),
                                 node=c(cut.1.Long$longitude,cut.1.Lat$latitude)))

 index_A = which.min(cut.1_result$AIC & cut.1_result$BIC)
 bound_A = cut.1_result[index_A,3]

 Area_I_ind = which(DATA2$Lon0.25<=bound_A)
 Area_II_ind = which(DATA2$Lon0.25>bound_A)

 DATA2$Area_tree = NA
 DATA2[Area_I_ind,27] <- 1
 DATA2[Area_II_ind,27] <- 2
 
 # define the two area ---------------->
 area1 = subset(DATA2,DATA2$Area_tree==1)
 area2 = subset(DATA2,DATA2$Area_tree==2)

 #------------------------------------------------------------------------------
 # Node 2 ; 3 areas
 # branch A-1
 #------------------------------------------------------------------------------
 lon_range = factor(area1$Lon0.25)
 lat_range = factor(area1$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of longitude
 for (i in c(2:length(lon_index))){
 sel_index = which(area1$Lon0.25<=lon_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 3

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lon_index[1:length(lon_index)-1])
 names(AIC_record) <- c("AIC","longitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lon_index[1:length(lon_index)-1])
 names(BIC_record) <- c("BIC","longitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.2.Long = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])

#-------------------------------------------------------------------------------
 # Node 2 ; 3 area
 # branch A-2
#-------------------------------------------------------------------------------
 lon_range = factor(area1$Lon0.25)
 lat_range = factor(area1$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of latitude
 for (i in c(2:length(lat_index))){

 sel_index = which(area1$Lat0.25<=lat_index[i-1])

 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 3

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)
# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lat_index[1:length(lat_index)-1])
 names(AIC_record) <- c("AIC","latitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lat_index[1:length(lat_index)-1])
 names(BIC_record) <- c("BIC","latitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.2.Lat = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])

#-------------------------------------------------------------------------------
 # Node 2 ; 3 area
 # branch B-1
#-------------------------------------------------------------------------------
 lon_range = factor(area2$Lon0.25)
 lat_range = factor(area2$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of longitude
 for (i in c(2:length(lon_index))){

 sel_index = which(area2$Lon0.25<=lon_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 3

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLM_model))
 temp_BIC = print(BIC(GLM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lon_index[1:length(lon_index)-1])
 names(AIC_record) <- c("AIC","longitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)
 
 BIC_record = cbind(BIC_record,lon_index[1:length(lon_index)-1])
 names(BIC_record) <- c("BIC","longitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.2.1.Long = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])
 
#-------------------------------------------------------------------------------
 # Node 2 ; 3 area
 # branch A-2
#-------------------------------------------------------------------------------
 lon_range = factor(area2$Lon0.25)
 lat_range = factor(area2$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of latitude
 for (i in c(2:length(lat_index))){

 sel_index = which(area2$Lat0.25<=lat_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 3

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lat_index[1:length(lat_index)-1])
 names(AIC_record) <- c("AIC","latitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)
 
 BIC_record = cbind(BIC_record,lat_index[1:length(lat_index)-1])
 names(BIC_record) <- c("BIC","latitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.2.1.Lat = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])


 cut.2_result = data.frame(cbind(AIC=c(cut.2.Long$AIC,cut.2.Lat$AIC,cut.2.1.Long$AIC,cut.2.1.Lat$AIC),
                                 BIC=c(cut.2.Long$BIC,cut.2.Lat$BIC,cut.2.1.Long$BIC,cut.2.1.Lat$BIC),
                                 node=c(cut.2.Long$longitude,cut.2.Lat$latitude,
                                        cut.2.1.Long$longitude,cut.2.1.Lat$latitude)))

 index_B.aic = which.min(cut.2_result$AIC)
 index_B.bic = which.min(cut.2_result$BIC)

 bound_B = cut.2_result[index_B.aic,3]

#---------------------------------------------------------------------------------
 for(j in 1:nrow(DATA2)){

 if (DATA2$Lon0.25[j]<=bound_A & DATA2$Lat0.25[j]<=bound_B){
 DATA2$Area_tree[j] <- 1
 }

 else if (DATA2$Lon0.25[j]>bound_A){
 DATA2$Area_tree[j] <- 2
 }

 else if (DATA2$Lon0.25[j]<=bound_A & DATA2$Lat0.25[j]> bound_B){
 DATA2$Area_tree[j] <- 3
 }
 }

 # view the data
 area1 = subset(DATA2,DATA2$Area_tree==1)
 area2 = subset(DATA2,DATA2$Area_tree==2)
 area3 = subset(DATA2,DATA2$Area_tree==3)

#-------------------------------------------------------------------------------
 # Node 3 ; 4 areas
 # branch A-1
#-------------------------------------------------------------------------------
 lon_range = factor(area1$Lon0.25)
 lat_range = factor(area1$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of longitude
 for (i in c(2:length(lon_index))){

 sel_index = which(area1$Lon0.25<=lon_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 4

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lon_index[1:length(lon_index)-1])
 names(AIC_record) <- c("AIC","longitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)
 
 BIC_record = cbind(BIC_record,lon_index[1:length(lon_index)-1])
 names(BIC_record) <- c("BIC","longitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.3.Long = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])

#-------------------------------------------------------------------------------
 # Node 3; 4 area
 # branch A-2
#-------------------------------------------------------------------------------
 lon_range = factor(area1$Lon0.25)
 lat_range = factor(area1$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of latitude
 for (i in c(2:length(lat_index))){

 sel_index = which(area1$Lat0.25<=lat_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 4

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lat_index[1:length(lat_index)-1])
 names(AIC_record) <- c("AIC","latitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)
 
 BIC_record = cbind(BIC_record,lat_index[1:length(lat_index)-1])
 names(BIC_record) <- c("BIC","latitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.3.Lat = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])
 
#-------------------------------------------------------------------------------
 # Node 3 ; 4 areas
 # branch B-1
#-------------------------------------------------------------------------------
 lon_range = factor(area2$Lon0.25)
 lat_range = factor(area2$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of longitude
 for (i in c(2:length(lon_index))){

 sel_index = which(area2$Lon0.25<=lon_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 4

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lon_index[1:length(lon_index)-1])
 names(AIC_record) <- c("AIC","longitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lon_index[1:length(lon_index)-1])
 names(BIC_record) <- c("BIC","longitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.3.1.Long = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])

#-------------------------------------------------------------------------------
 # Node 3 ; 4 area
 # branch B-2
#-------------------------------------------------------------------------------
 lon_range = factor(area2$Lon0.25)
 lat_range = factor(area2$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of latitude
 for (i in c(2:length(lat_index))){

 sel_index = which(area2$Lat0.25<=lat_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 4

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lat_index[1:length(lat_index)-1])
 names(AIC_record) <- c("AIC","latitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lat_index[1:length(lat_index)-1])
 names(BIC_record) <- c("BIC","latitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.3.1.Lat = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])

#-------------------------------------------------------------------------------
 # Node 3 ; 4 areas
 # branch C-1
#-------------------------------------------------------------------------------
 lon_range = factor(area3$Lon0.25)
 lat_range = factor(area3$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL

 # beginning of longitude
 for (i in c(2:length(lon_index))){

 sel_index = which(area3$Lon0.25<=lon_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 4
 retain_data = DATA2[-sel_index,]
 
 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lon_index[1:length(lon_index)-1])
 names(AIC_record) <- c("AIC","longitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lon_index[1:length(lon_index)-1])
 names(BIC_record) <- c("BIC","longitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.3.2.Long = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])

#-------------------------------------------------------------------------------
 # Node 3 ; 4 area
 # branch C-2
#-------------------------------------------------------------------------------
 lon_range = factor(area3$Lon0.25)
 lat_range = factor(area3$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of latitude
 for (i in c(2:length(lat_index))){

 sel_index = which(area3$Lat0.25<=lat_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 4
 retain_data = DATA2[-sel_index,]
 
 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lat_index[1:length(lat_index)-1])
 names(AIC_record) <- c("AIC","latitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lat_index[1:length(lat_index)-1])
 names(BIC_record) <- c("BIC","latitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)
 
 cut.3.2.Lat = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])
 
 cut.3_result = data.frame(cbind(AIC=c(cut.3.Long$AIC,cut.3.Lat$AIC,
                                       cut.3.1.Long$AIC,cut.3.1.Lat$AIC,
                                       cut.3.2.Long$AIC,cut.3.2.Lat$AIC),
                                       
                                 BIC=c(cut.3.Long$BIC,cut.3.Lat$BIC,
                                       cut.3.1.Long$BIC,cut.3.1.Lat$BIC,
                                       cut.3.2.Long$BIC,cut.3.2.Lat$BIC),
                                       
                                 node=c(cut.3.Long$longitude,cut.3.Lat$latitude,
                                        cut.3.1.Long$longitude,cut.3.1.Lat$latitude,
                                        cut.3.2.Long$longitude,cut.3.2.Lat$latitude
                                        )))

 index_C.aic = which.min(cut.3_result$AIC)
 index_C.bic = which.min(cut.3_result$BIC)

 bound_C = cut.3_result[index_C.bic,3]
#-------------------------------------------------------------------------------
 for(j in 1:nrow(DATA2)){

 if (DATA2$Lon0.25[j]<=bound_C & DATA2$Lat0.25[j]<=bound_B){
 DATA2$Area_tree[j] <- 1
 }

 else if (DATA2$Lon0.25[j]>bound_A ){
 DATA2$Area_tree[j] <- 2
 }

 else if (DATA2$Lon0.25[j]<=bound_A & DATA2$Lat0.25[j]>= bound_B){
 DATA2$Area_tree[j] <- 3
 }


 else if (DATA2$Lon0.25[j]<=bound_A & DATA2$Lat0.25[j]<= bound_C){
 DATA2$Area_tree[j] <- 4
 }
 }

 # view the data
 area1 = subset(DATA2,DATA2$Area_tree==1)
 area2 = subset(DATA2,DATA2$Area_tree==2)
 area3 = subset(DATA2,DATA2$Area_tree==3)
 area4 = subset(DATA2,DATA2$Area_tree==4)

#-------------------------------------------------------------------------------
 # Node 4 ; 5 areas
 # branch A-1
#-------------------------------------------------------------------------------
 lon_range = factor(area1$Lon0.25)
 lat_range = factor(area1$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of longitude
 for (i in c(2:length(lon_index))){

 sel_index = which(area1$Lon0.25<=lon_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 5

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lon_index[1:length(lon_index)-1])
 names(AIC_record) <- c("AIC","longitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lon_index[1:length(lon_index)-1])
 names(BIC_record) <- c("BIC","longitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.4.Long = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])
#-------------------------------------------------------------------------------
 # Node 4; 5 area
 # branch A-2
#-------------------------------------------------------------------------------
 lon_range = factor(area1$Lon0.25)
 lat_range = factor(area1$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL

 # beginning of latitude
 for (i in c(2:length(lat_index))){

 sel_index = which(area1$Lat0.25<=lat_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 5

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lat_index[1:length(lat_index)-1])
 names(AIC_record) <- c("AIC","latitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lat_index[1:length(lat_index)-1])
 names(BIC_record) <- c("BIC","latitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.4.Lat = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])
#-------------------------------------------------------------------------------
 # Node 4 ; 5 areas
 # branch B-1
#-------------------------------------------------------------------------------
 lon_range = factor(area2$Lon0.25)
 lat_range = factor(area2$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of longitude
 for (i in c(2:length(lon_index))){

 sel_index = which(area2$Lon0.25<=lon_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 5

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lon_index[1:length(lon_index)-1])
 names(AIC_record) <- c("AIC","longitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)
 
 BIC_record = cbind(BIC_record,lon_index[1:length(lon_index)-1])
 names(BIC_record) <- c("BIC","longitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.4.1.Long = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])

#-------------------------------------------------------------------------------
 # Node 4; 5 area
 # branch B-2
#-------------------------------------------------------------------------------
 lon_range = factor(area2$Lon0.25)
 lat_range = factor(area2$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of latitude
 for (i in c(2:length(lat_index))){

 sel_index = which(area2$Lat0.25<=lat_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 5

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lat_index[1:length(lat_index)-1])
 names(AIC_record) <- c("AIC","latitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lat_index[1:length(lat_index)-1])
 names(BIC_record) <- c("BIC","latitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.4.1.Lat = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])

#-------------------------------------------------------------------------------
 # Node 4 ; 5 areas
 # branch C-1
#-------------------------------------------------------------------------------
 lon_range = factor(area3$Lon0.25)
 lat_range = factor(area3$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of longitude
 for (i in c(2:length(lon_index))){

 sel_index = which(area3$Lon0.25<=lon_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 5

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lon_index[1:length(lon_index)-1])
 names(AIC_record) <- c("AIC","longitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lon_index[1:length(lon_index)-1])
 names(BIC_record) <- c("BIC","longitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.4.2.Long = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])
#-------------------------------------------------------------------------------
 # Node 4; 5 area
 # branch C-2
#-------------------------------------------------------------------------------
 lon_range = factor(area3$Lon0.25)
 lat_range = factor(area3$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of latitude
 for (i in c(2:length(lat_index))){

 sel_index = which(area3$Lat0.25<=lat_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 5

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lat_index[1:length(lat_index)-1])
 names(AIC_record) <- c("AIC","latitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lat_index[1:length(lat_index)-1])
 names(BIC_record) <- c("BIC","latitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.4.2.Lat = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])
#-------------------------------------------------------------------------------
 # Node 4 ; 5 areas
 # branch D-1
#-------------------------------------------------------------------------------
 lon_range = factor(area4$Lon0.25)
 lat_range = factor(area4$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of longitude
 for (i in c(2:length(lon_index))){

 sel_index = which(area4$Lon0.25<=lon_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 5

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lon_index[1:length(lon_index)-1])
 names(AIC_record) <- c("AIC","longitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lon_index[1:length(lon_index)-1])
 names(BIC_record) <- c("BIC","longitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.4.3.Long = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])
#-------------------------------------------------------------------------------
 # Node 4; 5 area
 # branch D-2
#-------------------------------------------------------------------------------
 lon_range = factor(area4$Lon0.25)
 lat_range = factor(area4$Lat0.25)

 lon_index = as.numeric(levels(lon_range))
 lat_index = as.numeric(levels(lat_range))

 AIC_record = NULL
 BIC_record = NULL
 
 # beginning of latitude
 for (i in c(2:length(lat_index))){

 sel_index = which(area4$Lat0.25<=lat_index[i-1])
 sel_data = DATA2[sel_index,]
 sel_data$Area_tree <- 5

 retain_data = DATA2[-sel_index,]

 DATA4 = rbind(sel_data,retain_data)

# prepare data for glmm ---------------------------------------------------------

 # glmm data
 glmmdata <- within(
 DATA4,{
 Year <- factor(Year)
 Month <- factor(Month)
 Area <- factor(Area_tree)
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 # GLMM model
 GLMM_model <- lmer(log(total_catch/op_day) ~ Year+Area+SST+I(SST^2)+(1|v_id)+(1|Year:Area)-1,
               data = glmmdata,REML =T)
 temp_AIC = print(AIC(GLMM_model))
 temp_BIC = print(BIC(GLMM_model))
 AIC_record = data.frame(AIC=rbind(AIC_record,temp_AIC))
 BIC_record = data.frame(BIC=rbind(BIC_record,temp_BIC))
 }

 AIC_record = cbind(AIC_record,lat_index[1:length(lat_index)-1])
 names(AIC_record) <- c("AIC","latitude")
 min_aic = min(AIC_record$ AIC)
 min_aic_index = which(AIC_record$AIC==min_aic)

 BIC_record = cbind(BIC_record,lat_index[1:length(lat_index)-1])
 names(BIC_record) <- c("BIC","latitude")
 min_bic = min(BIC_record$ BIC)
 min_bic_index = which(BIC_record$BIC==min_bic)

 cut.4.3.Lat = cbind(AIC_record[min_aic_index,],BIC_record[min_bic_index,])

 cut.4_result = data.frame(cbind(AIC=c(cut.4.Long$AIC,cut.4.Lat$AIC,cut.4.1.Long$AIC,cut.4.1.Lat$AIC,
                                       cut.4.2.Long$AIC,cut.4.2.Lat$AIC,cut.4.3.Long$AIC,cut.4.3.Lat$AIC),

                                 BIC=c(cut.4.Long$BIC,cut.4.Lat$BIC,cut.4.1.Long$BIC,cut.4.1.Lat$BIC,
                                       cut.4.2.Long$BIC,cut.4.2.Lat$BIC,cut.4.3.Long$BIC,cut.4.3.Lat$BIC),

                                 node=c(cut.4.Long$longitude,cut.4.Lat$latitude,
                                        cut.4.1.Long$longitude,cut.4.1.Lat$latitude,
                                        cut.4.2.Long$longitude,cut.4.2.Lat$latitude,
                                        cut.4.3.Long$longitude,cut.4.3.Lat$latitude
                                        )))
                                        
 index_D.aic = which.min(cut.4_result$AIC)
 index_D.bic = which.min(cut.4_result$BIC)

 bound_D = cut.4_result[index_D.bic,3]

 Report = data.frame(rbind(cut.1 = cut.1_result,cut.2 = cut.2_result,cut.3 = cut.3_result,
                         cut.4 = cut.4_result))

 # output results
 write.table(Report,"C://Users//user//Dropbox//MS_STD CPUE//02_GLM glm tree//no_month//report.csv",sep=",")
 write.table(DATA2,"C://Users//user//Dropbox//MS_STD CPUE//02_GLM glm tree//no_month//saurydata_1997_2019_GLM_tree0.25_BIC.csv",sep=",",row.names = F)
 
