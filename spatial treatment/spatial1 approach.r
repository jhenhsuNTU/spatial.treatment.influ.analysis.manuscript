 #------------------------------------------------------------------------------
 # Authors:Jhen Hsu, Yi-Jay Chang, Nicholas D.Ducharme-Barth
 # Date:05/10/2022
 # spatial clustering with weighting = 0.1
 #------------------------------------------------------------------------------
 library(factoextra)
 library(NbClust)
 library(cluster)

 DATA = read.csv(".//fishery_data.csv",header=T,sep=",")
 DATA2 <- DATA

 # aggregate original data into 0.25 grid
 catch = aggregate(DATA2$total_catch,by=list(DATA2$Lat0.25,DATA2$Lon0.25),FUN=sum)
 effort = aggregate(DATA2$op_day,by=list(DATA2$Lat0.25,DATA2$Lon0.25),FUN=sum)
 CPUE <- log(catch/effort)

 DATA3 <- data.frame(
 cbind(Lat0.25 = catch[,1],Lon0.25 = catch[,2],CPUE =CPUE[,3])
 )

 # dissimilarity martix
 DATA4 = data.frame(
 var_1 = (DATA3$Lat0.25-mean(DATA3$Lat0.25))/sd(DATA3$Lat0.25),
 var_2 = (DATA3$Lon0.25-mean(DATA3$Lon0.25))/sd(DATA3$Lon0.25),
 var_3 = (DATA3$CPUE-mean(DATA3$CPUE))/sd(DATA3$CPUE)
 )

 DIM = dim(DATA4)

 # spatial_clustering method
 W = 1  #-------------------->

 DIST = matrix(NA,nrow = DIM[1], ncol = DIM[1])
 for(i in 1:DIM[1]){
 for(j in 1:DIM[1]){

 DIST[i,j] = sqrt(W^2*((DATA[i,1]-DATA[j,1])^2+(DATA[i,2]-DATA[j,2])^2)+(DATA[i,3]-DATA[j,3])^2)
 }
 }

 # spatial clustering--> k-medoid
 for (r in c(2:6)){

 PAM = pam(DIST, r, diss = TRUE) #------> 2 - 6 strata
 DATA$clu_area = PAM$clustering

 # assign the area cluster result into original fishery data
 RESULT = NULL
 for(k in c(1:r)){

 index_area = which(DATA$clu_area==k)
 temp_area_Lat = DATA[index_area,1]
 temp_area_Lon = DATA[index_area,2]

 for(i in c(1:length(temp_area_Lat))){
 temp_area = subset(DATA2,DATA2$Lat0.25==temp_area_Lat[i] & DATA2$Lon0.25==temp_area_Lon[i])
 temp_area$area_clu <- k
 RESULT = rbind(RESULT,temp_area)
 }
 }

 glmmdata <- within(

 RESULT,{
 Year <- factor(Year)
 Area <- factor(area_clu) #----->
 v_id <- factor(ID) # vessel id is random effect
 }
 )

 CPUE_model <- lmer(log(total_catch/op_day) ~ Year+Area+(1|v_id)+(1|Year:Area)SST+I(SST^2),
               data = glmmdata,REML =T)
 print(AIC(CPUE_model))
 }
