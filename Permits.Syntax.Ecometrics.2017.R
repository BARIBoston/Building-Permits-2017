#---- individual-specific: set path to BARI Google Drive
BARIDrive_path <- "~/Google Drive/BARI/BARI Research Team Data Library/"

#---- packages
library(rgdal)
library(lubridate)

#---- INPUT PATHS ----
project_path <- "Permits/"
permits2017_path <- paste0(BARIDrive_path,project_path,"Data/Permits.Records.Geocoded.20170801.csv")
landParcels_path <- paste0(BARIDrive_path,"GeoInfrastructure2017/LandParcels.2017.csv")
landParcelsShpPath = paste0(BARIDrive_path,"GeoInfrastructure2017/LandParcels.2017")
landParcelsShpName = "LandParcels.2017"

propertyAssessment_path <- paste0(BARIDrive_path,project_path,"../GeoInfrastructure2017/Properties.2017.csv")


blockGroupsShpPath = paste(BARIDrive_path,"GeoInfrastructure2017/Block Groups 2010 BARI",sep="")
blockGroupsShpName = "Census Block Groups"
tractsShpPath = paste0(BARIDrive_path,"GeoInfrastructure2017/Tracts_Boston_2010_BARI")
tractsShpName = "Tracts_Boston BARI"

#---- OUTPUT PATHS ----
lpEcometricsPath = paste0(BARIDrive_path,project_path,"Data/Parcels/")
lpEcometricsName = "Permits.Ecometrics.LP"

cbgEcometricsPath = paste0(BARIDrive_path,project_path,"Data/CBG/")
cbgEcometricsName = "Permits.Ecometrics.CBG"

ctEcometricsPath = paste0(BARIDrive_path,project_path,"Data/CT/")
ctEcometricsName = "Permits.Ecometrics.CT"

#---- READ IN DATA 
permits = read.csv(permits2017_path,stringsAsFactors=FALSE)
landParcels = read.csv(landParcels_path, stringsAsFactors=F)
cts_shp = readOGR(path.expand(tractsShpPath),tractsShpName, stringsAsFactors=F)
bgs_shp = readOGR(path.expand(blockGroupsShpPath),blockGroupsShpName, stringsAsFactors=F)
lps_shp = readOGR(path.expand(landParcelsShpPath),landParcelsShpName, stringsAsFactors=F)
propertyAssessment = read.csv(propertyAssessment_path,stringsAsFactors=F)



#---- PREP BEFORE AGGREGATION ----

#check out the data
sum(!is.na(permits$BG_ID_10))/length(permits$BG_ID_10)
sum(!is.na(permits$CT_ID_10))/length(permits$CT_ID_10)
sum(!is.na(permits$ISSUED_DATE))/length(permits$ISSUED_DATE)


#this doesn't depend on the year so i can do it outside the loop
#highlights the fact that we are using the same number of parcels for each year
lps_perBG = aggregate(Land_Parcel_ID ~ BG_ID_10, data=landParcels,length)
names(lps_perBG)[2]="numLandParcels"

lps_perCT = aggregate(Land_Parcel_ID ~ CT_ID_10, data=landParcels,length)
names(lps_perCT)[2]="numLandParcels"

lps_agg_long = data.frame(Land_Parcel_ID = landParcels$Land_Parcel_ID)
cbgs_agg_long = lps_perBG
cts_agg_long = lps_perCT

cbgs_agg_long_shp = merge(bgs_shp,lps_perBG,by="BG_ID_10",all.x=T)
cts_agg_long_shp = merge(cts_shp,lps_perCT,by="CT_ID_10",all.x=T)

#loops through for each year
for (i in c(2010:2017)){
      print(i)
      
      permits_sub = permits[year(permits$ISSUED_DATE)==i,]
      permits_sub$DECLARED_VALUATION_c99 = ifelse(permits_sub$DECLARED_VALUATION<0, 0,
                                                  ifelse(permits_sub$DECLARED_VALUATION>quantile(permits_sub$DECLARED_VALUATION,.99),
                                                         quantile(permits_sub$DECLARED_VALUATION,.99),
                                                         permits_sub$DECLARED_VALUATION))
      temp = permits_sub[ permits_sub$newcon==1,]
      permits_sub_nd =  rbind(permits_sub[ permits_sub$newcon!=1, ],
                              temp[!duplicated(paste(temp$DECLARED_VALUATION,temp$Land_Parcel_ID)),])
      rm(temp)
      
      permits_sub_nd = permits_sub_nd[ !duplicated(permits_sub_nd$PermitNumber),]
      
      
      #aggregating
      
      types_byParcel  = aggregate( cbind(demo,newcon,addition,reno)~Land_Parcel_ID,data =permits_sub, FUN=sum, na.rm=T)
      names(types_byParcel) = c("Land_Parcel_ID","DEMO","NEWCON","ADD","RENO")
      types_byParcel2 = aggregate(cbind(((demo+newcon+addition+reno)*DECLARED_VALUATION_c99),
                                        ((demo+newcon+addition+reno)*DECLARED_VALUATION_c99*government))~Land_Parcel_ID, data = permits_sub_nd,FUN=sum,na.rm=T)
      names(types_byParcel2) = c("Land_Parcel_ID","DV","DV_public")
      types_byParcel = merge(types_byParcel,types_byParcel2,by="Land_Parcel_ID",all=T)
      
      types_byParcel$NEWCON[types_byParcel$NEWCON > 0 ]=1
      types_byParcel$DEMO = ifelse(types_byParcel$NEWCON == 1,0,ifelse(types_byParcel$DEMO>0,1,0))
      types_byParcel$ADD = ifelse(types_byParcel$NEWCON == 1 | types_byParcel$DEMO == 1,0,ifelse(types_byParcel$ADD>0,1,0))
      types_byParcel$RENO = ifelse(types_byParcel$NEWCON == 1 | types_byParcel$DEMO == 1 | types_byParcel$ADD == 1,0,ifelse(types_byParcel$RENO > 0,1,0))
      
      
      types_byParcel = types_byParcel[!is.na(types_byParcel$Land_Parcel_ID)& (!types_byParcel$Land_Parcel_ID==0),]
      
      # i pull the geographic information from the permits, although i could just merge it from the land parcels
      # it felt more appropriate to get it from the permits, even though it doesnt make a difference
      types_byParcel = merge(types_byParcel,permits[!duplicated(permits$Land_Parcel_ID),c("Land_Parcel_ID","BG_ID_10","CT_ID_10")],by="Land_Parcel_ID",all.x=T)
      
      # output this year's file out:
      dir.create(paste0(lpEcometricsPath,i), showWarnings = FALSE)
      write.csv(types_byParcel,paste0(lpEcometricsPath,i,"/",lpEcometricsName,".",i,".csv"),row.names=F)
      
      # rename the data frame so it persists
      assign(paste("types_byParcel",i,sep="_"), types_byParcel)
      
      # append years to colnames for wide longitudinal format:
      lps_agg <- types_byParcel
      names(lps_agg)[2:ncol(lps_agg)] = paste( names(lps_agg)[2:ncol(lps_agg)], i, sep=".")
      lps_agg_long = merge(lps_agg_long, lps_agg,by="Land_Parcel_ID",all=T)
      
      ecom_lps_shp = merge(lps_shp,types_byParcel,by.x="Ln_P_ID",by.y="Land_Parcel_ID",all.x=T)
      # output the shp
      # writeOGR(ecom_lps_shp, paste(c(lpEcometricsPath,i,"/"),collapse=""),paste(lpEcometricsName,i,sep="."),driver="ESRI Shapefile",overwrite_layer=TRUE)
      
      # aggregating to CBG level
      cbgs_agg = aggregate(cbind(DEMO, NEWCON, ADD, RENO,
                                 DEMO*DV, NEWCON*DV, ADD*DV, RENO*DV,DV_public)~BG_ID_10,data=types_byParcel,FUN=sum,na.rm=T)
      names(cbgs_agg)[2:ncol(cbgs_agg)] = c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                                            "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")
      # merge on the number of lps in the cbg
      cbgs_agg = merge(cbgs_agg, lps_perBG, by="BG_ID_10",all=T)
      # make 0s for any CT ids that were not in the aggregation
      cbgs_agg[is.na(cbgs_agg$DEMO_count),c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                                            "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")]=0
      
      # divide by number of LPs
      cbgs_agg[,c("DEMO_PP","NEWCON_PP","ADD_PP","RENO_PP",
                  "DEMO_DV_PP","NEWCON_DV_PP","ADD_DV_PP","RENO_DV_PP","Public_DV_PP")] = 
            cbgs_agg[,c("DEMO_count","NEWCON_count","ADD_count","RENO_count",
                        "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")] / cbgs_agg$numLandParcels
      
      
      # make the major dev and local investment vars
      cbgs_agg[,c("MAJORDEV_count","MAJORDEV_PP","MAJORDEV_DV_PP","LOCALINV_count","LOCALINV_PP","LOCALINV_DV_PP")] = 
            cbgs_agg[,c("NEWCON_count","NEWCON_PP","NEWCON_DV_PP","ADD_count","ADD_PP","ADD_DV_PP")] +
            cbgs_agg[,c("DEMO_count","DEMO_PP","DEMO_DV_PP","RENO_count","RENO_PP","RENO_DV_PP")] 
      
      cbgs_agg[,c("TOTAL_count","TOTAL_PP","TOTAL_DV_PP")] = 
            cbgs_agg[,c("MAJORDEV_count","MAJORDEV_PP","MAJORDEV_DV_PP")] +
            cbgs_agg[,c("LOCALINV_count","LOCALINV_PP","LOCALINV_DV_PP")] 
      
      # log the valuation variables
      #cbgs_agg[,c("NEWCON_DV_PP","DEMO_DV_PP","ADD_DV_PP","RENO_DV_PP","LOCALINV_DV_PP","MAJORDEV_DV_PP","TOTAL_DV_PP","Public_DV_PP")] = 
      #  log(cbgs_agg[,c("NEWCON_DV_PP","DEMO_DV_PP","ADD_DV_PP","RENO_DV_PP","LOCALINV_DV_PP","MAJORDEV_DV_PP","TOTAL_DV_PP","Public_DV_PP")]+1)
      
      # reorder the variables and save the CSV
      cbgs_agg = cbgs_agg[,c("BG_ID_10", "numLandParcels",  
                             "NEWCON_count", "NEWCON_PP","NEWCON_DV_PP", "DEMO_count", "DEMO_PP", "DEMO_DV_PP",
                             "ADD_count","ADD_PP","ADD_DV_PP","RENO_count","RENO_PP","RENO_DV_PP", 
                             "MAJORDEV_count","MAJORDEV_PP","MAJORDEV_DV_PP","LOCALINV_count","LOCALINV_PP","LOCALINV_DV_PP", 
                             "TOTAL_count","TOTAL_PP","TOTAL_DV_PP", 
                             "Public_DV_PP")]
      
      dir.create(paste0(cbgEcometricsPath,i), showWarnings = FALSE)
      write.csv(cbgs_agg,paste0(cbgEcometricsPath,i,"/",cbgEcometricsName,".",i,".csv"),row.names=F)
      
      # rename the data frame so it persists
      assign(paste("cbgs_agg",i,sep="_"), cbgs_agg)
      
      # change names for the longitudinal file
      names(cbgs_agg)[3:ncol(cbgs_agg)] = paste( names(cbgs_agg)[3:ncol(cbgs_agg)], i, sep=".")
      # merges on, dropping the numLandParcels column
      cbgs_agg_long = merge(cbgs_agg_long, cbgs_agg[,-2],by="BG_ID_10",all=T)
      
      # change names for the shp
      names(cbgs_agg) = c("BG_ID_10", "nLP","NC_c","NC_PP", "NC_DV", "D_c",  "D_PP" , "D_DV",
                          "A_c", "A_PP","A_DV","R_c", "R_PP","R_DV","MD_c","MD_PP","MD_DV",   "LI_c","LI_PP","LI_DV","T_c","T_PP","T_DV", 
                          "P_DV")
      ecom_bgs_shp = merge(bgs_shp,cbgs_agg,by="BG_ID_10",all.x=T)
      # save the shp
      writeOGR(ecom_bgs_shp, path.expand(paste0(cbgEcometricsPath,i,"/")),paste(cbgEcometricsName,".",i),driver="ESRI Shapefile",overwrite_layer=TRUE)
      
      # rename for long shapefile
      names(cbgs_agg)[3:ncol(cbgs_agg)] = paste(names(cbgs_agg)[3:ncol(cbgs_agg)],gsub(pattern = "20","",as.character(i)),sep=".")
      # merge for long shapefile
      cbgs_agg_long_shp = merge(cbgs_agg_long_shp,cbgs_agg[,-2],by="BG_ID_10",all.x=T)
      
      
      
      ### aggregating to CT level
      
      cts_agg = aggregate(cbind(DEMO, NEWCON, ADD, RENO, DEMO*DV, NEWCON*DV, ADD*DV, RENO*DV,DV_public)~CT_ID_10,data=types_byParcel,FUN=sum,na.rm=T)
      names(cts_agg)[2:ncol(cts_agg)] = c("DEMO_count","NEWCON_count","ADD_count","RENO_count", "DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")
      # merge on number of lps in the ct
      cts_agg = merge(cts_agg, lps_perCT,by="CT_ID_10",all=T)
      
      # make 0s for any CT ids that were not in the aggregation
      cts_agg[is.na(cts_agg$DEMO_count),c("DEMO_count","NEWCON_count","ADD_count","RENO_count","DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")]=0
      
      # divide by number of LPs
      cts_agg[,c("DEMO_PP","NEWCON_PP","ADD_PP","RENO_PP", "DEMO_DV_PP","NEWCON_DV_PP","ADD_DV_PP","RENO_DV_PP","Public_DV_PP")] = 
            cts_agg[,c("DEMO_count","NEWCON_count","ADD_count","RENO_count","DEMO_DV","NEWCON_DV","ADD_DV","RENO_DV","Public_DV")] / cts_agg$numLandParcels
      
      # make the major dev, local investment, and total vars
      cts_agg[,c("MAJORDEV_count","MAJORDEV_PP","MAJORDEV_DV_PP","LOCALINV_count","LOCALINV_PP","LOCALINV_DV_PP")] = 
            cts_agg[,c("NEWCON_count","NEWCON_PP","NEWCON_DV_PP","ADD_count","ADD_PP","ADD_DV_PP")] +
            cts_agg[,c("DEMO_count","DEMO_PP","DEMO_DV_PP","RENO_count","RENO_PP","RENO_DV_PP")] 
      
      cts_agg[,c("TOTAL_count","TOTAL_PP","TOTAL_DV_PP")] = 
            cts_agg[,c("MAJORDEV_count","MAJORDEV_PP","MAJORDEV_DV_PP")] +
            cts_agg[,c("LOCALINV_count","LOCALINV_PP","LOCALINV_DV_PP")] 
      
      # log the valuation variables
      #cts_agg[,c("NEWCON_DV_PP","DEMO_DV_PP","ADD_DV_PP","RENO_DV_PP","LOCALINV_DV_PP","MAJORDEV_DV_PP","TOTAL_DV_PP","Public_DV_PP")] = 
      #  log(cts_agg[,c("NEWCON_DV_PP","DEMO_DV_PP","ADD_DV_PP","RENO_DV_PP","LOCALINV_DV_PP","MAJORDEV_DV_PP","TOTAL_DV_PP","Public_DV_PP")]+1)
      
      cts_agg = cts_agg[,c("CT_ID_10", "numLandParcels",  
                           "NEWCON_count", "NEWCON_PP","NEWCON_DV_PP", "DEMO_count", "DEMO_PP", "DEMO_DV_PP",
                           "ADD_count","ADD_PP","ADD_DV_PP","RENO_count","RENO_PP","RENO_DV_PP", 
                           "MAJORDEV_count","MAJORDEV_PP","MAJORDEV_DV_PP","LOCALINV_count","LOCALINV_PP","LOCALINV_DV_PP", 
                           "TOTAL_count","TOTAL_PP","TOTAL_DV_PP", "Public_DV_PP")]
      
      dir.create(paste0(ctEcometricsPath,i), showWarnings = FALSE)
      write.csv(cts_agg,paste0(ctEcometricsPath,i,"/",ctEcometricsName,".",i,".csv"),row.names=F)
      assign(paste("cts_agg",i,sep="_"), cts_agg)
      
      # change names for the longitudinal file
      names(cts_agg)[3:ncol(cts_agg)] = paste( names(cts_agg)[3:ncol(cts_agg)], i, sep=".")
      # merges on, dropping the numLandParcels column
      cts_agg_long = merge(cts_agg_long, cts_agg[,-2],by="CT_ID_10",all=T)
      
      # rename for shape file
      names(cts_agg) = c("CT_ID_10",  "nLP","NC_c","NC_PP", "NC_DV", "D_c",  "D_PP" , "D_DV",
                         "A_c", "A_PP","A_DV","R_c", "R_PP","R_DV","MD_c","MD_PP","MD_DV",   "LI_c","LI_PP","LI_DV","T_c","T_PP","T_DV", 
                         "P_DV")
      ecom_cts_shp = merge(cts_shp, cts_agg, by = "CT_ID_10",all.x=T)
      writeOGR(ecom_cts_shp, path.expand(paste0(ctEcometricsPath,i,"/")),paste0(ctEcometricsName,".",i),driver="ESRI Shapefile",overwrite_layer=TRUE)
      
      # rename for long shapefile
      names(cts_agg)[3:ncol(cts_agg)] = paste(names(cts_agg)[3:ncol(cts_agg)],gsub(pattern = "20","",as.character(i)),sep=".")
      # merge for long shapefile
      cts_agg_long_shp = merge(cts_agg_long_shp,cts_agg[,-2],by="CT_ID_10",all.x=T)
}

# longitudinal Land Parcels:
dir.create(paste0(lpEcometricsPath,"Longitudinal"), showWarnings = FALSE)
write.csv(lps_agg_long,paste0(lpEcometricsPath,"Longitudinal/",lpEcometricsName,".Longitudinal.csv"),row.names=F)
# merge longitudinal tabular to shapefile:
lps_agg_long_shp = merge(lps_shp,lps_agg_long,by.x="Ln_P_ID",by.y="Land_Parcel_ID",all.x=T)
# writeOGR(lps_agg_long_shp, paste0(lpEcometricsPath,"Longitudinal","/",lpEcometricsName,".","Longitudinal"),driver="ESRI Shapefile",overwrite_layer=TRUE)

# write longitudinal CBG 
dir.create(paste0(cbgEcometricsPath,"Longitudinal"), showWarnings = FALSE)
write.csv(cbgs_agg_long,paste0(cbgEcometricsPath,"Longitudinal/",cbgEcometricsName,".Longitudinal.csv"),row.names=F)
writeOGR(cbgs_agg_long_shp, path.expand(paste0(ctEcometricsPath,"Longitudinal/")),paste0(ctEcometricsName,".Longitudinal"),driver="ESRI Shapefile",overwrite_layer=TRUE)

# write longitudinal CT 
dir.create(paste0(ctEcometricsPath,"Longitudinal"), showWarnings = FALSE)
write.csv(cts_agg_long,paste0(ctEcometricsPath,"Long/",ctEcometricsName,".Long.csv"),row.names=F)
writeOGR(cts_agg_long_shp, path.expand(paste0(ctEcometricsPath,"Long/")),paste0(ctEcometricsName,".Long"),driver="ESRI Shapefile",overwrite_layer=TRUE)

