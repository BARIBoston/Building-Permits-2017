#---- packages
library(rgdal)

#---- INPUT PATHS ----
permits2017_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Building Permits 2017/Permits.Records.2017.csv"
landParcels_path =      "Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.csv"

blockGroupsShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Block Groups 2015/"
blockGroupsShpName = "Census Block Groups"
tractsShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Tracts/"
tractsShpName = "Tracts_Boston_2010_BARI"

#---- OUTPUT PATHS ----
cbgEcometricsPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Building Permits 2017/CBG/"
cbgEcometricsName = "Permits.Ecometrics.CBG"

ctEcometricsPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Building Permits 2017/CT/"
ctEcometricsName = "Permits.Ecometrics.CT"

#---- READ IN DATA 
permits = read.csv(permits2017_path,stringsAsFactors=FALSE)
landParcels = read.csv(landParcels_path, stringsAsFactors=F)
cts_shp = readOGR(tractsShpPath,tractsShpName, stringsAsFactors=F)
bgs_shp = readOGR(blockGroupsShpPath,blockGroupsShpName, stringsAsFactors=F)




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

cbgs_agg_long = lps_perBG
cts_agg_long = lps_perCT

cbgs_agg_long_shp = merge(bgs_shp,lps_perBG,by="BG_ID_10",all.x=T)
cts_agg_long_shp = merge(cts_shp,lps_perCT,by="CT_ID_10",all.x=T)

#loops through for each year
for (i in c(2010:2017)){
  print(i)
  
  permits_sub = permits[year(permits$ISSUED_DATE)==i,]
  
  
  #aggregating
 
  types_byParcel  = aggregate( x = cbind(permits_sub$demo, permits_sub$newcon, permits_sub$addition, permits_sub$reno), 
                               by = list(permits_sub$Land_Parcel_ID), FUN=max, na.rm=T)
  names(types_byParcel) = c("Land_Parcel_ID","DEMO","NEWCON","ADD","RENO")
  types_byParcel$DEMO[types_byParcel$NEWCON == 1 ]=0
  types_byParcel$ADD[ types_byParcel$NEWCON == 1 | types_byParcel$DEMO == 1 ]=0
  types_byParcel$RENO[types_byParcel$NEWCON == 1 | types_byParcel$DEMO == 1 | types_byParcel$ADD == 1 ]=0
  
  types_byParcel$MAJORDEV = types_byParcel$NEWCON + types_byParcel$DEMO
  types_byParcel$LOCALINV = types_byParcel$ADD + types_byParcel$RENO
  
  types_byParcel = types_byParcel[!is.na(types_byParcel$Land_Parcel_ID)& (!types_byParcel$Land_Parcel_ID==0),]

  # i pull the geographic information from the permits, although i could just merge it from the land parcels
  # it felt more appropriate to get it from the permits, even though it doesnt make a difference
  types_byParcel = merge(types_byParcel,permits[!duplicated(permits$Land_Parcel_ID),c("Land_Parcel_ID","BG_ID_10","CT_ID_10")],by="Land_Parcel_ID",all.x=T)
  
  # aggregating to CBG level
  cbgs_agg = aggregate(cbind(DEMO, NEWCON, ADD, RENO,MAJORDEV,LOCALINV)~BG_ID_10,data=types_byParcel,FUN=sum)
  names(cbgs_agg)[2:ncol(cbgs_agg)] = paste(names(cbgs_agg)[2:ncol(cbgs_agg)],"_count",sep="")
  # merge on the number of lps in the cbg
  cbgs_agg = merge(cbgs_agg, lps_perBG, by="BG_ID_10",all=T)
  # make 0s for any CT ids that were not in the aggregation
  cbgs_agg[is.na(cbgs_agg$DEMO_count),c("DEMO_count","NEWCON_count","ADD_count","RENO_count","MAJORDEV_count","LOCALINV_count")]=0
  
  for (type in c("DEMO","NEWCON","ADD","RENO","MAJORDEV","LOCALINV")) {
    cbgs_agg[, paste(type,"_PC",sep="")] = cbgs_agg[, paste(type,"_count",sep="")]/cbgs_agg$numLandParcels
  }
 
  # reorder the variables and save the CSV
  cbgs_agg = cbgs_agg[,c("BG_ID_10", "numLandParcels", "DEMO_count",  "DEMO_PC" , "NEWCON_count",	  "NEWCON_PC", "MAJORDEV_count","MAJORDEV_PC", 
                         "ADD_count",	"ADD_PC","RENO_count", "RENO_PC", "LOCALINV_count","LOCALINV_PC")]

  write.csv(cbgs_agg,paste(c(cbgEcometricsPath,i,"/",cbgEcometricsName,".",i,".csv"),collapse=""),row.names=F)
  
  # rename the data frame so it persists
  assign(paste("cbgs_agg",i,sep="_"), cbgs_agg)
  
  # change names for the longitudinal file
  names(cbgs_agg)[3:ncol(cbgs_agg)] = paste( names(cbgs_agg)[3:ncol(cbgs_agg)], i, sep=".")
  # merges on, dropping the numLandParcels column
  cbgs_agg_long = merge(cbgs_agg_long, cbgs_agg[,-2],by="BG_ID_10",all=T)
  
  # change names for the shp
  names(cbgs_agg) = c("BG_ID_10", "nLP", "D_c",  "D_PC" , "NC_c",    "NC_PC", "MD_c","MD_PC",  "A_c",	"A_PC","R_c", "R_PC", "LI_c","LI_PC")
  ecom_bgs_shp = merge(bgs_shp,cbgs_agg,by="BG_ID_10",all.x=T)
  # save the shp
  writeOGR(ecom_bgs_shp, paste(c(cbgEcometricsPath,i,"/"),collapse=""),paste(cbgEcometricsName,i,sep="."),driver="ESRI Shapefile",overwrite_layer=TRUE)
  
  # rename for long shapefile
  names(cbgs_agg)[3:ncol(cbgs_agg)] = paste(names(cbgs_agg)[3:ncol(cbgs_agg)],gsub(pattern = "20","",as.character(i)),sep=".")
  # merge for long shapefile
  cbgs_agg_long_shp = merge(cbgs_agg_long_shp,cbgs_agg[,-2],by="BG_ID_10",all.x=T)
  
  
  
  ### aggregating to CT level
  cts_agg = aggregate(cbind(DEMO, NEWCON, ADD, RENO,MAJORDEV,LOCALINV)~CT_ID_10,data=types_byParcel,FUN=sum)
  names(cts_agg)[2:ncol(cts_agg)] = paste(names(cts_agg)[2:ncol(cts_agg)],"_count",sep="")
  
  # merge on number of lps in the ct
  cts_agg = merge(cts_agg, lps_perCT,by="CT_ID_10",all=T)
  
  cts_agg[is.na(cts_agg$DEMO_count),c("DEMO_count","NEWCON_count","ADD_count","RENO_count","MAJORDEV_count","LOCALINV_count")]=0
  
  # make per lp
  for ( type in c("DEMO","NEWCON","ADD","RENO","MAJORDEV","LOCALINV")) {
    cts_agg[, paste(type,"_PC",sep="")] = cts_agg[, paste(type,"_count",sep="")]/cts_agg$numLandParcels
  }
  
  cts_agg = cts_agg[,c("CT_ID_10",   "numLandParcels", "DEMO_count",  "DEMO_PC" , "NEWCON_count",    "NEWCON_PC", "MAJORDEV_count","MAJORDEV_PC", 
                       "ADD_count",	"ADD_PC","RENO_count", "RENO_PC", "LOCALINV_count","LOCALINV_PC")]
  
  write.csv(cts_agg,paste(c(ctEcometricsPath,i,"/",ctEcometricsName,".",i,".csv"),collapse=""),row.names=F)
  assign(paste("cts_agg",i,sep="_"), cts_agg)
 
  # change names for the longitudinal file
  names(cts_agg)[3:ncol(cts_agg)] = paste( names(cts_agg)[3:ncol(cts_agg)], i, sep=".")
  # merges on, dropping the numLandParcels column
  cts_agg_long = merge(cts_agg_long, cts_agg[,-2],by="CT_ID_10",all=T)
  
  # rename for shape file
  names(cts_agg) = c("CT_ID_10", "nLP", "D_c",  "D_PC" , "NC_c",    "NC_PC", "MD_c","MD_PC",  "A_c",  "A_PC","R_c", "R_PC", "LI_c","LI_PC")
  ecom_cts_shp = merge(cts_shp, cts_agg, by = "CT_ID_10",all.x=T)
  writeOGR(ecom_cts_shp, paste(c(ctEcometricsPath,i,"/"),collapse=""),paste(ctEcometricsName,i,sep="."),driver="ESRI Shapefile",overwrite_layer=TRUE)
 
  # rename for long shapefile
  names(cts_agg)[3:ncol(cts_agg)] = paste(names(cts_agg)[3:ncol(cts_agg)],gsub(pattern = "20","",as.character(i)),sep=".")
  # merge for long shapefile
  cts_agg_long_shp = merge(cts_agg_long_shp,cts_agg[,-2],by="CT_ID_10",all.x=T)
}
  

# write longitudinal CBG 
write.csv(cbgs_agg_long,paste(c(cbgEcometricsPath,"Long","/",ctEcometricsName,".Long.csv"),collapse=""),row.names=F)
writeOGR(cbgs_agg_long_shp, paste(c(ctEcometricsPath,"Long","/"),collapse=""),paste(ctEcometricsName,"Long",sep="."),driver="ESRI Shapefile",overwrite_layer=TRUE)

# write longitudinal CT 
write.csv(cts_agg_long,paste(c(ctEcometricsPath,"Long/",ctEcometricsName,".Long.csv"),collapse=""),row.names=F)
writeOGR(cts_agg_long_shp, paste(c(ctEcometricsPath,"Long/"),collapse=""),paste(ctEcometricsName,"Long",sep="."),driver="ESRI Shapefile",overwrite_layer=TRUE)


