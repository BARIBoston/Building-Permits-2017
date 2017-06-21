#---- SETUP ----
permits2017_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Building Permits 2017/Permits.Records.2017.csv"
Land_Parcels_path =      "Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.csv"
cbgsEcometricsPath = "/Users/henrygomory/Documents/Research/BARI/Building Permits/Building Permits 2016/Creating Ecometrics/Outputs/Permits.Ecometrics.CBG."
cbgsEcometricsShpPath = "/Users/henrygomory/Documents/Research/BARI/Building Permits/Building Permits 2016/Creating Ecometrics/Outputs/CBG_Shapefile/"
cbgsEcometricsShpName = "Permits.Ecometrics.CBG."

#read in the data
permits_b = read.csv(permits2017_path,stringsAsFactors=FALSE)
Land_Parcels_b = read.csv(Land_Parcels_path, stringsAsFactors=F)
ct_shp_b = getTractsShp()
bgs_shp_b = getBGsShp()

permits = permits_b
Land_Parcels = Land_Parcels_b
ct_shp = ct_shp_b
bgs_shp = bgs_shp_b


#---- PREP BEFORE AGGREGATION ----

#check out the data
sum(!is.na(permits$BG_ID_10))/length(permits$BG_ID_10)
sum(!is.na(permits$CT_ID_10))/length(permits$CT_ID_10)
sum(!is.na(permits$ISSUED_DATE))/length(permits$ISSUED_DATE)


#this doesn't depend on the year so i can do it outside the loop
#highlights the fact that we are using the same number of parcels for each year
lps_perBG = aggregate(Land_Parcel_ID ~ BG_ID_10, data=Land_Parcels,length)
names(lps_perBG)[2]="numLandParcels"

lps_perCT = aggregate(Land_Parcel_ID ~ CT_ID_10, data=Land_Parcels,length)
names(lps_perCT)[2]="numLandParcels"

#doing this just so i can resuse the code i already wrote using permit, but for yearly aggregations
permits_all = permits


#loops through for each year
for (i in c(2015:2015)){
  permits <- NULL
  types_byParcel <- NULL
  cbgs_byParcel <- NULL
  cbgs_types_byParcel <- NULL
  cbgs_agg <- NULL
  cts_byParcel <- NULL
  cts_types_byParcel <- NULL
  cts_agg <- NULL
  types_byAddress <- NULL
  permits = permits_all[year(permits_all$ISSUED_DATE)==i,]
  
  
  #aggregating
  permits$MAJORDEV = permits$demo + permits$newcon
  permits$LOCALINV = permits$addition + permits$reno
  types_byParcel  = aggregate( x = cbind(permits$demo, permits$newcon, permits$addition, permits$reno, permits$MAJORDEV,permits$LOCALINV), 
                               by = list(permits$Land_Parcel_ID), FUN=max, na.rm=T)
  names(types_byParcel) = c("Land_Parcel_ID","DEMO","NEWCON","ADD","RENO","MAJORDEV","LOCALINV")
  types_byParcel = types_byParcel[!is.na(types_byParcel$Land_Parcel_ID)& (!types_byParcel$Land_Parcel_ID==0),]

  # i pull the geographic information from the permits, although i could just merge it from the land parcels
  # it felt more appropriate to get it from the permits, even though it doesnt make a difference
  types_byParcel = merge(types_byParcel,permits[!duplicated(permits$Land_Parcel_ID),c("Land_Parcel_ID","BG_ID_10","CT_ID_10")],by="Land_Parcel_ID",all.x=T)

  
  #aggregating to CBG level
  cbgs_agg = aggregate(cbind(1,DEMO, NEWCON, ADD, RENO,MAJORDEV,LOCALINV)~BG_ID_10,data=types_byParcel,FUN=sum)
  cbgs_agg = rename(cbgs_agg,numLandParcels = V1 )
  names(cbgs_agg)[3:ncol(cbgs_agg)] = paste(names(cbgs_agg)[3:ncol(cbgs_agg)],"_count",sep="")
  
  cbgs_agg[, c("DEMO_count","NEWCON_count","ADD_count","RENO_count","MAJORDEV_count","LOCALINV_count")] = 
    
  c("DEMO","NEWCON","ADD","RENO","MAJORDEV","LOCALINV")
    cbgs_agg[, paste(type,"_PC",sep="")] = cbgs_agg[, paste(type,"_count",sep="")]/cbgs_agg$numLandParcels
  }
 
  cbgs_agg = cbgs_agg[,c("BG_ID_10", "numLandParcels", "DEMO_count",  "DEMO_PC" , "NEWCON_count",	  "NEWCON_PC", "MAJORDEV_count","MAJORDEV_PC", 
                         "ADD_count",	"ADD_PC","RENO_count", "RENO_PC", "LOCALINV_count","LOCALINV_PC")]

  
  write.csv(cbgs_agg,paste(paste(cbgEcometricsPath,i,sep=""),".csv",sep=""),row.names=F)
  assign(paste("cbgs_agg",i,sep="_"), cbgs_agg)
  

  names(cbgs_agg) = c("BG_ID_10", "NP","RU", "D_c",  "D_PC" , "NC_c",    "NC_PC", "MD_c","MD_PC", 
                                "A_c",	"A_PC","R_c", "R_PC", "LI_c","LI_PC",	"m_c",	"m_PU")
  ecom_bgs_shp = merge(bgs_shp,cbgs_agg,by="BG_ID_10",all.x=T)
  writeOGR(ecom_bgs_shp, paste(paste(cbgsEcometricsShpPath,i,sep=""),"/",sep=""),paste(cbgsEcometricsShpName,i,sep=""),driver="ESRI Shapefile",overwrite_layer=TRUE)
  
  
  
  
  #aggregating to CT level
  cts_agg = aggregate(cbind(DEMO, NEWCON, ADD, RENO)~CT_ID_10,data=types_byParcel,FUN=sum)
  names(cts_agg)[2:ncol(cts_agg)] = paste(names(cts_agg)[2:ncol(cts_agg)],"_count",sep="")
  cts_agg$MAJORDEV_count = cts_agg$DEMO_count + cts_agg$NEWCON_count
  cts_agg$LOCALINV_count =   cts_agg$ADD_count +   cts_agg$RENO_count
  cts_agg = merge(cts_agg, lps_perCT,by="CT_ID_10")
  
  for ( type in c("DEMO","NEWCON","ADD","RENO","MAJORDEV","LOCALINV")) {
    cts_agg[, paste(type,"_PC",sep="")] = cts_agg[, paste(type,"_count",sep="")]/cts_agg$numLandParcels
  }
  
  cts_agg = cts_agg[,c("CT_ID_10",   "numLandParcels", "DEMO_count",  "DEMO_PC" , "NEWCON_count",    "NEWCON_PC", "MAJORDEV_count","MAJORDEV_PC", 
                       "ADD_count",	"ADD_PC","RENO_count", "RENO_PC", "LOCALINV_count","LOCALINV_PC")]
  
  write.csv(cts_agg,paste(paste("/Users/henrygomory/Documents/Research/BARI/Building Permits/Building Permits 2016/Creating Ecometrics/Outputs/Permits.Ecometrics.CT.",i,sep=""),".csv",sep=""),row.names=F)
  assign(paste("cts_agg",i,sep="_"), cts_agg)
  
  names(cts_agg) = c("CT_ID_10", "NP","RU", "D_c",  "D_PC" , "NC_c",    "NC_PC", "MD_c","MD_PC", 
                      "A_c",  "A_PC","R_c", "R_PC", "LI_c","LI_PC",	"m_c",	"m_PU")
  ecom_cts_shp = merge(ct_shp, cts_agg, by = "CT_ID_10",all.x=T)
  writeOGR(ecom_cts_shp, paste(paste("/Users/henrygomory/Documents/Research/BARI/Building Permits/Building Permits 2016/Creating Ecometrics/Outputs/CT_Shapefile/",i,sep=""),"/",sep=""),paste("Permits.Ecometrics.CT.",i,sep=""),driver="ESRI Shapefile",overwrite_layer=TRUE)
 
 
  
}
  

elements = list(cbgs_agg_2010,cbgs_agg_2011,cbgs_agg_2012,cbgs_agg_2013,cbgs_agg_2014,cbgs_agg_2015,cbgs_agg_2016)

j = 2010
cbgs_long = merge(parcels_per_BG,RentalUnitsbyBG,by="BG_ID_10",all=T)
cbgs_long = cbgs_long[!is.na(cbgs_long$numParcels),]
vars = c("DEMO_count","DEMO_PC","NEWCON_count","NEWCON_PC","MAJORDEV_count","MAJORDEV_PC","ADD_count" ,
         "ADD_PC","RENO_count","RENO_PC","LOCALINV_count","LOCALINV_PC","moves_count","moves_per_unit")
for (i in elements) {
  n = i[,c("BG_ID_10",vars)]
  names(n)[2:(length(vars)+1)]<-paste(names(n)[2:(length(vars)+1)],j,sep="_")
  cbgs_long = merge(cbgs_long,n,by="BG_ID_10",all=TRUE)
  
  j = j + 1
}
  
write.csv(cbgs_long,"/Users/henrygomory/Documents/Research/BARI/Building Permits/Building Permits 2016/Creating Ecometrics/Outputs/Permits.Ecometrics.CBG.Long.csv",row.names=F)
  

elements = list(cts_agg_2010,cts_agg_2011,cts_agg_2012,cts_agg_2013,cts_agg_2014,cts_agg_2015,cts_agg_2016)

j = 2010
cts_long = merge(parcels_per_CT,RentalUnitsbyTract,by="CT_ID_10",all=T)
cts_long = cts_long[!is.na(cts_long$numParcels),]
vars = c("DEMO_count","DEMO_PC","NEWCON_count","NEWCON_PC","MAJORDEV_count","MAJORDEV_PC","ADD_count" ,
         "ADD_PC","RENO_count","RENO_PC","LOCALINV_count","LOCALINV_PC","moves_count","moves_per_unit")
for (i in elements) {
  n = i[,c("CT_ID_10",vars)]
  names(n)[2:(length(vars)+1)]<-paste(names(n)[2:(length(vars)+1)],j,sep="_")
  cts_long = merge(cts_long,n,by="CT_ID_10",all=TRUE)
  
  j = j + 1
}

write.csv(cts_long,"/Users/henrygomory/Documents/Research/BARI/Building Permits/Building Permits 2016/Creating Ecometrics/Outputs/Permits.Ecometrics.CT.Long.csv",row.names=F)

short_names = c( "D_c",  "D_PC" , "NC_c",    "NC_PC", "MD_c","MD_PC", 
               "A_c",  "A_PC","R_c", "R_PC", "LI_c","LI_PC",	"m_c",	"m_PU")
short_names_long = c("NP","RU",
                    paste(short_names,"2010",sep="_"),
                    paste(short_names,"2011",sep="_"),
                    paste(short_names,"2012",sep="_"),
                    paste(short_names,"2013",sep="_"),
                    paste(short_names,"2014",sep="_"),
                    paste(short_names,"2015",sep="_"),
                    paste(short_names,"2016",sep="_"))

cbgs_long_shortNames = cbgs_long
names(cbgs_long_shortNames)[2:length(names(cbgs_long_shortNames))] = short_names_long
ecom_bgs_shp = merge(bgs_shp,cbgs_long_shortNames,by="BG_ID_10",all.x=T)
writeOGR(ecom_bgs_shp, "/Users/henrygomory/Documents/Research/BARI/Building Permits/Building Permits 2016/Creating Ecometrics/Outputs/CBG_Shapefile/Long/","Permits.Ecometrics.CBG.Long",driver="ESRI Shapefile",overwrite_layer=TRUE)

cts_long_shortNames = cts_long
names(cts_long_shortNames)[2:length(names(cts_long_shortNames))] = short_names_long
ecom_cts_shp = merge(ct_shp, cts_long_shortNames, by = "CT_ID_10",all.x=T)
writeOGR(ecom_cts_shp, "/Users/henrygomory/Documents/Research/BARI/Building Permits/Building Permits 2016/Creating Ecometrics/Outputs/CT_Shapefile/Long/","Permits.Ecometrics.CT.Long",driver="ESRI Shapefile",overwrite_layer=TRUE)






