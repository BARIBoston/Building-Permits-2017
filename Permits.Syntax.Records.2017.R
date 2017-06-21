#---- SETUP ----
permits_path = "/Users/henrygomory/Downloads/buildingpermits.csv"
ID_walkover_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/IDConnector.2017.csv"
properties_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/Properties.2017.csv"
landParcels_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.csv"
permits2017_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Building Permits 2017/Permits.Records.2017.csv"

permits_b = read.csv(permits_path, stringsAsFactors=F)
IDconnector_b = read.csv(ID_walkover_path, stringsAsFactors=F)
properties_b = read.csv(properties_path, stringsAsFactors=F)
landParcels_b = read.csv(landParcels_path,stringsAsFactors=F)

permits = permits_b
IDconnector = IDconnector_b
properties = properties_b
landParcels = landParcels_b
#geographic data is merged in from parcels files, using the Property_ID value in each file
#then extra geographic data is brought in from geocoding


#---- ADDING GEOGRAPHICAL DATA ----
permits = standardizeGeoNames(permits)

#connecting ID connector to geo data first, because connections based on parcel_num are better than those based on Land Parcel ID, this ensures that however we connect to the 
# ID connector, we prefer these connections to the Geo - this is probably a very small thing in practice, but it was bugging me how to implement it
IDconnector.geo = merge(IDconnector,properties[,c("parcel_num","X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")],by="parcel_num",all.x=T)
IDconnector.geo = merge(IDconnector.geo,landParcels[,c("Land_Parcel_ID","X","Y","TLID_1","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")],by="Land_Parcel_ID",all.x=T)
for (var in c("X","Y","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")) {
  IDconnector.geo[,var] = ifelse(!is.na(IDconnector.geo[,paste(var,".x",sep="")]),IDconnector.geo[,paste(var,".x",sep="")],IDconnector.geo[,paste(var,".y",sep="")])
  IDconnector.geo[,paste(var,".x",sep="")] = NULL
  IDconnector.geo[,paste(var,".y",sep="")] = NULL
}
IDconnector.geo$TLID = ifelse(!is.na(IDconnector.geo$TLID),IDconnector.geo$TLID, IDconnector.geo$TLID_1 )
IDconnector.geo$TLID_1  = NULL



permits$Property_ID[ !is.na(permits$Property_ID) & permits$Property_ID == 0] = NA
sum(is.na(permits$parcel_num) & is.na(permits$Property_ID))

permits = merge(permits, IDconnector.geo[ !duplicated(IDconnector.geo$Property_ID) & !is.na(IDconnector.geo$Property_ID),],by = "Property_ID",all.x=T)
permits = merge(permits, IDconnector.geo[ !duplicated(IDconnector.geo$parcel_num) & !is.na(IDconnector.geo$parcel_num),],by.x="parcel_num.x",by.y="parcel_num",all.x=T)
for (var in c("Property_ID","parcel_num","GIS_ID","Land_Parcel_ID","X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")) {
  permits[,var] = ifelse(!is.na(permits[,paste(var,".x",sep="")]),permits[,paste(var,".x",sep="")],permits[,paste(var,".y",sep="")])
  permits[,paste(var,".x",sep="")] = NULL
  permits[,paste(var,".y",sep="")] = NULL
}

# number that are missing geographic data
sum(is.na(permits$X))
sum(is.na(permits$BG_ID_10))
sum(is.na(permits$CT_ID_10))

#number that were not in the ID connector
sum((!permits_b$Property_ID %in% IDconnector$Property_ID[ !is.na(IDconnector$Property_ID)]) & !permits_b$Parcel_ID %in% IDconnector$parcel_num)
#View(permits[ is.na(permits$X),c("parcel_num","Property_ID")])


## GEOCODE THE REST BASED ON ADDRESS
permits.geocode = permits[ is.na(permits$X),]

#
temp = clean_address(permits.geocode$ADDRESS)
permits.geocode$num1 = temp[,2]
permits.geocode$num2 = temp[,3]
permits.geocode$street_c = temp[,4]
permits.geocode$suffix_c = temp[,5]
rm(temp)
permits.geocode$city_c=clean_city(permits.geocode$CITY)
permits.geocode$zip_c= clean_zip(permits.geocode$ZIP)
temp = str_match(permits.geocode$Location,"\\(([0-9-.]*), ([0-9-.]*)\\)")
permits.geocode$X = as.numeric(temp[,3])
permits.geocode$Y = as.numeric(temp[,2])
rm(temp)
permits.geocode.shp = permits.geocode[ !is.na(permits.geocode$X),]
coordinates(permits.geocode.shp) = ~X+Y 
proj4string(permits.geocode.shp) = latLongProj

# geocode against land parcels, without geographic data
geocoded = data.frame(
  geocode(toGeocode = permits.geocode[ !duplicated(permits.geocode$PermitNumber),],tgID = "PermitNumber",refName = "landParcels",smallestGeo = "Land_Parcel_ID",geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"))[1],
  stringsAsFactors=F)
geocoded = geocoded[apply(geocoded[,c("Land_Parcel_ID","X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")],1,function(x)any(!is.na(x))),]

#geocode against land parcels, with geographic data
geocoded.geo = data.frame(
  geocode(toGeocode = permits.geocode.shp[ !duplicated(permits.geocode.shp@data$PermitNumber),],tgID = "PermitNumber",refName = "landParcels",smallestGeo = "Land_Parcel_ID",geographies = c("X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"),xy=T)[1],
  stringsAsFactors=F)
#combine them
#geocoded.geo = merge(geocoded.geo[,c("PermitNumber","Land_Parcel_ID")], landParcels[,c("Land_Parcel_ID","X","Y","TLID_1","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")],by="Land_Parcel_ID",all.x=T)
#geocoded.geo = rename(geocoded.geo, TLID = TLID_1)
geocoded = merge(geocoded[,c("PermitNumber","Land_Parcel_ID","X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")], 
                 geocoded.geo[,c("PermitNumber","Land_Parcel_ID","X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")], by="PermitNumber",all=T)
for (var in c("Land_Parcel_ID","X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")) {
  geocoded[,var] = ifelse(!is.na(geocoded[,paste(var,".x",sep="")]),geocoded[,paste(var,".x",sep="")],geocoded[,paste(var,".y",sep="")])
  geocoded[,paste(var,".x",sep="")] = NULL
  geocoded[,paste(var,".y",sep="")] = NULL
}

# geocode against streets, without geographic data
geocoded2 = data.frame(
  geocode(toGeocode = permits.geocode[ !duplicated(permits.geocode$PermitNumber),],tgID = "PermitNumber",refName = "Roads",smallestGeo = "TLID",geographies = c("BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"))[1],
  stringsAsFactors=F)
geocoded2 = geocoded2[apply(geocoded2[,c(c("TLID","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"))],1,function(x)any(!is.na(x))),]

# geocode against streets, with geographic data
geocoded.geo2 = data.frame(
  geocode(toGeocode = permits.geocode.shp[ !duplicated(permits.geocode.shp@data$PermitNumber),],tgID = "PermitNumber",refName = "Roads",smallestGeo = "TLID",geographies = c("BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD"),xy=T)[1],
  stringsAsFactors=F)


geocoded2 = merge(geocoded2[,c("PermitNumber","TLID","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")], geocoded.geo2[,c("PermitNumber","TLID","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")], by="PermitNumber",all=T)
for (var in c("TLID","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")) {
  geocoded2[,var] = ifelse(!is.na(geocoded2[,paste(var,".x",sep="")]),geocoded2[,paste(var,".x",sep="")],geocoded2[,paste(var,".y",sep="")])
  geocoded2[,paste(var,".x",sep="")] = NULL
  geocoded2[,paste(var,".y",sep="")] = NULL
}



permits = merge(permits,geocoded,by="PermitNumber",all.x=T)
for (var in c("Land_Parcel_ID","X","Y","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")) {
  permits[,var] = ifelse(!is.na(permits[,paste(var,".x",sep="")]),permits[,paste(var,".x",sep="")],permits[,paste(var,".y",sep="")])
  permits[,paste(var,".x",sep="")] = NULL
  permits[,paste(var,".y",sep="")] = NULL
}

permits = merge(permits,geocoded2,by="PermitNumber",all.x=T)
for (var in c("TLID","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD")) {
  permits[,var] = ifelse(!is.na(permits[,paste(var,".x",sep="")]),permits[,paste(var,".x",sep="")],permits[,paste(var,".y",sep="")])
  permits[,paste(var,".x",sep="")] = NULL
  permits[,paste(var,".y",sep="")] = NULL
}


sum(is.na(permits$X))
sum(!is.na(permits$X))/nrow(permits)
sum(is.na(permits$BG_ID_10))
sum(!is.na(permits$BG_ID_10))/nrow(permits)
sum(is.na(permits$CT_ID_10))
sum(!is.na(permits$CT_ID_10))/nrow(permits)


#-------------------------------------------#
#       Making new vars                     #
#-------------------------------------------#
#This syntax comes from 2014 students

############### categorizing permit types ###########################

#this is obviously not the most efficient way to do these categorizations, 
#but it is split up and then recombined like this to better see the sources ofthe categorizations
#1: my categorization based on article
#2: from students' code
permits.1 = permits
permits.1$PermitTypeDescr = trim(str_replace_all(permits.1$PermitTypeDescr,"\xe4\xf3\xf1",""))

DEMO1 = permits.1$DESCRIPTION=="Demolition - Exterior" | permits.1$DESCRIPTION=="Demolition - Interior" 
DEMO2 = (permits.1$PermitTypeDescr== "BFD Construction  Demo  Reno" )

NEWCON1 = 
  permits.1$DESCRIPTION=="Excavation Borings Test Pits" | 
  permits.1$DESCRIPTION == "New construction" | 
  permits.1$PermitTypeDescr=="Erect/New Construction" |
  permits.1$PermitTypeDescr == "Driveway Excavation Permit" | 
  permits.1$PermitTypeDescr == "Emergency Excavation Permit" | 
  permits.1$PermitTypeDescr == "Excavation Permit" | 
  permits.1$PermitTypeDescr == "Foundation Permit"

NEWCON2<-           (  permits.1$PermitTypeDescr== "Erect/New Construction" | 
                         permits.1$PermitTypeDescr== "Emergency Excavation Permit" | 
                         permits.1$PermitTypeDescr== "Excavation Permit" | 
                         permits.1$PermitTypeDescr== "Foundation Permit" |
                         permits.1$PermitTypeDescr== "Emergency Excavation Permit" )


ADD1 = 
  permits.1$DESCRIPTION == "Addition" | 
  permits.1$PermitTypeDescr == "Amendment to a Long Form" | 
  permits.1$PermitTypeDescr == "Board of Appeals" | 
  permits.1$PermitTypeDescr == "Long Form/Alteration Permit"
ADD2 =                (  
  permits.1$PermitTypeDescr== "Long Form/Alteration Permit" | 
    permits.1$PermitTypeDescr== "Amendment to a Long Form" | 
    permits.1$PermitTypeDescr== "BFD Intent To Comply" |
    permits.1$PermitTypeDescr== "Board of Appeals" )

RENO1 = (
  permits.1$DESCRIPTION == "Electrical" | 
    permits.1$DESCRIPTION == "Gas" | 
    permits.1$DESCRIPTION == "Plumbing" | 
    permits.1$PermitTypeDescr == "BFD  Asbestos Removal" | 
    permits.1$PermitTypeDescr == "BFD  Cutting-Burning-Welding" | 
    permits.1$PermitTypeDescr == "Electrical Permit" | 
    permits.1$PermitTypeDescr == "Gas Permit" | 
    permits.1$PermitTypeDescr == "Plumbing Permit" | 
    permits.1$PermitTypeDescr == "Short Form Bldg Permit" | 
    permits.1$PermitTypeDescr == "Use of Premises") 

RENO2 =           (  permits.1$PermitTypeDescr== "BFD  Asbestos Removal" | 
                       permits.1$PermitTypeDescr== "BFD  Cutting-Burning-Welding" | 
                       permits.1$PermitTypeDescr== "Plumbing Permit" | 
                       permits.1$PermitTypeDescr== "Gas Permit" | 
                       permits.1$PermitTypeDescr== "Electrical Fire Alarms" | 
                       permits.1$PermitTypeDescr== "Electrical Low Voltage" | 
                       permits.1$PermitTypeDescr== "Electrical Permit" | 
                       permits.1$PermitTypeDescr== "Electrical Temporary Service" | 
                       permits.1$PermitTypeDescr== "Fire Protection Permit" | 
                       permits.1$PermitTypeDescr== "Short Form Bldg Permit" | 
                       permits.1$PermitTypeDescr== "Temporary Construction Drivewa" | 
                       permits.1$PermitTypeDescr== "BFD  Bag Smoke Detectors" | 
                       permits.1$PermitTypeDescr== "BFD  Temporary Dumpster" | 
                       permits.1$PermitTypeDescr== "BFD Chemist Certificate proces" | 
                       permits.1$PermitTypeDescr== "BBFD Application Tank Removal" | 
                       permits.1$PermitTypeDescr== "BFD Approval of Tank Truck" | 
                       permits.1$PermitTypeDescr== "BFD Install or Modify Undergrd" | 
                       permits.1$PermitTypeDescr== "BFDAlteration of Fuel Oil Burn" | 
                       permits.1$PermitTypeDescr== "Sidewalk Deposit" )

#combining the two categorizations
permits.1$demo<-ifelse(DEMO1 | DEMO2, 1,0)
permits.1$newcon<-ifelse(NEWCON1 | NEWCON2, 1,0)
permits.1$addition<-ifelse(ADD1 | ADD2, 1,0)
permits.1$reno<-ifelse((RENO1 | RENO2) & (!DEMO1 & !DEMO2 & !NEWCON1 & !NEWCON2 & !ADD1 & !ADD2), 1,0)

##“Moving” Variable
#permits.1$moving<-ifelse(permits.1$PermitTypeDescr== "Street Occupancy Permit" & permits.1$WORKTYPE== "Movetrucks" & 
#                           !permits.1$demo & !permits.1$newcon & !permits.1$addition & !permits.1$reno & 
#                           !is.na(permits.1$PermitTypeDescr) & !is.na(permits.1$WORKTYPE), 1,0)

##“Special Events and Change to Premises” Variable
permits.1$specialevents<-ifelse(permits.1$PermitTypeDescr== "BFD Special Effects/Fireworks" | permits.1$PermitTypeDescr== "BFD General Permit" | permits.1$PermitTypeDescr== "Public Event" | permits.1$PermitTypeDescr== "BFDTemporary Place of Assembly" | permits.1$PermitTypeDescr== "BFD Tent with Assembly" | permits.1$PermitTypeDescr== "Use of Premises" | permits.1$PermitTypeDescr== "BFD Abandon Undergrd Storage" | permits.1$PermitTypeDescr== "BFD Alter Stationary Portable" |  permits.1$PermitTypeDescr== "BFD Blasting Permit" |  permits.1$PermitTypeDescr== "BFD Maintain Undergrd Storage" | permits.1$PermitTypeDescr== "BFD Temporary Out of Services" | permits.1$PermitTypeDescr== " BFD Use of Candles in a Place" | permits.1$PermitTypeDescr== "Certificate of Compliance", 1,0)

#owner var
permits.1$OWNER<-toupper(permits.1$OWNER)

##“Upper Education” Variable
permits.1$uppereducation<-ifelse(permits.1$OWNER== "ART INSTITUTE O" | permits.1$OWNER== "THE NORTHEASTER" | permits.1$OWNER== "ART INSTITUTE OF BOSTON" | permits.1$OWNER== "BARNES & NOBEL-BOSTON UNIVERSITY" | permits.1$OWNER== "BOSTON ARCHITEC" | permits.1$OWNER== "BOSTON ARCHITECT COLLEGE" | permits.1$OWNER== "BOSTON COLLEGE " | permits.1$OWNER== "BOSTON COLLEGE PURCHASING DEPT" | permits.1$OWNER== "BOSTON UNIVERSI" | permits.1$OWNER== "BOSTON UNIVERSITY TRSTS" | permits.1$OWNER== "BOSTON UNIVERSITY TRSTS OF" | permits.1$OWNER== "BOSTON UNIVRSTY TRSTS OF" | permits.1$OWNER== "BOSTON UNV PHYSICAL PLANT" | permits.1$OWNER== "EMMANUEL COLLEG" | permits.1$OWNER== "HARVARD CLUB OF" | permits.1$OWNER== "HARVARD COLL PR" | permits.1$OWNER== "HARVARD COLLEGE" | permits.1$OWNER== "HARVARD COLLEGE PRES/FELLOWS" | permits.1$OWNER== "HARVARD UNIV/FAC MAINT OPERATI" | permits.1$OWNER== "NORTHEASTERN CONFERENCE CORP" | permits.1$OWNER== "NORTHEASTERN UNIV HILLEL" | permits.1$OWNER== "NORTHEASTERN UNIV MASS" | permits.1$OWNER== "NORTHEASTERN UNIVERSITY" | permits.1$OWNER== "SUFFOLK UNIVERS" | permits.1$OWNER== "WENTWORTH INSTI" | permits.1$OWNER== "WENTWORTH INSTITUTE" | permits.1$OWNER== "WENTWORTH INSTITUTE OF" | permits.1$OWNER== "WENTWORTH INSTITUTE OF TECH" | permits.1$OWNER== "WENTWOWORTH INS" | permits.1$OWNER== "WHEELOCK COLLEG" | permits.1$OWNER== "WHEELOCK COLLEGE" | permits.1$OWNER== "MODERN SCHOOL OF FASHION" | permits.1$OWNER== "N.E. COLLEGE OF OPTOMETRY" | permits.1$OWNER== "NEW ENG CONSERV" | permits.1$OWNER== "New England College of Optometry" | permits.1$OWNER== "SIMMONS COLLEGE" | permits.1$OWNER== "HARVARD RE SERV ATTN ACCT PAY" | permits.1$OWNER== "HARVARD REAL ES" | permits.1$OWNER== "HARVARD REAL ESTATE", 1,0)

##“Healthcare” Variable
permits.1$healthcare<-ifelse(permits.1$OWNER== "CHILDREN'S HOSP" | permits.1$OWNER== "N E BAPTIST HOS" | permits.1$OWNER== "B'NAI B'RITH SR CITIZENS" | permits.1$OWNER== "CHILDRENS HOSPI" | permits.1$OWNER== "CHILDRENS HOSPITAL" | permits.1$OWNER== "ASIAN HEALTH CARE FOUNDATION" | permits.1$OWNER== "BETH ISRAEL DEA" | permits.1$OWNER== "BETH ISRAEL HOSPITAL" | permits.1$OWNER== "BRIGHAM  MEDICA" | permits.1$OWNER== "BRIGHAM & WOMEN'S HOSPITAL" | permits.1$OWNER== "BRIGHAM CIRCLE" | permits.1$OWNER== "BRIGHAM CIRCLE REALTY" | permits.1$OWNER== "BRIGHAM MEDICAL" | permits.1$OWNER== "EAST BOSTON HEALTH CENTER" | permits.1$OWNER== "PARTNERS HEALTHCARE SYSTEM MGH" | permits.1$OWNER== "UNIV HOSPITAL I" | permits.1$OWNER== "UNIVERSITY HOSP" | permits.1$OWNER== "UNIVERSITY OF MASS MEDICAL CTR" | permits.1$OWNER== "BOSTONIAN NURSING CARE" | permits.1$OWNER== "DEACONESS HOSPI" | permits.1$OWNER== "NEW ENGLAND DEA" | permits.1$OWNER== "NEW ENGLAND MED CTR HOSP" | permits.1$OWNER== "RIVERSIDE NURSING HOME" | permits.1$OWNER== "SPAULDING REHAB" | permits.1$OWNER== "ST ELIZABETH'S " | permits.1$OWNER== "ST ELIZABETHS HOSP OF BOS" | permits.1$OWNER== "NEWBURY DENTAL ASSC.REALTY TR" | permits.1$OWNER== "RUGGLES ASSISTED LIVING LP" | permits.1$OWNER== "MEDICAL AREA TO" | permits.1$OWNER== "SO. COVE NURSING INC" | permits.1$OWNER== "BROOKS PHARMACY", 1,0)

##“Religious Institutions” Variable
permits.1$religious<-ifelse(permits.1$OWNER== " BETHEL BAPTIST " | permits.1$OWNER== " BETHEL TABERNAC" | permits.1$OWNER== "BETHEL TABERNACLE" | permits.1$OWNER== "BETHESDA HAITIAN BAPTIST CHURCH" | permits.1$OWNER== "BI DEACONESS ME" | permits.1$OWNER== "BOSTON CHINESE EVANGELICAL" | permits.1$OWNER== "CH OF THE HOLY" | permits.1$OWNER== "CHRIST APOSTOLIC MT JOY" | permits.1$OWNER== "CHRIST CHURCH I" | permits.1$OWNER== "CHRIST TABERNACLE CHURCH" | permits.1$OWNER== "CHRISTO TRUST" | permits.1$OWNER== "CHURCH CHRIST OF SCIENCE" | permits.1$OWNER== "CHURCH COURT CO" | permits.1$OWNER== "CHURCH OF SCIENTOLOGY" | permits.1$OWNER== "CHURCH OF THE C" | permits.1$OWNER== "EMMANUEL CHURCH" | permits.1$OWNER== "CONCORD BAPTIST" | permits.1$OWNER== "DORCH TEMPLE BAPTIST CHURCH" | permits.1$OWNER== "EBENEZER BAPTIS" | permits.1$OWNER== "EBENEZER BAPTIST CH" | permits.1$OWNER== "EMMANUEL GOSPEL CENTER INC" | permits.1$OWNER== "EMPIRE OF HOLY " | permits.1$OWNER== "EPIS CITY MISSION" | permits.1$OWNER== "FIRST BAPT CHUR" | permits.1$OWNER== "GLADTIDINGS PENTECOSTAL CHURCH" | permits.1$OWNER== "GREEK ORTHODOX " | permits.1$OWNER== "HOLY ORDER OF M" | permits.1$OWNER== "HOLY ORDER OF MANS" | permits.1$OWNER== "HOLY TRINITY CHURCH" | permits.1$OWNER== "J.P. TRINITY LATVEANS LUTH CH" | permits.1$OWNER== "KING'S CHAPEL HOUSE" | permits.1$OWNER== "MACEDONIA MISS BAP CHURCH" | permits.1$OWNER== "MATTAPAN CHURCH" | permits.1$OWNER== "MOUNT CALVARY HOLY CHURCH" | permits.1$OWNER== "MOUNT OLIVE TEM" | permits.1$OWNER== "N E CON 7TH DAY ADV" | permits.1$OWNER== "NEW ENGLAND BAP" | permits.1$OWNER== "OUR LADY OF GOOD VOYAGE CHAPEL" | permits.1$OWNER== "OUR LADY OF THE CEDAR CHURCH" | permits.1$OWNER== "OUR SAVIOUR'S LUTHERAN CHURCH" | permits.1$OWNER== "PARISH OF CHRIS" | permits.1$OWNER== "PARK STREET CHURCH" | permits.1$OWNER== "PRAYER ROOM PENTECOST CHURCH" | permits.1$OWNER== "PRAYER TOWER AP" | permits.1$OWNER== "PRAYER TOWER APOSTOLIC" | permits.1$OWNER== "ROMAN CATH ARCH" | permits.1$OWNER== "ROMAN CATH ARCHBISHOP" | permits.1$OWNER== "SHAWMUT COMMUNITY CHURCH" | permits.1$OWNER== "SHAWMUT CONGREG" | permits.1$OWNER== "SISTERS OF NOTR" | permits.1$OWNER== "SISTERS OF NOTRE DAME" | permits.1$OWNER== "SISTERS OF SAIN" | permits.1$OWNER== "SOC FOR ISLAMIC BROTHERHOOD IN" | permits.1$OWNER== "SONS OF DIVINE PROV INC" | permits.1$OWNER== "ST ANNE CHURCH" | permits.1$OWNER== "ST CYPRIANS CHU" | permits.1$OWNER== "ST GEORGE ORTHODOX CHURCH / BOSTON" | permits.1$OWNER== "ST LEONARD'S CHURCH" | permits.1$OWNER== "ST LUKES AND STMGRTS CHURCH" | permits.1$OWNER== "ST MONICA'S CHURCH" | permits.1$OWNER== "ST THOMAS AQUINAS CHURCH" | permits.1$OWNER== "Temple Bnai Moshe" | permits.1$OWNER== "BEREA SEVENTH DAY ADVENT" | permits.1$OWNER== "GREATER LOVE TABERNACLE" | permits.1$OWNER== "SOC JESUS OF NE" | permits.1$OWNER== "SOCIETY FOR ISL" | permits.1$OWNER== "THE CHRISTIAN ASSEM OF ROSLINDALE" | permits.1$OWNER== "UNITED PRESBYTE" | permits.1$OWNER== "MT CALVARY HOLY ASSEMBLY #1" | permits.1$OWNER== "THE MARIST FATHERS" | permits.1$OWNER== "UNIT UNIVERSALIST ASSOC" | permits.1$OWNER== "DAUGHTERS OF ST" | permits.1$OWNER== "ST JAMES ED CENTER" | permits.1$OWNER== "ST JOSEPH COMMUNITY INC" | permits.1$OWNER== "UNITARIAN UNIVE", 1,0)

##“Government” Variable
permits.1$government<-ifelse(permits.1$OWNER== "BOSTON DEVELOPMENT" | permits.1$OWNER== "Boston fire department" | permits.1$OWNER== "BOSTON HOUSING" | permits.1$OWNER== "BOSTON HOUSING " | permits.1$OWNER== "Boston Housing Authority" | permits.1$OWNER== "BOSTON MUNRCIPA" | permits.1$OWNER== "Boston Police Department" | permits.1$OWNER== "BOSTON PORT * S" | permits.1$OWNER== "BOSTON PUBLIC HEALTH COMM" | permits.1$OWNER== "BOSTON REDEVELO" | permits.1$OWNER== "BOSTON REDEVELOPMENT AUTH" | permits.1$OWNER== "BOSTON REDEVELP" | permits.1$OWNER== "CITY OF BOSTON" | permits.1$OWNER== "CITY OF BOSTON - DND" | permits.1$OWNER== "CITY OF BOSTON - PUB FAC " | permits.1$OWNER== "CITY OF BOSTON (REO)" | permits.1$OWNER== "CITY OF BOSTON BY FCL" | permits.1$OWNER== "CITY OF BOSTON PROP MGMT DEPT" | permits.1$OWNER== "CITY OF BOSTON-GEORGE WHITE FUND" | permits.1$OWNER== "COMMONWLTH OF M" | permits.1$OWNER== "COMMWLTH OF MAS" | permits.1$OWNER== "M B T A" | permits.1$OWNER== "MASS BAY TRANSP" | permits.1$OWNER== "MASS BAY TRANSPORTATION AUTH" | permits.1$OWNER== "MASS PORT AUTHO" | permits.1$OWNER== "MASS PORT AUTHORITY" | permits.1$OWNER== "MASS TURNPIKE A" | permits.1$OWNER== "MASS TURNPIKE AUTHORITY" | permits.1$OWNER== "MASSACHUSETTS BAY TRANS AUTH" | permits.1$OWNER== "MASSACHUSETTS PORT AUTHORITY" | permits.1$OWNER== "MASSPORT AUTHOR" | permits.1$OWNER== "MBTA" | permits.1$OWNER== "MSS PORT AUTHOR" | permits.1$OWNER== "COMMMONWEALTH O" | permits.1$OWNER== "COMMONWEALTH FL" | permits.1$OWNER== "COMMONWEALTH OF" | permits.1$OWNER== "UNITED STATES OF AMER" | permits.1$OWNER== "FEDERAL HOME LOAN MORTGAGE" | permits.1$OWNER== "FEDERAL HOME LOAN MTG CORP" | permits.1$OWNER== "FEDERAL MORTGAGE ASSOC" | permits.1$OWNER== "FEDERAL NATIONAL MORTGAGE ASSO" | permits.1$OWNER== "FEDERAL NATIONAL MTG ASSOC", 1,0)

##“Civic” Variable
permits.1$civic<-ifelse(permits.1$OWNER== "BOSTON PUBLIC LIBRARY" | permits.1$OWNER== "CHILDRENS MUSEU" | permits.1$OWNER== "CHILDRENS WORLD EDUCATIONAL" | permits.1$OWNER== "INSTITUTE OF CO" | permits.1$OWNER== "ISABELLA GARDNE" | permits.1$OWNER== "ISABELLA STEWAR" | permits.1$OWNER== "MUSEUM OF FINE " | permits.1$OWNER== "MUSEUM OF FINE ARTS" | permits.1$OWNER== "THE MUSEUM OF AFRICAN" | permits.1$OWNER== "MUSEUM OF AFRO AMER HISTORY" | permits.1$OWNER== "MUSEUM PROPERTI" | permits.1$OWNER== "R F KENNEDY GREENWAY CONSERVAN" | permits.1$OWNER== "BLACKSTONE PARK" | permits.1$OWNER== "BOSTON COMMUNITY CENTERS", 1,0)

##Creating Industry Category Variable 
permits.1$industrycategory<-"None"
permits.1$industrycategory<-ifelse(permits.1$uppereducation==1, "UpperEd", permits.1$industrycategory)
permits.1$industrycategory<-ifelse(permits.1$healthcare==1, "HealthCare", permits.1$industrycategory)
permits.1$industrycategory<-ifelse(permits.1$religious==1, "Religious", permits.1$industrycategory)
permits.1$industrycategory<-ifelse(permits.1$government==1, "Government", permits.1$industrycategory)
permits.1$industrycategory<-ifelse(permits.1$civic==1, "Civic", permits.1$industrycategory)


permits.1$TOTAL_FEES = ifelse(!is.na(permits.1$TOTAL_FEES) & permits.1$TOTAL_FEES<0,NA, permits.1$TOTAL_FEES)

#lubridating ISSUED_DATE
#getting rid of ""
permits.1$ISSUED_DATE[permits.1$ISSUED_DATE ==""]=NA
sum(is.na(permits.1$ISSUED_DATE))
permits.1$ISSUED_DATE = ymd_hms(permits.1$ISSUED_DATE)
sum(is.na(permits.1$ISSUED_DATE))

#lubridating EXPIRATION_DATE
permits.1$EXPIRATION_DATE[permits.1$EXPIRATION_DATE==""|permits.1$EXPIRATION_DATE==" "]=NA
sum(is.na(permits.1$EXPIRATION_DATE))
permits.1$EXPIRATION_DATE = ymd_hms(permits.1$EXPIRATION_DATE)
sum(is.na(permits.1$EXPIRATION_DATE))

permits.1$PermitDuration = permits.1$EXPIRATION_DATE - permits.1$ISSUED_DATE
permits.1$PermitDuration[!is.na(permits.1$PermitDuration) & permits.1$PermitDuration<0]= NA

permits.1 = rename(permits.1,
                   OCCUPANCY = OCCUPANCYTYPE,
                   NOTES = Comments)



varnames = c( "PermitNumber","WORKTYPE","PermitTypeDescr","DESCRIPTION","NOTES",
              "APPLICANT", "DECLARED_VALUATION","TOTAL_FEES",
              "ISSUED_DATE","EXPIRATION_DATE","STATUS","OWNER","OCCUPANCY","sq_feet", "ADDRESS", "CITY","STATE","ZIP", "Location",
              "Property_ID","parcel_num","X","Y","GIS_ID","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD",
              "newcon","addition","demo","reno","specialevents","uppereducation",
              "healthcare","religious","government","civic","industrycategory","PermitDuration")
setdiff(names(permits.1),varnames)
setdiff(varnames,names(permits.1))

permits.1 = permits.1[,varnames]

#save!
write.csv(permits.1, permits2017_path,row.names=F)


