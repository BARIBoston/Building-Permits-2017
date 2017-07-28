#---- INPUT PATHS ----
permits_path = "/Users/henrygomory/Downloads/buildingpermits.csv"
ID_walkover_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/IDConnector.2017.csv"
properties_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/Properties.2017.csv"

landParcels_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.csv"
landParcelsShpPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/LandParcels.2017.shp/"
landParcelsShpName = "LandParcels.2017"

roadsCSVPath  = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Roads 2015/roads_updated.csv"
roadsShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Roads 2015/"
roadsShpName = "roads_updated"

samPath = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Geographical Infrastructure 2017/sam_wGeos.csv"

blkShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Blocks/"
blkShpName = "Blocks_Boston_2010_BARI"

bgShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Block Groups 2015/"
bgShpName = "Census Block Groups"

ctShpPath = "Documents/Research/BARI/Geographic Infrastructure/Geographical Infrastructure 2015/Tracts/"
ctShpName = "Tracts_Boston_2010_BARI"



#---- OUTPUT PATHS ----
permits2017_path = "/Users/henrygomory/Documents/Research/BARI/Git/New-BARI/Building Permits 2017/Permits.Records.2017.csv"

#---- READ IN FILES ----
permits = read.csv(permits_path, stringsAsFactors=F)


#---- ADDING GEOGRAPHICAL DATA BASED ON ID ----
permits = standardizeGeoNames(permits)

# first must clean
temp = clean_address(permits$ADDRESS)
permits$num1 = temp[,2]
permits$num2 = temp[,3]
permits$street_c = temp[,4]
permits$suffix_c = temp[,5]
rm(temp)
permits$city_c=clean_city(permits$CITY)
permits$zip_c= clean_zip(permits$ZIP)
temp = str_match(permits$Location,"\\(([0-9-.]*), ([0-9-.]*)\\)")
permits$lng = as.numeric(temp[,3])
permits$lat = as.numeric(temp[,2])
rm(temp)


permits_ = placeInGI(df=permits,IDConnectorPath = ID_walkover_path,fuzzyMatching = F,fuzzyMatchDBPath = "",
                    landParcelsPath = landParcels_path,landParcelsShpName = landParcelsShpName,landParcelsShpPath=landParcelsShpPath,
                    roadsPath = roadsCSVPath,roadsShpPath = roadsShpPath,roadsShpName = roadsShpName,samPath = samPath,
                    blkShpPath =blkShpPath,blkShpName=blkShpName,bgShpPath=bgShpPath,bgShpName=bgShpName,ctShpPath=ctShpPath,ctShpName=ctShpName )

sum(!is.na(permits$Land_Parcel_ID))/nrow(permits)
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

permits$PermitTypeDescr = trim(str_replace_all(permits$PermitTypeDescr,"\xe4\xf3\xf1",""))



NEWCON = (!is.na(permits$DESCRIPTION) & 
            (permits$DESCRIPTION=="Excavation Borings Test Pits" | 
               permits$DESCRIPTION == "New construction" | 
               permits$DESCRIPTION == "Erect" | 
               permits$DESCRIPTION == "New construction")) | 
  (!is.na(permits$PermitTypeDescr) & 
     (permits$PermitTypeDescr=="Erect/New Construction" |
        permits$PermitTypeDescr == "Driveway Excavation Permit" | 
        permits$PermitTypeDescr == "Emergency Excavation Permit" | 
        permits$PermitTypeDescr == "Excavation Permit" | 
        permits$PermitTypeDescr == "Foundation Permit"))

DEMO = (!is.na(permits$DESCRIPTION) & 
          (permits$DESCRIPTION =="Demolition - Exterior" | permits$DESCRIPTION=="Demolition - Interior")) |  
       (!is.na(permits$PermitTypeDescr) & 
          permits$PermitTypeDescr== "BFD Construction  Demo  Reno" )


ADD = (!is.na(permits$DESCRIPTION) & 
         (permits$DESCRIPTION == "Addition")) | 
  (!is.na(permits$PermitTypeDescr) & 
     (permits$PermitTypeDescr == "Amendment to a Long Form" | 
      permits$PermitTypeDescr == "Board of Appeals" | 
      permits$PermitTypeDescr == "Long Form/Alteration Permit"|
      permits$PermitTypeDescr== "BFD Intent To Comply"))



RENO = (!is.na(permits$DESCRIPTION) & 
          (permits$DESCRIPTION == "Electrical" | 
          permits$DESCRIPTION == "Gas" | 
          permits$DESCRIPTION == "Plumbing" )) |
       (!is.na(permits$PermitTypeDescr) & 
          (permits$PermitTypeDescr == "BFD  Asbestos Removal" | 
          permits$PermitTypeDescr == "BFD  Cutting-Burning-Welding" | 
          permits$PermitTypeDescr == "Electrical Permit" | 
          permits$PermitTypeDescr == "Gas Permit" | 
          permits$PermitTypeDescr == "Plumbing Permit" | 
          permits$PermitTypeDescr == "Short Form Bldg Permit" | 
          permits$PermitTypeDescr == "Use of Premises" |
          permits$PermitTypeDescr== "Electrical Fire Alarms" | 
          permits$PermitTypeDescr== "Electrical Low Voltage" | 
          permits$PermitTypeDescr== "Electrical Temporary Service" | 
          permits$PermitTypeDescr== "Fire Protection Permit" | 
          permits$PermitTypeDescr== "Temporary Construction Drivewa" | 
          permits$PermitTypeDescr== "BFD  Bag Smoke Detectors" | 
          permits$PermitTypeDescr== "BFD  Temporary Dumpster" | 
          permits$PermitTypeDescr== "BFD Chemist Certificate proces" | 
          permits$PermitTypeDescr== "BBFD Application Tank Removal" | 
          permits$PermitTypeDescr== "BFD Approval of Tank Truck" | 
          permits$PermitTypeDescr== "BFD Install or Modify Undergrd" | 
          permits$PermitTypeDescr== "BFDAlteration of Fuel Oil Burn" | 
          permits$PermitTypeDescr== "Sidewalk Deposit" ))

#combining the two categorizations
permits$newcon<-ifelse(NEWCON, 1,0)
permits$demo<-ifelse(!NEWCON & DEMO, 1,0)
permits$addition<-ifelse(!NEWCON & !DEMO & ADD, 1,0)
permits$reno<-ifelse(!NEWCON & !DEMO & !ADD & RENO, 1,0)

##“Moving” Variable
#permits$moving<-ifelse(permits$PermitTypeDescr== "Street Occupancy Permit" & permits$WORKTYPE== "Movetrucks" & 
#                           !permits$demo & !permits$newcon & !permits$addition & !permits$reno & 
#                           !is.na(permits$PermitTypeDescr) & !is.na(permits$WORKTYPE), 1,0)

##“Special Events and Change to Premises” Variable
permits$specialevents<-ifelse(permits$PermitTypeDescr== "BFD Special Effects/Fireworks" | permits$PermitTypeDescr== "BFD General Permit" | permits$PermitTypeDescr== "Public Event" | permits$PermitTypeDescr== "BFDTemporary Place of Assembly" | permits$PermitTypeDescr== "BFD Tent with Assembly" | permits$PermitTypeDescr== "Use of Premises" | permits$PermitTypeDescr== "BFD Abandon Undergrd Storage" | permits$PermitTypeDescr== "BFD Alter Stationary Portable" |  permits$PermitTypeDescr== "BFD Blasting Permit" |  permits$PermitTypeDescr== "BFD Maintain Undergrd Storage" | permits$PermitTypeDescr== "BFD Temporary Out of Services" | permits$PermitTypeDescr== " BFD Use of Candles in a Place" | permits$PermitTypeDescr== "Certificate of Compliance", 1,0)

#owner var
permits$OWNER<-toupper(trim(permits$OWNER))

##“Upper Education” Variable
permits$uppereducation<-ifelse(permits$OWNER== "ART INSTITUTE O" | permits$OWNER== "THE NORTHEASTER" | permits$OWNER== "ART INSTITUTE OF BOSTON" | permits$OWNER== "BARNES & NOBEL-BOSTON UNIVERSITY" | permits$OWNER== "BOSTON ARCHITEC" | permits$OWNER== "BOSTON ARCHITECT COLLEGE" | permits$OWNER== "BOSTON COLLEGE" | permits$OWNER== "BOSTON COLLEGE PURCHASING DEPT" | permits$OWNER== "BOSTON UNIVERSI" | permits$OWNER== "BOSTON UNIVERSITY TRSTS" | permits$OWNER== "BOSTON UNIVERSITY TRSTS OF" | permits$OWNER== "BOSTON UNIVRSTY TRSTS OF" | permits$OWNER== "BOSTON UNV PHYSICAL PLANT" | permits$OWNER== "EMMANUEL COLLEG" | permits$OWNER== "HARVARD CLUB OF" | permits$OWNER== "HARVARD COLL PR" | permits$OWNER== "HARVARD COLLEGE" | permits$OWNER== "HARVARD COLLEGE PRES/FELLOWS" | permits$OWNER== "HARVARD UNIV/FAC MAINT OPERATI" | permits$OWNER== "NORTHEASTERN CONFERENCE CORP" | permits$OWNER== "NORTHEASTERN UNIV HILLEL" | permits$OWNER== "NORTHEASTERN UNIV MASS" | permits$OWNER== "NORTHEASTERN UNIVERSITY" | permits$OWNER== "SUFFOLK UNIVERS" | permits$OWNER== "WENTWORTH INSTI" | permits$OWNER== "WENTWORTH INSTITUTE" | permits$OWNER== "WENTWORTH INSTITUTE OF" | permits$OWNER== "WENTWORTH INSTITUTE OF TECH" | permits$OWNER== "WENTWOWORTH INS" | permits$OWNER== "WHEELOCK COLLEG" | permits$OWNER== "WHEELOCK COLLEGE" | permits$OWNER== "MODERN SCHOOL OF FASHION" | permits$OWNER== "N.E. COLLEGE OF OPTOMETRY" | permits$OWNER== "NEW ENG CONSERV" | permits$OWNER== "NEW ENGLAND COLLEGE OF OPTOMETRY" | permits$OWNER== "SIMMONS COLLEGE" | permits$OWNER== "HARVARD RE SERV ATTN ACCT PAY" | permits$OWNER== "HARVARD REAL ES" | permits$OWNER== "HARVARD REAL ESTATE", 1,0)

##“Healthcare” Variable
permits$healthcare<-ifelse(permits$OWNER== "CHILDREN'S HOSP" | permits$OWNER== "N E BAPTIST HOS" | permits$OWNER== "B'NAI B'RITH SR CITIZENS" | permits$OWNER== "CHILDRENS HOSPI" | permits$OWNER== "CHILDRENS HOSPITAL" | permits$OWNER== "ASIAN HEALTH CARE FOUNDATION" | permits$OWNER== "BETH ISRAEL DEA" | permits$OWNER== "BETH ISRAEL HOSPITAL" | permits$OWNER== "BRIGHAM  MEDICA" | permits$OWNER== "BRIGHAM & WOMEN'S HOSPITAL" | permits$OWNER== "BRIGHAM CIRCLE" | permits$OWNER== "BRIGHAM CIRCLE REALTY" | permits$OWNER== "BRIGHAM MEDICAL" | permits$OWNER== "EAST BOSTON HEALTH CENTER" | permits$OWNER== "PARTNERS HEALTHCARE SYSTEM MGH" | permits$OWNER== "UNIV HOSPITAL I" | permits$OWNER== "UNIVERSITY HOSP" | permits$OWNER== "UNIVERSITY OF MASS MEDICAL CTR" | permits$OWNER== "BOSTONIAN NURSING CARE" | permits$OWNER== "DEACONESS HOSPI" | permits$OWNER== "NEW ENGLAND DEA" | permits$OWNER== "NEW ENGLAND MED CTR HOSP" | permits$OWNER== "RIVERSIDE NURSING HOME" | permits$OWNER== "SPAULDING REHAB" | permits$OWNER== "ST ELIZABETH'S" | permits$OWNER== "ST ELIZABETHS HOSP OF BOS" | permits$OWNER== "NEWBURY DENTAL ASSC.REALTY TR" | permits$OWNER== "RUGGLES ASSISTED LIVING LP" | permits$OWNER== "MEDICAL AREA TO" | permits$OWNER== "SO. COVE NURSING INC" | permits$OWNER== "BROOKS PHARMACY", 1,0)

##“Religious Institutions” Variable
permits$religious<-ifelse(permits$OWNER== "BETHEL BAPTIST" | permits$OWNER== "BETHEL TABERNAC" | permits$OWNER== "BETHEL TABERNACLE" | permits$OWNER== "BETHESDA HAITIAN BAPTIST CHURCH" | permits$OWNER== "BI DEACONESS ME" | permits$OWNER== "BOSTON CHINESE EVANGELICAL" | permits$OWNER== "CH OF THE HOLY" | permits$OWNER== "CHRIST APOSTOLIC MT JOY" | permits$OWNER== "CHRIST CHURCH I" | permits$OWNER== "CHRIST TABERNACLE CHURCH" | permits$OWNER== "CHRISTO TRUST" | permits$OWNER== "CHURCH CHRIST OF SCIENCE" | permits$OWNER== "CHURCH COURT CO" | permits$OWNER== "CHURCH OF SCIENTOLOGY" | permits$OWNER== "CHURCH OF THE C" | permits$OWNER== "EMMANUEL CHURCH" | permits$OWNER== "CONCORD BAPTIST" | permits$OWNER== "DORCH TEMPLE BAPTIST CHURCH" | permits$OWNER== "EBENEZER BAPTIS" | permits$OWNER== "EBENEZER BAPTIST CH" | permits$OWNER== "EMMANUEL GOSPEL CENTER INC" | permits$OWNER== "EMPIRE OF HOLY" | permits$OWNER== "EPIS CITY MISSION" | permits$OWNER== "FIRST BAPT CHUR" | permits$OWNER== "GLADTIDINGS PENTECOSTAL CHURCH" | permits$OWNER== "GREEK ORTHODOX" | permits$OWNER== "HOLY ORDER OF M" | permits$OWNER== "HOLY ORDER OF MANS" | permits$OWNER== "HOLY TRINITY CHURCH" | permits$OWNER== "J.P. TRINITY LATVEANS LUTH CH" | permits$OWNER== "KING'S CHAPEL HOUSE" | permits$OWNER== "MACEDONIA MISS BAP CHURCH" | permits$OWNER== "MATTAPAN CHURCH" | permits$OWNER== "MOUNT CALVARY HOLY CHURCH" | permits$OWNER== "MOUNT OLIVE TEM" | permits$OWNER== "N E CON 7TH DAY ADV" | permits$OWNER== "NEW ENGLAND BAP" | permits$OWNER== "OUR LADY OF GOOD VOYAGE CHAPEL" | permits$OWNER== "OUR LADY OF THE CEDAR CHURCH" | permits$OWNER== "OUR SAVIOUR'S LUTHERAN CHURCH" | permits$OWNER== "PARISH OF CHRIS" | permits$OWNER== "PARK STREET CHURCH" | permits$OWNER== "PRAYER ROOM PENTECOST CHURCH" | permits$OWNER== "PRAYER TOWER AP" | permits$OWNER== "PRAYER TOWER APOSTOLIC" | permits$OWNER== "ROMAN CATH ARCH" | permits$OWNER== "ROMAN CATH ARCHBISHOP" | permits$OWNER== "SHAWMUT COMMUNITY CHURCH" | permits$OWNER== "SHAWMUT CONGREG" | permits$OWNER== "SISTERS OF NOTR" | permits$OWNER== "SISTERS OF NOTRE DAME" | permits$OWNER== "SISTERS OF SAIN" | permits$OWNER== "SOC FOR ISLAMIC BROTHERHOOD IN" | permits$OWNER== "SONS OF DIVINE PROV INC" | permits$OWNER== "ST ANNE CHURCH" | permits$OWNER== "ST CYPRIANS CHU" | permits$OWNER== "ST GEORGE ORTHODOX CHURCH / BOSTON" | permits$OWNER== "ST LEONARD'S CHURCH" | permits$OWNER== "ST LUKES AND STMGRTS CHURCH" | permits$OWNER== "ST MONICA'S CHURCH" | permits$OWNER== "ST THOMAS AQUINAS CHURCH" | permits$OWNER== "TEMPLE BNAI MOSHE" | permits$OWNER== "BEREA SEVENTH DAY ADVENT" | permits$OWNER== "GREATER LOVE TABERNACLE" | permits$OWNER== "SOC JESUS OF NE" | permits$OWNER== "SOCIETY FOR ISL" | permits$OWNER== "THE CHRISTIAN ASSEM OF ROSLINDALE" | permits$OWNER== "UNITED PRESBYTE" | permits$OWNER== "MT CALVARY HOLY ASSEMBLY #1" | permits$OWNER== "THE MARIST FATHERS" | permits$OWNER== "UNIT UNIVERSALIST ASSOC" | permits$OWNER== "DAUGHTERS OF ST" | permits$OWNER== "ST JAMES ED CENTER" | permits$OWNER== "ST JOSEPH COMMUNITY INC" | permits$OWNER== "UNITARIAN UNIVE", 1,0)

##“Government” Variable
permits$government = ifelse(permits$OWNER == "CITY OF BOSTON" |
                          permits$OWNER == "BOSTON REDEVELOPMNT AUTH" |
                          permits$OWNER == "BOSTON REDEVELOPMENT AUTH" |
                          permits$OWNER == "BOSTON REDEVELOPMENT" |
                          permits$OWNER == "BOSTON HOUSING AUTHORITY" |
                          permits$OWNER == "BOSTON HOUSING AUTH" |
                          permits$OWNER == "BOSTON POLICE POST 1018 VFW" |
                          permits$OWNER == "CITY OF BOSTON SCHOOL DEPT" |
                          permits$OWNER == "CITY OF BOSTON PUBLIC HEALTH" |
                          permits$OWNER == "CITY OF BOSTON SCHOOL DEPT" |
                          permits$OWNER == "CITY OF BOSTON BY FCL" |
                          permits$OWNER == "CITY OF BOSTON PUB FACIL" |
                          permits$OWNER == "BOSTON REDEVLOPMENT AUTHORIT" |
                          permits$OWNER == "BOSTON POLICE DETECTIVE" |
                          permits$OWNER == "CITY OF BOSTON PARKS" |
                          permits$OWNER == "BOSTON REDEVELOP AUTHORITY" |
                          permits$OWNER == "CITY OF BOSTON PARKS AND" |
                          permits$OWNER == "THE BOSTON REDEVELOPMENT" |
                          permits$OWNER == "BOSTON REDEVOPMENT AUTH" |
                          permits$OWNER == "BOSTON REDEVLPMNT AUTHOR" |
                          permits$OWNER == "BOSTON REDEVLOPMENT AUTHOR" |
                          permits$OWNER == "MBTA" |
                          permits$OWNER == "BOSTON PUBLIC HEALTH COMM" |
                          permits$OWNER == "CITY OF BOSTON PUBLIC HEALTH" |
                          permits$OWNER == "CITY OB BOSTON PUBLIC HEALTH" |
                          permits$OWNER == "PUBLIC FACILITIES COMM" |
                          permits$OWNER== "BOSTON DEVELOPMENT" |
                          permits$OWNER== "BOSTON FIRE DEPARTMENT" | 
                          permits$OWNER== "BOSTON HOUSING" | 
                          permits$OWNER== "BOSTON MUNRCIPA" | 
                          permits$OWNER== "BOSTON POLICE DEPARTMENT" | 
                          permits$OWNER== "BOSTON PORT * S" | 
                          permits$OWNER== "BOSTON PUBLIC HEALTH COMM" | 
                          permits$OWNER== "BOSTON REDEVELO" | 
                          permits$OWNER== "BOSTON REDEVELOPMENT AUTH" | 
                          permits$OWNER== "BOSTON REDEVELP" | 
                          permits$OWNER== "CITY OF BOSTON" | 
                          permits$OWNER== "CITY OF BOSTON - DND" | 
                          permits$OWNER== "CITY OF BOSTON - PUB FAC " | 
                          permits$OWNER== "CITY OF BOSTON (REO)" | 
                          permits$OWNER== "CITY OF BOSTON BY FCL" | 
                          permits$OWNER== "CITY OF BOSTON PROP MGMT DEPT" | 
                          permits$OWNER== "CITY OF BOSTON-GEORGE WHITE FUND" | 
                          permits$OWNER== "COMMONWLTH OF M" | 
                          permits$OWNER== "COMMWLTH OF MAS" | 
                          permits$OWNER== "M B T A" | 
                          permits$OWNER== "MASS BAY TRANSP" | 
                          permits$OWNER== "MASS BAY TRANSPORTATION AUTH" | 
                          permits$OWNER== "MASS PORT AUTHO" | 
                          permits$OWNER== "MASS PORT AUTHORITY" | 
                          permits$OWNER== "MASS TURNPIKE A" | 
                          permits$OWNER== "MASS TURNPIKE AUTHORITY" | 
                          permits$OWNER== "MASSACHUSETTS BAY TRANS AUTH" | 
                          permits$OWNER== "MASSACHUSETTS PORT AUTHORITY" | 
                          permits$OWNER== "MASSPORT AUTHOR" | 
                          permits$OWNER== "MBTA" | 
                          permits$OWNER== "MSS PORT AUTHOR" | 
                          permits$OWNER== "COMMMONWEALTH O" | 
                          permits$OWNER== "COMMONWEALTH FL" | 
                          permits$OWNER== "COMMONWEALTH OF" | 
                          permits$OWNER== "UNITED STATES OF AMER" | 
                          permits$OWNER== "FEDERAL HOME LOAN MORTGAGE" | 
                          permits$OWNER== "FEDERAL HOME LOAN MTG CORP" | 
                          permits$OWNER== "FEDERAL MORTGAGE ASSOC" | 
                          permits$OWNER== "FEDERAL NATIONAL MORTGAGE ASSO" | 
                          permits$OWNER== "FEDERAL NATIONAL MTG ASSOC",1,0)

##“Civic” Variable
permits$civic<-ifelse(permits$OWNER== "BOSTON PUBLIC LIBRARY" | permits$OWNER== "CHILDRENS MUSEU" | permits$OWNER== "CHILDRENS WORLD EDUCATIONAL" | permits$OWNER== "INSTITUTE OF CO" | permits$OWNER== "ISABELLA GARDNE" | permits$OWNER== "ISABELLA STEWAR" | permits$OWNER== "MUSEUM OF FINE" | permits$OWNER== "MUSEUM OF FINE ARTS" | permits$OWNER== "THE MUSEUM OF AFRICAN" | permits$OWNER== "MUSEUM OF AFRO AMER HISTORY" | permits$OWNER== "MUSEUM PROPERTI" | permits$OWNER== "R F KENNEDY GREENWAY CONSERVAN" | permits$OWNER== "BLACKSTONE PARK" | permits$OWNER== "BOSTON COMMUNITY CENTERS", 1,0)

##Creating Industry Category Variable 
permits$industrycategory<-"None"
permits$industrycategory<-ifelse(permits$uppereducation==1, "UpperEd", permits$industrycategory)
permits$industrycategory<-ifelse(permits$healthcare==1, "HealthCare", permits$industrycategory)
permits$industrycategory<-ifelse(permits$religious==1, "Religious", permits$industrycategory)
permits$industrycategory<-ifelse(permits$government==1, "Government", permits$industrycategory)
permits$industrycategory<-ifelse(permits$civic==1, "Civic", permits$industrycategory)


permits$TOTAL_FEES = ifelse(!is.na(permits$TOTAL_FEES) & permits$TOTAL_FEES<0,NA, permits$TOTAL_FEES)

#lubridating ISSUED_DATE
#getting rid of ""
permits$ISSUED_DATE[permits$ISSUED_DATE ==""]=NA
sum(is.na(permits$ISSUED_DATE))
permits$ISSUED_DATE = ymd_hms(permits$ISSUED_DATE)
sum(is.na(permits$ISSUED_DATE))

#lubridating EXPIRATION_DATE
permits$EXPIRATION_DATE[permits$EXPIRATION_DATE==""|permits$EXPIRATION_DATE==" "]=NA
sum(is.na(permits$EXPIRATION_DATE))
permits$EXPIRATION_DATE = ymd_hms(permits$EXPIRATION_DATE)
sum(is.na(permits$EXPIRATION_DATE))

permits$PermitDuration = permits$EXPIRATION_DATE - permits$ISSUED_DATE
permits$PermitDuration[!is.na(permits$PermitDuration) & permits$PermitDuration<0]= NA

permits = rename(permits,
                   OCCUPANCY = OCCUPANCYTYPE,
                   NOTES = Comments)



varnames = c( "PermitNumber","WORKTYPE","PermitTypeDescr","DESCRIPTION","NOTES",
              "APPLICANT", "DECLARED_VALUATION","TOTAL_FEES",
              "ISSUED_DATE","EXPIRATION_DATE","STATUS","OWNER","OCCUPANCY","sq_feet", "ADDRESS", "CITY","STATE","ZIP", "Location",
              "Property_ID","parcel_num","X","Y","Land_Parcel_ID","TLID","Blk_ID_10","BG_ID_10","CT_ID_10","NSA_NAME","BRA_PD",
              "newcon","addition","demo","reno","specialevents","uppereducation",
              "healthcare","religious","government","civic","industrycategory","PermitDuration")
setdiff(names(permits),varnames)
setdiff(varnames,names(permits))

permits = permits[,varnames]

#save!
write.csv(permits, permits2017_path,row.names=F)


