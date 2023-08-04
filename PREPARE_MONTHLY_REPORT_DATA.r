############################################################################
######## COLLATE MONITORING DATA FROM GOUGH ##############
############################################################################
### this script is designed to collate all current monitoring data from various databases
### this script produces an R workspace
### once this R workspace (with current data) is uploaded to https://github.com/steffenoppel/GoughReports a GitHub Action will automatically create a monthly report
### this script only needs to be modified once (to adjust working directories), but should then run without modification



## set the general drive where data are held here
## each database is expected to be in a subdirectory of this location - if this is not the case you may need to comment out the 'try' lines below
#MYDATADRIVE<-"C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\DATA"
#MYREPORTDRIVE<-"C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport"

MYDATADRIVE<-"C:\\Users\\sop\\Documents\\Steffen\\RSPB\\Gough"
MYREPORTDRIVE<-"C:\\Users\\sop\\Documents\\Steffen\\RSPB\\Gough"

#MYDATADRIVE<-"C:\\Users\\Gough Conservation\\Documents\\Gough Birders\\Database"
#MYREPORTDRIVE<-"C:\\Users\\Gough Conservation\\Documents\\Gough Birders\\2022-2023\\12.Monthly reports 2022-23"



###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
###  NEED TO SPECIFY THE YEAR AND MONTH FOR WHICH YOU WANT A SUMMARY ########
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
### the reported month is always the month previous to the script being run

today<-as.Date(Sys.time())
day(today)<-1  ## we set it to the first day of the month so the report is from first to last day of previous month 
startreportperiod<- today-months(1)
endreportperiod<- today-days(1)




###################################################################################
##   1. READ IN DATA FROM DATABASES AND FILTER DATA FROM LAST MONTH ####
###################################################################################
library(RODBC)
library(tidyverse)
library(lubridate)
library(data.table)
filter<-dplyr::filter
select<-dplyr::select


### LOAD THE BACKGROUND DATA

setwd(MYREPORTDRIVE)
load("GoughMonthlyReport_background.RData")


setwd(MYDATADRIVE)

### FIND MOST UP-TO-DATE VERSION OF BREEDING DATABASE
try(setwd(paste(MYDATADRIVE,"Breeding_Database",sep="\\")), silent=T)
details <- file.info(list.files(pattern="GOUGH_BreedingDatabase*"))
details <- details[order(as.POSIXct(details$mtime),decreasing=T),]
upddb<-rownames(details)[1]
db_file_path<-rownames(details)[1]
db <- odbcConnectAccess2007(upddb)
seabirdcounts<- sqlQuery(db, "SELECT * FROM EXPORT_COUNT_SURVEYS")
landbirdsurveys<- sqlQuery(db, "SELECT * FROM tblLandbirdSurveys")
landbirdcounts<- sqlQuery(db, "SELECT * FROM tblLandBirdData")
transects<- sqlQuery(db, "SELECT * FROM tblLandbirdTransects")
nestvisits<- sqlQuery(db, "SELECT * FROM Export_nest_visits")
deaths<- sqlQuery(db, "SELECT * FROM tblIncidentalRecords")
incbirds<- sqlQuery(db, "SELECT * FROM tblIncidentalRecords")
transects<- sqlQuery(db, "SELECT * FROM tblLandbirdTransects")
location<- sqlQuery(db, "SELECT * FROM tblLocation")
odbcClose(db)


### FIND MOST UP-TO-DATE VERSION OF CMR DATABASE
try(setwd(paste(MYDATADRIVE,"CMR_Database",sep="\\")), silent=T)
details <- file.info(list.files(pattern="Gough_CMR_BackEnd*"))
details <- details[order(as.POSIXct(details$mtime),decreasing=T),]
updcmr<-rownames(details)[1]
db <- odbcConnectAccess2007(updcmr)
contacts<- sqlQuery(db, "SELECT * FROM EXPORT_all_contacts")
allcontacts<- sqlQuery(db, "SELECT * FROM tbl_Contacts")
marks<- sqlQuery(db, "SELECT * FROM tbl_Marks")
species<- sqlQuery(db, "SELECT * FROM tbl_Species")
locs<- sqlQuery(db, "SELECT * FROM tbl_Locations")
contact_mark<- sqlQuery(db, "SELECT * FROM tbl_Contact_Mark_Hash")
colours<- sqlQuery(db, "SELECT * FROM tbl_Band_Colors")
mark_types<- sqlQuery(db, "SELECT * FROM tbl_Mark_Types")
birds<- sqlQuery(db, "SELECT * FROM tbl_Birds")
comments<- sqlQuery(db, "SELECT * FROM LOOKUP_captive_GOBU_names")
odbcClose(db)


### FIND MOST UP-TO-DATE VERSION OF TRAP DATABASE
try(setwd(paste(MYDATADRIVE,"Trap_Database",sep="\\")), silent=T)
details <- file.info(list.files(pattern="GOUGH_MouseDatabase*"))
details <- details[order(as.POSIXct(details$mtime),decreasing=T),]
upddb<-rownames(details)[1]
db_file_path<-rownames(details)[1]
db <- odbcConnectAccess2007(upddb)
GOMOlocs<- sqlQuery(db, "SELECT * FROM tbl_locations")
deploys<- sqlQuery(db, "SELECT * FROM tbl_deployments")
detections<- sqlQuery(db, "SELECT * FROM tbl_detections")
odbcClose(db)

head(contacts)  ## CMR data
head(nestvisits)  ## nest monitoring data
head(landbirdcounts)  ## landbird count data
head(landbirdsurveys)  ## landbird count data
head(seabirdcounts)  ## seabird count data
head(deaths)  ## incidental records
head(birds)
transects$Transect<-as.character(transects$Transect)



###################################################################################
##   2. MODIFY DATA ####
###################################################################################


### RELEASE LOCATIONS
GOMOrel<- releases %>% rename(LocationName=ReleaseLocation,
                              Latitude=ReleaseLat,Longitude=ReleaseLong) %>%
  group_by(LocationName, Latitude, Longitude) %>%
  mutate(Notes=paste0(ID, collapse = ", ")) %>%
  group_by(LocationName, Latitude, Longitude, Notes) %>%
  summarise(Number= length(unique(ID))) %>%
  ungroup() %>%
  mutate(Date=ymd("2021-10-16"), Type="release") %>%
  select(LocationName, Date, Latitude, Longitude,Number,Notes,Type)



## INCIDENTAL DATA FROM BREEDING DATABASE

GOMOopp<-incbirds %>% filter(Species=="GOMO") %>%
  filter(year(Date)>2021) %>%
  rename(LocationName=Area) %>%
  select(ID, LocationName, Date,Number,Notes) %>%
  left_join(location, by="LocationName") %>%
  select(LocationName, Date, Latitude, Longitude,Number,Notes) %>%
  mutate(Number=ifelse(Number==0,1,Number)) %>%
  mutate(LocationName=as.character(LocationName), Date=as.POSIXct(Date)) %>%
  mutate(Type="opportunistic")




### CAMERA TRAP DATA FROM MOUSE DATABASE

GOMOcam<-detections %>% 
  filter(Species=="GOMO") %>%
  filter(tolower(DeviceType)=="camera") %>%
  mutate(DateTime=ymd_hms(paste(Date, format(Time,format="%H:%M:%S")))) %>%
  select(DeployID,DateTime,Number,Age) %>%
  left_join(deploys, by="DeployID") %>%
  select(DeployID,LocID,DateTime,Number,Age,ActiveNights) %>%
  left_join(GOMOlocs, by="LocID") %>%
  select(Area,DateTime,Latitude, Longitude,Number,Age) %>%
  rename(LocationName=Area,Date=DateTime,Notes=Age) %>%
  mutate(LocationName=as.character(LocationName), Date=as.POSIXct(Date)) %>%
  mutate(Type="camera")



#############################################################################
##   3. FILTER DATA FOR THE YEAR AND MONTH FOR WHICH YOU WANT A SUMMARY ####
#############################################################################


## filter data for the reporting period
contacts<-contacts %>% filter(Date_Time>=startreportperiod) %>% filter(Date_Time<endreportperiod)
nestvisits<-nestvisits %>% filter(Date>=startreportperiod) %>% filter(Date<endreportperiod)
landbirdsurveys<-landbirdsurveys %>% filter(Date>=startreportperiod) %>% filter(Date<endreportperiod)
landbirdcounts <- landbirdcounts %>% group_by(LandbirdSurveyID,Species) %>% summarise(N=sum(N_birds)) %>%
  spread(key=Species, value=N, fill=0)
landbirdcounts <- landbirdsurveys %>% left_join(landbirdcounts, by="LandbirdSurveyID") %>%
  mutate(GOBU=ifelse(is.na(GOBU),0,GOBU),GOMO=ifelse(is.na(GOMO),0,GOMO))

seabirdcounts<-seabirdcounts %>% filter(Date>=startreportperiod) %>% filter(Date<=endreportperiod)
deaths<-deaths %>% filter(Date>=startreportperiod) %>% filter(Date<=endreportperiod)




#############################################################################
##   4. SUMMARISE THE MONTHLY MONITORING DATA ######################
#############################################################################

### summary of number of contacts and birds per species
CMR_summary<-contacts %>% mutate(count=1) %>%
  filter(!is.na(SpeciesCode)) %>%
  mutate(SpeciesCode=if_else(SpeciesCode %in% c("SUSK","BRSK"),"SKUA",as.character(SpeciesCode))) %>%
  group_by(SpeciesCode) %>%
  summarise(n_sites=length(unique(Location)),n_individuals=length(unique(BirdID)),n_contacts=sum(count))
CMR_summary


### summary of number of nests and visits per species
nest_summary<-nestvisits %>% mutate(count=1) %>%
  group_by(Species) %>%
  summarise(n_nests=length(unique(Nest_label)),n_visits=sum(count))
nest_summary


### summary of counts per species
count_summary<-seabirdcounts %>% 
  group_by(Species,Cohort, Breed_Stage) %>%
  summarise(n_sites=length(unique(Colony)),n_counts=length(unique(Date)),n_birds=sum(Number))
count_summary


### summary of dead birds found
death_summary<-deaths %>% filter(RecordType==2) %>%
  group_by(Species,Area) %>%
  summarise(n=sum(Number))
death_summary

### summary of landbird counts per species
transects$Transect<-as.integer(transects$Transect)
landbird_summary<-landbirdcounts %>% left_join(transects, by="Transect") %>%
  mutate(count=1) %>%
  group_by(Habitat_description) %>%
  summarise(n_sites=length(unique(Transect)),n_counts=sum(count),GOBU=sum(GOBU,na.rm=T),GOMO=sum(GOMO,na.rm=T))
landbird_summary

## prevent empty table being printed when there are no data
do_it = dim(landbird_summary)[1]>0


#############################################################################
##   5. SUMMARISE THE GOBU RESIGHTING DATA ######################
#############################################################################

### SPECIFY GOBU NAMES FOR BIRD ID in DATABASE 
BirdNames<-comments %>% filter(str_detect(Comment, "GOBU")) %>%
  filter(nchar(Comment)<8) %>% rename(BirdName=Comment) %>% full_join(birds,'BirdID') %>%
  group_by(BirdID,BirdName) %>%
  summarise(SpeciesID=min(SpeciesID)) %>%
  ungroup()


### available colours and abbreviations from https://www.cr-birding.org/node/112
colors<-c("None","White","Black","Pink","Red","Orange","Yellow","Green","Blue","Purple","Pale Blue","Light Green","Metal")
colabb<-c(NA,"W","N","K","R","O","Y","G","B","V","P","L","M")
colloockup<-data.frame(col=tolower(colors),abb=colabb)

### LINK ALL DATA TOGETHER ##
GOBUcontacts<- allcontacts %>% left_join(BirdNames,'BirdID') %>% left_join(species,'SpeciesID') %>%
  filter(SpeciesCode=="GOBU") %>%
  left_join(locs,'LocationID') %>%
  select(SpeciesCode,BirdID,BirdName,ContactID,Contact_Year,Date_Time,Location,Age,Sex,Contact_TypeID)

GOBUrings<- GOBUcontacts %>% full_join(contact_mark,'ContactID') %>% left_join(marks,'MarkID') %>%
  left_join(mark_types,'Mark_TypeID') %>% left_join(colours,'Band_ColorID') %>%
  select(SpeciesCode,BirdID,BirdName,ContactID,Contact_Year,Date_Time,Location,Age,Sex,Mark_Code,Band_Number,Band_Color,Side_Of_Body,Band_Order,Contact_TypeID) %>%
  filter(Mark_Code %in% c("FED","ColB"))

GOBUcolourrings<-GOBUrings %>%
  filter(Contact_TypeID==1001332577) %>%  ## 'Capture' = 1001332577
  select(ContactID,BirdID,Band_Color,Side_Of_Body,Band_Order) %>%
  mutate(col=colloockup$abb[match(tolower(Band_Color),colloockup$col)]) %>%
  mutate(SEQ=ifelse(Side_Of_Body=="L",0+Band_Order,3+Band_Order)) %>%
  select(-Band_Order,-Side_Of_Body,-Band_Color) %>%
  #GOBUcolourrings[c(148,151),]
  spread(key=SEQ, value=col) %>%
  rename(L0=`0`,L1=`1`,L2=`2`,R3=`3`,R4=`4`,R5=`5`) %>%
  mutate(colour_combo=paste(ifelse(is.na(L0),"",as.character(L0)),
                            ifelse(is.na(L1),"",as.character(L1)),
                            ifelse(is.na(L2),"",as.character(L2)),"/",
                            ifelse(is.na(R3),"",as.character(R3)),
                            ifelse(is.na(R4),"",as.character(R4)),
                            ifelse(is.na(R5),"",as.character(R5)), sep="")) %>%
  select(BirdID,colour_combo)

GOBUcapt<- GOBUrings %>% filter(Mark_Code=="FED") %>%
  filter(SpeciesCode=="GOBU") %>%
  arrange(Date_Time) %>%
  filter(Contact_TypeID==1001332577) %>%
  group_by(BirdID, BirdName,Band_Number) %>%
  summarise(Captured=first(Date_Time), CaptLoc=first(Location)) %>%
  left_join(GOBUcolourrings, by="BirdID") %>%
  ungroup() %>%
  arrange(BirdName) %>%
  select(BirdName,Band_Number,colour_combo,Captured,CaptLoc) %>%
  mutate(Captured=format(Captured,format="%d %b %Y"))

### specify release and resights
### SUMMARISE THE DATA FOR EACH BIRD
GOBUrel<-GOBUcontacts %>% arrange(BirdName, Date_Time) %>% 
  filter(Contact_TypeID==1001332578) %>%
  group_by(BirdName) %>%
  summarise(Released=last(Date_Time), ReleaseLoc=last(Location)) %>%
  mutate(Released=format(Released,format="%d %b %Y"))

resights<-GOBUcontacts %>% arrange(BirdName, Date_Time) %>% 
  filter(Contact_TypeID %in% c(1,2)) %>%
  filter(Date_Time<startreportperiod) %>%
  group_by(BirdName) %>%
  summarise(PreviousObs=last(Date_Time)) %>%
  mutate(PreviousObs=format(PreviousObs,format="%d %b %Y"))

ALLresights<-GOBUcontacts %>% arrange(BirdName, Date_Time) %>% 
  filter(Contact_TypeID %in% c(1,2)) %>%
  group_by(BirdName) %>%
  summarise(LastObs=last(Date_Time), LastLoc=last(Location)) %>%
  mutate(LastObs=format(LastObs,format="%d %b %Y"))
GOBUall<-GOBUcapt %>% left_join(GOBUrel, by="BirdName") %>% left_join(ALLresights, by="BirdName") %>%
  mutate(LastObs=ifelse(LastObs<Released,NA,LastObs),LastLoc=ifelse(LastObs<Released,NA,LastLoc))


### CREATE THE FINAL TABLE IN THE REPORT

GOBU<- GOBUrings %>% filter(Mark_Code=="FED") %>%
  filter(SpeciesCode=="GOBU") %>%
  arrange(Date_Time) %>%
  filter(Date_Time>=startreportperiod) %>% filter(Date_Time<endreportperiod) %>%
  filter(!(Contact_TypeID %in% c(1001332577,1001332578))) %>% ### exclude capture and release contacts
  group_by(SpeciesCode,BirdID, BirdName,Band_Number) %>%
  summarise(LastObs=max(Date_Time), LastLoc=last(Location)) %>%
  left_join(GOBUcolourrings, by="BirdID") %>%
  left_join(resights, by="BirdName") %>%
  left_join(GOBUrel, by="BirdName") %>% 
  ungroup() %>%
  arrange(BirdName) %>%
  select(BirdName,Band_Number,colour_combo,ReleaseLoc,PreviousObs,LastObs,LastLoc) %>%
  mutate(LastObs=format(LastObs,format="%d %b"))



###################################################################################
##   6. COMBINE THE MOORHEN DATA INTO A SINGLE TABLE WITH SUMS OF RECORDS EVERY 2 WEEKS ####
###################################################################################

GOMOsummary<-bind_rows(GOMOopp,GOMOcam,GOMOplay) %>%
  filter(Date<endreportperiod) %>%
  mutate(week=isoweek(Date),year=year(Date)) %>%
  mutate(month=month(Date)) %>% 
  mutate(first=as.Date(ymd(paste(year,month,"01",sep="-")))) %>%
  group_by(year,month,first,Type) %>%
  summarise(N=sum(Number)) %>%
  spread(key=Type, value=N, fill=0) %>%
  filter(year>2021) %>%
  mutate(Total=camera+opportunistic+playback) %>%
  #mutate(first=ISOweek::ISOweek2date(paste(year,"-","W",ifelse(fortnight<10,paste("0",as.character(fortnight),sep=""),fortnight),"-1",sep=""))) %>%
  ungroup() %>%
  #filter(month(first)>3) %>%      ## Andrew C wants only last 4 months
  arrange(first) %>%
  mutate(Month=month.abb[month]) %>%
  mutate(Year=year(first)) %>%
  select(Month,Year,opportunistic,playback,camera,Total,first)


###################################################################################
##   7. CREATE MOORHEN MAP DATA FILE FOR PLOTTING A MAP ####
###################################################################################

GOMOall<-bind_rows(GOMOopp,GOMOcam,GOMOplay, GOMOrel) %>%
  filter(Date<endreportperiod) %>%
  mutate(icon=ifelse(Type=="camera","http://maps.google.com/mapfiles/kml/pal4/icon38.png",
                     ifelse(Type=="opportunistic","http://maps.google.com/mapfiles/kml/paddle/O.png",
                            ifelse(Type=="release","http://maps.google.com/mapfiles/kml/shapes/volcano.png",
                                   "http://maps.google.com/mapfiles/kml/shapes/hospitals.png")))) %>%
  filter(ifelse(Type!="release",Date>ymd_hms("2022-02-01 12:00:00"),Date>ymd("2020-01-01"))) %>%
  filter(!is.na(Date)) %>%
  filter(Number>0) %>%
  mutate(DAY=yday(Date)) %>%
  arrange(DAY)

## add random noise over location to avoid overplotting
GOMOall$Latitude<-GOMOall$Latitude+rnorm(length(GOMOall$Latitude),0.0001,0.0002)
GOMOall$Longitude<-GOMOall$Longitude+rnorm(length(GOMOall$Latitude),0.0001,0.0002)


GOMOall<-GOMOall %>% mutate(Month=month(Date)) %>% filter(Type!="release") %>% #filter(Month>(month(endreportperiod)-6))
  filter(Date>=startreportperiod-months(5)) %>%
  filter(Date<endreportperiod) %>%
  mutate(MonthLab=format(Date,format="%b %Y")) 

### ensure proper ordering of months across years
sortedmonths<-GOMOall %>% group_by(Month) %>% summarise(first=min(Date))%>%
  mutate(MonthLab=format(first,format="%b %Y")) %>%
  arrange(first)
GOMOall$MonthLab<-factor(GOMOall$MonthLab, levels=sortedmonths$MonthLab)




###################################################################################
##   8. SAVE THE DATA TO THE SPECIFIED DRIVE ####
###################################################################################
setwd(MYREPORTDRIVE)
save.image("MONTHLY_REPORT_DATA.RData")