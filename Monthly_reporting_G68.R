############################################################################
######## SUMMARISE MONTHLY ACTIVITIES FOR CONSERVATION REPORT ##############
############################################################################

library(tidyverse)
library(lubridate)
library(data.table)
library(knitr)
library(rmarkdown)
filter<-dplyr::filter
select<-dplyr::select
require(maps)
library(ggpubr)
library(ggmap)
library(sp)
library(rgdal)
library(maptools)
library(readxl)
library(grid)
library(gtable)
library(magick)
library(ISOweek)
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens"), silent=T)
# try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens"), silent=T)
# imgGOMO<-image_read("moorhen.jpg")
# GOMOicon <- rasterGrob(imgGOMO, interpolate=TRUE)



###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########
###  NEED TO SPECIFY THE YEAR AND MONTH FOR WHICH YOU WANT A SUMMARY ########
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~########

today<-as.Date(ymd("2023-03-01"))
startreportperiod<- today-months(1)
endreportperiod<- today-months(0)


###################################################################################
##   1. READ IN DATA FROM DATABASES AND FILTER DATA FROM LAST MONTH ####
###################################################################################

## run the RODBC import in a 32-bit version of R
system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport\\RODBC_imports.r")), wait = TRUE, invisible = FALSE)
#system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\Users\\Gough Conservation\\Documents\\Gough Birders\\2018-2019\\12.Monthly reports 2018-19\\RODBC_imports.r")), wait = TRUE, invisible = FALSE)
#system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport\\RODBC_imports.r")), wait = TRUE, invisible = FALSE)
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport"), silent=T)
#try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport"), silent=T)
#try(setwd("C:\\Users\\Gough Conservation\\Documents\\Gough Birders\\2018-2019\\12.Monthly reports 2018-19"), silent=T)
load("GoughMonthlyReport_background.RData")
load("MONTHLY_REPORT_DATA.RData")
head(contacts)  ## CMR data
head(nestvisits)  ## nest monitoring data
head(landbirdcounts)  ## landbird count data
head(landbirdsurveys)  ## landbird count data
head(seabirdcounts)  ## seabird count data
head(deaths)  ## incidental records
head(birds)



### RELEASE LOCATIONS
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\Eradication\\Aviculture"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\Eradication\\Aviculture"), silent=T)
releases<-read.table("GOMO_release_locations.csv", sep=",", header=T)

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

#try(setwd("C:\\Users\\Gough Conservation\\Documents\\Gough Birders\\2018-2019\\12.Monthly reports 2018-19"), silent=T)
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens"), silent=T)
system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens\\RODBC_import_incidentals.r")), wait = TRUE, invisible = FALSE, intern = T)
#system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens\\RODBC_import_incidentals.r")), wait = TRUE, invisible = FALSE, intern = T)

load("INCIDENTAL_DATA.RData")
transects$Transect<-as.character(transects$Transect)
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

try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens"), silent=T)
system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens\\RODBC_MOORHEN_IMPORT.R")), wait = TRUE, invisible = FALSE, intern = T)
#system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens\\RODBC_MOORHEN_IMPORT.R")), wait = TRUE, invisible = FALSE, intern = T)
load("MOORHEN_DATA.RData")
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



### LOAD PLAYBACK TRANSECT DATA FROM EXCEL SPREADSHEET
## combine with other tables to extract coordinates of transects

try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens"), silent=T)
GOMO<-read_excel("GOMO_postrelease_monitoring.xlsx",sheet= "DATA")
GOMOplay<-GOMO %>% select(Transect,Day,GOMOheard,GOMOresponded,GOMOseen,Notes) %>%
  left_join(transects, by="Transect") %>%
  rename(LocationName=Transect) %>%
  mutate(LocationName=ifelse(LocationName=="Area1","Area 1",LocationName)) %>%
  mutate(LocationName=ifelse(LocationName=="BlechBridge","Blechnum Bridge",LocationName)) %>% 
  left_join(location, by="LocationName") %>%
  mutate(GOMOsolicit=(GOMOheard+GOMOseen+GOMOresponded)) %>%
  filter(GOMOsolicit<10) %>% ## remove two counts at Blechnum Bridge on 24 Oct that sound ominous and may just have counted all the released birds from that day??
  mutate(Latitude=ifelse(is.na(Latitude), (Lat_start+Lat_end)/2,Latitude),Longitude=ifelse(is.na(Longitude), (Long_start+Long_end)/2,Longitude)) %>%
  select(LocationName,Day,Latitude, Longitude,GOMOsolicit,Notes) %>%
  rename(Date=Day,Number=GOMOsolicit) %>%
  mutate(LocationName=as.character(LocationName), Date=as.POSIXct(Date)) %>%
  mutate(Type="playback")


#### check from where recent records are
bind_rows(GOMOopp,GOMOcam,GOMOplay) %>%
  mutate(week=isoweek(Date),year=year(Date)) %>%
  mutate(fortnight=round(week/2,0)*2) %>%
  filter(Number>0) %>%
  filter(year>2022) %>%
  filter(fortnight>4) %>%
  group_by(LocationName, Type) %>%
  summarise(first=min(Date), last=max(Date))



#############################################################################
##   2. FILTER DATA FOR THE YEAR AND MONTH FOR WHICH YOU WANT A SUMMARY ####
#############################################################################
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport"), silent=T)

## filter data for the reporting period
contacts<-contacts %>% filter(Date_Time>=startreportperiod) %>% filter(Date_Time<endreportperiod)
nestvisits<-nestvisits %>% filter(Date>=startreportperiod) %>% filter(Date<endreportperiod)
landbirdsurveys<-landbirdsurveys %>% filter(Date>=startreportperiod) %>% filter(Date<endreportperiod)
landbirdcounts <- landbirdcounts %>% group_by(LandbirdSurveyID,Species) %>% summarise(N=sum(N_birds)) %>%
  spread(key=Species, value=N, fill=0)
landbirdcounts <- landbirdsurveys %>% left_join(landbirdcounts, by="LandbirdSurveyID", fill=0) %>%
  mutate(GOBU=ifelse(is.na(GOBU),0,GOBU),GOMO=ifelse(is.na(GOMO),0,GOMO))

seabirdcounts<-seabirdcounts %>% filter(Date>=startreportperiod) %>% filter(Date<=endreportperiod)
deaths<-deaths %>% filter(Date>=startreportperiod) %>% filter(Date<=endreportperiod)




#############################################################################
##   3. SUMMARISE THE MONTHLY MONITORING DATA ######################
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
##   4. SUMMARISE THE GOBU RESIGHTING DATA ######################
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
  ##   5. COMBINE THE MOORHEN DATA INTO A SINGLE TABLE WITH SUMS OF RECORDS EVERY 2 WEEKS ####
  ###################################################################################
  
  GOMOsummary<-bind_rows(GOMOopp,GOMOcam,GOMOplay) %>%
    filter(Date<endreportperiod) %>%
    mutate(week=isoweek(Date),year=year(Date)) %>%
    # mutate(fortnight=round(week*2/2)) %>%   ### removed this from table format on 7 Dec 2022, moved to monthly summaries
    mutate(month=month(Date)) %>% 
    # filter(year>2021) %>%
    # filter(fortnight>40) %>% select(Date,week,year,fortnight,Number) %>% arrange(Date) %>%
    # mutate(first=ISOweek::ISOweek2date(paste(year,"-","W",ifelse(fortnight<10,paste("0",as.character(fortnight),sep=""),fortnight),"-1",sep=""))) %>%
    #mutate(first=ISOweek::ISOweek2date(paste(year,"-","W",ifelse(fortnight<10,paste("0",as.character(fortnight),sep=""),fortnight),"-1",sep=""))) %>%
    mutate(first=as.Date(ymd(paste(year,month,"01",sep="-")))) %>%
    group_by(year,month,first,Type) %>%
    summarise(N=sum(Number)) %>%
    spread(key=Type, value=N, fill=0) %>%
    filter(year>2021) %>%
    # filter(fortnight>4) %>%
    mutate(Total=camera+opportunistic+playback) %>%
    #mutate(first=ISOweek::ISOweek2date(paste(year,"-","W",ifelse(fortnight<10,paste("0",as.character(fortnight),sep=""),fortnight),"-1",sep=""))) %>%
    ungroup() %>%
    #filter(month(first)>3) %>%      ## Andrew C wants only last 4 months
    arrange(first) %>%
    mutate(Month=month.abb[month]) %>%
    mutate(Year=year(first)) %>%
    select(Month,Year,opportunistic,playback,camera,Total,first)
  
  # export<-GOMOsummary %>% mutate(FortnightStart=format(first,format="%d-%b-%y")) %>%
  #   select(FortnightStart,opportunistic,playback,camera,Total)
  
#fwrite(GOMOsummary,sprintf("GOMO_Update_%s.csv",format(Sys.time(), '%d%B%Y')))
  
  ###################################################################################
  ##   6. PRODUCE MOORHEN GRAPH FOR DETECTIONS ####
  ###################################################################################
  
  bind_rows(GOMOopp,GOMOcam,GOMOplay) %>%
    filter(Date<endreportperiod) %>%
    mutate(week=isoweek(Date),year=year(Date), month=month(Date)) %>%
    mutate(first=as.Date(ymd(paste(year,month,"01",sep="-")))) %>%
    # mutate(fortnight=round(week/2,0)*2) %>%
    # mutate(first=ISOweek::ISOweek2date(paste(year,"-","W",ifelse(fortnight<10,paste("0",as.character(fortnight),sep=""),fortnight),"-1",sep=""))) %>%
    # group_by(year,fortnight,first,Type) %>%
    group_by(year,month,first,Type) %>%
    summarise(N=sum(Number)) %>%
    spread(key=Type, value=N, fill=0) %>%
    gather(key="Type",value="N",-year,-month,-first) %>%
    filter(year>2021) %>%
    #filter(month>) %>%
    #filter(month(first)>3) %>%      ## Andrew C wants only last 4 months
    mutate(first=as.Date(first)) %>% 
    
    ### create plot
    ggplot()+
    geom_line(aes(x=first, y=N, col=Type), size=1,linetype=2)+
    geom_line(data=GOMOsummary,aes(x=as.Date(first), y=Total), size=2,linetype=1,col="darkred")+
    
    ## format axis ticks
    scale_y_continuous(name="Total number of moorhen detections", limits=c(0,50),breaks=seq(0,50,10))+
    scale_x_date(name="",date_breaks="1 month", date_labels="%b")+
    
    ### add the gimicky moorhen icon
    annotation_custom(GOMOicon, xmin=as.Date(ymd("2022-01-01")), xmax=as.Date(ymd("2022-02-01")), ymin=40, ymax=50) +
    
    
    ## beautification of the axes
    theme(panel.background=element_rect(fill="white", colour="black"), 
          axis.text.y=element_text(size=16, color="black"), 
          axis.text.x=element_text(size=16, color="black"), 
          axis.title=element_text(size=18), 
          strip.text=element_text(size=18, color="black"),
          legend.text=element_text(size=14, color="black"),
          legend.title=element_text(size=18, color="black"),
          legend.key=element_blank(),
          legend.position=c(0.25,0.87),
          strip.background=element_rect(fill="white", colour="black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank())
  
#ggsave(sprintf("GOMO_trend_plot_%s.jpg",format(Sys.time(), '%d%B%Y')),width=10,height=7)
  
  
  ###################################################################################
  ##   7. CREATE MOORHEN KML FILE TO LOOK AT ALL DETECTIONS ON A MAP ####
  ###################################################################################
  
  # GOMOall<-bind_rows(GOMOopp,GOMOcam,GOMOplay, GOMOrel) %>%
  #   mutate(icon=ifelse(Type=="camera","C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens\\camera.jpg",
  #                      ifelse(Type=="opportunistic","C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens\\observer.jpg",
  #                             ifelse(Type=="release","C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens\\moorhenREL.jpg",
  #                                    "C:\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens\\speaker.jpg")))) %>%
  GOMOall<-bind_rows(GOMOopp,GOMOcam,GOMOplay, GOMOrel) %>%
    filter(Date<endreportperiod) %>%
    mutate(icon=ifelse(Type=="camera","http://maps.google.com/mapfiles/kml/pal4/icon38.png",
                       ifelse(Type=="opportunistic","http://maps.google.com/mapfiles/kml/paddle/O.png",
                              ifelse(Type=="release","http://maps.google.com/mapfiles/kml/shapes/volcano.png",
                                     "http://maps.google.com/mapfiles/kml/shapes/hospitals.png")))) %>%
    # mutate(icon=ifelse(Type=="camera","C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens\\camera.jpg",
    #                    ifelse(Type=="opportunistic","C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens\\observer.jpg",
    #                           ifelse(Type=="release","C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens\\moorhenREL.jpg",
    #                                  "C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\ANALYSIS\\Moorhens\\speaker.jpg")))) %>%
    filter(ifelse(Type!="release",Date>ymd_hms("2022-02-01 12:00:00"),Date>ymd("2020-01-01"))) %>%
    filter(!is.na(Date)) %>%
    filter(Number>0) %>%
    mutate(DAY=yday(Date)) %>%
    arrange(DAY)
  
  GOMOall %>% filter(is.na(Latitude))
  ## add random noise over location to avoid overplotting
  GOMOall$Latitude<-GOMOall$Latitude+rnorm(length(GOMOall$Latitude),0.0001,0.0002)
  GOMOall$Longitude<-GOMOall$Longitude+rnorm(length(GOMOall$Latitude),0.0001,0.0002)
  
  SPDF<-SpatialPointsDataFrame(coords=SpatialPoints(GOMOall[GOMOall$Date>=(startreportperiod-months(1)),4:3], proj4string=CRS("+proj=longlat +datum=WGS84")),data=GOMOall[GOMOall$Date>=(startreportperiod-months(1)),c(2,5,6,7,8)])
  kmlPoints(obj=SPDF, kmlfile="Gough_moorhen_detections.kml", kmlname="Gough_moorhen_detections", kmldescription="",
            name=SPDF@data$Date, description=SPDF@data$Notes,
            icon=SPDF@data$icon)

  #############################################################################
  ##   8. CREATE STATIC MAP OF MOORHEN DETECTIONS PER MONTH ####
  #############################################################################
  ## ADD BASE TO MAP
  BASE<-location %>%
    filter(LocationName %in% c("Weather station","Helipad")) %>%
    select(LocationName,Latitude,Longitude)
  
  ## DOWNLOAD MAP FOR BACKGROUND
  # Goughsat <- get_stamenmap(bbox = matrix(c(-9.89,-9.87,-40.355,-40.345),ncol=2,byrow=T),
  #                           maptype = "terrain", 
  #                           crop = TRUE,
  #                           zoom = 13)
  head(GOMOall)
  
  GOMOall<-GOMOall %>% mutate(Month=month(Date)) %>% filter(Type!="release") %>% #filter(Month>(month(endreportperiod)-6))
    filter(Date>endreportperiod-months(6)) %>%
    filter(Date<endreportperiod) %>%
    mutate(MonthLab=format(Date,format="%b %Y")) 
  
  ### ensure proper ordering of months across years
  sortedmonths<-GOMOall %>% group_by(Month) %>% summarise(first=min(Date))%>%
    mutate(MonthLab=format(first,format="%b %Y")) %>%
    arrange(first)
  GOMOall$MonthLab<-factor(GOMOall$MonthLab, levels=sortedmonths$MonthLab)
  
  
  
  ggmap(Goughsat) +
    
    geom_point(data=BASE, aes(x=Longitude,y=Latitude),col="grey27",size=4,pch=15) +
    geom_point(data=GOMOrel, aes(x=Longitude,y=Latitude),col="gold",size=4,pch=18) +
    geom_point(data=GOMOall, aes(x=Longitude,y=Latitude,col=Type),size=3,pch=16) +
    xlab("Longitude")+
    ylab("Latitude")+
    #facet_wrap(~Month, ncol=2, labeller= labeller(Month=c("1"=month.abb[1],"2"=month.abb[2],"3"=month.abb[3],"4"=month.abb[4],"5"=month.abb[5],"6"=month.abb[6],"7"=month.abb[7],"8"=month.abb[8],"9"=month.abb[9],"10"=month.abb[10],"11"=month.abb[11],"12"=month.abb[12]))) +
    #facet_wrap(~Month, ncol=2, labeller= labeller(Month=GOMOall$MonthLab)) +
    facet_wrap(~MonthLab, ncol=2) +
    
    ## beautification of the axes
    theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text=element_text(size=12, color="black"),
          plot.title=element_text(size=16, color="black"),
          axis.title=element_text(size=14), 
          legend.text=element_text(size=14, color="black"),
          legend.background=element_blank(),
          legend.title=element_text(size=18, color="black"),
          legend.key=element_blank(),
          legend.position=c(0.38,0.77),
          strip.text=element_text(size=16, color="black"), 
          strip.background=element_rect(fill="white", colour="black"))
  
#ggsave(sprintf("GOMO_sightings_map_%s.jpg",format(Sys.time(), '%d%B%Y')),width=10,height=10)
  
  
  
  
  




##################################################################
### PRODUCE OUTPUT REPORT WITH KEY TABLES ###
##################################################################
## https://stackoverflow.com/questions/25407102/conditionally-display-a-block-of-text-in-r-markdown
## NEED TO FIX CONDITIONAL CODE CHUNKS

### create HTML report for overall summary report
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files (x86)/RStudio/bin/pandoc")
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")

# rmarkdown::render('C:\\Users\\Gough Conservation\\Documents\\Gough Birders\\2018-2019\\12.Monthly reports 2018-19\\MonthlyReport.Rmd',
#                   output_file = sprintf("MonthlyReport%s.html",format(Sys.time(), '%d%B%Y')),
#                   output_dir = 'C:\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport')

### HTML OUTPUT - has table of kontents but is >2MB
# rmarkdown::render('C:\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport\\MonthlyReport_G68.Rmd',
#                   output_file = sprintf("MonthlyReport%s.html",format(Sys.time(), '%d%B%Y')),
#                   output_dir = 'C:\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport')

### PDF OUTPUT - no table of contents but is 250kB
## need to fix figure captions in the Rmd file
rmarkdown::render('C:\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport\\MonthlyReport_G68_pdf.Rmd',
                  output_file = sprintf("MonthlyReport%s.pdf",format(Sys.time(), '%d%B%Y')),
                  output_dir = 'C:\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport')

rmarkdown::render('C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport\\MonthlyReport_G68_pdf.Rmd',
                  output_file = sprintf("MonthlyReport%s.pdf",format(Sys.time(), '%d%B%Y')),
                  output_dir = 'C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\MonthlyReport')


