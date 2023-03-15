############################################################################
######## PREPARE COMPREHENSIVE ANNUAL REPORT ##############
############################################################################
### script prepared by Antje Steinfurth and Steffen Oppel
### expanded on 7 May 2020 to include species-specific sections in report
## this script only does the general data import and formatting, and then calls the Rmd file to create report
## most tables and figures are embedded in the markdown file, and NOT in this script

## modified on 18 August 2020 to include density for ATPE and GRSH based on Mark Bolton's suggestion
# !diagnostics off

## modified on 26 July 2021: removed 'TERR' from AYNA nest success area counts because it caused difference among tables

## modified on 13 June 2022: no chick counts for AYNA - reduced table
### NEED TO PUT QUERY EXPORT APPENDIX ANNUAL REPORT BACK INTO CMR DATABSE

## updated on 17 Aug 2022 to include eradication text and update breeding success estimates

library(tidyverse)
library(lubridate)
library(data.table)
library(knitr)
library(rmarkdown)
library(dplyr)
library(janitor)
filter<-dplyr::filter
select<-dplyr::select

#############################################################################
##   1. SPECIFY THE YEAR FOR WHICH YOU WANT A SUMMARY ####
#############################################################################
## ENTER DATES MANUALLY OR USE AUTOMATED CODE BELOW

today<-as.Date(Sys.time())
startreportperiod<- ymd("2020-12-01")  #today-months(14)
endreportperiod<- ymd("2022-07-31")  #today-months(2)
BTOsubmitdate<- ymd("2021-12-11")  
SAFRINGsubmitdate<- ymd("2021-12-31")  







###################################################################################
##   2. READ IN DATA FROM DATABASES AND FILTER DATA FROM LAST YEAR ####
###################################################################################
### NEED TO DO THIS MANUALLY AS R4.0.2 somehow does not execute this line
## run the RODBC import in a 32-bit version of R

#library(RODBC)
system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\AnnualReport\\RODBC_imports.r")), wait = TRUE, invisible = FALSE, intern = T)
system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\AnnualReport\\RODBC_imports.r")), wait = TRUE, invisible = FALSE, intern = T)
#system(paste0(Sys.getenv("R_HOME"), "/bin/i386/Rscript.exe ", shQuote("C:\\Users\\Gough Conservation\\Documents\\Gough Birders\\2018-2019\\12.Annual reports 2018-19\\RODBC_imports.r")), wait = FALSE, invisible = FALSE)
try(setwd("C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\AnnualReport"), silent=T)
try(setwd("C:\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\AnnualReport"), silent=T)

load("ANNUAL_REPORT_DATA2022.RData")
head(contacts)  ## CMR data
head(landbirdcounts)  ## landbird count data
head(seabirdcounts)  ## seabird count data
head(deaths)  ## incidental records
head(nests)  ## nest records
head(species)  ## species names

## filter data for the reporting period
# removed filters for counts where we need longer time horizon
contacts<-contacts %>% filter(Date_Time>startreportperiod) %>% filter(Date_Time<endreportperiod) #%>%
    #filter(BirdID!="GO242398381") ### remove single contact with wrong date from previous July
nestvisits<-nestvisits %>% filter(Date>startreportperiod) #%>% filter(Date<endreportperiod)
# landbirdcounts<-landbirdcounts %>% filter(Date>startreportperiod) %>% filter(Date<endreportperiod)
# seabirdcounts<-seabirdcounts %>% filter(Date>startreportperiod) %>% filter(Date<endreportperiod) 
deaths<-deaths %>% filter(Date>startreportperiod) %>% filter(Date<endreportperiod)
# nests<-nests %>% filter(DateFound>startreportperiod) %>% filter(DateLastChecked<endreportperiod)






#############################################################################
##  1. SUMMARISE THE BREEDING SUCCESS DATA FROM NEST RECORDS #################
#############################################################################

### remove only partially monitored nests
exclude <- nests %>% filter(DateFound>startreportperiod) %>% filter(DateLastChecked<endreportperiod) %>%
  filter(LastStage=="INCU") %>%
  filter(SUCCESS==1)

### summary of breeding success per year from nests
BreedingSuccess<-nests %>% filter(Year==year(startreportperiod)+1) %>% #filter(DateLastChecked<endreportperiod) %>%
  mutate(count=1) %>%
  mutate(Speciesname=species$Species[match(Species,species$SpeciesCode)]) %>%
  filter(!NestID %in% exclude$NestID) %>%
  group_by(Speciesname) %>%
  summarise(n_nests=sum(count),BREED_SUCC=mean(SUCCESS, na.rm=T)*100)
BreedingSuccess$BREED_SUCC<-round(BreedingSuccess$BREED_SUCC,1)

### summarise breeding success for Great Shearwater from burrow counts FOR THE GENERAL SUMMARY
## in 2022 there was no fledging count at Tumbledown, so that is excluded
GRSH_succ<-seabirdcounts %>% filter(Date>startreportperiod+months(6)) %>% filter(Date<endreportperiod) %>% 
  filter(Cohort %in% c("CHIC","INCU","AON","EGG","ATTC","LOAF")) %>%  ### added LOAF in 2021 because adult counts were lower than chick counts
  filter(Colony %in% c("Tumbledown","Snoekgat","Seal Beach")) %>%
  mutate(Site=if_else(grepl("Q",Site)==T,str_remove(Site, "Q"),Site)) %>%
  mutate(Quad=paste(Colony,Site,sep="")) %>%
  mutate(month=month(Date)) %>%
  filter(Species=="GRSH") %>%
  filter (Quad %in% c("Snoekgat2","Snoekgat4","Snoekgat8","Seal Beach1","Seal Beach5","Seal Beach7","Tumbledown1","Tumbledown2","Tumbledown8")) %>%
  mutate(Cohort=ifelse(Cohort %in% c("INCU","AON","EGG","OCCU","LOAF"),"ADULT","CHIC")) %>%   ### added LOAF in 2021 because adult counts were lower than chick counts
  mutate(Cohort=ifelse(Cohort=="CHIC" & month %in% c(1,2),"CHICKhatched",ifelse(Cohort=="CHIC" & month>2,"FLEDGLING",ifelse(Cohort=="ADULT" & month %in% c(1,2),"CHICKhatched",Cohort)))) %>%
  group_by(Cohort,Colony) %>%
  #filter(!(Colony=="Tumbledown")) %>%
  summarise(n_birds=sum(Number)) %>%
  spread(key=Cohort,value=n_birds) %>%
  mutate(CHICKhatched=if_else(FLEDGLING>CHICKhatched,FLEDGLING,CHICKhatched)) %>%
  mutate(ADULT=if_else(CHICKhatched>ADULT,CHICKhatched,ADULT)) %>%
  adorn_totals("row") %>%
  mutate(hatch.succ=(CHICKhatched)/(ADULT), fledg.succ=(FLEDGLING)/(CHICKhatched), breed.succ=(FLEDGLING)/(ADULT))


### summarise breeding success for Atlantic Petrel from nest records up to 2018
if(year(startreportperiod)+1 <2019){
  ATPE_succ<-nests %>% filter(year(DateFound) == year(startreportperiod)+1) %>%
    #filter(Cohort %in% c("CHIC","INCU","AON","EGG","OCCU")) %>%
    filter(Species=="ATPE") %>%
    mutate(count=1) %>%
    mutate(Speciesname=species$Species[match(Species,species$SpeciesCode)]) %>%
    filter(!NestID %in% exclude$NestID) %>%
    group_by(Speciesname) %>%
    summarise(ADULT=sum(count),breed.succ=mean(SUCCESS, na.rm=T))
}


### summarise breeding success for Atlantic Petrel from burrow counts from 2019 onwards
## based on 6 quadrats
if(year(startreportperiod)+1 >2018){
ATPE_succ<-seabirdcounts %>% filter(year(Date) == year(startreportperiod)+1) %>%
  filter(!Cohort %in% c("LOAF","NOBR","TERR","UNOC")) %>%
  filter (Site %in% c(1,2,13,23,26,27,32,38,39)) %>%
  filter(Species=="ATPE") %>%
  mutate(month=month(Date)) %>%
  mutate(Cohort=ifelse(Cohort %in% c("INCU","AON","EGG","OCCU"),"ADULT","CHIC")) %>%
  mutate(Cohort=ifelse(Cohort=="CHIC" & month %in% c(8,9),"CHICKhatched",ifelse(Cohort=="CHIC" & month>9,"FLEDGLING",ifelse(Cohort=="ADULT" & month %in% c(9),"CHICKhatched",Cohort)))) %>%
  filter(!(Cohort=="ADULT" & month>9)) %>%   ## remove unhatched eggs etc. that may be counted in December
  group_by(Cohort,Colony) %>%
  summarise(n_birds=sum(Number)) %>%
  spread(key=Cohort,value=n_birds) %>%
  adorn_totals("row") %>%
  mutate(hatch.succ=(CHICKhatched)/(ADULT), fledg.succ=(FLEDGLING)/(CHICKhatched), breed.succ=(FLEDGLING)/(ADULT))
}

### ADD SUMMARY FOR GRSH and ATPE

BreedingSuccess <- GRSH_succ[4,c(2,7)] %>% mutate(Speciesname="Great Shearwater") %>% rename(n_nests=ADULT,BREED_SUCC=breed.succ) %>% mutate(BREED_SUCC=ifelse(BREED_SUCC<1,BREED_SUCC*100,BREED_SUCC)) %>% select(Speciesname,n_nests,BREED_SUCC) %>%
  bind_rows((ATPE_succ[4,c(2,7)] %>% mutate(Speciesname="Atlantic Petrel") %>% rename(n_nests=ADULT,BREED_SUCC=breed.succ) %>% mutate(BREED_SUCC=ifelse(BREED_SUCC<1,BREED_SUCC*100,BREED_SUCC)) %>% select(Speciesname,n_nests,BREED_SUCC))) %>%
  bind_rows(BreedingSuccess) %>%
  mutate(BREED_SUCC=round(BREED_SUCC,1)) %>%
  arrange(BREED_SUCC)
BreedingSuccess


########################################################################################################################################
##  ADD BREEDING SUCCESS FROM AREA COUNTS FOR AYNA, SOAL AND TRAL BECAUSE JO GILBERT WANTED THIS UP FRONT IN THE TABLE #################
########################################################################################################################################

## changed from 2021 because no chick count was done for AYNA
ayna_breed_succ<-seabirdcounts %>% filter(Date > startreportperiod+months(6)) %>% filter(Date < endreportperiod) %>%
  mutate(Year=year(Date)) %>%
  filter(Cohort %in% c("INCU","AON","CHIC","EGG","ATTC","FLED","BROO","AD")) %>%
  filter(Species=="AYNA") %>%
  mutate(month=month(Date)) %>%
  filter(!(Year==(year(startreportperiod)+1) & month<6)) %>% ## remove the fledgling count from previous season
  filter(!(Year==year(endreportperiod) & month>6)) %>% ## remove the incubator count from next season
  mutate(Cohort=ifelse(Cohort %in% c("INCU","AON","EGG","TERR","AD"),"BREED_PAIR","CHIC")) %>%
  mutate(Cohort=ifelse(Cohort=="CHIC" & month %in% c(11,12),"CHICKhatched",ifelse(Cohort=="CHIC" & month<6,"FLEDGLING",ifelse(Cohort=="BREED_PAIR" & month %in% c(11,12),"CHICKhatched",Cohort)))) %>%
  group_by(Cohort,Colony) %>%
  summarise(n_birds=sum(Number)) %>%
  filter(!(Cohort=="FLEDGLING" & Colony=="Area 1")) %>% ## remove the fledgling count in Area 1 to avoid confusion with Fig. 2.2
  spread(key=Cohort,value=n_birds) %>%
  mutate(order=as.numeric(gsub("Area", "", Colony))) %>%
  arrange(order) %>%
  select(-order) %>%
  adorn_totals("row") %>%
  mutate(#hatch.succ=round(((CHICKhatched)/(BREED_PAIR))*100,1),
         #fledg.succ=round(((FLEDGLING)/(CHICKhatched))*100,1),
         breed.succ=round(((FLEDGLING)/(BREED_PAIR))*100,1))
PAIRS<-sum(ayna_breed_succ[!is.na(ayna_breed_succ$FLEDGLING),2])-ayna_breed_succ[12,2]
#ayna_breed_succ[12,5]<-round((ayna_breed_succ[12,3]/PAIRS)*100,1)
#ayna_breed_succ[12,7]<-round((ayna_breed_succ[12,4]/PAIRS)*100,1)
ayna_breed_succ[12,4]<-round((ayna_breed_succ[12,3]/PAIRS)*100,1)  ## when no chick count happened then no hatch/fledg success is calculated


#fwrite(ayna_breed_succ,"AYNABreedSucc2020.csv")

SOAL_breed_succ<-seabirdcounts %>% filter(Date > startreportperiod +months(7)) %>% filter(Date < endreportperiod) %>%
  mutate(Year=year(Date)) %>%
  filter(Cohort %in% c("INCU","AON","CHIC","EGG","ATTC","FLED","BROO","TERR")) %>%
  filter(Species=="SOAL") %>%
  filter(!Colony %in% c("Seal Beach","Crane Point")) %>%  ## these counts are not needed for this table
  mutate(month=month(Date)) %>%
  filter(!(Year==(year(startreportperiod)+1) & month<6)) %>% ## remove the fledgling count from previous season
  filter(!(Year==year(endreportperiod) & month>6)) %>% ## remove the incubator count from next season
  mutate(Cohort=ifelse(Cohort %in% c("INCU","AON","EGG","TERR"),"BREED_PAIR","CHIC")) %>%
  mutate(Cohort=ifelse(Cohort=="CHIC" & month %in% c(12,1),"CHICKhatched",ifelse(Cohort=="CHIC" & month %in% c(3,4,5),"FLEDGLING",ifelse(Cohort=="BREED_PAIR" & month %in% c(12,1),"CHICKhatched",Cohort)))) %>%
  group_by(Cohort,Colony) %>%
  summarise(n_birds=sum(Number)) %>%
  spread(key=Cohort,value=n_birds) %>%
  mutate(FLEDGLING=ifelse(is.na(FLEDGLING) & CHICKhatched==0,0,FLEDGLING)) %>%
  mutate(CHICKhatched=ifelse(is.na(CHICKhatched) & !is.na(FLEDGLING),FLEDGLING,CHICKhatched)) %>%
  mutate(CHICKhatched=ifelse(FLEDGLING>CHICKhatched,FLEDGLING,CHICKhatched)) %>%
  mutate(BREED_PAIR=ifelse(!is.na(CHICKhatched) & CHICKhatched>BREED_PAIR,CHICKhatched,BREED_PAIR)) %>%
  mutate(order=as.numeric(gsub("SOAL Plot ", "", Colony))) %>%
  arrange(order) %>%
  select(-order) %>%
  adorn_totals("row") %>%
  mutate(hatch.succ=round(((CHICKhatched)/(BREED_PAIR))*100,1), fledg.succ=round(((FLEDGLING)/(CHICKhatched))*100,1), breed.succ=round(((FLEDGLING)/(BREED_PAIR))*100,1)) %>%
  mutate(fledg.succ=ifelse(fledg.succ>100,100,fledg.succ))
PAIRS<-sum(SOAL_breed_succ[!is.na(SOAL_breed_succ$FLEDGLING),2])-SOAL_breed_succ[dim(SOAL_breed_succ)[1],2]
SOAL_breed_succ[dim(SOAL_breed_succ)[1],5]<-round((SOAL_breed_succ[dim(SOAL_breed_succ)[1],3]/PAIRS)*100,1)
SOAL_breed_succ[dim(SOAL_breed_succ)[1],7]<-round((SOAL_breed_succ[dim(SOAL_breed_succ)[1],4]/PAIRS)*100,1)

#fwrite(SOAL_breed_succ,"SOALBreedSucc2020.csv")


TRAL_breed_succ<-seabirdcounts %>%
  filter(Species=="TRAL") %>%
  mutate(Year=year(Date)) %>%
  filter(Year==(year(startreportperiod)+1)) %>%
  filter(Cohort %in% c("INCU","AON","CHIC","TERR")) %>%
  mutate(Cohort=ifelse(Cohort %in% c("INCU","AON","TERR"),"ADULT","CHICK")) %>%
  group_by(Cohort,Colony) %>%
  summarise(n_birds=sum(Number)) %>%
  spread(key=Cohort,value=n_birds) %>%
  mutate(breed.succ=round(((CHICK)/(ADULT))*100,1)) %>%
  arrange(breed.succ) %>%
  adorn_totals("row") %>%
  mutate(breed.succ=round(((CHICK)/(ADULT))*100,1)) 

BreedingSuccess<-ayna_breed_succ %>% filter(Colony=="Total") %>% select(breed.succ) %>% mutate(Speciesname="Atlantic Yellow-nosed Albatross") %>%
  bind_rows(SOAL_breed_succ %>% filter(Colony=="Total") %>% select(breed.succ) %>% mutate(Speciesname="Sooty Albatross")) %>%
  bind_rows(TRAL_breed_succ %>% filter(Colony=="Total") %>% select(breed.succ) %>% mutate(Speciesname="Tristan Albatross")) %>%
  full_join(BreedingSuccess, by="Speciesname") %>%
  select(Speciesname,n_nests,BREED_SUCC,breed.succ) %>%
  arrange(BREED_SUCC)
BreedingSuccess

#fwrite(BreedingSuccess,"GoughBreedSucc2022.csv")
  

#############################################################################
##  2. SUMMARISE CMR DATA OF MARKED INDIVIDUALS #################
#############################################################################

### fix spelling and inconsistent labels
contacts<-contacts %>% mutate(Age=ifelse(is.na(Age),"Adult",as.character(Age))) %>%
  mutate(Age=ifelse(Age %in% c("Fledgling","Juvenile","0-6 months"),"Chick",as.character(Age))) %>%
  mutate(Contact_Type=ifelse(Contact_Type=="Recapture","Resight",as.character(Contact_Type))) %>%
  mutate(Contact_Type=ifelse(Contact_Type=="Capture","Ringed",as.character(Contact_Type))) %>%
  mutate(SpeciesCode=ifelse(SpeciesCode=="BRSK","SKUA",as.character(SpeciesCode))) %>%
  mutate(SpeciesCode=ifelse(SpeciesCode=="SUSK","SKUA",as.character(SpeciesCode))) %>%
  mutate(Speciesname=species$Species[match(SpeciesCode,species$SpeciesCode)])  
head(contacts)


### FIX RECORDS WHERE A 'CHICK' WAS RESIGHTED 
contacts %>% filter(Age=="Chick") %>% filter(Contact_Type=="Resight")
contacts %>% filter(Age=="Chick") %>% filter(SpeciesCode=="SKUA")
## these are not actually fake records - these are the chicks that hadn't fledged at that time


### summary of birds newly ringed per year 
RingSummary<-contacts %>%
  filter(!(Contact_Type=='Recovery'))  %>%
  filter(!(Contact_Type=='Band Only'))  %>%
  filter(!(Contact_Type=='Release'))  %>%
  group_by(Speciesname, Contact_Type, Age) %>%
  summarise(n_individuals=length(unique(BirdID))) %>%
  ungroup() %>%
  mutate(Group=paste(Age,Contact_Type,sep="_")) %>%
  filter(Group!="Chick_Resight") %>%
  select(Speciesname,Group,n_individuals) %>%
  spread(key=Group, value=n_individuals, fill=0) %>%
  arrange(desc(Adult_Resight))%>%
  mutate(Speciesname=ifelse(Speciesname=="Subantarctic Skua","Tristan Skua",Speciesname))
RingSummary


### summary of all rings deployed in a given year
RingDeployAppendix<-ringdeployments %>%
  dplyr::filter(Mark_Type %in% c('Federal',"Plastic FR"))  %>%
  mutate(Mark_Type=ifelse(Mark_Type=="Plastic FR","darvic","metal")) %>%
  filter(Date_Time> (endreportperiod-years(1)))  %>%
  mutate(Speciesname=species$Species[match(SpeciesCode,species$SpeciesCode)]) %>%
  select(Speciesname,Age,Location,Mark_Type,Band_Color,Band_Size,Band_Number) %>%
  arrange(Speciesname,Mark_Type,Band_Number)
RingDeployAppendix
dim(RingDeployAppendix)
unique(RingDeployAppendix$Mark_Type)
RingDeployAppendix %>% filter(Mark_Type=="darvic")
#View(RingDeployAppendix)


#############################################################################
##  3. SUMMARISE RECOVERIES OF DEAD BIRDS #################
#############################################################################

### summary of dead birds found
death_summary<-deaths %>% filter(RecordType==2) %>%
  mutate(Speciesname=species$Species[match(Species,species$SpeciesCode)])  %>%
  mutate(Speciesname=ifelse((Species %in% species$SpeciesCode),Speciesname,Species)) %>%
  group_by(Speciesname) %>%
  summarise(n_unringed=sum(Number))
death_summary

recovery<-contacts %>% mutate(count=1) %>% filter(Contact_Type=="Recovery") %>%  mutate(count=1) %>% 
  mutate(Age=ifelse(is.na(Age),"Adult",as.character(Age))) %>%
  mutate(Speciesname=species$Species[match(SpeciesCode,species$SpeciesCode)])  %>%
  group_by(Speciesname) %>%
  summarise(n_ringed=sum(count))
recovery
DeathSummary<-full_join(death_summary, recovery, by='Speciesname') %>%
      mutate(n_unringed=ifelse(is.na(n_unringed), 0, n_unringed)) %>%
      arrange(desc(n_unringed)) %>%
      mutate(Speciesname=ifelse(Speciesname=="Subantarctic Skua","Tristan Skua",Speciesname))
DeathSummary



#############################################################################
##  4. SUMMARISE ABUNDANCE OF LANDBIRDS #################
#############################################################################
## requires merging of 2 tables to include 0 counts
head(landbirdsurveys)
head(landbirdcounts)

## specify the habitat for each transect
habitat<-landbirdcounts %>% group_by(Transect) %>% summarise(Habitat_description=first(Habitat_description, na.rm=T))

## combine all the data
landbird_trend<-landbirdsurveys %>%
  select(LandbirdSurveyID,Transect,Date,Start_time) %>%
  left_join(landbirdcounts, by=c('Transect','Date','Start_time')) %>%
  mutate(Habitat_description=habitat$Habitat_description[match(Transect,habitat$Transect)]) %>%
  #landbirdcounts %>% 
  #filter(month(Date) %in% c(1,2,3)) %>%
  gather(key="Species", value="Count",-LandbirdSurveyID,-Transect,-Habitat_description,-Date,-Start_time) %>% #filter(Habitat_description !="Coastal tussock") %>% ## remove tussock as transect 13 was forgotten in 2022
  mutate(Species=species$Species[match(Species,species$SpeciesCode)]) %>%
  mutate(Count=ifelse(is.na(Count),0,Count)) %>%
  mutate(Year=year(Date)) %>%
  group_by(Habitat_description,Year, Species) %>%
  summarise(mean=mean(Count,na.rm=T),lcl=mean(Count,na.rm=T)-0.5*sd(Count,na.rm=T),ucl=mean(Count,na.rm=T)+0.5*sd(Count,na.rm=T)) %>%
  mutate(lcl=ifelse(lcl<0,0,lcl)) %>%
  filter(!(Species=="Gough Bunting" & Habitat_description %in% c("Fern bush","Coastal tussock"))) %>%   ## remove counts in suboptimal habitat
  filter(!(Species=="Gough Moorhen" & Habitat_description %in% c("Moorland","Sphagnum bogs")))   ## remove counts in suboptimal habitat

### create plot
gg31<-ggplot(landbird_trend)+
  geom_point(aes(x=Year, y=mean,col=Habitat_description),size=2,position=position_dodge(0.15)) +
  geom_line(aes(x=Year, y=mean,col=Habitat_description),size=1,) +
  geom_errorbar(aes(x=Year, ymin=lcl, ymax=ucl,col=Habitat_description), width=.1,position=position_dodge(0.15))+
  facet_wrap(~Species, ncol=1) +
  #geom_smooth(aes(x=Year, y=mean),method="loess") +
  
  ## format axis ticks
  guides(color=guide_legend(title="Habitat"))+
  scale_y_continuous(name="Mean number of birds per 100 m transect", limits=c(0,6), breaks=seq(0,6,1)) +
  scale_x_continuous(name="", limits=c(2015.75,2022.25), breaks=seq(2016,2022,1)) +
  
  
  ## beautification of the axes
  theme(panel.background=element_rect(fill="white", colour="black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y=element_text(size=16, color="black"),
        axis.text.x=element_text(size=16, color="black"), 
        axis.title=element_text(size=16),
        legend.title=element_text(size=14),
        legend.text=element_text(size=12),
        legend.background=element_blank(),
        legend.key=element_blank(),
        legend.position=c(0.12, 0.91),
        strip.text.x=element_text(size=16, color="black"),
        strip.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(margin=margin(0,20,0,0)), 
        strip.background=element_rect(fill="white", colour="black"))

gg31



##################################################################
### PRODUCE OUTPUT REPORT WITH KEY TABLES ###
##################################################################


### create HTML report for overall summary report
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files (x86)/RStudio/bin/pandoc")
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")

# rmarkdown::render('C:\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\AnnualReport\\FullAnnualReport_bySpecies2022.Rmd',
#                   output_file = sprintf("FullAnnualReport%s.html",format(Sys.time(), '%d%B%Y')),
#                   output_dir = 'C:\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\AnnualReport')

rmarkdown::render('C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\AnnualReport\\FullAnnualReport_bySpecies2022.Rmd',
                  output_file = sprintf("FullAnnualReport%s.html",format(Sys.time(), '%d%B%Y')),
                  output_dir = 'C:\\STEFFEN\\OneDrive - THE ROYAL SOCIETY FOR THE PROTECTION OF BIRDS\\STEFFEN\\RSPB\\UKOT\\Gough\\Reports\\ScriptedReports\\AnnualReport')




##################################################################
### SCRATCH-PAD: just copy code chunks from RMD here to TROUBLESHOOT ####
##################################################################


seabirdcounts %>% filter(year(Date) == year(startreportperiod)+1) %>%
  filter(!Cohort %in% c("LOAF","NOBR","TERR","UNOC")) %>%
  filter (Site %in% c(1,2,13,23,26,27,32,38,39)) %>%
  filter(Species=="ATPE") %>%
  mutate(month=month(Date)) %>% #summarise(unique(Cohort))
  mutate(Cohort=ifelse(Cohort %in% c("INCU","AON","EGG","OCCU"),"ADULT","CHIC")) %>%
  mutate(Cohort=ifelse(Cohort=="CHIC" & month %in% c(8,9),"CHICKhatched",ifelse(Cohort=="CHIC" & month>9,"FLEDGLING",ifelse(Cohort=="ADULT" & month %in% c(9),"CHICKhatched",Cohort)))) %>%
  filter(!(Cohort=="ADULT" & month>9)) %>%
  group_by(Cohort,Colony,Site) %>%
  summarise(n_birds=sum(Number)) %>%
  spread(key=Cohort,value=n_birds) %>%
  adorn_totals("row") %>%
  mutate(hatch.succ=(CHICKhatched)/(ADULT), fledg.succ=(FLEDGLING)/(CHICKhatched), breed.succ=(FLEDGLING)/(ADULT))