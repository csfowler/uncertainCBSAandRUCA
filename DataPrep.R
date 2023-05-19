######################################
# Data Prep Script for Uncertainty for CBSA and RUCA
######################################

#Outcomes:
#1)Crosswalk for 'original' calculation of centrality status for counties.

#2)Flows: County to county commuter flows for 2010 with attached 'Central/Outlying'
#designation, MOE's

#3)Counties: sf object with county FIPS codes and counties for 2010
#3A)Neighborhood object with all of the neighbors of each county

#4)Tracts: Tract to tract commuter flows for 2010 with MOE's and attached RUCA codes

#5)Lodes_Cty: LODES data for county to county comparisons
#5b)Lodes_Tract: LODES data for tract to tract comparisons

#6)Tract and Lodes Tract data with information on UA's attached.
#7)Official RUCA delineation

#######################################
#Items to download before running:
#1) County to County Flows with 2003 and 2010 CBSA designations
#https://www.census.gov/data/tables/time-series/demo/commuting/commuting-flows.html
#Excel file has headings simplified and converted to csv. Retained columns are: St_Res	Cnty_Res	St_Work	Cnty_Work	FlowCount	MOE
# save in "./Input Data/CountyFlow10/ACS 2006-2010.csv"

#2)Official 2010 RUCA codes
#from https://www.ers.usda.gov/webdocs/DataFiles/53241/ruca2010revised.xlsx?v=74.8
#Excel has headings simplified, retain xlsx format. Column names are: STCTYFIPS	STATE	County	FIPS	RUCA1	RUCA2	Pop	LandArea	PopDensity
#save in "./Input Data/ERS RUCA data/RUCA 2010.xlsx"

#######################################
#Thresholds that can be modified:
#urban_area_threashold, this is the minimum size for an urban area to count as metropolitan
urban_area_threshold = 50000
#urban_cluster_threshold, this is the minimum size for an urban cluster to count as micropolitan
urban_cluster_threshold = 10000
CBSA_commuting_threshold = .25
RUCA_commuting_threshold = .30
#share_pop_in_UC_to_be_metro, this can be adjusted to change the share of a county's population living in urban clusters of at least 10,000 people that is required for a county to be considered metropolitan. Federal definition requires 50%
share_pop_in_UC_to_be_metro = 50
#other_share_pop_in_UC_to_be_core comes from the RUCA definition of whether a tract should be considered as part of a core (metro, micro, or small town). Weirdly, the threshold is 30% as opposed to the 50% for counties
other_share_pop_in_UC_to_be_core = .3
#######################################
#Libraries
require(sf)
require(spdep)
require(tidycensus)
#census_api_key("your_api_here",install=TRUE) ##uncomment this line for first use of tidycensus
require(RODBC)
require(tidyr)
require(dplyr)
require(lwgeom)
require(readxl)

source("./AssociatedFunctions.R")
#######################################

#1. Calculation of county Central status from urbanized area population.
#We can't just pull central or outlying status for counties from extant documentation of CBSA's, it has to be calculated from urbanized area population because some of the central counties get merged into larger CBSA's In this segment I download the county to urbanized area crosswalk from the census and calculate a field for Original_status that indicates whether a county could have been central pre merge.
# From the Federal Register the central county or counties of a CBSA are those counties that:
#(a) Have at least 50 percent of their population in urban areas of at least 10,000 population; or
#(b) Have within their boundaries a population of at least 5,000 located in a single urban area of at least 10,000 population.
#A central county is associated with the urbanized area or urban cluster that accounts for the largest portion of the county’s population. The central counties associated with a particular urbanized area or urban cluster are grouped to form a single cluster of central counties for purposes of measuring commuting to and from potentially qualifying outlying counties.

if(!file.exists("./Output Data/crosswalk.Rdata")){

  if(!file.exists("./Input Data/crosswalk.txt")){
    url="http://www2.census.gov/geo/docs/maps-data/data/rel/ua_county_rel_10.txt?"
    download.file(url,destfile="./Input Data/crosswalk.txt")
  }
  cwk<-read.csv("./Input Data/crosswalk.txt")
  cwk<-cwk[,c("UA","STATE","COUNTY","GEOID","POPPT","UAPOP","CPOP","UAPOPPCT","CPOPPCT")]
  cwk$FIPS<-cwk$GEOID #just for clarity on my part
  county_out<-data.frame(FIPS=unique(cwk$FIPS),Criteria_A=NA,Criteria_B=NA)#3234 counties touch a UA in some way
  #Criteria a 50% of pop in UA of at least 10K
  cwk_a<-cwk[cwk$UAPOP>=urban_cluster_threshold,]
  pop_ua10k<-aggregate(cwk_a$CPOPPCT,by=list(cwk_a$FIPS),FUN=sum)
  colnames(pop_ua10k)<-c("FIPS","CTYPOP_UA10K")
  pop_ua10k<-pop_ua10k[pop_ua10k$CTYPOP_UA10K>=share_pop_in_UC_to_be_metro,]
  county_out$Criteria_A<-ifelse(county_out$FIPS%in%pop_ua10k$FIPS,1,0)
  #need to assign UA designation for criteria a
  cwk_a %>% group_by(FIPS) %>% top_n(1, CPOPPCT)%>%select(c(UA,FIPS))->ua_max
  county_out<-merge(county_out,ua_max,by="FIPS",all.x=TRUE)
  county_out$UA<-ifelse(county_out$Criteria_A==1,county_out$UA,NA)
  colnames(county_out)[4]<-"Criteria_A_CBSA"
  #Criteria b 5000 people in a UA of at least 10k
  #cwk_a already grabs the UA records greater than 10K
  crit_b<-cwk_a[cwk_a$POPPT>=5000,]
  crit_b_in<-unique(crit_b$FIPS)
  county_out$Criteria_B<-ifelse(county_out$FIPS%in%crit_b_in,1,0)
  crit_b %>% group_by(FIPS) %>% top_n(1, CPOPPCT)%>%select(c(UA,FIPS))->crit_b_ua
  county_out<-merge(county_out,crit_b_ua,by="FIPS",all.x=TRUE)
  colnames(county_out)[5]<-"Criteria_B_CBSA"
  #if a county fulfills either of the two criteria then it is designated as central
  county_out$Central<-ifelse(county_out$Criteria_A==1 | county_out$Criteria_B==1,1,0)
  test<-county_out[complete.cases(county_out[,c("Criteria_A_CBSA","Criteria_B_CBSA")]),]
  identical(test$Criteria_A_CBSA,test$Criteria_B_CBSA) #A and B always yield the same UA
  county_out$UA_CBSA<-NA
  county_out$UA_CBSA<-apply(county_out,MAR=1,FUN=function(x) ifelse(!is.na(x["Criteria_A_CBSA"]),x["Criteria_A_CBSA"],x["Criteria_B_CBSA"]))
  county_out$ID<-ifelse(!is.na(county_out$UA_CBSA),county_out$UA_CBSA,county_out$FIPS)
  crosswalk<-county_out[order(county_out$FIPS),c("FIPS","Central","UA_CBSA","ID")]
  crosswalk[!is.na(crosswalk$UA_CBSA),"ID"]<-paste0("UA",crosswalk[!is.na(crosswalk$UA_CBSA),"UA_CBSA"])
  colnames(crosswalk)<-c("FIPS","Pre_Merge_Central","UA_CBSA","Pre_Merge_ID")
  cwk2<-unique(cwk[,c("UA","UAPOP")])
  crosswalk<-merge(crosswalk,cwk2,by.x="UA_CBSA",by.y="UA",all.x=TRUE)
  crosswalk<-crosswalk[order(crosswalk$FIPS),]
  #At this stage counties are identified as Central if they meet the basic criteria (A or B). However, at a later stage Central counties can merge and some of these that are currently Central will become outlying to larger urban agglomerations. That is the reason for referring to the Pre-Merge-Central Status and ID
  save(crosswalk,file="./Output Data/crosswalk.Rdata")
}
#######################################
#2)County to county flow data from ACS 2006-2010 linked to 2010 CBSA delineations
# For accurate results it is crucial that data and delineations match up. This report is
# useful in that regard https://www.everycrsreport.com/reports/R42005.html
if(!file.exists("./Output Data/flows.Rdata")){

#1) County to County Flows with 2010 CBSA designations
#https://www.census.gov/data/tables/time-series/demo/commuting/commuting-flows.html
flows<-read.csv(file = "./Input Data/CountyFlow10/ACS 2006-2010.csv")
#get rid of records with no state or county of work as well as some records with invalid St_Work numbers
flows<-flows[complete.cases(flows),]
# Get the variables into consistent format Origin | Destination | Flow
flows$Cnty_Res<-flows$St_Res*1000+flows$Cnty_Res
flows$Cnty_Work<-flows$St_Work*1000+flows$Cnty_Work
flows<-flows[,c("Cnty_Res","Cnty_Work","FlowCount","MOE")]
colnames(flows)[colnames(flows)=="FlowCount"]<-"Flow"
#Remove work destinations abroad
flows<-flows[flows$Cnty_Work<73000,]
#Remove Territories not Puerto Rico
flows<-flows[flows$Cnty_Work<60000 | flows$Cnty_Work > 70000,]

#Metro Definitions 2010
##CBSAs file is based on http://www.census.gov/population/metro/data/def.html
#now moved to https://www.census.gov/programs-surveys/metro-micro.html
metro10 <- read.csv("./Input Data/Metropolitan Definitions/list1_Feb2013CBSA.csv",skip = 2,stringsAsFactors = FALSE)
metro10$FIPS<-(metro10$FIPS.State.Code*1000)+metro10$FIPS.County.Code
metro10<-metro10[,c("CBSA.Code","FIPS","Central.Outlying.County","Metropolitan.Micropolitan.Statistical.Area")]
colnames(metro10)<-c("CBSA","FIPS","Cen.Out","Metro.Micro")
metro10<-metro10[complete.cases(metro10),]

#Link 2010 CBSA and Metro Micro categories to flows
flows<-merge(flows,metro10,by.x="Cnty_Res",by.y="FIPS",all.x=TRUE,sort=FALSE)
colnames(flows)[colnames(flows)%in%c("CBSA","Cen.Out","Metro.Micro")]<-c("Res.CBSA","Res.Cen.Out","Res.Metro.Micro")
flows<-merge(flows,metro10,by.x="Cnty_Work",by.y="FIPS",all.x=TRUE,sort=FALSE)
colnames(flows)[colnames(flows)%in%c("CBSA","Cen.Out","Metro.Micro")]<-c("Wk.CBSA","Wk.Cen.Out","Wk.Metro.Micro")

flows[is.na(flows$Res.Cen.Out),"Res.Cen.Out"]<-"Nonmetro"
flows[is.na(flows$Res.Metro.Micro),"Res.Metro.Micro"]<-"Nonmetro"
flows[is.na(flows$Wk.Cen.Out),"Wk.Cen.Out"]<-"Nonmetro"
flows[is.na(flows$Wk.Metro.Micro),"Wk.Metro.Micro"]<-"Nonmetro"

flows$Sd<-flows$MOE/1.645 #Census defined conversion from MOE to standard deviation

#Generate Origin and Destination fields that will hold either FIPS or CBSA code depending on whether the Work and Residence FIPS values are a core county or not
#These counties (Core) are not subject to 'in' or 'out' designation, they just are 'in' by definition
flows$Destination<-flows$Cnty_Work
flows$Origin<-flows$Cnty_Res
for(i in 1:length(flows[,1])){
  if(flows[i,"Wk.Cen.Out"]=="Central"){
    flows[i,"Destination"]<-flows[i,"Wk.CBSA"]
  }
  if(flows[i,"Res.Cen.Out"]=="Central"){
    flows[i,"Origin"]<-flows[i,"Res.CBSA"]
  }
}
flows$MOEtoFlow<-flows$MOE/flows$Flow #data collection column for use in paper (ratio of MOE to flow)


#need to convert flow to reflect pre-merge origin and destination classifications
load("./Output Data/crosswalk.Rdata")  #crosswalk between urbanized areas and counties
preMerge<-crosswalk[c("FIPS","Pre_Merge_Central","Pre_Merge_ID")]
colnames(preMerge)<-c("FIPS","O_Pre_Merge_Central","O_Pre_Merge_ID")
flows<-merge(flows,preMerge,by.x="Cnty_Res",by.y="FIPS",all.x=TRUE)
colnames(preMerge)<-c("FIPS","D_Pre_Merge_Central","D_Pre_Merge_ID")
flows<-merge(flows,preMerge,by.x="Cnty_Work",by.y="FIPS",all.x=TRUE)

save(flows,file="./Output Data/flows.Rdata")
result<-unique(flows[,c("Cnty_Res","Res.CBSA","Res.Cen.Out")])
save(result,file="./Output Data/empty_result.Rdata")
rm(list=ls())
}
#######################################
#3)County polygons for visualization.
if(!file.exists("./Output Data/county_map.Rdata")){
  #read in county boundaries

  counties<-get_decennial(geography="county",geometry = TRUE,
                          variables = c(Total="P001001",Hispanic="P004003",NHBlack="P005004"),
                          year=2010,output = "wide")
  counties$NAME<-iconv(x=counties$NAME,from = "latin1",to="UTF-8")
  counties$FIPS<-as.numeric(counties$GEOID)
  #Get census contiguity file
  save(counties,file="./Output Data/county_map.Rdata")
}
#3a)County neighbors file
if(!file.exists("./Output Data/nba.Rdata")){
  #Source:
  if(!file.exists("./Input Data/county_adjacency2010.csv")){
    URL="https://data.nber.org/census/geo/county-adjacency/2010/county_adjacency2010.csv"
    download.file(url=URL,destfile = "./Input Data/county_adjacency2010.csv")
  }
  nb<-read.csv("./Input Data/county_adjacency2010.csv")
  nb<-nb[,c("fipscounty","fipsneighbor")]
  colnames(nb)<-c("Origin","Neighbor")
  #Do we drop self-adjacency? Currently no
  nba=nb
  #nba<-nb[nb$Origin !=nb$Neighbor,]
  nba<-nba[order(nba$Origin,nba$Neighbor),]

  #Now create an nb object for use in spatial analysis
  nb<-poly2nb(counties,queen = TRUE)

  #needs to be edited to match official adjacency
  fips<-data.frame(FIPS=counties$FIPS,position=1:length(counties$FIPS))
  fips$position<-as.integer(fips$position)
  for (i in 1:length(fips$FIPS)){
    fp<-nba[nba$Origin==fips[i,"FIPS"],"Neighbor"]
    if(length(fp)==1){ #if only neighbor is self then 0 neighbors
      pos=0
    }else{ #otherwise drop self and convert back to position
      fp<-fp[fp!=fips[i,"FIPS"]]
      pos<-fips[fips$FIPS %in% fp,"position"]
      nb[[i]]<-pos
    }
  }
  weights<-nb2listw(nb,style="W",zero.policy = TRUE)
  us_states <- states(cb = TRUE, resolution = "20m") %>%
          shift_geometry()
  save(us_states,file="./Output Data/states.Rdata")
  save(weights,file="./Output Data/weights.Rdata")
  save(nba,file="./Output Data/nba.Rdata")
}
#######################################
# 3)Tract to tract flows
#Source:
#From https://www.fhwa.dot.gov/planning/census_issues/ctpp/data_products/2006-2010_tract_flows/
if(!file.exists("./Output Data/tracts.Rdata")){

if(!file.exists("./Input Data/CTPP 2006-2010/tract-flows.accdb")){
  URL="https://www.fhwa.dot.gov/planning/census_issues/ctpp/data_products/2006-2010_tract_flows/tract-flows.accdb"
  download.file(url=URL,destfile = "./Input Data/CTPP 2006-2010/tract-flows.accdb")
}
DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
MDBPATH <- normalizePath("./Input Data/CTPP 2006-2010/tract-flows.accdb",winslash = "/")
PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

## Establish connection
channel <- odbcDriverConnect(PATH)
df<-sqlFetch(channel,sqtable = 'Tract-flows',colnames = FALSE,rownames = FALSE)

## Close and remove channel
close(channel)
rm(channel)

df$Res_FIPS<-(df$Residence_State_FIPS_Code*1000000000)+(df$Residence_County_FIPS_Code*1000000)+df$Residence_Tract_FIPS_Code
df$Wk_FIPS<-(df$Workplace_State_FIPS_Code*1000000000)+(df$Workplace_County_FIPS_Code*1000000)+df$Workplace_Tract_FIPS_Code
colnames(df)<-c("O_State","O_Cty","O_Tract","D_State","D_Cty","D_Tract","Flow","MOE","Res_FIPS","Wk_FIPS")
tracts<-df
#Need to get 0 flow tracts as well
#We just find the tracts that are not in the transportation data set for origins
#We create a 0 flow record for internal flows
tct<-read.csv("./Input Data/NHGIS tracts/nhgis0078_csv/nhgis0078_ds172_2010_tract.csv")
tct<-tct[,c("STATEA","COUNTYA","TRACTA")]
tct$Res_FIPS<-(tct$STATEA*1000000000)+(tct$COUNTYA*1000000)+tct$TRACTA
colnames(tct)<-c("O_State","O_Cty","O_Tract","Res_FIPS")
tct2<-tct
colnames(tct2)<-c("D_State","D_Cty","D_Tract","Wk_FIPS")
tct<-cbind(tct,tct2)
tct$Flow=0
tct$MOE=NA
findzero<-unique(tracts[,"Res_FIPS"])
tct2<-tct[tct$Res_FIPS %in% findzero==FALSE,]
tracts<-rbind(tracts,tct2)
save(tracts,file="./Output Data/tracts.Rdata")
}
###################################################################
# 4 and 5)LODES tract to tract and LODES county to county
#
# need a list of states with abbreviated state names

if(!file.exists("./Output Data/lodes.Rdata")){
states<-tolower(c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"))
for (i in 1:51) {
  stusab <- states[i]
  #Download flow information within state
  url <- paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", stusab,"/od/",stusab,"_od_main_JT00_2011.csv.gz")
  gz_filename<-paste0(getwd(),"/Input Data/LODES/",stusab,"_od_main_JT00_2011.csv.gz")
  if(!file.exists(gz_filename)){
    download.file(url, destfile = gz_filename)
  }
  #Download flow information outside state
  url <- paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", stusab,"/od/",stusab,"_od_aux_JT00_2011.csv.gz")
  gz_filename<-paste0(getwd(),"/Input Data/LODES/",stusab,"_od_aux_JT00_2011.csv.gz")
  if(!file.exists(gz_filename)){
    download.file(url, destfile = gz_filename)
  }
  #Download geographic id variables
  url2 <- paste0("http://lehd.ces.census.gov/data/lodes/LODES7/", stusab,"/",stusab,"_xwalk.csv.gz")
  gz_filename2<-paste0(getwd(),"/Input Data/LODES/",stusab,"_xwalk.csv.gz")
  if(!file.exists(gz_filename2)){
    download.file(url2, destfile = gz_filename2)
  }
}

#First pass in state flows only read in data, merge on workplace and home and aggregate to tract
for (i in 1:51) {
  stusab <- states[i]
  gz_filename<-paste0(getwd(),"/Input Data/LODES/",stusab,"_od_main_JT00_2011.csv.gz")
  tmp<-read.csv(gz_filename) #read in main (in state) flow file
  tmp<-tmp[,1:3]
  gz_filename2<-paste0(getwd(),"/Input Data/LODES/",stusab,"_xwalk.csv.gz")
  tmp2<-read.csv(gz_filename2)
  tmp2<-tmp2[,c(1,2,5,7)]

  #merge geog variables to origin and destination
  tmp_out<-merge(tmp,tmp2,by.x="h_geocode",by.y="tabblk2010",all.x=TRUE)
  tmp_out<-merge(tmp_out,tmp2,by.x="w_geocode",by.y="tabblk2010",all.x=TRUE)

  #aggregate from block group to tract
  tmp_out<-tmp_out[,3:9]
  tmp_out<-aggregate(tmp_out$S000,by = list(tmp_out$st.x,tmp_out$cty.x,tmp_out$trct.x,tmp_out$st.y,tmp_out$cty.y,tmp_out$trct.y),FUN = sum)
  colnames(tmp_out)<-c("O_State","O_Cty","O_Tract","D_State","D_Cty","D_Tract","Flow")

  if(i==1){
    lodes=tmp_out
    state_xwalk=tmp2
  }else{
    lodes<-rbind(lodes,tmp_out)
    state_xwalk<-rbind(state_xwalk,tmp2)
  }
}

#Second pass adds out of state flows. Read in data, merge on workplace and home and aggregate to tract
for (i in 1:51) {
  stusab <- states[i]
  gz_filename<-paste0(getwd(),"/Input Data/LODES/",stusab,"_od_aux_JT00_2011.csv.gz")
  tmp<-read.csv(gz_filename) #read in aux (out of state) flow file
  tmp<-tmp[,1:3]

  #merge geog variables to origin and destination
  tmp_out<-merge(tmp,state_xwalk,by.x="h_geocode",by.y="tabblk2010",all.x=TRUE)
  tmp_out<-merge(tmp_out,state_xwalk,by.x="w_geocode",by.y="tabblk2010",all.x=TRUE)

  #aggregate from block group to tract
  tmp_out<-tmp_out[,3:9]
  tmp_out<-aggregate(tmp_out$S000,by = list(tmp_out$st.x,tmp_out$cty.x,tmp_out$trct.x,tmp_out$st.y,tmp_out$cty.y,tmp_out$trct.y),FUN = sum)
  colnames(tmp_out)<-c("O_State","O_Cty","O_Tract","D_State","D_Cty","D_Tract","Flow")

  lodes<-rbind(lodes,tmp_out)
}

#A couple of conversions to bring data sets into line
#46102->46113 Shannon County SD became Oglala Lakota County revert back to 2013 def.
lodes[lodes$O_Cty==46102,"O_Cty"]<-46113
lodes[lodes$D_Cty==46102,"D_Cty"]<-46113
#2158->2270 #Wade-Hampton Census Area becomes Kusilvak Census Area revert back to 2013 def.
lodes[lodes$O_Cty==2158,"O_Cty"]<-2270
lodes[lodes$D_Cty==2158,"D_Cty"]<-2270

#aggregate to county level and save
lodes_cty<-aggregate(lodes$Flow,by = list(lodes$O_State,lodes$O_Cty,lodes$D_State,lodes$D_Cty),FUN = sum)
colnames(lodes_cty)<-c("O_State","O_Cty","D_State","D_Cty","Flow")

load("./Output Data/crosswalk.Rdata")
lodes_cty<-lodes_cty[,colnames(lodes_cty)%in% c("O_State","D_State")==FALSE]
preMerge<-crosswalk[c("FIPS","Pre_Merge_Central","Pre_Merge_ID")]
colnames(preMerge)<-c("FIPS","O_Pre_Merge_Central","O_Pre_Merge_ID")
flows.lodes<-merge(lodes_cty,preMerge,by.x="O_Cty",by.y="FIPS",all.x=TRUE)
colnames(preMerge)<-c("FIPS","D_Pre_Merge_Central","D_Pre_Merge_ID")
flows.lodes<-merge(flows.lodes,preMerge,by.x="D_Cty",by.y="FIPS",all.x=TRUE)
colnames(flows.lodes)[1:2]<-c("Cnty_Work","Cnty_Res")
lodes_cty<-flows.lodes


lodes$O_Cty<-lodes$O_Cty-floor(lodes$O_Cty/1000)*1000
lodes$O_Tract<-lodes$O_Tract-floor(lodes$O_Tract/1000000)*1000000
lodes$D_Cty<-lodes$D_Cty-floor(lodes$D_Cty/1000)*1000
lodes$D_Tract<-lodes$D_Tract-floor(lodes$D_Tract/1000000)*1000000

lodes$Res_FIPS<-nhgisfips(lodes[,c("O_State","O_Cty","O_Tract")],validfields = c("O_State","O_Cty","O_Tract"))
lodes$Wk_FIPS<-nhgisfips(lodes[,c("D_State","D_Cty","D_Tract")],validfields = c("D_State","D_Cty","D_Tract"))


save(lodes,file="./Output Data/lodes.Rdata")
save(lodes_cty, file="./Output Data/lodes_cty.Rdata")
rm(list=ls())
}

###################################################################

#6 Urbanized area shapefile
#We will need to assign tracts to an urbanized area for aggregation, this requires that we load the spatial information for this data and then attach it to tracts in the tract and lodes data sets
#Required: Is a tract in a UA? If so which one? What type of UA is it

#UATYPE10 u = Urbanized Are 50K+ people, C= urban cluster 2500-49999 people
#I chose the UA with the largest population in the tract. In cases where there were two Urbanized Areas or two Urban Clusters, I was not able to determine which one was larger in terms of population and the choice became arbitrary.
#To address this issue I do a did a spatial overlay of the UA/UC boundaries on the tracts to determine which UA or UC had the largest overlap with the tract. It is not a guarantee that land area equates to larger population since pop. density can vary significantly, but this is the closest we can come without more information from the Census.
#type = 1 urban 30%+ pop in UA, type 2= urban clust or micro 10K to 49,999 pop, type 3=small town 2500 pop to 9,999

if(!file.exists("./Output Data/lodesUA.Rdata")){

  #Need to start by deciding which tracts have the population in a UA or UC to warrant inclusion.
  #Next search for the largest UA or UC that intersects it.
  #Now determine if each tract has 30% population in an urbanized area. Controlled by the other_share_pop_in_UC_to_be_core variable
  #tract in urbanized area stats
  tct_ua<-read.csv("./Input Data/NHGIS tracts/nhgis0078_csv/nhgis0078_ds172_2010_tract.csv")
  tct_ua<-tct_ua[,c("GISJOIN","H7W001","H7W003","H7W004","H7W005")]
  colnames(tct_ua)<-c("GISJOIN","TotalPop","UAPop","UCPop","RuralPop")
  tct_ua$InUA<-tct_ua$UAPop/tct_ua$TotalPop
  tct_ua[tct_ua$TotalPop==0,"InUA"]<-0
  tct_ua$InUC<-tct_ua$UCPop/tct_ua$TotalPop
  tct_ua[tct_ua$TotalPop==0,"InUC"]<-0
  tct_ua$InRU<-tct_ua$RuralPop/tct_ua$TotalPop
  tct_ua[tct_ua$TotalPop==0,"InRU"]<-0
  tct_ua$Keep<-NA
  tct_ua[tct_ua$InRU >(1-other_share_pop_in_UC_to_be_core),"Keep"]<-"Rural"
  tct_ua[tct_ua$InUC>other_share_pop_in_UC_to_be_core,"Keep"]<-"UC"
  tct_ua[tct_ua$InUA>other_share_pop_in_UC_to_be_core & tct_ua$InUA > tct_ua$InUC,"Keep"]<-"UA"
  tct_ua[(is.na(tct_ua$Keep) & tct_ua$TotalPop != 0) & tct_ua$InUA>tct_ua$InUC,"Keep"]<-"UA"
  tct_ua[(is.na(tct_ua$Keep) & tct_ua$TotalPop != 0) & tct_ua$InUA<tct_ua$InUC,"Keep"]<-"UC"
  tct_ua[is.na(tct_ua$Keep) & tct_ua$TotalPop == 0,"Keep"]<-"Empty"
  tct_ua$Keep<-as.factor(tct_ua$Keep)
  #To determine which UA or UC a tract is associated with we need to find the largest one that
  #overlaps with it.
  #urbanized area boundaries
  ua<-st_read(dsn = "./Input Data/NHGIS Urban Areas Boundaries",layer = "US_urb_area_2010")
  ua<-ua[,c("UACE10","UATYP10","GISJOIN")]
  #water clipped tract boundaries
  tct<-st_read(dsn = "./Input Data/NHGIS tracts", layer="US_tract_2010")
  tct<-tct[,c("STATEFP10","COUNTYFP10","TRACTCE10","GISJOIN")]
  #retain just the parts of tracts from tct that intersect with ua
  tct<-st_make_valid(tct)
  ua<-st_make_valid(ua)
  inua<-st_intersection(tct,ua)
  #Get the land area of the different intersections so we can make choices between UA's of the same type (we will take the largest)
  inua$UAArea<-st_area(inua)
  inua<-st_drop_geometry(inua)
  #Separate to urban areas and urban clusters
  ualink<-inua[inua$UATYP10=="U",c("GISJOIN","UACE10","UAArea")]
  uclink<-inua[inua$UATYP10=="C",c("GISJOIN","UACE10","UAArea")]

  ualink<-ualink[with(ualink,order(-xtfrm(GISJOIN),-xtfrm(UAArea))),]
  ualink<-ualink[!duplicated(ualink$GISJOIN),] #largest UA overlap (by area) for each tract

  uclink<-uclink[with(uclink,order(-xtfrm(GISJOIN),-xtfrm(UAArea))),]
  uclink<-uclink[!duplicated(uclink$GISJOIN),] #largest UC overlap (by area) for each tract

  #Note that the choice of UA or UC is made based on population, then we only have to deal with which among those
  keepua<-merge(tct_ua[tct_ua$Keep=="UA",],ualink,by="GISJOIN",all.x=TRUE)
  keepuc<-merge(tct_ua[tct_ua$Keep=="UC",],uclink,by="GISJOIN",all.x=TRUE)
  keepru<-tct_ua[tct_ua$Keep=="Rural",]
  keepru$UACE10<-NA
  keepru$UAArea<-0
  keepempty<-tct_ua[tct_ua$Keep=="Empty",]
  keepempty$UACE10<-NA
  keepempty$UAArea<-0

  bound<-rbind(keepua,keepuc,keepru,keepempty)
  bound$UAA<-as.numeric(bound$UACE10)
  #Now we need to know the population of each of these ua's so we can assign size
  ua_pop<-read.csv("./Input Data/NHGIS tracts/nhgis0078_csv/nhgis0078_ds172_2010_urb_area.csv")
  ua_pop<-ua_pop[,c("UAA","H7V001")]
  colnames(ua_pop)<-c("UAA","UAOfficialPop")
  ua_pop$UAType<-7
  ua_pop[ua_pop$UAOfficialPop >= urban_cluster_threshold,"UAType"]=4
  ua_pop[ua_pop$UAOfficialPop >= urban_area_threshold,"UAType"]=1
  final<-merge(bound,ua_pop[,c("UAA","UAOfficialPop","UAType")],by="UAA",all.x=TRUE) #join boundaries and pop/type info

  final[is.na(final$UAType) & final$Keep=="Rural","UAType"]<-10
  final[is.na(final$UAType) & final$Keep=="Empty","UAType"]<-99

  final$UAUCRural<-final$Keep

  final<-final[,c("GISJOIN","UACE10","UAUCRural","UAType")]
  #Tracts. We will be attaching information on whether each tract is considered to be in
  #an urbanized area (e.g. 30% + population inside). We will also indicate the kind of ua
  #the tract is in and the FIPS for the UA where applicable
  load("./Output Data/tracts.Rdata")
  tracts$Res_GISJOIN<-nhgisfips(x = tracts,validfields = c("O_State","O_Cty","O_Tract"))
  tracts$Wk_GISJOIN<-nhgisfips(x = tracts,validfields = c("D_State","D_Cty","D_Tract"))
  tracts<-tracts[,c("Res_GISJOIN","Wk_GISJOIN","Flow","MOE")]
  tracts<-merge(tracts,final[,c("GISJOIN","UACE10","UAType")],by.x="Res_GISJOIN",by.y="GISJOIN",all.x=TRUE)
  colnames(tracts)<-c("Res_GISJOIN","Wk_GISJOIN","Flow","MOE","Res_UA","Res_UAType")
  tracts<-merge(tracts,final[,c("GISJOIN","UACE10","UAType")],by.x="Wk_GISJOIN",by.y="GISJOIN",all.x=TRUE)
  colnames(tracts)<-c("Wk_GISJOIN","Res_GISJOIN","Flow","MOE","Res_UA","Res_UAType","Wk_UA","Wk_UAType")
  tracts[tracts=="<NA>"]<-NA
  tracts$Res_MergeID<-tracts$Res_UA
  tracts[is.na(tracts$Res_MergeID),"Res_MergeID"]<-tracts[is.na(tracts$Res_MergeID),"Res_GISJOIN"]
  tracts$Wk_MergeID<-tracts$Wk_UA
  tracts[is.na(tracts$Wk_MergeID),"Wk_MergeID"]<-tracts[is.na(tracts$Wk_MergeID),"Wk_GISJOIN"]
  tractsUA<-tracts
  load("./Output Data/crosswalk.Rdata")  #crosswalk between urbanized areas and counties


  save(tractsUA,file="./Output Data/tractsUA.Rdata")
  load("./Output Data/lodes.Rdata")
  lodesUA<-lodes[,c("Res_FIPS","Wk_FIPS","Flow")]
  colnames(lodesUA)<-c("Res_GISJOIN","Wk_GISJOIN","Flow")
  lodesUA<-merge(lodesUA,final[,c("GISJOIN","UACE10","UAType")],by.x="Res_GISJOIN",by.y="GISJOIN",all.x=TRUE)
  colnames(lodesUA)<-c("Res_GISJOIN","Wk_GISJOIN","Flow","Res_UA","Res_UAType")
  lodesUA<-merge(lodesUA,final[,c("GISJOIN","UACE10","UAType")],by.x="Wk_GISJOIN",by.y="GISJOIN",all.x=TRUE)
  colnames(lodesUA)<-c("Wk_GISJOIN","Res_GISJOIN","Flow","Res_UA","Res_UAType","Wk_UA","Wk_UAType")
  lodesUA[lodesUA=="<NA>"]<-NA
  lodesUA$Res_MergeID<-lodesUA$Res_UA
  lodesUA[is.na(lodesUA$Res_MergeID),"Res_MergeID"]<-lodesUA[is.na(lodesUA$Res_MergeID),"Res_GISJOIN"]
  lodesUA$Wk_MergeID<-lodesUA$Wk_UA
  lodesUA[is.na(lodesUA$Wk_MergeID),"Wk_MergeID"]<-lodesUA[is.na(lodesUA$Wk_MergeID),"Wk_GISJOIN"]
  save(lodesUA,file="./Output Data/lodesUA.Rdata")
}


#7)RUCA delineations
#These are the official RUCA codes downloaded from XXXXXXX and modified here for consistent column names.
if(!file.exists("./Output Data/RUCA_Official.Rdata")){

  RUCAOfficial<-read_excel("./Input Data/ERS RUCA data/RUCA 2010.xlsx")
  RUCAOfficial$STATE<-sapply(X = RUCAOfficial$FIPS,FUN = function(x) substr(x,1,2))
  RUCAOfficial$COUNTY<-sapply(X=RUCAOfficial$FIPS,FUN= function(x) substr(x,3,5))
  RUCAOfficial$TRACT<-sapply(X=RUCAOfficial$FIPS,FUN= function(x) substr(x,6,11))
  RUCAOfficial$Res_GISJOIN<-nhgisfips(x = RUCAOfficial,validfields = c("STATE","COUNTY","TRACT"))
  save(RUCAOfficial,file="./Output Data/RUCA_Official.Rdata")
}

rm(list=ls())