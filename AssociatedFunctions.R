## Functions associated with Uncertainty Analysis files
#Libraries
require(sf)
require(grid)
require(ggthemes)
require(scales)
require(tidyverse)
require(gt)



#Functions
#######################################
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}
#Function for assembling NHGIS style fips codes from State/County/Tract
#Modified from https://rdrr.io/github/ejanalysis/analyze.stuff/src/R/lead.zeroes.R
lead.zeroes <- function(fips, length.desired) {
  navalues <- which(is.na(fips))
  fips <- as.character(fips)
  if ( (length(length.desired) > 1) & (length(fips) != length(length.desired))) {print("warning: #s of inputs don't match")}
  if ( any(length.desired == 0 | length.desired >= 100) ) {stop("error: string lengths must be >0 & <100")}
  if ( any(nchar(fips) > length.desired, na.rm = TRUE) ) {stop("error: some are longer than desired length")}
  fips <- paste( paste( rep( rep("0", length(length.desired)), length.desired), collapse = ""), fips, sep = "")
  fips <- substr(fips, nchar(fips) - length.desired + 1, nchar(fips))
  fips[navalues] <- NA
  return(fips)
}
nhgisfips <- function(x,validfields = c('STATE', 'COUNTY', 'TRACT'),
                      #fullname=c("FIPS"),
                      leadz = c(2, 4, 7)) {
  # leadz is correct total nchar() for this portion of fips, including leading zeroes
  xfipscols <- x[, validfields]
  FIPS <- mapply(FUN = lead.zeroes, xfipscols, leadz)
  FIPS <- apply(FIPS,1,FUN = function(z)  paste(z, collapse = ''))
  # return as a 1-column data.frame
  FIPS<-paste0("G",FIPS)
  #FIPS <- data.frame(FIPS, stringsAsFactors = FALSE)
  #colnames(FIPS) <- fullname
  return(FIPS)
}
#######################################
rnorm2<-function(mn,y){
  rand<-rnorm(n=1,mean=mn,sd=y)
  rand<-max(0,rand)
  return(round(rand,0))
}
is_adjacent<-function(x,nb,result){
  if(x["Assigned"]=="Nonmetro"){
    return("Nonmetro")
  }else{
    a<-as.numeric(x["Cnty_Res"])
    tmp<-nb[nb$Origin ==a,]    #who are this counties neighbors?
    tmp2<-as.numeric(result[result$Assigned ==as.character(x["Assigned"]),"Cnty_Res"]) #get the other counties assigned to this CBSA
    if(length(tmp2)==1){
      return("Singleton")
    }else{
      nbs<-sum(sapply(tmp$Neighbor,FUN= function(x)x%in%tmp2[tmp2!=a]))
      #print(nbs)
      if(nbs>0){
        return("Adjacent")
      }else{
        return("Not Adjacent")
      }
    }
  }
}
trans<-function(x,y){
  out<-x
  out[x==y]<-"Same"
  out[x!=y]<-"Changed Metro"
  out[x!=y & x=="Nonmetro"]<-"To Metro"
  out[x!=y & y=="Nonmetro"]<-"To Nonmetro"
  return(out)
}
#This function replicates the OMB methodology for assigning counties to CBSA's.
#methods include identifying flows between counties, checking for adjacency
#testing for adjacent CBSA's with strong commute flows between central counties
assign_county<-function(data_ag,nb){
  #data_ag is a dataframe containing Origin, Destination, ResCo, WorkCo, and Flow
  #These columns signify county of origin, county of employment, then the OMB classification of each county or CBSA core county group
  #nb is a dataframe containing county FIPS along with all (Queen) neighbors. both origin and neighor listing also have a CBSA code assigned to them if appropriate.

  #Get totals for each county/cbsa core group
  Residents<-aggregate(x=data_ag$Flow,by=list(data_ag$Cnty_Res),FUN=sum)
  colnames(Residents)<-c("Cnty_Res","TotRes")
  Residents[Residents$TotRes==0,"TotRes"]<-1 #This prevents a divide by zero operation

  Workforce<-aggregate(x=data_ag$Flow,by=list(data_ag$Cnty_Work),FUN=sum)
  colnames(Workforce)<-c("Cnty_Work","TotWork")
  Workforce[Workforce$TotWork==0,"TotWork"]<-1
  #Attach to main data and create share variable
  data_s<-merge(data_ag,Residents,by="Cnty_Res",sort=FALSE)
  data_s<-merge(data_s,Workforce,by="Cnty_Work",sort=FALSE)
  data_s$ResShare<-data_s$Flow/data_s$TotRes
  data_s$WorkShare<-data_s$Flow/data_s$TotWork

  #A 'blank' data.frame of counties with FIPS and 2010 CBSA assignment info
  load("./Output Data/empty_result.Rdata")
  #merge with crosswalk info on pre-merge ids
  load("./Output Data/crosswalk.Rdata")
  result<-merge(result,crosswalk[,c("FIPS","Pre_Merge_Central","Pre_Merge_ID","UAPOP")],by.x="Cnty_Res",by.y="FIPS",all.x=TRUE)
  #Create linkage between CBSA and UA titles
  uatocbsa<-unique(result[result$Res.CBSA !="Nonmetro" & result$Pre_Merge_Central==1,c("Res.CBSA","Pre_Merge_ID")])
  colnames(uatocbsa)<-c("New.CBSA","Merged_Assignment")
  result$Assigned<-NA
  result$New_Res_Share<-NA
  result$New_Wk_Share<-NA
  #The main work area of this function. For each county it finds the maximum connection in both workforce and residence
  #If it finds a connection greater than 25% that is not itself it assigns that to the county
  for(i in 1:length(result$Cnty_Res)){
    if(result[i,"Pre_Merge_Central"]==1){
      result[i,"Assigned"]<-result[i,"Pre_Merge_ID"]
    }else{
      fps<-result[i,"Cnty_Res"]
      hold<-.25
      assign<-"Nonmetro"
      tmp<-data_s[data_s$Cnty_Res==result[i,"Cnty_Res"] & data_s$D_Pre_Merge_Central==1,]
      if(length(tmp[,1])>0){
        tmp2<-aggregate(tmp$ResShare,by=list(tmp$Cnty_Res,tmp$D_Pre_Merge_ID),FUN = sum)
        colnames(tmp2)<-c("Cnty_Res","D_Pre_Merge_ID","ResShare")
        tmp3<-tmp2[tmp2$ResShare==max(tmp2$ResShare),c("D_Pre_Merge_ID","ResShare")]
        if(tmp3[1,"ResShare"] >.25){ #sometimes we have two equal values they will rarely be high enough to matter
          hold<-max(tmp3$ResShare)
          assign<-tmp3$D_Pre_Merge_ID
          if(length(assign)>1){
            #Pick the consistent one if I can
            cross_to_CBSA<-uatocbsa[uatocbsa$Merged_Assignment %in% assign,]
            look<-result[result$Cnty_Res==fps,]
            test_for<-cross_to_CBSA[cross_to_CBSA$New.CBSA==look$Res.CBSA,"Merged_Assignment"]
            if(length(test_for)>0 &!is.na(test_for)){
              assign<-cross_to_CBSA[cross_to_CBSA$New.CBSA==look$Res.CBSA,"Merged_Assignment"][1]  #need the [1] in case multiple UA's assigned to the same correct New.CBSA
            }else{#the rare case where there are multiple UA's and neither was originally assigned
              rbi<-round(runif(n=1,min=1,max=length(tmp3$D_Pre_Merge_ID)),0)
              assign<-tmp3$D_Pre_Merge_ID[rbi]
            }
          }
        }
      }
      #also check to see if any core county sends 25% of its workforce to this county
      tmp<-data_s[data_s$Cnty_Work==result[i,"Cnty_Res"]& data_s$O_Pre_Merge_Central==1,]
      if(length(tmp[,1])>0){
        tmp2<-aggregate(tmp$WorkShare,by=list(tmp$Cnty_Work,tmp$O_Pre_Merge_ID),FUN = sum)
        colnames(tmp2)<-c("Cnty_Work","O_Pre_Merge_ID","WorkShare")
        tmp3<-tmp2[tmp2$WorkShare==max(tmp2$WorkShare), c("O_Pre_Merge_ID","WorkShare")]
        if(tmp3[1,"WorkShare"] >hold){
          hold<-max(tmp3$WorkShare)
          assign<-tmp3$O_Pre_Merge_ID
          if(length(assign)>1){
            #Pick the consistent one if I can
            cross_to_CBSA<-uatocbsa[uatocbsa$Merged_Assignment %in% assign,]
            look<-result[result$Cnty_Res==fps,]
            test_for<-cross_to_CBSA[cross_to_CBSA$New.CBSA==look$Res.CBSA,"Merged_Assignment"]
            if(length(test_for)>0){
              assign<-cross_to_CBSA[cross_to_CBSA$New.CBSA==look$Res.CBSA,"Merged_Assignment"][1]
            }else{#the rare case where there are multiple UA's and neither was originally assigned
              rbi<-round(runif(n=1,min=1,max=length(tmp3$O_Pre_Merge_ID)),0)
              assign<-tmp3$O_Pre_Merge_ID[rbi]
            }
          }
        }
      }
      result[i,"Assigned"]<-assign
    }

    if(result[i,"Assigned"] != "Nonmetro"){ #if county is assigned to a CBSA collect info on Flow in and Flow from
      O<-result[i,"Cnty_Res"]
      D<-result[i,"Assigned"]
      tmp1<-data_s[data_s$Cnty_Res==O & data_s$D_Pre_Merge_ID==D,"ResShare"]
      tmp2<-data_s[data_s$O_Pre_Merge_ID==D & data_s$Cnty_Work==O,"WorkShare"]
      result[i,"New_Res_Share"]<-ifelse(length(tmp1)==0,NA,sum(tmp1))
      result[i,"New_Wk_Share"]<-ifelse(length(tmp2)==0,NA,sum(tmp2))
    }
  }

  result[is.na(result$Res.CBSA),"Res.CBSA"]<-"Nonmetro" #any counties that weren't originally assigned to a CBSA become nonmetro

  #Check for adjacency
  result$Adjacent<-NA
  result$Adjacent<-apply(result,MARGIN = 1,FUN = function(x) is_adjacent(x,nba,result))
  #Fix any adjacency issues that arise
  result$Reassigned<-result$Assigned
  if(length(result[result$Adjacent=="Not Adjacent",1])>0){
    for(i in 1:length(result[result$Adjacent=="Not Adjacent",1]))
      cty<-result[result$Adjacent=="Not Adjacent","Cnty_Res"][i]
    if(result[result$Cnty_Res==cty,"Pre_Merge_Central"]==0){
      #if it wasn't Central and got merged to something then revert back
      result[result$Cnty_Res==cty,"Reassigned"]<-"Nonmetro"
    }
  }

  #Now we need to go through and decide if any of the UA's in "Reassigned" meet the criteria for merging into a nearby
  #CBSA. Requirement is that 25% of flows from central counties of CBSA 1 are to central counties of CBSA 2 or vice versa. If true, then merge smaller into larger.
  #Aggregate flows for CBSA pairs
  res_merge<-result[,c("Cnty_Res","Reassigned","Pre_Merge_Central")]
  colnames(res_merge)<-c("Cnty_Res","O_Assigned","O_Central")
  data_s<-merge(data_s,res_merge,by="Cnty_Res")
  colnames(res_merge)<-c("Cnty_Work","D_Assigned","D_Central")
  data_s<-merge(data_s,res_merge,by="Cnty_Work")
  #Collect total workforce and total residents aggregated to cbsa
  wor<-unique(data_s[,c("Cnty_Work","D_Assigned","TotWork")])
  res<-unique(data_s[,c("Cnty_Res","O_Assigned","TotRes")])
  dRes<-aggregate(res$TotRes,by=list(res$O_Assigned),FUN=sum)
  dWor<-aggregate(wor$TotWork,by=list(wor$D_Assigned),FUN=sum)
  colnames(dRes)<-c("O_Assigned","TotResCBSA")
  colnames(dWor)<-c("D_Assigned","TotWorkCBSA")

  #get rid of non metro
  tmp<-data_s[data_s$O_Assigned !="Nonmetro" & data_s$D_Assigned != "Nonmetro",c("O_Assigned","D_Assigned","Flow","O_Central","D_Central")]
  #get rid of non central
  tmp<-tmp[tmp$O_Central==1 & tmp$D_Central==1, c("O_Assigned","D_Assigned","Flow")]
  cbsa<-aggregate(tmp$Flow,by=list(tmp$O_Assigned,tmp$D_Assigned),FUN=sum)
  colnames(cbsa)<-c("O_Assigned","D_Assigned","Flow")
  #Join total flows to cbsa to cbsa flows
  cbsa<-merge(cbsa,dRes,by="O_Assigned",sort=FALSE)
  cbsa<-merge(cbsa,dWor,by="D_Assigned",sort=FALSE)

  cbsa$ResShare<-cbsa$Flow/cbsa$TotResCBSA
  cbsa$WorkShare<-cbsa$Flow/cbsa$TotWorkCBSA
  #Remove internal commute relationships
  cbsa<-cbsa[cbsa$D_Assigned !=cbsa$O_Assigned,]
  #Unique list of O-D pairs that qualify for merge
  candidates<-unique(rbind(cbsa[cbsa$ResShare>.25,c("D_Assigned","O_Assigned","ResShare","WorkShare")],
                           cbsa[cbsa$WorkShare>.25,c("D_Assigned","O_Assigned","ResShare","WorkShare")]))
  candidates$Share<-apply(candidates[,c("ResShare","WorkShare")],MARGIN = 1,FUN = max)

  #Now need to check for adjacency--need to bring back in outlying counties
  candidates$Adjacent<-"Not Adjacent"
  for(i in 1:length(candidates$Adjacent)){
    tmp1<-result[result$Reassigned ==candidates[i,"O_Assigned"],"Cnty_Res"]
    tmp2<-result[result$Reassigned ==candidates[i,"D_Assigned"],"Cnty_Res"]
    #Do any counties in tmp1 touch counties in tmp2?
    neigh<-nb[nb$Origin %in%tmp1,"Neighbor"]    #who are this counties neighbors?
    is_adj<-sum(tmp2%in%neigh)
    if(is_adj>0){candidates[i,"Adjacent"]="Adjacent"}
  }

  result$Merged_Assignment<-result$Reassigned
  candidates<-candidates[candidates$Adjacent=="Adjacent",]
  candidates$Becomes<-NA
  for(i in 1:length(candidates[,1])){
    O<-candidates[i,"O_Assigned"]
    D<-candidates[i,"D_Assigned"]
    O_pop<-unique(cbsa[cbsa$O_Assigned==O,"TotResCBSA"])
    D_pop<-unique(cbsa[cbsa$O_Assigned==D,"TotResCBSA"])
    if(O_pop > D_pop){#Origin bigger than destination merge dest to origin
      candidates[i,"Becomes"]<-O
    }else{ #Merge origin to destination
      candidates[i,"Becomes"]<-D
    }
  }
  Os<-subset(candidates,select=-D_Assigned)
  colnames(Os)[1]<-"Was"
  Ds<-subset(candidates,select=-O_Assigned)
  colnames(Ds)[1]<-"Was"
  movers<-rbind(Os,Ds)
  movers<-movers[movers$Was != movers$Becomes,]
  movers = movers[order(movers[,'Was'],-movers[,'Share']),]
  movers = movers[!duplicated(movers$Was),]
  for(i in 1:length(movers$Was)){
    result[result$Merged_Assignment==movers[i,"Was"],c("Merged_Assignment","New_Res_Share","New_Wk_Share")]<-movers[i,c("Becomes","ResShare","WorkShare")]
  }

  #Finally, we can clean up all this assignment nonsense by assigning UA values to CBSA values
  #checked for 1 to 1 relationship between UA and CBSA designation. No UA is assigned to two CBSA's

  result<-merge(result,uatocbsa,by="Merged_Assignment",all.x=TRUE)
  result<-result[order(result$Cnty_Res),]
  result[is.na(result$New.CBSA),"New.CBSA"]<-"Nonmetro"

  result$Consistent<-ifelse(result$Res.CBSA==result$New.CBSA,TRUE,FALSE) #Is the result Consistent with OMB delineation?
  cbsapop<-unique(result[!is.na(result$UAPOP),c("Assigned","UAPOP","New.CBSA")])
  cbsapop<-aggregate(x = cbsapop[,"UAPOP"],by =list(cbsapop$New.CBSA),FUN = sum)
  colnames(cbsapop)<-c("CBSA","CBSAPOP")
  result<-merge(result,cbsapop,by.x="New.CBSA",by.y="CBSA",all.x=TRUE)
  result<-result[order(result$Cnty_Res),]
  return(result)
}
assign_RUCA1<-function(RU){
  choice = 0 #flag for choosing internal flows over external flows
  smaller = 0 #flag for choosing lower number (larger UA/UC type)
  random = 0 #flag for choosing randomly
  problem = 0 #flag for problem flows (less than 10% in largest flow)

  tmp<-agg_tract[agg_tract$Res_MergeID==RU,]
  tot<-sum(tmp$Flow)
  tmp$pct<-tmp$Flow/tot
  tmp[is.nan(tmp$pct),"pct"]<-0
  use<-tmp[tmp$pct==max(tmp$pct),]
  if(length(use[,1])>1){ #problems when two or more flows are tied for largest
    #need to check if one is the same as self!
    if(use[1,"Res_MergeID"] %in% use$Wk_MergeID==TRUE){ #if one of the flows is internal then choose that one
      choice = 1
      use<-use[use$Wk_MergeID==use[1,"Res_MergeID"],]
    }else if (length(unique(use$Wk_UAType))>1){ #if one of the destination UA's is less (larger in significance) than the other choose that one
      smaller = 1
      use<-use[use$Wk_UAType==min(use$Wk_UAType),]
    }
  }
  if(length(use[,1])>1){ #can still be more than 1 record in use
    #otherwise, pick at random
    random = 1
    use<-sample_n(use,1)
  }
  if(max(use$pct) < .1){problem=1} #problem when none of the flows are as high as 10%
                                  #just document for now.
  ans=NA
  if (use$Res_MergeID==use$Wk_MergeID){
    ans=use$Res_UAType
  } else {
    if (use$Wk_UAType==1){
      if (use$pct >=.3){ans=2} else {ans=3}
    } else if (use$Wk_UAType==4){
      if (use$pct >=.3){ans=5}else {ans=6}
    }	else if (use$Wk_UAType==7){
      if (use$pct >=.3){ans=8}else {ans=9}
    } else {ans=10}
  }
  return(c(ans,choice, smaller, random, problem))
}


#Functions for assigning color palettes to graphics
theme_Publication <- function(base_size=12, base_family="Bloomsbury") {

  (theme_foundation(base_size=base_size, base_family=base_family)
   + theme(plot.title = element_text(face = "bold",
                                     size = rel(1.2), hjust = 0.5),
           text = element_text(),
           panel.background = element_rect(colour = NA),
           plot.background = element_rect(colour = NA),
           panel.border = element_rect(colour = NA),
           axis.title = element_text(face = "bold",size = rel(1)),
           axis.title.y = element_text(angle=90,vjust =2),
           axis.title.x = element_text(vjust = -0.2),
           axis.text = element_text(),
           axis.line = element_line(colour="black"),
           axis.ticks = element_line(),
           panel.grid.major = element_line(colour="#f0f0f0"),
           panel.grid.minor = element_blank(),
           legend.position = "none",
           #legend.key = element_rect(colour = NA),
           #legend.position = "bottom",
           #legend.direction = "horizontal",
           #legend.key.size= unit(0.2, "cm"),
           #legend.margin = unit(0, "cm"),
           #legend.title = element_text(face="italic"),
           plot.margin=unit(c(2,2,2,2),"mm"),
           strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
           strip.text = element_text(face="bold")
   ))

}
scale_fill_Publication <- function(...){

  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}
scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}
#A function to track down differences between my RUCA delineation and the official one.
sleuth<-function(record){
  FIPS<-record$FIPS
  GISJOIN<-record$Res_GISJOIN
  #How did he get to a six?
  ru17<-ru17redo04[ru17redo04$ruidr==FIPS,]
  df<-data.frame(Res=rep(ru17$ruidr,3),Wk=c(ru17$ruidw1,ru17$ruidw2,ru17$ruidw3),ResType=rep(ru17$ruidrtype,3),WkType=c(ru17$ruidwtype1,ru17$ruidwtype2,ru17$ruidwtype3),Flow=c(ru17$wfest1,ru17$wfest2,ru17$wfest3),Pct=round(c(ru17$wfestp1,ru17$wfestp2,ru17$wfestp3)*100,2))

  mine<-tractsUA[tractsUA$Res_GISJOIN==GISJOIN,]
  mineAgg<-aggregate(mine$Flow,by=list(mine$Res_GISJOIN,mine$Wk_MergeID,mine$Res_UAType,mine$Wk_UAType),sum)
  colnames(mineAgg)<-c("Res_GISJOIN","Wk_MergeID","Res_UAType","Wk_UAType","Flow")
  mineAgg<-mineAgg[order(mineAgg$Flow,decreasing = TRUE),]
  mineAgg$Pct<-round((mineAgg$Flow/sum(mineAgg$Flow))*100,2)
  mineAgg<-mineAgg[1:4,c("Res_GISJOIN","Wk_MergeID","Res_UAType","Wk_UAType","Flow","Pct")]
  colnames(mineAgg)<-c("Res","Wk","ResType","WkType","Flow","Pct")
  print("Problem:")
  print(record[,c("Res_GISJOIN","FIPS","Choice","Smaller","Random","Problem","County","RUCA","RUCA1","RUCA2","Pop","LandArea","PopDensity")])
  print("ERS flows:")
  print(df)
  print("My flows:")
  print(mineAgg)

  if(dim(ru17)[1]==0){
    print("tract assigned to a UA in ERS data")
    a<-ru01ForChris[ru01ForChris$sctf==FIPS,]
    ers_merge<-a$rufipsua
    uaru17<-ru17redo04[ru17redo04$ruidr==ers_merge,]
    df2<-data.frame(Res=rep(uaru17$ruidr,3),Wk=c(uaru17$ruidw1,uaru17$ruidw2,uaru17$ruidw3),ResType=rep(uaru17$ruidrtype,3),WkType=c(uaru17$ruidwtype1,uaru17$ruidwtype2,uaru17$ruidwtype3),Flow=c(uaru17$wfest1,uaru17$wfest2,uaru17$wfest3),Pct=round(c(uaru17$wfestp1,uaru17$wfestp2,uaru17$wfestp3)*100,2))
    print(df2)
    print("Search for tract in geographic area test")
    if(!exists("inua")){
      load("./Output Data/inua.Rdata")
    }
    inua[inua$GISJOIN==GISJOIN,]
  }
}

#Table formatting from https://towardsdatascience.com/create-flawless-tables-from-your-dataframe-ready-for-publication-7e3fe2d63a52
table_theme<-function(data){
c_col = c("#1e3048", "#274060", "#2f5375", "#4073a0", "#5088b9")

data %>%
tab_options(

  table.font.name = "Optima",
  table.font.color = c_col[1],
  table.border.top.style = "none",
  table.border.bottom.style = "solid",
  table.border.bottom.color = c_col[2],
  table.border.bottom.width = px(3),
  column_labels.border.top.color = "white",
  column_labels.border.top.width = px(3),
  column_labels.border.bottom.color = c_col[2],
  column_labels.border.bottom.width = px(3),
  data_row.padding = px(10)
)%>%
  tab_style(
    style = list(
      cell_text(
        size = px(12),
        weight = "normal",
        align = "left",
        font = "Bloomsbury"
      )
    ),
    locations = list(
      cells_title(groups = "title")
    )
   )%>%
  tab_style(
    style = list(
      cell_text(
        size = px(12),
        align = "left"
      )
    ),
    locations = list(
      cells_title(groups = "subtitle")
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(
        size = px(12)
      ),
      cell_borders(
        sides = c("bottom", "top"),
        color = c_col[1],
        weight = px(1)
      )
    ),
    locations = list(
      cells_body(gt::everything())
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(
        size = px(12),
        color = "#2f5375",
        font = "Bloomsbury"
      )
    ),
    locations = list(
      cells_column_labels(everything())
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(
        size = px(12),
        color = "#2f5375",
        font = "Bloomsbury"
      ),
      cell_borders(
        sides = c("bottom"),
        style = "solid",
        color = c_col[1],
        weight = px(2)
      )
    ),
    locations = list(
      cells_row_groups(gt::everything())
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(
        size = px(12),
        color = "#2f5375",
        font = "Bloomsbury"
      ),
      cell_borders(
        sides = c("bottom", "right"),
        style = "solid",
        color = "white",
        weight = px(1)
      )
    ),
    locations = list(
      cells_stub(gt::everything()),
      cells_stubhead()
    )
   )
}