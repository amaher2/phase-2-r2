#Ratio graphs
#File names need to be updated

library(smwrGraphs)



RatioDataGraph <- read.csv("P:/FPJ00/Phase2GeochemistryData/RatioDataComplete.csv")
sapply(RatioDataGraph, class)
RatioDataGraph %>% mutate_if(is.factor, as.character) -> RatioDataGraph
sapply(RatioDataGraph, class)
RatioDataGraph$result_va<-as.numeric(RatioDataGraph$result_va)

RatioDataGraph<-RatioDataGraph[with(RatioDataGraph,order(Site_name,MeanSampleDepth_ftBLS)),]

#More graphs GW
unique(RatioDataGraph$srsname[RatioDataGraph$anl_ent_cd=="USGSNWQL"])
RatioGraphs1<-c("Bromide","Chloride","Sodium", "Cl.Br", "Na.Cl")
parm<-c("Bromide",
        "Chloride",
        "Sodium",
        "Cl.Br",
        "Na.Cl")

a<-"Bromide"
s<-"Ol_1"

RatioDataGraph[is.na(RatioDataGraph$Piez_Nest),]
setPDF(layout = "landscape",basename = "ratioprofiles1",multiplefiles = FALSE)
setLayout(width = c(1.5,1.5,1.5,1.5,1.5),height = 6)
plot(0,0)
for(s in c("Ol_1","HFC_1","CW_1","CW_2","LF_1","LF_2")) {
  tmp.df<-RatioDataGraph[RatioDataGraph$Piez_Nest==s,]
  for(a in RatioGraphs1){
    tmp.df2<-tmp.df[tmp.df$srsname==a,]
    if(nrow(tmp.df2)>0){
      with(tmp.df2[tmp.df2$medium_cd=="WG",],xyPlot(x =result_va ,y=MeanSampleDepth_ftBLS,yaxis.rev = TRUE,
                                                    caption = paste(tmp.df2$Site_name[1],tmp.df2$Piez_Nest[1],tmp.df2$srsname[1], tmp.df2$parameter_units[1]),Plot=list(what="points")))
      with(tmp.df2[tmp.df2$medium_cd=="WG",],addXY(x =result_va ,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="lines")))
      with(tmp.df2[tmp.df2$medium_cd=="WI",],
           addXY(x=result_va,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="points",color="red")))
      with(tmp.df2[tmp.df2$anl_ent_cd=="USGSNWQL",],
           addXY(x=result_va,y=MeanSampleDepth_ftBLS*-1,Plot=list(what="points",filled=FALSE)))
    } else {
      next
    }
    
    
  }
}
dev.off()
