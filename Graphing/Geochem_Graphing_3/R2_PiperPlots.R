#script to make piper plots from confining beds water quality data.
#Needs file names updated

library(smwrGraphs)
library(plyr)
library(reshape2)
library(dplyr)

g.all<-read.csv("QwGdatWellExport2.csv",stringsAsFactors = FALSE)

sapply(g.all,class)
g.all$sample_dt<-as.Date(g.all$sample_dt,format="%m/%d/%Y")
g.all$sample_dt
g.all$result_va<-as.numeric(g.all$result_va)
unique(g.all$srsname)

#create object with the ions of interest
pp<-c("Magnesium","Calcium","Sodium","Chloride","Sulfate","Bicarbonate", "Potassium", "Carbonate (CO3)")

#select subset of ions for piper plots, create new smaller data set from master
g.all.pp<-g.all[which (g.all$srsname %in% pp),]

#check that all units are in mg/l
unique(g.all.pp$parameter_units)

#check medium codes of subset
unique(g.all.pp$medium_cd)

#check lab of subset
unique(g.all.pp$anl_ent_cd)

#only include the USGS labs
g.all.pp<-g.all.pp[grep(g.all.pp$anl_ent_cd,pattern = "USGS"),]

#Need to combine Sodium and Potassium

#Need to combine carbonate and bicarbonate

write.csv(g.all.pp, file="g_all_pp.csv")
g.all.pp2<-read.csv("P:/FPJ00/Phase2GeochemistryData/g_all_pp_ions.csv")

#then reshape data set 
names(g.all.pp2)
g.all.pp2<-dcast(g.all.pp2,formula = Site_name+Piez_Nest+Site_ID+medium_cd~srsname,value.var = "result_va_meq")
sapply(g.all.pp2,class)


#then convert to meq
names(g.all.pp2)

g.all.pp2$bicarbonate_carbonate<-conc2meq(conc = g.all.pp2$bicarbonate_carbonate,constituent = "bicarbonate")
g.all.pp2$Calcium<-conc2meq(conc = g.all.pp2$Calcium,constituent = "calcium")
g.all.pp2$Cl_F_NO2_NO3<-conc2meq(conc = g.all.pp2$Cl_F_NO2_NO3,constituent = "chloride")
g.all.pp2$Magnesium<-conc2meq(conc = g.all.pp2$Magnesium,constituent = "magnesium")
g.all.pp2$Sodium_Potassium<-conc2meq(conc = g.all.pp2$Sodium_Potassium,constituent = "sodium")
g.all.pp2$Sulfate<-conc2meq(conc = g.all.pp2$Sulfate,constituent = "sulfate")

with(g.all.pp2,piperPlot(xCat=Calcium,yCat=Magnesium,zCat=Sodium_Potassium,xAn=Cl_F_NO2_NO3,yAn=bicarbonate_carbonate,zAn=Sulfate))

#if you want different colors by site:
unique(g.all.pp2$Site_name)
setPDF(layout = "portrait",basename = "PiperBySite")
AA<-with(g.all.pp2,
         piperPlot(xCat=Calcium,yCat=Magnesium,zCat=Sodium_Potassium,xAn=Cl_F_NO2_NO3,yAn=bicarbonate_carbonate,zAn=Sulfate,
                   Plot=list(name=Site_name,color=setColor(Site_name))))
addExplanation(AA,where="ul",title="Explanation")
dev.off()

#Just want individual site

g.all.pp2<-read.csv("P:/FPJ00/Phase2GeochemistryData/g_all_pp_ions.csv")

#then reshape data set 
names(g.all.pp2)
g.all.pp2<-dcast(g.all.pp2,formula = Site_name+Piez_Nest+Site_ID+MeanSampleDepth_ftBLS+medium_cd~srsname,value.var = "result_va_meq")
sapply(g.all.pp2,class)


?dcast

g.all.pp2.Ol<-filter(g.all.pp2, Site_name=="Olivia")
g.all.pp2.Ol<-g.all.pp2.Ol[with(g.all.pp2.Ol,order(Site_name,MeanSampleDepth_ftBLS)),]


with(g.all.pp2.Ol,piperPlot(xCat=Calcium,yCat=Magnesium,zCat=Sodium_Potassium,xAn=Cl_F_NO2_NO3,yAn=bicarbonate_carbonate,zAn=Sulfate))



#if you want different colors by depth:
unique(g.all.pp2.Ol$MeanSampleDepth_ftBLS)
setPDF(layout = "portrait",basename = "PiperByOlivia")
AA<-with(g.all.pp2.Ol,
         piperPlot(xCat=Calcium,yCat=Magnesium,zCat=Sodium_Potassium,xAn=Cl_F_NO2_NO3,yAn=bicarbonate_carbonate,zAn=Sulfate,
                   Plot=list(name=Site_ID,color=setColor(Site_ID))))
addExplanation(AA,where="ul",title="Explanation")
dev.off()



#Just want individual site

g.all.pp2<-read.csv("P:/FPJ00/Phase2GeochemistryData/g_all_pp_ions.csv")

#then reshape data set 
names(g.all.pp2)
g.all.pp2<-dcast(g.all.pp2,formula = Site_name+Piez_Nest+Site_ID+MeanSampleDepth_ftBLS+medium_cd~srsname,value.var = "result_va_meq")
sapply(g.all.pp2,class)


?dcast

g.all.pp2.HFC<-filter(g.all.pp2, Site_name=="HFC")
g.all.pp2.HFC<-g.all.pp2.HFC[with(g.all.pp2.HFC,order(Site_name,MeanSampleDepth_ftBLS)),]


with(g.all.pp2.HFC,piperPlot(xCat=Calcium,yCat=Magnesium,zCat=Sodium_Potassium,xAn=Cl_F_NO2_NO3,yAn=bicarbonate_carbonate,zAn=Sulfate))



#if you want different colors by depth:
unique(g.all.pp2.HFC$MeanSampleDepth_ftBLS)
setPDF(layout = "portrait",basename = "PiperByHFC")
AA<-with(g.all.pp2.HFC,
         piperPlot(xCat=Calcium,yCat=Magnesium,zCat=Sodium_Potassium,xAn=Cl_F_NO2_NO3,yAn=bicarbonate_carbonate,zAn=Sulfate,
                   Plot=list(name=Site_ID,color=setColor(Site_ID))))
addExplanation(AA,where="ul",title="Explanation")
dev.off()


#Just want individual site

g.all.pp2<-read.csv("P:/FPJ00/Phase2GeochemistryData/g_all_pp_ions.csv")

#then reshape data set 
names(g.all.pp2)
g.all.pp2<-dcast(g.all.pp2,formula = Site_name+Piez_Nest+Site_ID+MeanSampleDepth_ftBLS+medium_cd~srsname,value.var = "result_va_meq")
sapply(g.all.pp2,class)


?dcast

g.all.pp2.LF<-filter(g.all.pp2, Site_name=="Litchfield")
g.all.pp2.LF<-g.all.pp2.LF[with(g.all.pp2.LF,order(Site_name,MeanSampleDepth_ftBLS)),]


with(g.all.pp2.LF,piperPlot(xCat=Calcium,yCat=Magnesium,zCat=Sodium_Potassium,xAn=Cl_F_NO2_NO3,yAn=bicarbonate_carbonate,zAn=Sulfate))



#if you want different colors by depth:
unique(g.all.pp2.LF$MeanSampleDepth_ftBLS)
setPDF(layout = "portrait",basename = "PiperByLF")
AA<-with(g.all.pp2.LF,
         piperPlot(xCat=Calcium,yCat=Magnesium,zCat=Sodium_Potassium,xAn=Cl_F_NO2_NO3,yAn=bicarbonate_carbonate,zAn=Sulfate,
                   Plot=list(name=Site_ID,color=setColor(Site_ID))))
addExplanation(AA,where="ul",title="Explanation")
dev.off()




#Just want individual site

g.all.pp2<-read.csv("P:/FPJ00/Phase2GeochemistryData/g_all_pp_ions.csv")

#then reshape data set 
names(g.all.pp2)
g.all.pp2<-dcast(g.all.pp2,formula = Site_name+Piez_Nest+Site_ID+MeanSampleDepth_ftBLS+medium_cd~srsname,value.var = "result_va_meq")
sapply(g.all.pp2,class)


?dcast

g.all.pp2.CW<-filter(g.all.pp2, Site_name=="Cromwell")
g.all.pp2.CW<-g.all.pp2.CW[with(g.all.pp2.CW,order(Site_name,MeanSampleDepth_ftBLS)),]


with(g.all.pp2.CW,piperPlot(xCat=Calcium,yCat=Magnesium,zCat=Sodium_Potassium,xAn=Cl_F_NO2_NO3,yAn=bicarbonate_carbonate,zAn=Sulfate))



#if you want different colors by depth:
unique(g.all.pp2.CW$MeanSampleDepth_ftBLS)
setPDF(layout = "portrait",basename = "PiperByCW")
AA<-with(g.all.pp2.CW,
         piperPlot(xCat=Calcium,yCat=Magnesium,zCat=Sodium_Potassium,xAn=Cl_F_NO2_NO3,yAn=bicarbonate_carbonate,zAn=Sulfate,
                   Plot=list(name=Site_ID,color=setColor(Site_ID))))
addExplanation(AA,where="ul",title="Explanation")
dev.off()


#just want Olivia and HFC

g.all.pp2<-read.csv("P:/FPJ00/Phase2GeochemistryData/g_all_pp_ions.csv")

#then reshape data set 
names(g.all.pp2)
g.all.pp2<-dcast(g.all.pp2,formula = Site_name+Piez_Nest+Site_ID+MeanSampleDepth_ftBLS+medium_cd~srsname,value.var = "result_va_meq")
sapply(g.all.pp2,class)



g.all.pp2.OLHFC<-filter(g.all.pp2, Site_name=="Olivia" | Site_name=="HFC")
g.all.pp2.OLHFC<-g.all.pp2.OLHFC[with(g.all.pp2.OLHFC,order(Site_name,MeanSampleDepth_ftBLS)),]

#if you want different colors by the two sites:
unique(g.all.pp2.OLHFC$MeanSampleDepth_ftBLS)
setPDF(layout = "portrait",basename = "PiperByOLHFC")
AA<-with(g.all.pp2.OLHFC,
         piperPlot(xCat=Calcium,yCat=Magnesium,zCat=Sodium_Potassium,xAn=Cl_F_NO2_NO3,yAn=bicarbonate_carbonate,zAn=Sulfate,
                   Plot=list(name=Site_name,color=setColor(Site_name))))
addExplanation(AA,where="ul",title="Explanation")
dev.off()


#Jared's script to add both different colors and different shapes
BB<-with(g.all.pp2.OLHFC[g.all.pp2.OLHFC$Site_name=="HFC",],
         piperPlot(xCat=Calcium,yCat=Magnesium,zCat=Sodium_Potassium,xAn=Cl_F_NO2_NO3,yAn=bicarbonate_carbonate,zAn=Sulfate,
                   Plot=list(what="none"),ticks=TRUE))
setPDF(layout = "portrait",basename = "PiperByOLHFC")
AA<-with(g.all.pp2.OLHFC[g.all.pp2.OLHFC$Site_name=="Olivia",],
         piperPlot(xCat=Calcium,yCat=Magnesium,zCat=Sodium_Potassium,xAn=Cl_F_NO2_NO3,yAn=bicarbonate_carbonate,zAn=Sulfate,
                   Plot=list(what="none",ticks=TRUE)))
with(AA,addPiper(xCat=cations$x,yCat=cations$y,xAn=anions$x,yAn=anions$y,
                 xPip=piper$x,yPip=piper$y, 
                 Plot=list(symbol="uptri",color="blue",name= "Olivia"),
                 current=AA))

AA<-with(BB,addPiper(xCat=cations$x,yCat=cations$y,xAn=anions$x,yAn=anions$y,
                     xPip=piper$x,yPip=piper$y, 
                     Plot=list(symbol="circle",color="orange",name="HFC"),
                     current=AA))
addExplanation(AA,where="ul",title="Explanation")
dev.off()
