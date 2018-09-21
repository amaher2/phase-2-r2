#Stiff Plots
#Files names need to be updated

#Just want individual site

g.all.pp2<-read.csv("P:/FPJ00/Phase2GeochemistryData/g_all_pp_ions.csv")

#then reshape data set 
names(g.all.pp2)
g.all.pp2<-dcast(g.all.pp2,formula = Site_name+Piez_Nest+Order+Site_ID+MeanSampleDepth_ftBLS+medium_cd~srsname,value.var = "result_va_meq")
sapply(g.all.pp2,class)


#Stiff Plots

g.all.pp2.Ol<-filter(g.all.pp2, Site_name=="Olivia")
g.all.pp2.Ol<-g.all.pp2.Ol[with(g.all.pp2.Ol,order(Site_name,MeanSampleDepth_ftBLS)),]



#Trying to figure out stiff diagram


setSweave("piperplot04", 6, 6)
AA.lo<-setLayout(height=3.5, explanation=list(bottom=1.1))
setGraph(1, AA.lo)

AA.pl<-with(g.all.pp2.Ol, stiffPlot(cbind(Calcium, Magnesium, Sodium), cbind(Chloride, Sulfate, Bicarbonate), ylabels = Order))


setGraph("explanation", AA.lo)
addExplanation(AA.pl)
dev.off()



#Stiff Plots

g.all.pp2.HFC<-filter(g.all.pp2, Site_name=="HFC")
g.all.pp2.HFC<-g.all.pp2.HFC[with(g.all.pp2.HFC,order(Site_name,MeanSampleDepth_ftBLS)),]



#Trying to figure out stiff diagram


setSweave("stiffplotHFC", 6, 6)
AA.lo<-setLayout(height=3.5, explanation=list(bottom=1.1))
setGraph(1, AA.lo)

AA.pl<-with(g.all.pp2.HFC, stiffPlot(cbind(Calcium, Magnesium, Sodium), cbind(Chloride, Sulfate, Bicarbonate), ylabels = Order))


setGraph("explanation", AA.lo)
addExplanation(AA.pl)
dev.off()


#Stiff Plots

g.all.pp2.LF<-filter(g.all.pp2, Site_name=="Litchfield")
g.all.pp2.LF<-g.all.pp2.LF[with(g.all.pp2.LF,order(Site_name,MeanSampleDepth_ftBLS)),]



#Trying to figure out stiff diagram


setSweave("stiffplotLF", 6, 6)
AA.lo<-setLayout(height=3.5, explanation=list(bottom=1.1))
setGraph(1, AA.lo)

AA.pl<-with(g.all.pp2.LF, stiffPlot(cbind(Calcium, Magnesium, Sodium), cbind(Chloride, Sulfate, Bicarbonate), ylabels = Order))


setGraph("explanation", AA.lo)
addExplanation(AA.pl)
dev.off()



#Stiff Plots

g.all.pp2.CW<-filter(g.all.pp2, Site_name=="Cromwell")
g.all.pp2.CW<-g.all.pp2.CW[with(g.all.pp2.CW,order(Site_name,MeanSampleDepth_ftBLS)),]



#Trying to figure out stiff diagram


setSweave("stiffplotCW", 6, 6)
AA.lo<-setLayout(height=3.5, explanation=list(bottom=1.1))
setGraph(1, AA.lo)

AA.pl<-with(g.all.pp2.CW, stiffPlot(cbind(Calcium, Magnesium, Sodium), cbind(Chloride, Sulfate, Bicarbonate), ylabels = Order))


setGraph("explanation", AA.lo)
addExplanation(AA.pl)
dev.off()
