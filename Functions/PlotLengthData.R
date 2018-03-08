PlotLengthData<- function(LengthDat,FigureFolder,Fish,Species,Theme)
{
  LengthDat<- LengthData
  
  #LengthDat<- subset(LengthDat,is.na(Length)==F & Length>0)
  
  #LengthDat$SiteType[LengthDat$MPA==0]<- 'Fished'
  
  #engthDat$SiteType[LengthDat$MPA==1]<- 'MPA'
  
  LengthDat$Year<-as.factor(LengthDat$Year)
  num<-nrow(LengthDat)
  minimum<-min(LengthDat$Length)-5
  maximum<-Fish$Linf+10
  
  data_LengthDist<-ggplot(data=LengthDat,aes(x=Length,fill=Data.Type,color=Data.Type))+
    geom_histogram(position="identity",binwidth=1,alpha=0.6)+
    geom_vline(xintercept=Lc,linetype='longdash')+
    geom_text(aes(x=Lc, y=5,label = "Lc "),hjust="right",color="black",size=3,family="Helvetica") +
    #geom_vline(xintercept=Fish$Linf,linetype='solid',color='black')+
    #facet_wrap(paste("n =",length(LengthData$Length),sep=""),as.table=F)+
    #facet_wrap(~Year,as.table=F)+
   # ggtitle(paste(Species,'\nn =',num))+
    Theme+xlab("Length (cm)")+ylab("Frequency")+
    scale_x_continuous(limits=c(minimum,maximum),expand = c(0, 0))+ 
    scale_y_continuous(expand = c(0, 0))+
    scale_colour_discrete(guide = FALSE) 
  #LengthDist<-LengthDist+labs(fill="Data Type",values=c(Surveycolor,Fisherycolor),breaks=c("Surveysss","Fishery"))
 # LengthDist<-LengthDist+scale_fill_discrete(name="Data Type", breaks=c("Fishery","Survey"),
  #                                           labels=c(paste('Fishery',sep='\nn=',FLA),paste('Survey',sep='\nn=',SLA)))
  
  
  yr_LengthDist<-ggplot(data=LengthDat,aes(x=Length,fill=Year,color=Year))+
    geom_histogram(position="identity",binwidth=1,alpha=0.6)+
    geom_vline(xintercept=Fish$Mat50,linetype='longdash')+
    geom_text(aes(x=Fish$Mat50, y=5,label = "Lmat ",fontface="plain"),hjust="right",color="black",size=3,family="Helvetica") +
    geom_vline(xintercept=Fish$Linf,linetype='solid',color='black')+
    geom_text(aes(x=Fish$Linf, y=5,label = "Linf ",fontface="plain"),hjust="right",color="black",size=3,family="Helvetica") +
    #facet_wrap(paste("n =",length(LengthData$Length),sep=""),as.table=F)+
    #facet_wrap(~Year,as.table=F)+
    ggtitle(paste(Species,'\nn =',num))+
    Theme+xlab("Length (cm)")+ylab("Frequency")+
    scale_x_continuous(limits=c(minimum,maximum),expand = c(0, 0))+ 
    scale_y_continuous(expand = c(0, 0))+
    scale_colour_discrete(guide = FALSE) 
  compare_means(Length ~ Year, data = LengthDat)
  
 avg_yr_plot<-ggplot(data=LengthDat,aes(Year,Length))+
    geom_boxplot()+
    Theme 
 # avg_yr_plot + stat_compare_means()

  pdf(file=paste(FigureFolder,Species,'_Length Hist.pdf',sep=''),width=7,height=8)
   ggarrange(yr_LengthDist, data_LengthDist,avg_yr_plot, nrow=3,ncol=1)
   dev.off()
  
}