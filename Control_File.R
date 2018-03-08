#Final Control File 
#MNI Length Assessment - All Species 
#
#Runs an LBAR assessment for each species in the data set
#Require data frame of length and data frame of life history paramters
#

##Set Up----

rm(list = ls())


if (basename(getwd())!='moorea'){
  print("moorea")
}

pkgs <- c('tidyr','readr', 'ggplot2','tidyr', 'R2admb', 'dplyr', 'animation', 'ggpubr')

#This will install necessary packages on local machine
load.packages = function(a){
  if(!require(a, character.only = TRUE)){
    install.packages(a)
    library(a, character.only = TRUE)
  }
}

lapply(pkgs, load.packages)

#Source functions to file
sapply(list.files(pattern="[.]R$", path="./Functions", full.names=TRUE), source)
source("./Functions/SubFunctions.R") #Pull in helper functions for assessment modules

############High Level Assessment Controls############

#Assessments to run with this control file
Assessments <- c('LBAR')

Counter<- 0

#Sites to assess
Sites<- c('All')

AssessmentResults<- list()

MonteResults<- list()

dir.create(Assessment)

NumIterations <- 1000

RunAssessments <- TRUE 

NumberOfSpecies <- 1

ReserveYear <- NA

MinSampleSize <- 1

############################Load and summarize length data######################
 LengthDataAll <- read_csv("./data/LBS_lengths_forereef.csv")
LengthDataAll$Year[]<-2015
                          # col_types =list ( col_factor(levels = NULL),
                           #                  col_double(),
                            #                 col_factor(levels = NULL),
                             #                col_factor(levels = NULL),
                              #               col_factor(levels = NULL) ))

#Filter out Species with a FISHERY DEPENDENT sample size < MinSampleSize 

#LengthDataFiltered<- SelectForLengthSampleSize(LengthDataAll)

#names(LengthDataFiltered)

#Prints Species to be included in the analysis (those with sufficient sample size)
LBar_sp<- unique(LengthDataAll$Species.ID)
print(LBar_sp)

#Prints gears represented in the selected sample
#print(unique(LengthDataFiltered$Gear.Type))


#All species IDs
#sp_id<-c("ACANCH","ACANCO","BALIVE","HOLOAD","LUTJMA","LUTJSY","LUTJVI","SCARVI","SERRGU")

#Generates named vector to link sp_id with common name
# common<-c(
#   ACANCH="Doctorfish",
#   ACANCO="Blue Tang",
#   BALIVE="Old wife",
#   HOLOAD="Squirrelfish",
#   LUTJMA="Mahogany snapper",
#   LUTJSY="Lane snapper",
#   LUTJVI="Silk snapper",
#   SCARVI="Redtail parrotfish", 
#   SERRGU="Red hind")

#Create Empty dataframes

Temp <- as_data_frame(matrix(NA,nrow=length(LBar_sp),ncol=9))


#Start for loop-----
for (i in 1:length(LBar_sp)){

  i=4
  LengthData<-filter(LengthDataAll, Species.ID==LBar_sp[i]) 


#######################Life History#############################################

 Fish<-read_csv("./data/LH_parameters.csv") %>%
   filter(Species==LBar_sp[i]) %>%
   spread(Parameter, Value) %>%
   rename("MaxAge"="tmax",
          "vbk" = "K") %>%
   mutate(LHITol = 0.1,
          LengthError = 0.5,
          AgeSD=0.5,
          MortalityError = 0.1,
          VBSD = 0.05,
          VBErrorSlope = 0.1,
          MvK = M/vbk) 
  
   


 #Fish$AgeMat50<- NA
 #Fish$AgeMatSource<- NA

 #Fish$MaxAge <- ceiling(-log(0.05)/Fish$M)

 Fishes <- unique(Fish$Species)

 getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
 }
Fishery_length <- LengthData[LengthData$Data.Type=="Fishery",]
 Lc <- getmode(Fishery_length$Length)

# Run Assessments ---------------------------------------------------------
s=1

#   for (s in 1:length(Sites))    
#   {

#show(Sites[s])

#show(Fishes)

##########Creates Folders for Output########## 

#Directory<- paste(Assessments, "/", Sites[s],'/',Fishes,'/',sep='')

FigureFolder<- paste0('Figures/')

ResultFolder<- paste('Results/')

  if (!file.exists(FigureFolder))
  {
    dir.create(FigureFolder,recursive=T)
    
    dir.create(ResultFolder,recursive=T)
  }



a=1
Fish$LHITol<- 0.99
#       
#       for (a in 1:length(Assessments)) #Loop over possible assessments, store in Assessment results. Many assessments have more detailed outputs than can also be accessed 
#       {

Counter<- Counter+i
 
#Checks for sufficient sample size with Years of species       
#SampleCheck<- CheckLengthSampleSize(LengthData)        
#           
#           if (SampleCheck$YearsWithEnoughData>0)
#           {

nyr<-length(unique(LengthData$Year))

Temp[i,]<- LBAR(LengthDat=LengthData,LagLength=2,Weight=1,IncludeMPA=0,ReserveYr=NA,OutsideBoundYr=NA,Iterations=1000,
            BootStrap=1,LifeError=0,Lc=Lc)$Output		

StoreAssess[i,]<- data.frame(Fishes,Sites[s],Assessments[a],Temp[i,],stringsAsFactors=F) 

}

colnames(StoreAssess) <- c("Species", "Site",	"Assessment","	Year",	"Method",	"SampleSize",	"Value",	"LowerCI","UpperCI",	"SD",	"Metric",	"Flag")

#######################Save Results#############################################


write.csv(file=paste('./Results/LBAE_Results.csv',sep=''), StoreAssess)


#######################Plot Length Distributions#############################################
sp_id<-unique(LengthDataAll$Species.ID)
Font <- 'Helvetica'
FontColor <- 'Black'
PlotFontSize <- 11
Surveycolor<-"red"
Fisherycolor <- "lightseagreen"

for (j in 1:length(sp_id)){
  j=4
#Life History Parameters for species[i]  
  Fish<-read.csv("./Data/LH_parameters.csv")%>%
    filter(Species==sp_id[j]) %>%
  #  select(-Reference) %>%
    spread(Parameter, Value)
  
  Fish$Mat50 <- Fish$m50


#Specifies where to save plots
  FigureFolder<- paste("./Figures/")
  name="Data Type"
  
  Theme<- theme(plot.background=element_rect(color='white'),
                rect=element_rect(fill='transparent',color=NA),
                text=element_text(size=11,color=FontColor),plot.title = element_text(),
                axis.text.x=element_text(color=FontColor), axis.text.y=element_text(color="black"),
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                panel.background=element_blank(),
                strip.background = element_rect(colour="white", fill="white"),
                axis.line.x = element_line(color="black", size = 0.6),
                axis.line.y = element_line(color="black", size = 0.6))
 
  Species=sp_id[j]

  LengthDataPlot<-filter(LengthDataAll, Species.ID==sp_id[j])
  
  PlotLengthData(LengthDat=LengthDataPlot,FigureFolder,Fish,Species,Theme)
}

