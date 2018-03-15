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

NumberOfSpecies <- 4

ReserveYear <- NA

MinSampleSize <- 1

years_combined = TRUE

############################Load and summarize length data######################
LengthDataAll <- read_csv("./data/LBS_lengths_forereef.csv")
 
LengthDataAll$Year[]<-ifelse(years_combined==TRUE,2015,LengthDataAll)
                          # col_types =list ( col_factor(levels = NULL),
                           #                  col_double(),
                            #                 col_factor(levels = NULL),
                             #                col_factor(levels = NULL),
                              #               col_factor(levels = NULL) ))


#Prints Species to be included in the analysis (those with sufficient sample size)
LBar_sp<- unique(LengthDataAll$Species.ID)
print(LBar_sp)

#Create Empty dataframes

Temp <- as_data_frame(matrix(NA,nrow=length(LBar_sp),ncol=9))
StoreAssess <- as_data_frame(matrix(NA,nrow=length(LBar_sp),ncol=12))


#Start for loop-----
for (i in 1:length(LBar_sp)){

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
  
# This should be the same species as the length data
 Fishes <- unique(Fish$Species)

# Function for calculating model (Lc)
 
 getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
 }
 # Use only fishery-dependent data to calculate Lc
 
 Fishery_length <- LengthData[LengthData$Data.Type=="Fishery",]
 
 Lc <- getmode(Fishery_length$Length)

# Run Assessments ---------------------------------------------------------
s=1

#   for (s in 1:length(Sites))    
#   {

#show(Sites[s])

#show(Fishes)

##########Creates Folders for Output########## 

a=1

FigureFolder<- paste0('Figures/')

ResultFolder<- paste('Results/')

  if (!file.exists(FigureFolder))
  {
    dir.create(FigureFolder,recursive=T)
    
    dir.create(ResultFolder,recursive=T)
  }

Fish$LHITol<- 0.99

nyr<-length(unique(LengthData$Year))

Temp[i,]<- LBAR(LengthDat=LengthData,LagLength=nyr,Weight=1,IncludeMPA=0,ReserveYr=NA,OutsideBoundYr=NA,Iterations=1000,
            BootStrap=1,LifeError=0,Lc=Lc)$Output		

StoreAssess[i,]<- data.frame(as.character(Fishes),as.character(Sites[s]),as.character(Assessments[a]),Temp[i,],stringsAsFactors = FALSE) 

}

colnames(StoreAssess) <- c("Species", "Site",	"Assessment","No_Year",	"Method",	"SampleSize",	"Value"	,"LowerCI","UpperCI",	"SD","Metric","Flag")

#######################Save Results#############################################
StoreAssess<-StoreAssess %>%
  select(Species,Assessment,SampleSize,Value,LowerCI,UpperCI,SD,Metric)

write.csv(file=paste('./Results/Moorea_LBAR_Results.csv',sep=''), StoreAssess)


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

