##################       Script to run full analysis      #############################
##################      Extracted from markdown file      #############################

rm(list=ls())

# load packages
library(BasketballAnalyzeR)
library(data.table)
library(ellipse)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(plotly)
library(RColorBrewer)
library(corrplot)
library(dygraphs)
library(dplyr)
library(knitr)
library(formattable)
library(kableExtra)
library(stringr)

## Must set directory first

################################## DATA DOWNLOAD #################################################

EventData <- read.csv("NBA-PBP-2018-2019.csv")
PlayerData <- read.csv("Player Data 2018-19.csv")
BioData <- read.csv("BioStats 2018-19.csv")

################################## DATA MANIPULATION #############################################

colnames(BioData)[1]<-'Player'

#Name Conversion Table
tabNames<- read.csv("NamingTable 2018-19.csv")
colnames(tabNames)<-c('Pbox','Bio','PbP','Team')

#No need for factors in naming conversion table
tabNames[,] <- lapply(tabNames, function(x) type.convert(as.character(x), as.is = TRUE))

#Manipulate data
PbP <- EventDataManipulation(EventData)
Pbox <- PlayerManipulation(PlayerData,BioData)

#Remove raw datasets that are no longer needed 
rm(BioData,EventData,PlayerData)

#sumTeam counts occurrences of shooting and rebound stats by team, and adds to Player data
Pbox=sumTeam(PbP,Pbox)


################################## VARIABLE CREATION #############################################
Pbox = data.table(Pbox)
#Four Factor variables
Pbox[, TPP := 100*(TOV / (FGA+0.45*FTA+TOV-ORB))]

Pbox[, ORp1 := 100*(((TMIN)/5)/(MIN))*(ORB/((TP2A-TP2M)+(TP3A-TP3M)+(TFTA-TFT)))]

Pbox[, ORp2 := 100*((ORB*(TMIN)/5)/(MIN*(TORB+ODRB)))]

# Complementary DRp
Pbox[, DRp2 := 100*((DRB*(TMIN)/5)/(MIN*(OORB+TDRB)))]

#Usage percentage 
Pbox[, USGp := (100*((FGA+0.44*FTA+TOV)*(TMIN/5)) / (MIN*(TFGA+0.44*TFTA+TTOV)))]

# Assist percentage (defensive player)
Pbox[, ASTp := (100*AST/(((MIN/(TMIN/5))*TFGM)-FGM))]

# Blocks and Steals per game
Pbox[, BLKg := BLK/G]
Pbox[, STLg := STL/G]

# Dropping teamcolumns we do not need anymore 
Pbox = subset(Pbox, select = -c(TDRB, TORB, ODRB, OORB, TMIN, TP3M, TP3A, TP2M, TP2A, TFT, TFTA, TFGM, TFGA, TTOV ))

Pbox = data.frame(Pbox)

#Add assist variables 
assistscore = assistStat(PbP)
assistscore = merge(tabNames, assistscore, by.x = c("PbP","Team"), by.y = c("Shooter","Team") )
assistscore = subset(assistscore, select=-c(Bio, PbP, AST, FGM))

# merge by team and player - collation by player occurs at later stage 
Pbox = merge(Pbox, assistscore , by.x = c("Player","Tm"), by.y =c("Pbox","Team"), all = TRUE)

Pbox= data.table(Pbox)

#Checking if we have duplicates:
Pbox[duplicated(Pbox[,c("Tm","Player")]),]

#Add column to display the total minutes a player plays across season
t=Pbox %>%
  dplyr::group_by(Player) %>%
  dplyr::summarise(TOTMins=sum(MIN))

Pbox=merge(Pbox,t,by.x = "Player",by.y = "Player")

#Create weighting column
Pbox$Weighting = Pbox$MIN/Pbox$TOTMins

#Alter columns by weighting scheme
weight_cols = c('height_cm', 'Weight', 'ORp1', 'ORp2', 'DRp2', 'USGp', 'ASTp')
for (col in weight_cols) {
  colVal = as.numeric(Pbox[[col]])
  Pbox[[col]]= colVal*(Pbox$Weighting)
}

#Standardise percentage variables
Pos=Pbox[,c("Player","Pos")]

Pbox=Pbox %>%
  dplyr::group_by(Player) %>% 
  dplyr::summarise_if(is.numeric,sum)

Pbox$Pos=Pos$Pos[match(Pbox$Player,Pos$Player)]

#Redo percentages
Pbox$FGp=100*Pbox$FGM/Pbox$FGA
Pbox$P3p=100*Pbox$P3M/Pbox$P3A
Pbox$P2p=100*Pbox$P2M/Pbox$P2A
Pbox$FTp=100*Pbox$FT/Pbox$FTA
Pbox$FGM_ASTp=100*Pbox$FGM_AST/Pbox$FGM
Pbox$TPP=100*(Pbox$TOV / (Pbox$FGA+0.45*Pbox$FTA+Pbox$TOV-Pbox$ORB))
Pbox$BLKg=Pbox$BLK/Pbox$G
Pbox$STLg=Pbox$STL/Pbox$G

#Add a column in PbP specifying shooter by Pbox naming convention
tabTemp=subset(PbP,Shooter!="")

#Add ID col to tabNames & tabTemp for ease of use
tabNames$ID=paste(tabNames$PbP,tabNames$Team,sep = "_")
tabTemp$ID=paste(tabTemp$Shooter,tabTemp$PlayTeam,sep = "_")

#add pbox names to tabTemp
tabTemp$Pbx_names=tabNames$Pbox[match(tabTemp$ID,tabNames$ID)]

#Add difficulty score from PbP to Pbox
tabTemp <- tabTemp %>%
  dplyr::group_by(Pbx_names) %>%
  dplyr::filter(Shooter!="") %>%
  dplyr::summarise(diffM=mean(score_diff))

##merge
Pbox = merge(Pbox,tabTemp, by.x = "Player",by.y = "Pbx_names",all.x = TRUE)

rm(tabTemp)

#Function to clean the Pbox for clustering
Pbox=CleanData(Pbox)

# df_select is the dataframe that we are working with
Pbox = dplyr::rename(Pbox,diff=diffM, ORp = ORp2, DRp = DRp2)
df = Pbox
df[is.na(df)] = 0
mMIN = 500
df = subset(df, MIN>=mMIN)
ID = df$Player
df_select= subset(df, select = c(FTp,P2p, P3p, diff, ORp, DRp, USGp, ASTp, FGM_ASTp, TPP, STLg,BLKg))

## study of correlation analysis to see if all variables should be included
out=cor(df_select)
corrplot(out, type = "lower", order = "hclust", tl.col = "black", tl.srt = 45)

# Calculate MDS and the points for the reduced two dimensions
points = MDS(df_select, 2, std=TRUE)[["points"]]
points = data.frame(points)
colnames(points) = c("X1","X2")

#add them to the end of our dataset
df = cbind(df_select, points)


################################### kmeans clustering #################################################
# inital k means to test 
set.seed(1)
kmeans_extension<- kclustering(df_select,nclumax = 15)

#8 clusters 
set.seed(1)
kclu8=kclustering(df_select,labels=ID,k=7)

#13 clusters to recreate book
set.seed(1)
kclu13=kclustering(df_select,labels=ID,k=13)

# MDS
points = MDS(df_select, 3, std=TRUE)[["points"]]
points = data.frame(points)
colnames(points) = c("X1","X2", "X3")

# Clustering
subjects = kclu13[["Subjects"]]

#Combining results 
ID = ID
df_extend = cbind(subjects, df_select)
df_extend = cbind(df_extend, points)

# we want the clusters to be discrete, that is why we are transforming them into factors 
df_extend$Cluster = as.factor(df_extend$Cluster)


data=Pbox
data[is.na(data)] <- 0
ID=ID
data=subset(data,MIN>=mMIN,select=c(FTp,P2p,P3p,diff,ORp,DRp,USGp,ASTp,FGM_ASTp,TPP,height_cm,Weight,MIN,PTS,STLg,BLKg))

########################################## VARIABLE SELECTION PROCEDURE ################################

#Run Variable Selection Algorithm 13 clusters, on all percentage variables
set.seed(1)
var13=VariableSelect(data,13,13,nStarts = 1,printFull = TRUE)


#Run Variable Selection Algorithm 13 clusters, on all percentage variables, now with 25 starts
set.seed(1)
varTemp=VariableSelect(data,13,13,nStarts = 25)

#Run Variable Selection Algorithm 13 clusters, on all percentage variables, now with 25 starts
set.seed(1)
varTemp=VariableSelect(data,2,13,nStarts = 1)

#Run Variable Selection Algorithm 13 clusters, on all percentage variables, now with 25 starts
set.seed(1)
varTemp=VariableSelect(data,2,13,nStarts = 25)


## Clustering Output from Variable Selection Algorithm 

#Set data frame to only include only these variables
df = data[,var13]

# Calculate kclustering with the selected variables
set.seed(1)
kclust_bic = kclustering(df, labels = ID, k=13)


# MDS
points = MDS(df, 3, std=TRUE)[["points"]]
points = data.frame(points)
colnames(points) = c("X1","X2", "X3")

# Clustering
bic_clust=kclust_bic[["Subjects"]]

#Combining results 

df$Player = ID
df = cbind(df, points)
df = merge(df, bic_clust, by.x = "Player", by.y = "Label")
df = dplyr::rename(df, bic_clust = Cluster)

# we want the clusters to be discrete, that is why we are transforming them into factors 
df$bic_clust = as.factor(df$bic_clust)











