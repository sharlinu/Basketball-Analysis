###############################################################################################################
# twod: 2-dimensional plotting function with ggplot
twod = function(data, data.var, w.var){
  # creating a subsetted dataset with the axis of the plot
  df <- data[, data.var]
  names(df) <- c("x","y")
  
  # creating another subset with the variable that should serve as the color scale 
  w <- data[, w.var]
  # depending on the datatype of the variable, the color scale will be discrete(for characters or factors) or continous(for numeric values)
  if (is.character(w)) {
    w = factor(w)
    df$w = w }
  else if (is.factor(w) | is.numeric(w)) {
    df$w = w}
  
  # plotting the values
  p <- ggplot(data=df, aes(x=x, y=y, color=w))
  
  # adding the markers 
  p <- p + geom_point()
  
  # adding either a discrete or continous color scale 
  if (is.factor(w)) {
    p = p + scale_color_manual(name=w.var, values=topo.colors(length(unique(w))))}
  else if (!is.factor(w)){
    p = p + scale_color_gradientn(name=w.var, colors=topo.colors(length(unique(w))))}
  
  # adjusting labels in the plot
  p <- p + labs(title="", x="", y="") + theme_light()
  return(p)
}

###############################################################################################################
# threed: 3-dimensional plotting function with ggplot
threed = function(data, data.var, w.var){
  
  # creating a subsetted dataset with the axis of the plot
  df <- data[, data.var]
  names(df) <- c("x","y","z")
  
  # creating another subset with the variable that should serve as the color scale 
  w <- data[, w.var]
  
  # depending on the datatype of the variable, the color scale will be discrete(for characters or factors) or continous(for numeric values)
  if (is.character(w)) {
    w = factor(w)
    df$w = w }
  else if (is.factor(w) | is.numeric(w)) {
    df$w = w}
  
  # plotting the values 
  p = ggplot(df, aes(x=x,y=y,z=z, color=w)) +
    axes_3D() +   # adding 3-d axes
    stat_3D() +   # adding 3-d markers
    theme_light() # choosing the style of the plot
  
  # adding either a discrete or continous color scale 
  if (is.factor(w)) {
    p = p + scale_color_manual(name=w.var, values=topo.colors(length(unique(w))))}
  else if (!is.factor(w)){
    p = p + scale_color_gradientn(name=w.var, colors=topo.colors(length(unique(w))))}
  
  return(p)
}


###############################################################################################################
# MDS: function to apply an MDS algorithm to reduce data into a k-dimensional metric space

MDS <- function(data, k=2, std=TRUE ) {
  # data must be one of the below datatypes
  if (!is.matrix(data) & !is.data.frame(data) & (!inherits(data, "dist"))) {
    stop("'data' must be a matrix, a data frame, or a distance matrix")
  }
  
  # we need to work with a distance matrix, that is why we first standardize (if choosen to) and then transform non-distance matrix to a distance matrix
  if (!inherits(data, "dist")) {
    if (std) {
      data_for_dist <- scale(data)
    } else {
      data_for_dist <- data
    }
    dist.mat <- dist(data_for_dist)
  } else {
    dist.mat <- data
  }
  
  # we apply the non-metric MDS approach here and configure the starting configuration as the result of the metric MDS calculation (cmdscale())
  # k is the number of dimensions we want to reduce to 
  # the outout typically comes with the k-dimensional coordinates for every observation in the dataset and the stress index
  out <- MASS::isoMDS(dist.mat, k, y=cmdscale(dist.mat, k), maxit=100, trace=FALSE)
  
  # here we add additional information to the output: 
  # the original data
  out[["data"]] <- data
  # the created distance matrix
  out[["dist"]] <- dist.mat
  # if the values are standarized
  out[["std"]] <- std
  
  class(out) <- append("MDSmap", class(out))
  return(out)
}

###############################################################################################################
## EventDataManipulation: Function to manipulate 'play-by-play' or 'event' data into required format for analysis 
EventDataManipulation <- function(EventData){
  
  #Remove playoff games
  data <- subset(EventData,GameType!="playoff") 
  NameTable=tabNames
  
  ##create dummy shot clock, using seconds left clock
  r=as.numeric(data$SecLeft)
  r1=c(r[2:nrow(data)],0)
  r2=r-r1
  
  #Remove end of quarter 'follow on' errors, and set columns for Play Length and Period Time
  r2=ifelse((r2<=-690),720+r2,r2)
  data$PlayLength=c(0,r2[1:nrow(data)-1])
  data$PeriodTime=720-data$SecLeft
  
  ##merge FT and FG into a unique Shot Type, Shooter and Shot Outcome column
  data$Shooter<-factor(paste(data$Shooter,data$FreeThrowShooter))
  data$ShotOutcome<-paste(data$ShotOutcome,data$FreeThrowOutcome)
  data$ShotType<-paste(data$ShotType,data$FreeThrowNum)
  
  ##Remove unneeded cols
  colsDrop <- c("FreeThrowShooter","FreeThrowOutcome","FreeThrowNum","GameType","Location","Date","Time","TimeoutTeam","EnterGame","LeaveGame","JumpballAwayPlayer","JumpballHomePlayer","JumpballPoss")
  data=data[,!(names(data) %in% colsDrop)]
  
  #Drop empty levels from factors
  fact_vars <- sapply(data, function(x) is.factor(x))
  data[,fact_vars] <- lapply(data[,fact_vars], function(x) droplevels(x))
  
  #Name each game
  URLid=factor(data$URL,labels=c(1:nlevels(data$URL)))
  data$ID=URLid
  
  #clean id
  data$ID <- readr::parse_number(as.character(data$ID))
  
  ##Remove Team Names & White spaces from players 
  colRemove=c("Shooter","Assister","Blocker","Fouler","Fouled","Rebounder","TurnoverPlayer","TurnoverCauser")
  for (i in colRemove) {
    data[,i]=removeTeam(data[,i])
    data[,i]=whiteSpace(data[,i])
  }
  data$ShotOutcome <- whiteSpace(data$ShotOutcome)
  
  ##Redo factor levels
  fact_vars <- sapply(data, function(x) is.factor(x))
  data[,fact_vars] <- lapply(data[,fact_vars], function(x) droplevels(x))
  
  ## Convert shot type to easier format
  data$ShotType<-factor(data$ShotType)
  shots=levels(data$ShotType)
  shots[str_detect(shots, " of ")] = "FT"
  shots[str_detect(shots, "2-pt")] = "2P"
  shots[str_detect(shots, "3-pt")] = "3P"
  shots[str_detect(shots, "technical")] = "FT"
  data$ShotType<-factor(data$ShotType,labels=shots)
  
  ## Add column to take into account points made from shot
  r=as.character(data$ShotType)
  r[str_detect(r, "2P")] = 2
  r[str_detect(r, "3P")] = 3
  r[str_detect(r, "FT")] = 1
  made=as.character(data$ShotOutcome)
  made[str_detect(made,"make")]=1
  made[str_detect(made,"miss")]=0
  points=as.numeric(r)*as.numeric(made)
  data$points=points
  
  ## Add column for away/home play ticker
  r=as.character(data$AwayPlay)
  r=ifelse(r!="","A","H")
  data$PlayTicker=r
  data$PlayTeam=ifelse(as.character(data$PlayTicker)=="A",as.character(data$AwayTeam),as.character(data$HomeTeam))
  
  #Add difficulty probability
  data<-ProbCalc(data)
  
  return(data)
}

#Function to remove any team name suffix from player name in a column 
removeTeam <- function(dataColumn){
  players=levels(dataColumn)
  players=sapply(players,function(x) str_replace_all(x,"\\s-\\s\\w{3}",""))
  r <- factor(dataColumn,labels=players)
  r=as.character(r)
  return(r)
}

##################################################################################################
## Function to add probability scores to shooting events based on probability scores in book
ProbCalc <- function(data){
  
  #Prob vec
  ProbVec <- c(0.4086
               ,0.4764
               ,0.4678
               ,0.5501
               ,0.6035
               ,0.6580
               ,0.2939
               ,0.3504
               ,0.3844
               ,0.3119
               ,0.7481
               ,0.7115
  )
  names(ProbVec)<-c(1:12)
  
  #Calc score diff for away & home options
  HomeAway=ifelse(data$PlayTicker=="H",1,-1)
  scoreDiff=(data$HomeScore-data$AwayScore)*HomeAway
  
  #Remove points scored on current shot for score difference calculation
  data$sc.diff <-scoreDiff-data$points 
  
  ## For free throw probability, we need to calculate whether each player scored on their previous free throw
  #  For each player/team combination, we subset and add ticker for whether the player scored their previous FT
  
  for (row in 1:nrow(tabNames)){
    
    nm=tabNames$PbP[row]
    tm=tabNames$Team[row]
    tabTemp<-subset(data,Shooter==nm & PlayTeam==tm & ShotType=="FT")
    
    ## VecPrev is the vector of 1's and 0's indication if the previous free throw was scored
    if (nrow(tabTemp)==1){
      
      #Assumption; all first FT's assume previous FT was made
      vecPrev=1
    } else if (nrow(tabTemp)>=2) {
      vecPrev=ifelse(tabTemp$ShotOutcome=="make",1,0)
      
      vecPrev=c(1,vecPrev[1:(length(vecPrev)-1)])
      
    }
    
    data$prev_ft[(data$Shooter==nm & data$PlayTeam==tm & data$ShotType=="FT")]=vecPrev
  }
  
  #Create only shooting table
  tabTemp=subset(data,Shooter!="")
  
  #create empty vector that we will assign shot difficulty to
  vecDifficulty=c(1:nrow(tabTemp))
  
  #VecOutcome is the vector that gives 1 if shot was made, 0 if missed
  vecOutcome=ifelse(tabTemp$points>=1,1,0)
  
  #Loop through all shots and assign difficulty to each
  for (row in 1:nrow(tabTemp)){
    shot_type=as.character(tabTemp$ShotType[row])
    score_diff=as.numeric(tabTemp$sc.diff[row])
    prev_ft=as.numeric(tabTemp$prev_ft[row])
    shot_clock=25-as.numeric(tabTemp$PlayLength[row])
    quarter_time=as.numeric(tabTemp$SecLeft[row]) 
    Index=sortIndex(shot_type,prev_ft,shot_clock,quarter_time,score_diff)
    vecDifficulty[row]=ProbVec[[Index]]
  } 
  
  #difficulty is the shot difficulty, score_diff is the actual score given to each shot, taking into account difficulty
  data$difficulty[(data$Shooter!="")]=vecDifficulty
  data$score_diff[(data$Shooter!="")]=vecOutcome-vecDifficulty  
  
  return(data)
  
}


#################################################################################################################################
## Function to remove white space in columns
whiteSpace <- function(dataColumn){
  dataColumn=as.character(dataColumn)
  dataColumn=trimws(dataColumn,which = c("both"))
  return (dataColumn)
}

################################################################################################################################
## Function to sort a shot by a specification into its correct category, specified by index. Assume made in original 24 sec clock
sortIndex <- function(shot_type,prev_ft,shot_clock,quarter_time,score_diff,poss_type=TRUE){
  
  
  if (shot_type=="2P"){
    if (shot_clock<=2){
      #time end
      index=1
    } else if (shot_clock>2 & shot_clock<=10){
      #middle end
      index=2
      
    } else if (shot_clock>10 & shot_clock<=17){
      #Early middle
      if (poss_type) {
        #made in original 24 secs
        if (score_diff<=-15){
          #if score difference less than -15
          index=3
        } else {
          index=4
        }
        
      } else {
        #made in second 14-secs restart
        index=5
      }
      
      
    } else {
      #early
      index=6
    }
  } else if (shot_type=="3P"){
    if (shot_clock<=2){
      #time end
      index=7
    } else {
      if (quarter_time>=100) {
        if (shot_clock>2 & shot_clock<=10) {
          index=8
        } else {
          index=9
        }
      } else {
        index=10
      }
    }
    
  } else {
    #Free throws
    
    if (prev_ft==1) {
      #made previous free throw
      index=11
    } else {
      #didn't make previous free throw
      index=12
    }
  }
  
  return(index)
  
}


#########################################################################################################
## Function to combine the player data and Bio data, and reformulate to a readable and replicable format 
PlayerManipulation <- function(PlayerData,BioData){
  
  # Remove empty rows
  data<-subset(PlayerData,Player!="")
  
  # Convert names of players
  plyer=as.character(data$Player)
  
  # encode special characters
  Encoding(plyer) <- "UTF-8"
  
  # remove ids
  plyer=str_replace_all(plyer,"\\\\.+","")
  
  # Remove special characters
  plyer=iconv(plyer, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  
  # Remove all name additions to ensure we can match with event data
  plyer=str_replace(plyer," Jr.","")
  plyer=str_replace(plyer," Sr.","")
  plyer=str_replace(plyer," III","")
  plyer=str_replace(plyer," II","")
  plyer=str_replace(plyer," IV","")
  
  data$Player=plyer
  
  # Remove Total rows
  data<-subset(data,Tm!="TOT")
  
  ## Drop empty levels from factors
  fact_vars <- sapply(data, function(x) is.factor(x))
  data[,fact_vars] <- lapply(data[,fact_vars], function(x) droplevels(x))
  
  #Assign transformed player data to variable PlayerDataNew. We will use this to merge later
  PlayerDataNew=data
  
  ## Now manipulate Bio Dataset and merge with Pbox
  
  #Remove any empty rows
  data<-subset(BioData,Player!="")
  
  #Convert height from ft and inches to cm  
  data$height_cm=(as.numeric(data$Height_ft)*30.48)+(as.numeric(data$Height_in)*2.54)
  
  #Convert names of players
  plyer=as.character(data$Player)
  
  #encode special characters
  Encoding(plyer) <- "UTF-8"
  plyer=iconv(plyer, from = 'UTF-8', to = 'ASCII//TRANSLIT')
  plyer=str_replace(plyer," Jr.","")
  plyer=str_replace(plyer," Sr.","")
  plyer=str_replace(plyer," III","")
  plyer=str_replace(plyer," II","")
  plyer=str_replace(plyer," IV","")
  
  data$Player=plyer
  
  ##Add Col for Pbox Naming Convention to merge
  data$Pbox_names=tabNames$Pbox[match(data$Player,tabNames$Bio)]
  
  #Merge Player Data with Bio data
  PlayerDataNew=merge(PlayerDataNew,data,by.x = "Player",by.y = "Pbox_names",all.x = TRUE)
  
  #Set new column names to ensure usability by any user
  colnames(PlayerDataNew)[which(names(PlayerDataNew) == c("FG."))] <- c("FGp")
  colnames(PlayerDataNew)[which(names(PlayerDataNew) == c("FG"))] <- c("FGM")
  colnames(PlayerDataNew)[which(names(PlayerDataNew) == c("X3P"))] <- c("P3M")
  colnames(PlayerDataNew)[which(names(PlayerDataNew) == c("X3PA"))] <- c("P3A")
  colnames(PlayerDataNew)[which(names(PlayerDataNew) == c("X3P."))] <- c("P3p")
  colnames(PlayerDataNew)[which(names(PlayerDataNew) == c("X2P"))] <- c("P2M")
  colnames(PlayerDataNew)[which(names(PlayerDataNew) == c("X2PA"))] <- c("P2A")
  colnames(PlayerDataNew)[which(names(PlayerDataNew) == c("X2P."))] <- c("P2p")
  colnames(PlayerDataNew)[which(names(PlayerDataNew) == c("eFG."))] <- c("EFGp")
  colnames(PlayerDataNew)[which(names(PlayerDataNew) == c("FT."))] <- c("FTp")
  colnames(PlayerDataNew)[which(names(PlayerDataNew) == c("MP"))] <- c("MIN")
  
  #Remove added teams and names from bio. Unneeded
  colsDrop <- c("Player.y","Team","Age.y")
  PlayerDataNew=PlayerDataNew[,!(names(PlayerDataNew) %in% colsDrop)]
  
  return(PlayerDataNew)
}
####################################################################################################
## Function to calculate rebounds count for each team so we can take team percentages for each player
sumTeam <- function(data,PlyrData){
  
  ## First assume that player is at home
  
  #Create RB marker
  data$RB = (ifelse (data$ReboundType=="" ,NaN, 
                     ifelse(data$ReboundType=="defensive",
                            ifelse(data$PlayTicker=="H", 1,2),
                            ifelse(data$PlayTicker=="H",3,4))))
  
  # 1: Home DRB
  # 2: Away DRB
  # 3: Home ORB
  # 4: Away ORB
  
  #Create unique list of teams
  dfH=data.frame(unique(data$PlayTeam))
  
  #Set home team
  colnames(dfH)[1]<-'HomeTeam'
  
  #For each type of rebound, we sum for each team
  for (r in c(1:4)){
    d = data %>%
      dplyr::filter(Rebounder!="",RB==r) %>%
      dplyr::group_by(HomeTeam) %>%
      dplyr::summarise(count = n()) 
    dfH=merge(dfH,d,by.x ="HomeTeam",by.y = "HomeTeam" )
    colnames(dfH)[r+1]=(as.numeric(r))
  }
  
  #Sort by alphabetical order
  dfH=dfH[order(dfH$HomeTeam),]
  
  #Switch for away teams so we can still collect 1's, 2's etc for total rebounds.
  
  #Create RB marker
  data$RB = (ifelse (data$ReboundType=="" ,NaN, 
                     ifelse(data$ReboundType=="defensive",
                            ifelse(data$PlayTicker=="A", 1,2),
                            ifelse(data$PlayTicker=="A",3,4))))
  
  # 1: Away DRB
  # 2: Home DRB
  # 3: Away ORB
  # 4: Home ORB
  
  dfA=data.frame(unique(data$PlayTeam))
  colnames(dfA)[1]<-'AwayTeam'
  for (r in c(1:4)){
    d = data %>%
      dplyr::filter(Rebounder!="",RB==r) %>%
      dplyr::group_by(AwayTeam) %>%
      dplyr::summarise(count = n()) 
    dfA=merge(dfA,d,by.x ="AwayTeam",by.y = "AwayTeam" )
    colnames(dfA)[r+1]=(as.numeric(r))
  }
  dfA=dfA[order(dfA$AwayTeam),]
  
  
  #Collect all rebounds into data frame for each team, then add to Pbox data
  Rebounds=data.frame(dfH$HomeTeam,(dfH$`1`)+(dfA$`1`),(dfH$`2`)+(dfA$`2`),(dfH$`3`)+(dfA$`3`),(dfH$`4`)+(dfA$`4`))
  
  colnames(Rebounds)=c("Team","TDRB","ODRB","TORB","OORB")
  
  # creating team variables that are later used for the creation of variables
  t=PlyrData %>%
    dplyr::group_by(Tm) %>%
    dplyr::summarise(TMIN=sum(MIN),TP3M=sum(P3M),TP3A=sum(P3A),TP2M=sum(P2M),TP2A=sum(P2A),
                     TFT=sum(FT),TFTA=sum(FTA),TFGM=sum(FGM), TFGA = sum(FGA), TTOV = sum(TOV))
  
  PlyrData=merge(PlyrData,Rebounds,by.x = 'Tm',by.y = 'Team')
  
  PlyrData=merge(PlyrData,t,by.x = 'Tm',by.y = 'Tm')
  
  
  return(PlyrData) 
}

#Function to run after all manipulation to remove variables unneeded for remainder of analysis
CleanData <- function(PlayerData){
  colsRemove=c("ï..Rk","Age.x",'GS','EFGp',"Height_ft","Height_in",'FGPTS','FGPTS_AST','FGPTS_ASTp','ASTPTS','TOTMins','Weighting',"TRB")
  PlayerDataNew=PlayerData[,!(names(PlayerData) %in% colsRemove)]
  return(PlayerDataNew)
}

################################################################################################################

assistnet <- function(data, Assister="Assister", Shooter="Shooter", points="points", ShotOutcome="ShotOutcome") 
{
  # creating a new data frame
  data = droplevels.data.frame(data)
  
  # filter all assisted shots and return a dataframe of Assisters and Shooters
  assist_player = subset(data, Assister!="", select=c(Assister,Shooter))
  assist_player = droplevels.data.frame(assist_player)
  
  # gives a sorted list of all the assisting players
  all_ast_plr = sort(unique(unlist(assist_player)))
  
  # factorizing all players in order to obtain a square matrix 
  assist_player$Assister = factor(assist_player$Assister, levels=all_ast_plr)
  assist_player$Shooter  = factor(assist_player$Shooter,  levels=all_ast_plr)
  
  # employ a pxp matrix that serves as an adjacency matrix for the network visualization
  tbl = as.matrix(table(assist_player, useNA="no"))
  if (nrow(tbl)!=ncol(tbl)) {
    stop("The number of players in 'assist' and 'player' variables are not the same.")
  }
  
  # create shooter statistics
  nodeData1 = data %>%
    dplyr::group_by(Shooter) %>%
    
    # filter out the rows that describes a shot 
    dplyr::filter(ShotOutcome=="make") %>% 
    dplyr::summarise(FGM=dplyr::n(),
                     FGM_AST=sum(Assister!=""),
                     FGM_ASTp=100*FGM_AST/FGM,
                     FGPTS=sum(points),
                     FGPTS_AST=sum(points*(Assister!="")), # 
                     FGPTS_ASTp=100*FGPTS_AST/FGPTS) %>%
    as.data.frame()
  
  # create assister statistics
  nodeData2 = data %>%
    # filter out all shots that have been assisted
    dplyr::filter(Assister!="") %>%
    dplyr::group_by(Assister) %>%
    dplyr::summarise(AST=dplyr::n(), ASTPTS=sum(points)) %>%
    as.data.frame()
  
  # merge together the measures we obtained for shooters and assisters
  nodeData <- merge(nodeData1, nodeData2, by.x="Shooter", by.y="Assister", all=T)
  
  # create a network with directed edges out of the pxp adjacency matrix
  net <- network::network(tbl, matrix.type="adjacency", directed=TRUE,
                          ignore.eval=FALSE,  names.eval="N")
  
  # aggregate all results that are desired to be outputted
  out = list(assistTable=tbl, nodeStats=nodeData, assistNet=net)
  class(out) <- append("assistnet", class(out))
  return(out)
}

################################################################################################################
# AssistStats for every player 

assistStat = function(df, PlayTeam="PlayTeam", Assister="Assister", Shooter="Shooter", points="points", ShotOutcome="ShotOutcome"){
  
  # creating a new data frame
  final = data.frame()
  
  # loop through all teams to obtain these statistics for all players 
  for (team in unique(df[[PlayTeam]])){
    
    data = subset(df, PlayTeam==team)
    data = droplevels.data.frame(data)
    
    # create shooter statistics
    nodeData1 <- data %>%
      dplyr::group_by(Shooter) %>%
      
      # filter out the rows that describes a shot 
      dplyr::filter(ShotOutcome=="make") %>% 
      dplyr::summarise(FGM=dplyr::n(),
                       FGM_AST=sum(Assister!=""),
                       FGM_ASTp=100*FGM_AST/FGM,
                       FGPTS=sum(points),
                       FGPTS_AST=sum(points*(Assister!="")), # 
                       FGPTS_ASTp=100*FGPTS_AST/FGPTS) %>%
      as.data.frame()
    
    
    # create assister statistics
    nodeData2 <- data %>%
      # filter out all shots that have been assisted
      dplyr::filter(Assister!="") %>%
      dplyr::group_by(Assister) %>%
      dplyr::summarise(AST=dplyr::n(), ASTPTS=sum(points)) %>%
      as.data.frame()
    
    # merge together the measures we obtained for shooters and assisters
    nodeData = merge(nodeData1, nodeData2, by.x="Shooter", by.y="Assister")
    
    # additionally assign the team to the newly created player statistics to make the rows identifiable, since one player can be in more than one team 
    nodeData$Team = team
    
    # add the statistics for all players within one team to the final data frame 
    final    = rbind(final, nodeData) }
  
  # after looping through every team, the output will be a list of all the players and the teams they played in with their respective statistics
  return(final)
}


############################################################################################
#Adapted the density plot function from book for our use 
#Only need to show analysis by period time, so strip out the rest of the code
period_densityplot <- function(data, shot.type="field", thresholds=NULL, best.scorer=FALSE,
                               period.length=12, bw=NULL, title=NULL) {
  
  if (is.null(bw)) {
    bw <- "nrd0"
  }
  
  #Only interested in Field Goals
  mat <- subset(data, ShotType=="2P" | ShotType=="3P")
  mat <- droplev_by_col(mat)
  
  var="PeriodTime"
  x <- mat[,var]
  #Give range for period times
  xrng <- c(0, period.length*60)
  
  #Return density structure, giving smooth curve approximation to shots by players at certain time periods 
  den <- stats::density(x, bw=bw, from=xrng[1], to=xrng[2])
  den <- as.data.frame(den[c("x", "y")])
  
  
  #Give thresholds for the different percentages
  if (is.null(thresholds)) {
    thr <- period.length*60*c(1/3,2/3)
  } else {
    thr <- thresholds
  }
  
  
  p <- plot_shotdensity(mat, den, var=var, thr=thr, xrng=xrng, ntks=10, title=title,
                        xadj=c(0,0,period.length*60), yadj=c(2,2,2,10), best.scorer=best.scorer, xlab="Period time")
  
  p <- p + theme_bw()
  
  return(p)
}

#Function to build the shot density plot, adapted from book
plot_shotdensity <- function(mat, den, var, thr, xrng, ntks, xadj, yadj, title=NULL, best.scorer=FALSE, xlab=NULL) {
  
  droplev_by_col <- function(data) {
    idx <- sapply(data, class)=="factor"
    data[, idx] <- lapply(data[, idx], droplevels)
    return(data)
  }
  
  y <- NULL
  x <- mat[, var]
  n <- nrow(mat)
  m1 <- droplev_by_col(mat[x <= thr[1], ])
  n1 <- nrow(m1)
  p1 <- round(n1/n*100,0)
  m1p <- round(sum(m1$ShotOutcome=="make")/n1*100,0)
  m2 <- droplev_by_col(mat[x <= thr[2] & x > thr[1], ])
  n2 <- nrow(m2)
  p2 <- round(n2/n*100,0)
  m2p <- round(sum(m2$ShotOutcome=="make")/n2*100,0)
  m3 <- droplev_by_col(mat[x > thr[2], ])
  n3 <- n - (n1+n2)
  p3 <- 100-(p1+p2)
  m3p <- round(sum(m3$ShotOutcome=="make")/n3*100,0)
  
  x1 <- (thr[1] + xadj[1])/2
  x2 <- (thr[1] + thr[2] + xadj[2])/2
  x3 <- (thr[2] + xadj[3])/2
  
  y1 <- mean(den$y[den$x<(x1 + yadj[4]) & den$x>(x1 - yadj[4])])/yadj[1]
  y2 <- mean(den$y[den$x<(x2 + yadj[4]) & den$x>(x2 - yadj[4])])/yadj[2]
  y3 <- mean(den$y[den$x<(x3 + yadj[4]) & den$x>(x3 - yadj[4])])/yadj[3]
  
  p <- ggplot(den,aes(x,y))+
    geom_line(col='gray',lwd=2) +
    geom_ribbon(data=subset(den,x<=thr[1]),aes(x=x, ymax=y, ymin=0), fill="blue", alpha=0.3) +
    geom_ribbon(data=subset(den,x<=thr[2] & x>thr[1]),aes(x=x, ymax=y, ymin=0), fill="blue", alpha=0.5) +
    geom_ribbon(data=subset(den,x>thr[2]),aes(x=x, ymax=y, ymin=0), fill="red", alpha=0.3) +
    annotate("text", label = paste(as.character(p1),"%",sep=""), x = x1, y = y1, size = 5, colour = "blue") +
    annotate("text", label = as.character(n1), x = x1, y = y1, size = 4, colour = "blue",vjust = 2) +
    annotate("text", label = paste("(",as.character(m1p),"% made)",sep=""), x = x1, y = y1, size = 4, colour = "blue",vjust = 4) +
    annotate("text", label = paste(as.character(p2),"%",sep=""), x = x2, y = y2, size = 5, colour = "blue") +
    annotate("text", label = as.character(n2), x = x2, y = y2, size = 4, colour = "blue",vjust = 2) +
    annotate("text", label = paste("(",as.character(m2p),"% made)",sep=""), x = x2, y = y2, size = 4, colour = "blue",vjust = 4) +
    annotate("text", label = paste(as.character(p3),"%",sep=""), x = x3, y = y3, size = 5, colour = "red") +
    annotate("text", label = as.character(n3), x = x3, y = y3, size = 4, colour = "red",vjust = 2) +
    annotate("text", label = paste("(",as.character(m3p),"% made)",sep=""), x = x3, y = y3, size = 4, colour = "red",vjust = 4) +
    labs(title = title) +
    scale_x_continuous(name=xlab, limits=c(xrng[1], xrng[2]), breaks=seq(xrng[1],xrng[2],length.out=ntks),
                       labels=seq(xrng[1],xrng[2],length.out=ntks)) +
    scale_y_continuous(name="Frequency of shots", limits=c(0, NA),labels=NULL)
  
  return(p)
}


droplev_by_col <- function(data) {
  idx <- sapply(data, class)=="factor"
  data[, idx] <- lapply(data[, idx], droplevels)
  return(data)
}
##### VariableSelect ####################################
##### Input #####
# PlayerData = Pbox season data on which to cluster
# k = Number of clusters

##### Output #####
# VarCluster = Variables to cluster on

#Used RAFTERY and DEAN (2006)

VariableSelect <- function(PlayerData,minK=2,k,algorithm="Hartigan-Wong",nStarts=1,printFull=FALSE){
  
  
  PlayerData = scale(PlayerData)
  PlayerData[is.na(PlayerData)]=0
  #Give data frame of numeric/integer values, scale data firstly
  
  #Extract PlayerData into matrix form
  matData=as.matrix.data.frame(PlayerData)
  
  varN=ncol(PlayerData)
  var_exc=c(1:varN)
  
  maxBICY1=-10000000
  #1/ Find first variable to select as one with highest univariate clustering potential
  for (i in var_exc){
    for (j in c(minK:k)){
      kmeans_i=kmeans(matData[,i],j,iter.max = 100, algorithm = algorithm,nstart = nStarts)
      bic_i=bic_kmeans(kmeans_i,matData[,i])
      if (maxBICY1<bic_i){
        maxBICY1=bic_i
        maxVar=i
        maxClust=j
      }  
    }
  }
  
  if (printFull) {
    print("Step 1:")
    print(paste("BIC = ",maxBICY1,sep = ""))
    print(paste("Optimal no of clusters = ",maxClust))
    print(paste("Best univariate variable = ", colnames(PlayerData)[maxVar]))
  }
  
  #Create list of variables 
  var_inc=c(maxVar)
  var_exc=var_exc[!(var_exc==maxVar)]
  
  #2/ Find second variable, take best bivariate clustering potential with first variable
  maxBICdiff=-10000000
  for (i in var_exc){
    for (j in c(minK:k)){
      
      #Calc regression BIC
      y1=matData[,c(var_inc)]
      y2=matData[,i]
      ft=lm(y2~y1)
      rss=sum((ft$residuals)^2)  
      n=dim(matData)[1]
      bic_reg=-(n*log(2*pi))-(n*log(rss/n))-n-((length(var_inc)+2)*log(n))
      
      kmeans_i=kmeans(matData[,c(var_inc,i)],j,iter.max = 100, algorithm = algorithm,nstart = nStarts)
      bic_clust=bic_kmeans(kmeans_i,matData[,c(var_inc,i)])
      
      bic_notClust=bic_reg+maxBICY1  
      
      bic_diff=bic_clust-bic_notClust
      
      if (maxBICdiff<bic_diff){
        maxBICdiff=bic_diff
        maxBIC=bic_clust
        maxVar=i
        maxClust=j
      } 
    }
  }
  
  #Create list of variables 
  var_inc=c(var_inc,maxVar)
  var_exc=var_exc[!(var_exc==maxVar)]
  
  #Set new model BIC
  maxBICY1=maxBIC
  
  if (printFull) {
    print("")
    print("Step 2:")
    print(paste("BIC = ",maxBICY1,sep = ""))
    print(paste("Optimal no of clusters (bivariate) = ",maxClust))
    print(paste("Best Bivariate variables = ", colnames(PlayerData)[var_inc[1]]," & ",colnames(PlayerData)[var_inc[2]]))
  }
  #Run algorithm
  # while true
  done=FALSE
  
  while (!done){
    
    #3/ Find variable with most multivariate clustering potential with those already selected
    maxBICdiff=-10000000
    for (i in var_exc){
      for (j in c(minK:k)){
        
        #Calc regression BIC
        y1=matData[,c(var_inc)]
        y2=matData[,i]
        ft=lm(y2~y1)
        rss=sum((ft$residuals)^2)  
        n=dim(matData)[1]
        bic_reg=-(n*log(2*pi))-(n*log(rss/n))-n-((length(var_inc)+2)*log(n))
        
        kmeans_i=kmeans(matData[,c(var_inc,i)],j,iter.max = 100, algorithm = algorithm,nstart = nStarts)
        bic_clust=bic_kmeans(kmeans_i,matData[,c(var_inc,i)])
        
        bic_notClust=bic_reg+maxBICY1  
        
        bic_diff=bic_clust-bic_notClust
        
        if (maxBICdiff<bic_diff){
          maxBICdiff=bic_diff
          maxBIC=bic_clust
          maxVar=i
          maxClust=j
        } 
      }
    }
    
    #If diff in BIC vals is positive, add variable maxVar
    if ((maxBICdiff)>0){
      var_inc=c(var_inc,maxVar)
      var_exc=var_exc[!(var_exc==maxVar)] 
      #update current Y1 bic
      maxBICY1=maxBIC
    } else {
      done=TRUE
      
    }
    
    if (printFull) {
      print("")
      print("Step 3:")
      print(paste("BIC = ",maxBICY1,sep = ""))
      print(paste("Optimal no of clusters (multivariate) = ",maxClust))
      print(paste("var ",c(1:length(var_inc)), " = ",colnames(PlayerData)[var_inc]))
    }
    #4/ Find variable for removal from currently selected variables by finding variable with weakest evidence for clustering inclusion
    #   and remove if evidence for not clustering is higher than clustering
    
    minBICdiff=10000000
    for (i in var_inc){
      for (j in c(minK:k)){
        tempCol=var_inc[!(var_inc==i)]
        
        #Calc regression BIC
        y1=matData[,tempCol]
        y2=matData[,i]
        ft=lm(y2~y1)
        rss=sum((ft$residuals)^2)  
        n=dim(matData)[1]
        bic_reg=-(n*log(2*pi))-(n*log(rss/n))-n-((length(var_inc)+2)*log(n))
        
        kmeans_i=kmeans(matData[,tempCol],j,iter.max = 100, algorithm = algorithm,nstart = nStarts)
        bic_clust=bic_kmeans(kmeans_i,matData[,tempCol])
        
        bic_notClust=bic_reg+bic_clust
        
        bic_diff=maxBICY1-bic_notClust
        
        if (minBICdiff>bic_diff){
          minBICdiff=bic_diff
          minBIC=bic_clust
          minVar=i
          minClust=j
        } 
      }
    }
    
    #If diff in BIC vals is ngative, remove minVar
    if ((minBICdiff)<=0){
      var_inc=var_inc[!(var_inc==minVar)]
      var_exc=c(var_exc,minVar) 
      var_exc=sort(var_exc)
      done=FALSE
      
      if (printFull) {
        print("")
        print("Step 4:")
        print(paste("BIC = ",maxBICY1,sep = ""))
        print(paste("Optimal no of clusters (multivariate) = ",maxClust))
        print(paste("remove: ",colnames(PlayerData)[minVar]))
        print(paste("var ",c(1:length(var_inc)), " = ",colnames(PlayerData)[var_inc]))
      }
      
    } else {
      
      if (printFull) {
        print("")
        print("Step 4:")
        print(paste("BIC = ",maxBICY1,sep = ""))
        print(paste("Optimal no of clusters (multivariate) = ",maxClust))
        print("Do not remove variable")
        print(paste("var ",c(1:length(var_inc)), " = ",colnames(PlayerData)[var_inc]))
      }
    }
    
  }
  
  if(!printFull){
    print(paste("Optimal no of clusters (multivariate) = ",maxClust))
    print(paste("var ",c(1:length(var_inc)), " = ",colnames(PlayerData)[var_inc]))
  }
  return(var_inc)
}


bic_kmeans <- function(kmeans,x){
  #    Function to estimate the BIC metric, for output of k-means algorithm
  #   - Estimates clusters to have gaussian mixture distribution, in order to calculate likelihood
  
  #  Parameters:
  ##########################################
  # kmeans:  data frame with output of kmeans algorithm
  
  # X     :  Data points
  ##########################################
  # assign centers and labels
  centers = kmeans$centers
  labels  = kmeans$cluster
  #number of clusters
  K = as.numeric(dim(centers)[1])
  # size of the clusters
  Rn = kmeans$size
  #R = number of data points, M=number of variables (dim)
  R = as.numeric(dim(as.matrix(x))[1])
  M = as.numeric(dim(as.matrix(x))[2])
  
  #Compute Variance
  cl_var = (1.0 / ((R - K)*M)) * (kmeans$tot.withinss)
  
  const_term = K * log(R)*(M+1)
  
  ll=sum(Rn*log(Rn))-R*log(R)-(R*M*0.5*log(2*pi*cl_var))-(0.5*M*(R-K))
  
  BIC = 2*ll - const_term
  
  return(BIC)
  
}
