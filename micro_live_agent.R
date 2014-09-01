# ==========================================================================#
#-- Project Name :  Microstructure Trading Agent with Protector Initial Code                                                     
#-- Task         :  Write a Microstructure Live Trading Strategy     
#-- version      :  1.0
#-- Date         :  07/MAY/2014
#-- Author       :  Maitreyi Mandal   
#-- SVN Directory:  \xxxx           
# ==========================================================================#
#============================== Get the environment variables===============#
#==============================
#mripPath = Sys.getenv("MRIP_HOME")
#===================================
# Common variables across the agency
#===================================
assign("checPkgVec",0, pos=.GlobalEnv)
assign("firstCall",TRUE, pos=.GlobalEnv)
assign("symbolVec",c("JPM"), pos=.GlobalEnv)
#assign("activeMQIp","tcp://10.10.5.40:61616", pos=.GlobalEnv)
#=========================
# Check the logger package
#=========================
if(!is.element("futile.logger", installed.packages())){
  install.packages("futile.logger",repos="http://lib.stat.cmu.edu/R/CRAN")
}
require("futile.logger")
#================== 
# Setting up logger
#==================
flog.info("Sourcing micro_live_agent.R file - Begin!")
flog.appender(appender.file("C:/Users/Maitreyi.Mandal/Desktop/R/micro1.log"))
flog.info("Sourcing micro_live_agent.R file - Begin!")

#====================================
# Common Functions across the agency
#====================================
#---------------------------------------------------------------------
# A simple function to check if the required version of R is installed
#---------------------------------------------------------------------
CheckRversion<-function(reqRVersion = '2.15.1'){
  if (getRversion() < reqRVersion){
    flog.error("Upgrade your R!")
    return(10)
  } else {
    return(0)
  }
}

packInfo <- installed.packages ()[ ,c("Package", "Depends", "Version")] 

#-----------------------------------------------------------------------------
# Check if a given package is installed and is >= required version
# Args: 
#   packName: Name of the package as a string
#   reqVersion: Required version as a string
#Returns:
#   0 if the package is already installed/ or was succesfully installed by the
#    function. Else returns 11.
#-----------------------------------------------------------------------------
CheckPackVer <- function(packName, reqVersion) {
  flog.info("Checking package and version for package %s",paste(packName,reqVersion,sep=":"))
  reqVersion <- unlist(strsplit(reqVersion, "[[:punct:][:space:]]+"))
  count <- 1
  tryCatch({
    currVersion <- packInfo[packName,3]
    currVersion <- unlist(strsplit(currVersion,"[[:punct:][:space:]]+"))
    
    if(length(currVersion) > length(reqVersion) ){
      reqVersion = c(reqVersion,rep("0",length(currVersion) - length(reqVersion)))
    }
    if(length(currVersion) < length(reqVersion) ){
      currVersion = c(currVersion,rep("0",length(reqVersion) - length(currVersion)))
    }
    
    if(any(currVersion != reqVersion)){
      chng <- which(currVersion != reqVersion)[1]
      
      if(reqVersion[chng] > currVersion[chng]){
        stop()
      }
      
    }
    flog.info("%s is installed",packName)
    return(0)
  },
  error = function(err){
    flog.error("Error in the package %s",packName)
    flog.error("Error is %s",err)
    
    while(count<4){
      flog.info("%s Installation attemp %d",packName,count)
      install.packages(packName,dependencies=TRUE,repos="http://lib.stat.cmu.edu/R/CRAN")
      packInfo <- installed.packages ()[ ,c("Package", "Depends", "Version")] 
      if(packName %in% packInfo[,1]) break
      count<<-count+1
    }
    if (count == 4 && !(packName %in% packInfo[,1])){
      return(11)
    } else{
      return(0)
    }
  },
  finally={})
}

flog.info("Sourcing PairTradingModel.R file - End!")


#====================================
# Check R version and package version
#====================================

errorCode<-CheckRversion()

checPkgVec[1]<-CheckPackVer("Rjms",'0.0.5')
checPkgVec[2]<-CheckPackVer("WeightedPortTest",'1.0')
if (any(checPkgVec!=0)){
  errorCode<<-11
}
#===========================
# Loading Required Libraries
#===========================
require("Rjms")
require("rjson")
flog.info("R libraries loaded succesfully")

#=====================
# Initialize Variables
#=====================
# Defining Global Variables#
#frameSize <- 100
#nSymbols <- length(symbolVec)
#waitTime <- 10000 # Time to wait (in millis) for the order to get filled 
#dfMatrix <-NULL
#fillCount <-NULL
newbid<-0
newask<-0
prevna<-0
prevnb<-0
nac<-0
nbc<-0
askPrice<-0
ask_old<-0
bidPrice<-0
bid_old<-0
min<-0
min_old<-0
PL<-0
cnt<-0
position<-0
gamma<-0.01
firstCall <- TRUE

#===============================
# LatencyComputation Variables
#===============================
#latencyFile = paste(mripPath,"/MINTlogs/NewpairTradingProcessLatency12.csv",sep="")

#=========================================
# Function to Initialize dynamic Variables
#=========================================

initParams <- function(){
  dfMatrix <<- matrix(-1,nrow=frameSize,ncol=nSymbols+1)
  fillCount <<- 1
  
}

#==================================
# Function to generate trade signal
#==================================
generateTradeSignal <- function(tick){
  
  tickList <- fromJSON(tick)
  
  marketStatus <- tickList$marketStatus[1]
  
  #tradeTime <- as.POSIXlt(tickList$timeStamp)
  
  #flog.info(paste("TIMESTAMP is - ", tickList$timeStamp,sep=" "))
  if (firstCall){
    ask_old<<-tickList$askPrice
    bid_old<<-tickList$bidPrice
    tradeTime <- as.POSIXlt(tickList$timeStamp)
    print(paste("First Ask old is:",ask_old,sep=","))
    print(paste("First Ask Price is:",askPrice,sep=","))
    print(paste("First bid old is:",bid_old,sep=","))
    print(paste("First bid Price is:",bidPrice,sep=","))
    min<<-as.POSIXct(tradeTime,origin = "1970-01-01")
    print("99")
    min<-(as.numeric(format(min,"%M")))
    print(paste("min is:",min, sep=","))
    print("199")
    min_old<<-min
    firstCall <<- FALSE
    return(paste("ask_old is",ask_old,"bid_old is:",bid_old,sep=","))
    
  }# End of First Call
  tradeTime <- as.POSIXlt(tickList$timeStamp)
  min<<-as.POSIXct(tradeTime,origin = "1970-01-01") 
  min<-(as.numeric(format(min,"%M")))
  print(paste("Min is:",min,sep=","))
  print(paste("Min_old is:",min_old,sep=","))
  t<-as.difftime(min,min_old,unit="mins")
  askPrice <<- tickList$askPrice
  bidPrice<<-tickList$bidPrice
  min<<-min
  print("299")
  print("499")
  print(paste("t is:",t, sep=","))
  #return(0)
  
  if ((t<60)&&(min_old==min)){ # computing new ask & previous new ask counts 
    if(ask_old!=askPrice)
    {
      #prevna<-prevna
      newask<<-newask+1
      print(paste("prevna is:",prevna,sep=","))
      print(paste("newask is:",newask,sep=","))
      print(paste("BidPrice is:",bidPrice,sep=","))
      print(paste("askPrice is:",askPrice,sep=","))
      print(paste("ask_old is:",ask_old,sep=","))
      print(paste("bid_old is:",bid_old,sep=","))
      print("12")
      #return(paste("newask is",newask,"prevna is:",prevna,sep=","))
    }
    else
    {
      
      #newask<-newask
      #prevna<-prevna
      print(paste("BidPrice is:",bidPrice,sep=","))
      print(paste("askPrice is:",askPrice,sep=","))
      print(paste("ask_old is:",ask_old,sep=","))
      print(paste("bid_old is:",bid_old,sep=","))
      print("13")
      #return(paste("newask is",newask,"prevna is:",prevna,sep=","))
    }
  }
  else 
  { nac<<-prevna
    prevna<<-newask
    newask<<-0 
    cnt<<-cnt+1
    print(paste("cnt is:",cnt,sep=","))
    print(paste("prevna is:",prevna,sep=","))
    print(paste("newask is:",newask,sep=","))
    print(paste("BidPrice is:",bidPrice,sep=","))
    print(paste("askPrice is:",askPrice,sep=","))
    print(paste("ask_old is:",ask_old,sep=","))
    print(paste("bid_old is:",bid_old,sep=","))
    print("15")
    #return(paste("newask is",newask,"prevna is:",prevna,sep=","))
  }
  
  if ((t<60) && (min_old==min)){
    # computing new bid & previous new bid counts  m 
    if (bid_old!=bidPrice)
    {
      prevnb<<-prevnb
      newbid<<-newbid+1
      print("16")
      #return(paste("newbid is",newbid,"prevnb is:",prevnb,sep=","))
      
    }
    else
    {
      #newbid<-newbid
      #prevnb<-prevnb
      print("18")
      #return(paste("newbid is",newbid,"prevnb is:",prevnb,sep=","))
    }
  }
  # if previous min!=this min
  else
  {
    nbc<<-prevnb
    prevnb<<-newbid
    newbid<<-0
    print("30")
    #return(paste("newbid is",newbid,"prevnb is:",prevnb,sep=","))
  }
  
  if (cnt>=2)
  {
    if (((newbid-prevnb)!=0) && ((bid_old)!=0) && ((bidPrice)!=0))
      
    {
      var1<-abs(1-gamma*(((newbid)*(prevnb))/(newbid-prevnb)))
      var2<-log(var1)
      var3<-(1/gamma)*var1
      
      optbid <<-ifelse(is.nan(bidPrice-var3),0,(bidPrice-var3))
      #return (paste("Optimal bid price is:",optbid,sep=","))
    }
    
    else
    {
      optbid<<-0
      #return (paste("Optimal bid price is:",optbid,sep=","))
    }
    
    if (((newask-prevna)!=0) && ((ask_old)!=0) && ((askPrice)!=0))
      
    {  
      var3<-abs(1-gamma*(((newask)*(prevna))/(newask-prevna)))
      var4<-log(var3)
      var5<-(1/gamma)*var4
      optask <<-ifelse(is.nan(askPrice-var5),0,(askPrice-var5))
      #return (paste("Optimal ask price is:",optask,sep=","))
    }
    else
    {
      optask<<-0
      #return (paste("Optimal ask price is:",optask,sep=","))
    }
    
{
  
  if(optbid> askPrice)
    
    
  {
    if(position <= 1)
    {
      #action = 'B' or BUY
      PL<<- PL-askPrice
      position <<- (position +1)
      print(PL)
      print("3")
      print(paste("position is:",position,sep=","))
      #return (paste("PL is:",PL,sep=","))
    }
    else { #action = 'N'
      #PL<<-PL
      #position<<-position
      print(PL)
      print("4")
      #return (paste("PL is:",PL,sep=","))
    }
  }
  else if(optask < bidPrice)
  {
    if(position >= 1)
    {
      #action = 'A'
      PL<<-PL+bidPrice
      position<<-position-1
      print(PL)
      print(paste("position is:",position,sep=","))
      print("5")
      #return (paste("PL is:",PL,sep=","))
    }
    else {#action = 'N'
      #PL <<-PL
      #position<<-position
      print(PL)
      print("6")
      #return (paste("PL is:",PL,sep=","))
      
    }
  }
  else
  {
    #between bid and ask, if closer to bid, buy, else sell
    if(abs(bidPrice-optbid) > abs(askPrice-optask))
    {
      #action = 'B'
      PL <<-(PL-askPrice)
      position <<-(position+1)
      print(paste("position7 is:",position,sep=","))
      print(PL)
      print("7")
      #return (paste("PL is:",PL,sep=","))
    }
    else (abs(bidPrice-optbid) > abs(askPrice-optask))
{
      #action = 'A'
      PL <<-(PL+bidPrice)
      position <<-(position-1)
      print(paste("position8 is:",position,sep=","))
      print(PL)
      print("8")
      #return (paste("PL is:",PL,sep=","))
    }
  }
}
  }

else {
  
  #return (paste("PL is:",PL,sep=","))
}

ask_old<<-askPrice
bid_old<<-bidPrice
min_old<<-min


}



#=========================================
# Main function to push signals to a queue 
#=========================================

MainSendSignal <- function(tick){
  
  t1 <- as.character(format(Sys.time(),"%Y-%m-%d %H:%M:%OS6"))
  
  if(firstCall){
    initParams()
    flog.info("Initialized Parameters for the first Call.")
    firstCall <<- FALSE
  }
  
  predSignal <- generateTradeSignal(tick)
  
  # Write the Latency values to a file
  if(predSignal != "ignore")
  {
    output <- toJSON(predSignal)
    #checkLogged <- to.logger(signalLogger,output,asString=T)
    
    # Calculate Latency values of R computation
    t2 <- as.character(format(Sys.time(),"%Y-%m-%d %H:%M:%OS6"))
    latVal = as.double(difftime(t2,t1)[1])
    latencyValues = c(t1,t2,latVal)
    write.table(x=t(latencyValues), file = latencyFile, row.names=FALSE, col.names=FALSE, sep=",",append=TRUE)
    flog.info("Sent a signal to the queue")
  }
}

#===================
# Clean Up Function
#===================

cleanUp <- function(){
  flog.info("The agent %s in agency %s has been killed. Clean up function evoked",
            agent__,agency__, name=logNS)
  
  #destroy.logger(signalLogger)
  rm(list=ls())
  flog.info("Clean up done.")
  
}

