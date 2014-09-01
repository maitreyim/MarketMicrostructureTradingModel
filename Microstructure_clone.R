# ==========================================================================#
#-- Project Name :  Microstructure Live Trading Agent with Protector Initial Code                                                     
#-- Task         :  Write a Microstructure Live Trading Strategy     
#-- version      :  1.0
#-- Date         :  07/MAY/2014
#-- Author       :  Maitreyi Mandal   
#-- SVN Directory:  \xxxx           
# ==========================================================================#
#============================== Get the environment variables===============#
#==============================
mripPath = Sys.getenv("MRIP_HOME")
#===================================
# Common variables across the agency
#===================================
assign("checPkgVec",0, pos=.GlobalEnv)
assign("firstCall",TRUE, pos=.GlobalEnv)
assign("symbolVec",c("AMZN"), pos=.GlobalEnv)
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
flog.threshold(INFO)
flog.appender(appender.file(paste(mripPath,"/MINTlogs/MicrostructureAgent3.log",sep="")))
flog.info("Sourcing micro_live_agent2.R file - Begin!")
#====================================
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

flog.info("Sourcing micro_live_agent2.R file - End!")


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
newbid<-0 # Number of new bid counts
newask<-0 # Number of new ask counts
prevna<-0 # Number of previous ask counts
prevnb<-0 # Number of previous bid counts
askPrice<-0 # current askPrice
ask_old<-0  # old ask price
bidPrice<-0 # current bid price
bid_old<-0  # old bid price
min<-0      # current minute
min_old<-0  # old minute
PL<-0       # Profit & Loss
cnt<-0      # No. of unique minutes in a day
position<-0 # inventory size
gamma<-0.01 # risk averseness of the trader
t<-0        # time difference in minutes in a single minute
firstCall <- TRUE # making the first call true at the initiation
buysize10<-100
sellsize10<- 100

#===============================
# LatencyComputation Variables
#===============================
latencyFile = paste(mripPath,"/MINTlogs/MicrostructureLatency3.csv",sep="")

#=========================================
# Function to Initialize dynamic Variables
#=========================================

#initParams <- function(){
#dfMatrix <<- matrix(-1,nrow=frameSize,ncol=nSymbols+1)
#fillCount <<- 1

#}

#==================================
# Function to generate trade signal
#==================================
generateTradeSignal <- function(tick){
  
  tickList <- fromJSON(tick)
  
  tickList <- lapply(tickList,FUN=function(x) x[length(x)])
  
  marketStatus <- tickList$marketStatus[1]
  
  askPrice <- tickList$askPrice[1]
  bidPrice <- tickList$bidPrice[1]
  
  tradeTime <- as.POSIXlt(tickList$timeStamp[1])
  
  #flog.info(paste("TIMESTAMP is - ", tickList$timeStamp,sep=" "))
  if (firstCall){
    ask_old<<-askPrice
    bid_old<<-bidPrice
    flog.info(paste("First Ask old is:",ask_old,sep=","))
    flog.info(paste("First Ask Price is:",askPrice,sep=","))
    flog.info(paste("First bid old is:",bid_old,sep=","))
    flog.info(paste("First bid Price is:",bidPrice,sep=","))
    min1<<-as.POSIXct(tradeTime,origin = "1970-01-01")
    min<-(as.numeric(format(min1,"%M")))
    flog.info(paste("min is:",min, sep=","))
    flog.info(paste("min1 is:",min1, sep=","))
    min_old<<-min
    firstCall <<- FALSE
    return("ignore")
    
  }# End of First Call
  else{
    min2<<-as.POSIXct(tradeTime,origin = "1970-01-01") 
    flog.info(paste("min2 is:",min2, sep=","))
    min3<-(as.numeric(format(min2,"%M")))
    t<-difftime(min2,min1,unit="mins")
    flog.info(paste("Min is:",min3,sep=","))
    flog.info(paste("Min_old is:",min_old,sep=","))
    askPrice <<- askPrice
    bidPrice<<-bidPrice
    min<<-min3
    flog.info(paste("t is:",t, sep=","))
    
    # Newask-Newbid computations
    
    
    if ((t<1)&&(min_old==min)){ # computing new ask & previous new ask counts 
      if(ask_old!=askPrice)
      {
        newask<<-newask+1
        flog.info(paste("prevna is:",prevna,sep=","))
        flog.info(paste("newask is:",newask,sep=","))
        flog.info(paste("BidPrice is:",bidPrice,sep=","))
        flog.info(paste("askPrice is:",askPrice,sep=","))
        flog.info(paste("ask_old is:",ask_old,sep=","))
        flog.info(paste("bid_old is:",bid_old,sep=","))
        
        
      }
      else
      {
        
        flog.info(paste("BidPrice is:",bidPrice,sep=","))
        flog.info(paste("askPrice is:",askPrice,sep=","))
        flog.info(paste("ask_old is:",ask_old,sep=","))
        flog.info(paste("bid_old is:",bid_old,sep=","))
        
        
      }
    }
    else if(t>=1 || (min_old!=min))
    { 
      flog.info(paste("min2 is:",min2, sep=","))
      flog.info(paste("min1 is:",min1, sep=","),name = "info_consume")
      min1<<-min2
      #nac<<-prevna
      prevna<<-newask
      newask<<-0 
      cnt<<-cnt+1
      flog.info(paste("cnt is:",cnt,sep=","))
      flog.info(paste("prevna is:",prevna,sep=","))
      flog.info(paste("newask is:",newask,sep=","))
      flog.info(paste("BidPrice is:",bidPrice,sep=","))
      flog.info(paste("askPrice is:",askPrice,sep=","))
      flog.info(paste("ask_old is:",ask_old,sep=","))
      flog.info(paste("bid_old is:",bid_old,sep=","))
      
    }
    
    else {
      
      
      flog.info("Nothing to do")
      
    }
    
    
    if ((t<1)&&(min_old==min)) {
      # computing new bid & previous new bid counts  m 
      if (bid_old!=bidPrice)
      {
        prevnb<<-prevnb
        newbid<<-newbid+1
        flog.info("16")
        flog.info(paste("prevnb is:",prevnb,sep=","))
        flog.info(paste("newbid is:",newbid,sep=","))
        
        
      }
      else 
      {
        
        flog.info("counter at 22")
        
      }
    }
    # if previous min!=this min
    else if((t>=1) || (min_old!=min))
    {
      flog.info(paste("min2 is:",min2, sep=","))
      flog.info(paste("min1 is:",min1, sep=","))
      min1<<-min2
      #nbc<<-prevnb
      prevnb<<-newbid
      newbid<<-0
      
    }
    
    else {
      flog.info("Nothing to do")
    }
    
    
    ##Strategy Execution##
    
    if (cnt>=2)
    {
      if (((newbid-prevnb)!=0) && ((bid_old)!=0) && ((bidPrice)!=0))
        
      {
        var1<-abs(1-gamma*(((newbid)*(prevnb))/(newbid-prevnb)))
        var2<-log(var1)
        var3<-(1/gamma)*var1
        
        optbid <<-ifelse(is.nan(bidPrice-var3),0,(bidPrice-var3))
        flog.info(paste("optbid is:",optbid, sep=","))
        
      }
      
      else
      {
        optbid<<-0
        flog.info(paste("optbid is:",optbid, sep=","))
        
      }
      
      if(((newask-prevna)!=0) && ((ask_old)!=0) && ((askPrice)!=0)) {  
        
        var3<-abs(1-gamma*(((newask)*(prevna))/(newask-prevna)))
        var4<-log(var3)
        var5<-(1/gamma)*var4
        optask <<-ifelse(is.nan(askPrice-var5),0,(askPrice-var5))
        flog.info(paste("optask is:",optask, sep=","))
        
      } else {
        optask<<-0
        flog.info(paste("optask is:",optask, sep=","))
        
      }
      ask_old<<-askPrice
      bid_old<<-bidPrice
      min_old<<-min
      
      if(optbid > (askPrice)) {
        
        #action = 'B' or BUY
        PL<<- PL-askPrice
        position <<- (position +1)
        flog.info(paste("position is:",position,sep=","))
        orderType <- "MKT"
        lmtPrice <- 0
        action<-"BUY"
        
        flog.info("Sending %s signal at askPrice %s and bidPrice %s at %s",action,
                  tickList$askPrice, tickList$bidPrice, tickList$timeStamp)
        
        ret <- list("agency" = list(agency__),
                    "agent" = list(agent__),
                    "symbol" = list(symbolVec[1]),
                    "action" = list(action),
                    "qty" = list(buysize10),
                    "orderType"= list(orderType),
                    "lmtPrice"=list(lmtPrice)
                    
                    
        )
        
        return (ret)
      } else if(optask < (bidPrice)) {
        
        #action = 'A'or SELL
        PL<<-PL+bidPrice
        position<<-position-1
        flog.info(PL)
        flog.info(paste("position is:",position,sep=","))
        
        orderType <- "MKT"
        lmtPrice <- 0
        action<-("SELL")
        flog.info("Sending %s signal at askPrice %s and bidPrice %s at %s",action,
                  tickList$askPrice, tickList$bidPrice, tickList$timeStamp)
        
        ret <- list("agency" = list(agency__),
                    "agent" = list(agent__),
                    "symbol" = list(symbolVec[1]),
                    "action" = list(action),
                    "qty" = list(sellsize10),
                    "orderType"= list(orderType),
                    "lmtPrice"=list(lmtPrice)
        )
        
        return (ret)
      } else {
        #between bid and ask, if closer to bid, buy, else sell
        if(abs(bidPrice-optbid) > abs(askPrice-optask))
        {
          #action = 'B'
          PL <<-(PL-askPrice)
          position <<-(position+1)
          flog.info(paste("position7 is:",position,sep=","))
          
          orderType <- "MKT"
          lmtPrice <- 0
          action<-("BUY")
          flog.info("Sending %s signal at askPrice %s and bidPrice %s at %s",action,
                    tickList$askPrice, tickList$bidPrice, tickList$timeStamp)
          ret <- list("agency" = list(agency__),
                      "agent" = list(agent__),
                      "symbol" = list(symbolVec[1]),
                      "action" = list(action),
                      "qty" = list(buysize10),
                      "orderType"= list(orderType),
                      "lmtPrice"=list(lmtPrice)
          )
          
          return (ret)
        }
        else if (abs(bidPrice-optbid) > abs(askPrice-optask))
        {
          #action = 'A'
          PL <<-(PL+bidPrice)
          position <<-(position-1)
          flog.info(paste("position8 is:",position,sep=","))
          orderType <- "MKT"
          lmtPrice <- 0
          action<-("SELL")
          flog.info("Sending %s signal at askPrice %s and bidPrice %s at %s",action,
                    tickList$askPrice, tickList$bidPrice, tickList$timeStamp)
          
          ret <- list("agency" = list(agency__),
                      "agent" = list(agent__),
                      "symbol" = list(symbolVec[1]),
                      "action" = list(action),
                      "qty" = list(sellsize10),
                      "orderType"= list(orderType),
                      "lmtPrice"=list(lmtPrice)
          )
          
          return (ret)    
        }
      }
    } else {
      return("ignore")
    }
  }
}

#=========================================
# Main function to push signals to a queue 
#=========================================

MainSendSignal <- function(tick){
  
  t1 <- as.character(format(Sys.time(),"%Y-%m-%d %H:%M:%OS6"))
  
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
    return(output)
  } else {
    return("ignore")
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
