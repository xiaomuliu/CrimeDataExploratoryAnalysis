#Exact Repeat
ExactRepeatTimeIntervalHist <- function(CrimeData,order=2,plot=TRUE,REH=TRUE,...){
  RepeatFreq <- count(CrimeData,vars=c("X_COORD","Y_COORD"))
  #isolate out order=order houses
  Loc.ord<- subset(RepeatFreq,freq==order,select=c("X_COORD","Y_COORD"))
  Tinterval <- rep(NA,nrow(Loc.ord)*(order-1))
  pair <- 1
  
  for (i in 1:nrow(Loc.ord)){
    Date.ord<- subset(CrimeData,X_COORD==Loc.ord[i,]$X_COORD & Y_COORD==Loc.ord[i,]$Y_COORD,
                      select="DATEOCC")
    for (j in 1:(nrow(Date.ord)-1)){
      Tinterval[pair] <- difftime(Date.ord$DATEOCC[j+1],Date.ord$DATEOCC[j],units="days")
      pair <- pair+1
    } 
  }
  
  #Random Event Hypothesis
  if(REH==TRUE){
    D <- length(unique(CrimeData$DATEOCC))
    tau <- seq(0,D,length.out=3*D)
    Prob.ord <- rep(NA,length(tau))
    for (i in seq_along(tau)){
      Prob.ord[i] <- order/(D+order-1)*prod( (D-tau[i]+0:(order-2)) / (D+0:(order-2)) )
    }
  }
  
  if(plot==TRUE){
    hist(Tinterval,breaks=0:max(Tinterval),freq=FALSE,xlab="Day",ylab="Probability",
         main=paste("Histogram of time intervals of two break-ins of order ",order, " houses",sep=""),...)
    if(REH==TRUE){
      lines(tau,Prob.ord,type='l',col="red",cex=1)
    }
  }
  
  if(REH==TRUE){
    return(list(TintHist=hist(Tinterval,breaks=0:max(Tinterval),plot=FALSE),REHprob=data.frame(tau=tau,Prob=Prob.ord)))
  }
  else{
    return(hist(Tinterval,breaks=0:max(Tinterval),plot=FALSE))
  } 
}

## Near Repeat
NearRepeatTimeIntervalHist <- function(CrimeData,radius,plot=TRUE,REH=TRUE,...){
  RepeatFreq <- count(CrimeData,vars=c("X_COORD","Y_COORD"))
  #isolate out order=1 houses
  Loc.d1<- subset(RepeatFreq,freq==1,select=c("X_COORD","Y_COORD"))
  Tinterval <- numeric()
  Loc.all <- subset(CrimeData,select=c("X_COORD","Y_COORD"))
  
  
  for (i in 1:nrow(Loc.d1)){
    XYrepMat <- matrix(rep(as.numeric(as.vector(Loc.d1[i,])),nrow(CrimeData)),
                       nrow=nrow(CrimeData),ncol=2,byrow=TRUE)
    diffMat <- XYrepMat-as.matrix(Loc.all)
    distance <- sqrt(diffMat[,1]^2+diffMat[,2]^2)
    isNear <- (distance <= radius & distance!=0)
    
    Date.d1 <- CrimeData[isNear,]["DATEOCC"]
    Date.exact <- subset(CrimeData,X_COORD==Loc.d1[i,]$X_COORD & Y_COORD==Loc.d1[i,]$Y_COORD,
                         select="DATEOCC")
    if (nrow(Date.d1)!=0){
      for (j in 1:(nrow(Date.d1))){
        Tinterval <- c(Tinterval, abs(difftime(Date.d1$DATEOCC[j],Date.exact$DATEOCC,units="days")))
      } 
    }
  }
  
  #Random Event Hypothesis
  if(REH==TRUE){
    D <- length(unique(CrimeData$DATEOCC))
    tau <- seq(0,D,length.out=3*D)
    Prob.d2 <- rep(NA,length(tau))
    for (i in seq_along(tau)){
      Prob.d2[i] <- 2*(D-tau[i]) / (D*(D+1))
    }
  }
  
  if(plot==TRUE){
    hist(Tinterval,breaks=0:max(Tinterval),freq=FALSE,xlab="Day",ylab="Probability",
         main=paste("Histogram of time intervals of two break-ins of order 1 houses within ", radius, " spatial units",sep=""),...)
    if(REH==TRUE){
      lines(tau,Prob.d2,type='l',col="red",cex=1)
    }
  }
  
  if(REH==TRUE){
    return(list(TintHist=hist(Tinterval,breaks=0:max(Tinterval),plot=FALSE),REHprob=data.frame(tau=tau,Prob=Prob.d2)))
  }
  else{
    return(hist(Tinterval,breaks=0:max(Tinterval),plot=FALSE))
  } 
}