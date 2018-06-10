library(dplyr)
rankall <- function(outcome, num='best' ) {
  dat<-read.csv("outcome-of-care-measures.csv",colClasses = 'character')
  mydat<-select(dat,State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  mydat1<-filter(mydat,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack!="Not Available")
  if( outcome=="heart attack"){
  mydat1<-mydat1[order(mydat1$State,mydat1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,mydat1$Hospital.Name),]
  my<-split(mydat1[,c("Hospital.Name")],mydat1$State)
  }
  if(outcome=="heart failure"){
    mydat1<-filter(mydat,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure!="Not Available")
    mydat1<-transform(mydat1,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    mydat1<-mydat1[order(mydat1$State,mydat1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,mydat1$Hospital.Name),]
    my<-split(mydat1[,c("Hospital.Name")],mydat1$State)
  }
  if (outcome=="pneumonia"){
    mydat1<-filter(mydat,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!="Not Available")
    mydat1<-transform(mydat1,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    mydat1<-mydat1[order(mydat1$State,mydat1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,mydat1$Hospital.Name),]
    my<-split(mydat1[,c("Hospital.Name")],mydat1$State)
  }
 myfunction<-function(x,num) {
   
   if (num=='best'){
     head(x,1)
   }
  else if(num=='worst'){
    tail(x,1)
  }
  else{
    x[num]
  }
 }
 final<-lapply(my,myfunction,num)
 data.frame(hospital=unlist(final),state=names(final))
 
}
