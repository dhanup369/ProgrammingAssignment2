library(dplyr)
rankhospital<-function(state,outcome,num='best'){
  dat<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  S<-unique(dat$State)
  sta<-state
  if(!sta %in% S){
    stop("invalid state")}
  if(!outcome %in% c("heart failure","heart attack","pneumonia")){
    stop("invalid outcome")}
  if (outcome=="heart attack")
  {
    k<-select(dat,State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    l<-filter(k,State==state)
    l1<-filter(l,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack!="Not Available")
    l2<-l1[order(l1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,l1$Hospital.Name),]
  }
  if (outcome=="heart failure")
  {
    k<-select(dat,State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    l<-filter(k,State==state)
    l1<-filter(l,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure!="Not Available")
    l1<-transform(l1,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    l2<-l1[order(l1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,l1$Hospital.Name),]
    
  }
  if (outcome=="pneumonia")
  {
    k<-select(dat,State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    l<-filter(k,State==state)
    l1<-filter(l,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!="Not Available")
    l1<-transform(l1,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia=as.numeric(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    l2<-l1[order(l1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,l1$Hospital.Name),]
  }
  if (num=='best'){
    l3<-head(l2,1)
    
  }
  
  else if (num=='worst'){
    l3<-tail(l2,1)
  }
  else
  {
    l3<-l2[num,]
  }
  
  l3$Hospital.Name
}
  