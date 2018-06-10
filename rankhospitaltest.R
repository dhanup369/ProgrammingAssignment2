library(dplyr)
rankhospital<-function(state,outcome,num){
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
        l2<-l1[order(l1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]
     }
  if (outcome=="heart failure")
  {
    k<-select(dat,State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Heart.failure)
    l<-filter(k,State==state)
    l1<-filter(l,Hospital.30.Day.Death..Mortality..Rates.from.Heart.failure!="Not Available")
    l2<-l1[order(l1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.failure),]
  }
  if (outcome=="Pneumonia")
  {
    k<-select(dat,State,Hospital.Name,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    l<-filter(k,State==state)
    l1<-filter(l,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia!="Not Available")
    l2<-l1[order(l1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]
  }
  
    
    if (num=='best'){
      l3<-head(l2,1)
      
    }
    
    if (num=='worst'){
      l3<-tail(l2,1)
    }
   l3$Hospital.Name
  
}
rankhospital("MD", "heart attack",'worst' )