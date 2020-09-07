rankhospital<-function(x,y,z){
  l<-read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(!x %in% l$State)
  {
    print("invalid state")
    stop()
  }
  disease<-c("heart attack", "heart failure", "pneumonia")
  if(!y %in% disease)
  {
    print("invalid outcome")
    stop()
  }
  if(z=="best"){z<-1}
  if(y=="heart attack")
  {
    
    l<-l%>%filter(State==as.character(x) )%>%mutate(rate=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)%>%filter(!rate=="Not Available")%>%mutate(rank=rank(as.numeric(rate)))%>%select(Hospital.Name, rank, rate)%>%arrange(rank)
    l$rank<-order(l$rank, l$Hospital.Name)
    if(z=="worst"){z<-length(l$rank)}
    if(z<=length(l$rank)){      a<-l$Hospital.Name[which(l$rank==z)]}
    else{a<-NA}
  }
  else if(y=="heart failure")
  {l<-l%>%filter(State==as.character(x) )%>%mutate(rate=Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)%>%filter(!rate=="Not Available")%>%mutate(rank=rank(as.numeric(rate)))%>%select(Hospital.Name, rank, rate)%>%arrange(rank)
  l$rank<-order(l$rank, l$Hospital.Name)
  if(z=="worst"){z<-length(l$rank)}
  if(z<=length(l$rank)){      a<-l$Hospital.Name[which(l$rank==z)]}
  else{a<-NA}
  }
  else{l<-l%>%filter(State==as.character(x) )%>%mutate(rate=Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)%>%filter(!rate=="Not Available")%>%mutate(rank=rank(as.numeric(rate)))%>%select(Hospital.Name, rank, rate)%>%arrange(rank)
  l$rank<-order(l$rank, l$Hospital.Name)
  if(z=="worst"){z<-length(l$rank)}
  if(z<=length(l$rank)){      a<-l$Hospital.Name[which(l$rank==z)]}
  else{a<-NA}
  }
a
  
}