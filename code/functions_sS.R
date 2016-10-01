### this is the functions for the sS inventory system ###

### this script would be to identify the next event, and the event time ! ###
event_id<-function(schedule,event){
  vec<-unlist(schedule)
  m<-min(vec)
  #z<-list(as.numeric(which(vec==m)),m)
  event$event<-c(event$event,as.numeric(which(vec==m)))
  event$time<-c(event$time,m)
  return(event)
}

### this function is for the update of the schedule after an event ###
schedule.update<-function(system,event,schedule,interD,orderlt){
  # update for a demand
  if(event$event[length(event$event)]==1){
    schedule$demand=schedule$demand+interD[system$id$interD+1]
  }
  # update for a review
  if(event$event[length(event$event)]==2){
    # since the order lead time is U(0,1) and we have review weekly, it is impossible to have two orders to be arrived
    if(length(system$coming_orders)==1){             
      schedule$order_arrival<-schedule$review+orderlt[system$id$orderlt]
    }
    schedule$review<-schedule$review+7
  }
  # update for an order arrival
  if(event$event[length(event$event)]==3){
    schedule$order_arrival=Inf
  }
  
  return(schedule)
}

### this function is for the update of all the system variables ###
system.update<-function(system,event,policy,demandAm){
  
  # first possible update is for a demand, but that is till later
  if(event$event[length(event$event)]==1){
    # first update the ids for future use
    system$id$interD=system$id$interD+1
    system$id$demandAm=system$id$demandAm+1
    # then update inventory
    system$inventory$position=system$inventory$position-demandAm[system$id$demandAm]
    system$inventory$level=system$inventory$level-demandAm[system$id$demandAm]
  }
  # the second case is for a review
  if(event$event[length(event$event)]==2){
    # first, if the inventory level is lower than s
    if(system$inventory$position<policy$s){
      system$coming_orders<-c(system$coming_orders,policy$S-system$inventory$position)
      system$inventory$position<-policy$S
      system$id$orderlt<-system$id$orderlt+1
    }
    # else, nothing changes
  }
  # the third case is for a order arrival
  if(event$event[length(event$event)]==3){
    system$inventory$level=system$inventory$level+system$coming_orders
    system$coming_orders=system$coming_orders[-1]
  }
  
  return(system)
}


simulation<-function(terminate,interD,demandAm,orderlt,policy,PLOT=FALSE){
  
  ### this is the initialization of the variables ####
  
  event<-list(vector(),vector())
  names(event)<-c('event','time')
  
  schedule<-list(0,0,0)
  names(schedule)<-c("demand","review","order_arrival")
  
  id<-list(0,0,0)
  names(id)<-c('interD','demandAm','orderlt')
  
  inventory<-list(40,40)
  names(inventory)<-c('position','level')
  
  coming_orders<-vector()
  
  system<-list(inventory,coming_orders,id)
  names(system)<-c('inventory','coming_orders','id')
  
  record<-list(vector(),vector(),vector(),vector()) 
  names(record)<-c('time','event','position','level')
  
  
  ## start the simulation with the first review ###
  
  schedule$demand<-interD[1]
  schedule$review<-0
  schedule$order_arrival<-Inf
  
  event<-event_id(schedule,event)
  
  system<-system.update(system,event,policy,demandAm)
  
  schedule<-schedule.update(system,event,schedule,interD,orderlt)
  
  record$position<-c(record$position,system$inventory$position)
  
  record$level<-c(record$level,system$inventory$level)
  
  
  ### from here, we would go into the second event, this is where we should use loops ###
  while(event$time[length(event$time)]<terminate){
    
    event<-event_id(schedule,event)   
    
    system<-system.update(system,event,policy,demandAm)
    
    schedule<-schedule.update(system,event,schedule,interD,orderlt)
    
    record$position<-c(record$position,system$inventory$position)
    
    record$level<-c(record$level,system$inventory$level)
    
  }
  
  record$time<-event$time
  
  record$event<-event$event
  
  ### this is to plot the sample path ###
  
  if(PLOT==TRUE){
    #par(mfrow=c(2,1))
    plot(record$time,record$level,type="s",xlab="Time of Events",ylab="Inventory Level",main="Inventory Level Sample Path",cex.main=0.85)
    abline(h=policy$s,col='green')
    text(x=0,y=policy$s-5,labels='s',col="green")
    text(x=0,y=policy$S-5,labels='S',col="red")
    abline(h=policy$S,col="red")
    #plot(record$time,record$position,type='s',xlab="Time of Evnets",ylab="Inventory Position",main="Inventory Position Sample Path",cex.main=0.85)
    #abline(h=policy$s,col="green")
    #abline(h=policy$S,col="red")
    #text(x=0,y=policy$s-5,labels='s',col="green")
    #text(x=0,y=policy$S-5,labels='S',col="red")
  }
  
  return(record)
  
  
}

### this function is to calculate all the costs and statistics ###
measures<-function(R,policy){
  ## first, calculate the holding cost h and shortage cost s\
  s=0
  h=0
  for(i in 1:(length(R$level)-1)){
    if(R$level[i]>0){
      s=s+0.1/7*R$level[i]*(R$time[i+1]-R$time[i])
    }
    else{
      h=h-10/7*R$level[i]*(R$time[i+1]-R$time[i])
    }
  }
  ## second, calculate the ordering cost C
  C=0
  for(i in which(R$event==2)){
    if(R$position[i]==policy$S){
      C_s=10
      C_am=0.1*(R$position[i]-R$level[i])
      C=C+C_s+C_am
      if(length(C)==0) break
    }
  }
  ## third,calculate the total cost
  cost<-s+h+C
  
  ## fourth, calculate the percentage of weeks with shortage occurance
  # since our order arrival time is positive. Therefore to see if shrotage occurred in a week, we only have to see its inventory level at the review of the week
  neg<-0
  vec<-which(R$event==2)
  for(j in 2:length(vec)){
    if(R$level[vec[j]]<0 || R$level[vec[j-1]+1]<0){
      neg=neg+1
    }
  }
  frac=neg/(length(vec)-1)
  
  p<-list(s,h,C,cost,frac)
  names(p)<-c('holding','shortage','ordering','total','frac')
  return(p)
}


### this is trying to parallel the simualtion. Simply run a specified number of replications and then return the cost ##

sim_sS<-function(N,policy){
  temc<-vector()
  for(i in 1:N){
    interD<-runif(n=1000)
    orderlt<-runif(n=1000)
    demandAm<--5*log(runif(n=1000))
    result<-simulation(terminate,interD=interD,orderlt=orderlt,demandAm=demandAm,PLOT=FALSE,policy=policy)
    P<-measures(result,policy)
    temc[i]<-P$total
  }
  return(temc)
}

