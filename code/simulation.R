#### this is writing the main simulation function ###
simulation<-function(arr_t,ser_t,capacity,terminate,PLOT=TRUE){


  # initialization the schdule function
  schedule<-list(arr_t[1],Inf,1,0)
  names(schedule)<-c("next_arrival","next_departure","arrival_id","departure_id")
  
  # initialization of the sytem states
  server_states<-rep(0,times=capacity)
  departure_times<-rep(Inf,times=capacity)
  customers<-rep(0,times=capacity)
  system<-list(server_states,departure_times,customers)
  names(system)<-c("server_states","departure_times","customers")

  # initialization of the record variable
  path.time<-0
  path.N<-0
  path.id<-vector()
  path.customer<-vector()
  p_statistics<-vector()
  End=0

  # after all the initilization, we could proceed to the simulation of the system
  while(End<terminate$quant){
    event<-event.id(schedule,system,capacity)
    #event
    path.time<-c(path.time,event$time)
    path.id<-c(path.id,event$event)
    path.customer<-c(path.customer,event$customer)
    
    
    # now update all the variables after the identification of the event
    if(event$event==1 || event$event==0){
      p_statistics=c(p_statistics,sum(system$server_states))
    }
    else{
      p_statistics=c(p_statistics,-1)
    }
   # system
    system<-system.update(schedule,event,system,capacity,ser_t,path.id)
   # system

    path.N<-c(path.N,sum(system$server_states))
    # little check
    #plot(path.time,path.N,type="s")
    
    #schedule
    schedule<-schedule.update(schedule,event,system,arr_t)
    #schedule
    
    
    if(terminate$type=="customer"){
      End=sum(path.id==-1)
    }
    if(terminate$type=="time"){
      End=path.time[length(path.time)]
    }
  }
  
  if(PLOT==TRUE){
    plot(path.time,path.N,type="s",xlab="time",ylab="number in system",main="Sample Path")
    points(x=path.time[which(path.id==0)+1],y=rep(capacity,times=sum(path.id==0)),pch=4,col="red",cex=1.5)
  }
  
  z<-list(path.time,path.N,path.id,path.customer,p_statistics)
  names(z)=c("time","number","event","customer","q")
  return(z)
  
}
