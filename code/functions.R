### this function is for the identification of the next event ###

# first generating exponential random variates
exponential<-function(rate){
  u<-runif(n=1000)  
  t=-1/rate*log(u)
  return(t)
}


event.id<-function(schedule,system,capacity){
  if(schedule$next_arrival>schedule$next_departure){
    event_id=-1
    event_time=schedule$next_departure
    event_customer=schedule$departure_id
  }
  if(schedule$next_arrival<schedule$next_departure){
    if(sum(system$server_states)==capacity){
      event_id=0
      event_time=schedule$next_arrival
      event_customer=schedule$arrival_id
    }
    if(sum(system$server_states)<capacity){
      event_id=1
      event_time=schedule$next_arrival
      event_customer=schedule$arrival_id
    }
  }
  event<-list(event_id,event_time,event_customer)
  names(event)<-c("event","time","customer")
  return(event)
}

system.update<-function(schedule,event,system,capacity,ser_t,path.id){
  if(event$event==1){
    s<-which(system$server_states==0)[1]
    system$server_states[s]=1
    system$customers[s]=schedule$arrival_id
    system$departure_times[s]=ser_t[system$customers[s]-sum(path.id==0)]+event$time
  }
  if(event$event==-1){
    s<-which(system$customers==event$customer)
    system$server_states[s]=0
    system$departure_times[s]=Inf
    system$customers[s]=0
  }
  if(event$event==0){
    # there is no change to the state of the system
  }
  return(system)
}


schedule.update<-function(schedule,event,system,arr_t){
  if(event$event==1){
    schedule$arrival_id=schedule$arrival_id+1
    schedule$next_arrival=event$time+arr_t[schedule$arrival_id]
    schedule$next_departure=min(system$departure_times)
    schedule$departure_id=system$customers[which(system$departure_times==schedule$next_departure)[1]]
  }
  if(event$event==-1){
    schedule$next_departure=min(system$departure_times)
    schedule$departure_id=system$customers[which(system$departure_times==schedule$next_departure)[1]]
  }
  if(event$event==0){
    schedule$arrival_id=schedule$arrival_id+1
    schedule$next_arrival=event$time+arr_t[schedule$arrival_id]
    schedule$next_departure=min(system$departure_times)
    schedule$departure_id=system$customers[which(system$departure_times==schedule$next_departure)[1]]
  }
  
  return(schedule)
}


