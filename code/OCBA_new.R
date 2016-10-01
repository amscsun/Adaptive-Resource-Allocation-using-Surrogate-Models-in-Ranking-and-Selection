### this script is for writing the OCBA procedure ###

pOCBA<-function(budget,N,type="min",corr){
  m<-vector()
  for(i in 1:N){
    m[i]<-OCBA_new(budget=budget,type=type)
  }
  return(sum(m==corr)/N)
}

### we still have to maintain the same pOCBA function. It is running smoothly with the other code ###

OCBA_new<-function(budget,type="min"){
  if(budget<200){
    n_0=5
    N_delta=as.integer((budget-n_0*10)/3)
  }else{
    n_0=budget/(4*10)
    N_delta=n_0*10
  }
  ## now we want to do the initial observation
  observation<-list()
  for(i in 1:10){
    obj<-vector()
    for(j in 1:n_0){
      #obj[j]<-ob_design(i)
      obj[j]<-ob_queue(i)
    }
    observation[[i]]<-obj
  }
  for(p in 1:3){
    ## with the initial observation, we can proceed to do more observations
    n_new<-sel_OCBA(N_delta=N_delta,observation=observation,type=type)
    ## now we proceed to new observation
    for(i in 1:10){
      obj<-vector()
      for(j in 1:n_new[i]){
        #obj[j]<-ob_design(i)
        obj[j]<-ob_queue(i)
      }
      observation[[i]]<-c(observation[[i]],obj)
    }
  }
  
  ## now we have the selection process
  Y_mean<-vector()
  for(i in 1:10){
    Y_mean[i]=mean(observation[[i]])
  }
  #points(theta,Y_mean,pch=19)
  if(type=="min"){
    return(which(Y_mean==min(Y_mean))[1])
  }
  if(type=="max"){
    return(which(Y_mean==max(Y_mean))[1])
  }
}


## this is the OCBA selection function which assigns the simulation resource according to the rules. This is also where the mistakes could have happended ###
sel_OCBA_new<-function(N_delta,observation,type="min"){
  ob_mean<-vector()
  for(i in 1:length(observation)){
    ob_mean[i]=mean(observation[[i]])
  }
  if(type=="min"){ind_b<-which(ob_mean==min(ob_mean))}
  if(type=="max"){ind_b<-which(ob_mean==max(ob_mean))}
  delta<-vector()
  s_data<-vector()
  for(i in 1:length(ob_mean)){
    delta[i]<-ob_mean[i]-ob_mean[ind_b[1]]
    s_data[i]=var(observation[[i]])
  }
  factor<-vector()
  for(i in 1:length(ob_mean)){
    if(i %in% ind_b){
      factor[i]<-0
    }
    else{
      factor[i]<-s_data[i]^2/delta[i]^2
    }
  }
  factor[ind_b]<-s_data[ind_b]*sqrt(sum(factor^2/s_data^2))
  if(sum(factor)==0){
    N<-c(0,0,0,0,0,0,0,0,0,0)
  }
  else{N<-as.integer(N_delta*factor/sum(factor))}
  return(N)
}