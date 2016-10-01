### this script is for the equal selection procedure ###

## we design our comparison to be runnning the selection 1000 times and count the number of correct selections we achieved

ob_eq<-function(budget){
  observation<-list()
  for(i in 1:10){
    new_ob<-vector()
    for(j in 1:(budget/10)){
      #new_ob[j]<-ob_design(i)
      #new_ob[j]<-ob_queue(i)
      new_ob[j]<-ob_sS(i)
    }
    observation[[i]]<-new_ob
  }
  return(observation)
}

EQ<-function(budget,type="min"){
  m<-vector()
  observation=ob_eq(budget)
  for(i in 1:length(observation)){
  	m[i]<-mean(observation[[i]])
  }
  if(type=="min"){
    return(which(m==min(m)))
  }
  if(type=="max"){
    return(which(m==max(m)))
  }
}

pEQ<-function(budget,N,type="min",corr){
	m<-vector()
	for(i in 1:N){
		m[i]<-EQ(budget,type=type)
	}
	return(sum(m==corr)/N)
}
