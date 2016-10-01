### this is writing the observation function for the (s,S) system ###

ob_sS<-function(i){
  S<-c(80,140,200,300,400,500,600,700,820,950)
  policy<-list(20,S[i])
  names(policy)<-c("s","S")
  interD<-runif(n=1000)
  orderlt<-runif(n=1000)
  demandAm<--5*log(runif(n=1000))
  terminate=365
  result<-simulation(terminate,interD=interD,orderlt=orderlt,demandAm=demandAm,PLOT=FALSE,policy=policy)
  return(measures(result,policy)$total)
}

ob_queue<-function(i){
  # the alternatives are the capacity, 1:10 in our case
  terminate=list(8,"time")
  names(terminate)=c("quant","type")
  t1<-exponential(5)  # inter arrival time
  t2<-exponential(1)  # service time
  result<-simulation(arr_t=t1,ser_t=t2,capacity=i,terminate=terminate,PLOT=FALSE)
  return((cost(result)[1]-4*i)[[1]])
}

ob_design<-function(i){
  alternatives<-c(3,3.5,4,4.5,5,5.5,6,6.5,7,7.5)
  y<-(alternatives[i]-4.5)^2+5*rnorm(1)
  return(y)
}