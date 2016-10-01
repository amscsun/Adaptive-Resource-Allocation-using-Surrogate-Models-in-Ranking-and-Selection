### this script is for calculating the wanted statistics of the problem ###

# use an easy system to develop the functions
#terminate$quant=10000
#terminate$type="time"
#t<-exponential(y0=443952721)
#s<-exponential(y0=1848090842)
#result=simulation(arr_t=t,ser_t=s,capacity=100,terminate=terminate)

# our statistics calculation function should be able get all the statistics with a result from the simulatior
# we should be very careful in this, because the starting and ending point of the sample path is not necessarily an event

# this modify function is to modify our simulation result so they could be more uniform, and readily used for analysis
modify<-function(start,end,result){
  m<-list()
  for(i in 1:length(result$time)){
    if(result$time[i]>start)
      break
  }
  for(j in length(result$time):1){
    if(result$time[j]<end)
      break
  }
  m[[1]]=c(start,result$time[i:j],end)
  m[[2]]=result$number[c((i-1):j,j)]
  m[[3]]=result$event[(i-1):(j-1)]
  m[[4]]=result$customer[(i-1):(j-1)]
  m[[5]]=result$q[(i-1):(j-1)]
  names(m)<-c("time","number","event","customer","q")
  
  return(m)
}

## now we get a function which calculates the designed cost, namely number of served customers, number of rejected customers

cost<-function(result){
  n1<-sum((result$event==1))
  n2<-sum((result$event==0))
  z<-c(n1,n2)
  names(z)<-c("served","rejected")
  return(z)
}

### this is simulating new observations under the specified number of simulations ###

cost_sim<-function(j,N){
  terminate=list(8,"time")
  names(terminate)=c("quant","type")
  m<-vector()
  for(i in 1:N){
    t1<-exponential(5)  # inter arrival time
    t2<-exponential(1)  # service time
    result<-simulation(arr_t=t1,ser_t=t2,capacity=j,terminate=terminate,PLOT=FALSE)
    m[i]<-cost(result)[1]
  }
  return(m)
}


### this is a function which returns a single new observationm, with specified alternative i ###

ob_queue<-function(i){
  terminate=list(8,"time")
  names(terminate)=c("quant","type")
  t1<-exponential(5)  # inter arrival time
  t2<-exponential(1)  # service time
  result<-simulation(arr_t=t1,ser_t=t2,capacity=i,terminate=terminate,PLOT=FALSE)
  return((cost(result)[1]-4*i)[[1]])
}