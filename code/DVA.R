### thsi script is for compiling everything into a big function ###


## Dynamic Resource Allocation via Objective Function Approximation, let's OFA procedure ###

DVA<-function(budget,type){
  
  #alternatives=1:10
  alternatives<-c(80,140,200,300,400,500,600,700,820,950)

  observation<-list()
  
  for(i in 1:10){
    #observation[[i]]<-ob_design(i)
    #observation[[i]]<-ob_queue(i)
    observation[[i]]<-ob_sS(i)
  }
  
  result<-initial_regression(theta=alternatives,y=unlist(observation))
 
  ind<-selection(result,theta=alternatives,observation,PLOT=FALSE,type=type)  

  #count=length(alternatives)'
  count=10

    while(count<budget){
      
      for(i in 1:length(ind)){
        
        observation<-new_observation(ind[i],observation)
        
        count=count+1
        
        result<-update_func(theta=alternatives,observation,result=result,ind[i])
      }
      ind<-selection(result,theta=alternatives,observation,PLOT=FALSE,type=type) 
    }
  return(ind[1])
}


## this is the function that takes in the simulation budget and number of replications as arguments and then return a PCS

pDVA<-function(budget,N,type="min",corr){
    m<-vector()
    for(i in 1:N){
      m[i]<-DVA(budget=budget,type=type)
    }
    return(sum(m==corr)/N)
}
