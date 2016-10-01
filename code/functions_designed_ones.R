### this script includes the designed functions for demonstration ###
vfunc<-function(x){
  y<-(x-4.75)^2
  return(y)
}



### this function is the initial regression for constructing the starting point for the procedure ###

initial_regression<-function(theta,y){
  # theta should be a vector of alternatives
  # y should be initial observation on each alternative
  theta=theta/10
  phi<-vector()
  for(i in 1:length(theta)){
    feature<-c(theta[i]^2,theta[i],1,1/theta[i])
    phi<-rbind(phi,feature,deparse.level = 0)
  }
  # now we have phi
  B<-solve(t(phi)%*%phi)
  beta<-B%*%t(phi)%*%y
  z<-list(B,beta)
  names(z)<-c("matrix","coefficients")
  return(z)
}

### this is the estimated function, which is used for selecting the potential candidate ###
#estimated_func<-function(x,result=result) result[[2]][1]*x^2+result[[2]][2]*x+result[[2]][3]+result[[2]][4]/x

estimated_func<-function(x,result=result) result[[2]][1]*(x/10)^2+result[[2]][2]*(x/10)+result[[2]][3]+result[[2]][4]/(x/10)

## this function is reading in the new observations and update all the matrix and vectors ##=
update_func<-function(theta,observation,result,i){
  # this is getting a new observation on alternative theta, and 
  # update the coefficients & matrix
  theta=theta/10
  phi<-rbind(theta[i]^2,theta[i],1,1/theta[i],deparse.level=0)
  y<-observation[[i]][length(observation[[i]])]
  B<-result[[1]]
  coeff<-result[[2]]
  coeff_new<-coeff+as.numeric((y-t(phi)%*%coeff)/(1+t(phi)%*%B%*%phi))*(B%*%phi)
  B_new<-B-B%*%phi%*%t(phi)%*%t(B)/as.numeric(1+t(phi)%*%B%*%phi)
  z<-list(B_new,coeff_new)
  names(z)<-c("matrix",'vector')
  return(z)
}


## this function is reading in the result for the alternative, and then perform a new simulation ###
# make this function compatible with the observation function we designed ###
new_observation<-function(i,observation){
  #y<-ob_design(i)
  #y<-ob_queue(i)
  y<-ob_sS(i)
  observation[[i]]=c(observation[[i]],y)
  return(observation)
}

### this function is for plotting all available observations, because we are using the animation package. This is a necessary step ###
observation_plot<-function(observation,theta){
  for(i in 1:length(theta)){
    for(j in 1:length(observation[[i]])){
      points(theta[i],observation[[i]][j],pch=18,col="red")
    }
  }
}



### this function is the selection function we designed, also it points an arrow at the currect optimal point ###
selection<-function(result,theta,observation,PLOT=FALSE,type='min'){
  J_hat<-vector()
  Y_mean<-vector()
  Er<-vector()
  for(i in 1:length(theta)){
    J_hat[i]<-estimated_func(theta[i],result)
    Y_mean[i]=mean(observation[[i]])
    Er[i]=1/length(observation[[i]])*(sum(observation[[i]]-J_hat[i]))
  }
  r1<-sort(J_hat)
  r2<-sort(Y_mean)
  r3<-sort(abs(Er))

  
  ## this selection rule is trying to avoid local traps, where we allocate all our simualtion resource to one single alternative
  if(type=="min"){
    p1<-which(J_hat==r1[1])
    p2<-which(Y_mean==r2[1])
    if(p1==p2){
      p2=which(Y_mean==r2[2])
    }
    p3<-which(abs(Er)==r3[length(r3)])
    if(p3==p1||p3==p2){
      p3<-which(abs(Er)==r3[length(r3)-1])
    }
    if(p3==p1||p3==p2){
      p3<-which(abs(Er)==r3[length(r3)-2])
    }
    ind<-c(p1,p2,p3)
  }
  
  ## this selection considers the ones that coincide 
  #p1<-which(J_hat==r1[1])
  #p2<-which(Y_mean==r2[1])
  #p3<-which(abs(Er)==r3[length(r3)])
  
  if(type=="max"){
    p1<-which(J_hat==r1[length(r1)])[1]
    p2<-which(Y_mean==r2[length(r2)])[1]
    if(p1==p2){
      p2=which(Y_mean==r2[length(r2)-1])[1]
    }
    p3<-which(abs(Er)==r3[length(r3)])
    if(p3==p1||p3==p2){
      p3<-which(abs(Er)==r3[length(r3)-1])[1]
    }
    if(p3==p1||p3==p2){
      p3<-which(abs(Er)==r3[length(r3)-2])[1]
    }
    ind<-c(p1,p2,p3)
  }
  
  if(PLOT==TRUE){
    arrows(x0=theta[p1],y0=2000,x1=theta[p1],y1=1800,length=0.2,col="green",lwd=6.5)
  }
  
  return(c(p1,p2,p3))
}





