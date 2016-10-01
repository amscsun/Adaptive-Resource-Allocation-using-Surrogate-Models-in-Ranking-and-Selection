
library(animation)
ani.options(interval=0.1,nmax=300,title="simulation procedure",description="this shows the step by step selection process",autoplay=TRUE)

#alternatives<-c(3,3.5,4,4.5,5,5.5,6,6.5,7,7.5)
# observation should contain all simulation results on all alternatives in a list
alternatives<-c(80,140,200,300,400,500,600,700,820,950)
observation<-list()

for(i in 1:10){
  observation[[i]]<-ob_sS(i)
}
#curve((x-4.5)^2,2.5,9,ylim=c(-10,30),xlab=expression(theta),lwd=2,ylab="Objective Value",main="Selection Process")
plot(x=0,y=0,xlim=c(20,1000),ylim=c(1800,4000))

## these three functions complete a circle of getting a plot for doing the wanted animation ###
observation_plot(observation,theta=alternatives)

result<-initial_regression(theta=alternatives,y=unlist(observation))

curve(result[[2]][1]*(x/10)^2+result[[2]][2]*(x/10)+result[[2]][3]+result[[2]][4]/(x/10),20,1000,col="blue",lty=4,cex=1.5,lwd=2.5,add=TRUE)

legend("topright",legend=c("Observations","True Objective","Estimated Ohjective"),pch=c(18,NA,NA),lty=c(NA,1,4),lwd=c(NA,2,2.5),col=c("red","black","blue"))

ind<-selection(result,theta=alternatives,observation,PLOT=TRUE)  

budget=100
count=1

saveHTML({
  while(count<budget){
    
    for(i in 1:length(ind)){
      
      plot(x=0,y=0,xlim=c(20,1000),ylim=c(1800,4000))
      
      observation_plot(observation,theta=alternatives)
      
      observation<-new_observation(ind[i],observation)
      
      count=count+1
      
      points(alternatives[ind[i]],observation[[i]][length(observation[[i]])],pch=13,col="chartreuse4")
      
      result<-update_func(theta=alternatives,observation,result=result,ind[i])
      
      curve(result[[2]][1]*(x/10)^2+result[[2]][2]*(x/10)+result[[2]][3]+result[[2]][4]/(x/10),20,1000,col="blue",lty=4,cex=1.5,lwd=2.5,add=TRUE)
      
      legend("topright",legend=c("Observations","True Objective","Estimated Ohjective"),pch=c(18,NA,NA),lty=c(NA,1,4),lwd=c(NA,2,2.5),col=c("red","black","blue"))
      
      selection(result,theta=alternatives,observation,PLOT=TRUE)  
      
    }
    ind<-selection(result,theta=alternatives,observation) 
    
  }
})

ani.options()
