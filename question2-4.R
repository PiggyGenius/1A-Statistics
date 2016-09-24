########## Question 2.4 ##########

theta_0 <- function(numbers){
  max=numbers[1]
  for(i in 2:length(numbers)){
    if(numbers[i]>max)
      max=numbers[i]
  }
  return(max)
}

theta_1 <- function(numbers){
  max=numbers[1]
  n=length(numbers)
  for(i in 2:n){
    if(numbers[i]>max)
      max=numbers[i]
  }
  return(((n+1)/n)*max-1)
}

theta_2 <- function(numbers){
  max=numbers[1]
  min=numbers[1]
  for(i in 2:length(numbers)){
    if(numbers[i]>max){
      max=numbers[i]
    }
    else if(numbers[i]<min){
      min=numbers[i]
    }
  }
  return(max+min-1)
}

theta_3 <- function(numbers){
  # average=numbers[1]
  # for(i in 2:length(numbers)){
  #   average=average+numbers[i]
  # }
  # average=average/length(numbers)
  # return (2*average-1)
  return (mean(numbers)*2)-1
}

main <- function(n,theta){
  numbers=sample(1:theta,n)
  cat("The real number of tanks is",theta,"\n")
  print("The estimations are as follow:")
  cat("theta_0",theta_0(numbers),"\n")
  cat("theta_1",theta_1(numbers),"\n")
  cat("theta_2",theta_2(numbers),"\n")
  cat("theta_tilde",theta_3(numbers),"\n")
}

score_thetas <-function(theta,count){
  cat("The real number of tanks is:",theta,"\n\n")
  scales=c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.75,1)
  for(i in 1:length(scales)){
    n=ceiling(theta*scales[i])
    cat("When n=theta*",scales[i],"we have:\n")
    score_n(theta,n,count)
  }
}

score_n <- function(theta,n,count){
  t_0=rep(1,count)
  t_1=rep(1,count)
  t_2=rep(1,count)
  t_tilde=rep(1,count)
  for(i in 1:count){
    numbers=sample(1:theta,n)
    t_0[i]=theta_0(numbers);
    t_1[i]=theta_1(numbers);
    t_2[i]=theta_2(numbers);
    t_tilde[i]=theta_3(numbers);
  }
  bias=c(mean(t_0)-theta,mean(t_1)-theta,mean(t_2)-theta,mean(t_tilde)-theta)
  cat("Bias:\n")
  cat("theta_0",bias[1],"\n")
  cat("theta_1",bias[2],"\n")
  cat("theta_2",bias[3],"\n")
  cat("theta_tilde",bias[4],"\n\n")
  vars=c(var(t_0)-(bias[1])^2,var(t_1)-(bias[2])^2,var(t_2)-(bias[3])^2,var(t_tilde)-(bias[4])^2)
  cat("Mean Squared Error:\n")
  cat("theta_0",vars[1],"\n")
  cat("theta_1",vars[2],"\n")
  cat("theta_2",vars[3],"\n")
  cat("theta_tilde",vars[4],"\n\n")
}

##########   Fin 2.4    ##########