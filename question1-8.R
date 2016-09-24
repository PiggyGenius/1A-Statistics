########## Question 1.8 ##########
# theta tilde=theta_0
# theta' tilde=theta_1
# theta chapeau=theta_2
#theta caron=theta_3
#theta_g

theta_00 <- function(numbers){
  return (mean(numbers)*2)-1
}

theta_11 <- function(numbers){
  return (median(numbers)*2)-1
}

theta_22 <- function(numbers){
  return (max(numbers,na.rm=TRUE))
}

theta_33 <- function(numbers){
  n=length(numbers)
  maxX=max(numbers,na.rm=TRUE)
  a=maxX^(n+1)-(maxX-1)^(n+1)
  b=maxX^(n)-(maxX-1)^(n)
  return (a/b)
}

theta_g  <- function(numbers){
  n=length(numbers)
  ordered=sort(numbers)
  plot(ordered)
  abline(lsfit(ordered,seq(1:n-1)/n),col="green")
  value=coef(lm((seq(1:n-1)/n)~ordered))[2]
}

score_values <-function(theta,count){
  cat("The real number of tanks is:",theta,"\n\n")
  scales=c(0.01,0.1,0.2,0.3,0.4,0.5,0.6,0.75,1)
  for(i in 1:length(scales)){
    n=ceiling(theta*scales[i])
    cat("When n=theta*",scales[i],"we have:\n")
    score_n(theta,n,count)
  }
}

score_estimators <- function(theta,n,count){
  t_0=rep(1,count)
  t_1=rep(1,count)
  t_2=rep(1,count)
  t_tilde=rep(1,count)
  for(i in 1:count){
    numbers=sample(1:theta,n,replace=T)
    t_0[i]=theta_00(numbers);
    t_1[i]=theta_11(numbers);
    t_2[i]=theta_22(numbers);
    t_tilde[i]=theta_33(numbers);
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

##########   Fin 1.8    ##########