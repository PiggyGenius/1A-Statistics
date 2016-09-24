#
# 1.7
#

# Histogramme
draw_hist <- function(x, ttl) {
  n = length(x)
  k = floor(1 + log2(n))
  x1_star = min(x)
  xn_star = max(x)
  a0 = floor(x1_star - 0.025 * (xn_star - x1_star))
  ak = ceiling(xn_star + 0.025 * (xn_star - x1_star))
  h = (ak - a0)/k
  hist(x, prob=T, breaks=seq(a0, ak, h), main=ttl, xlab='Observations', ylab='Densité')
}

# Estimations
get_estimates <- function(x) {
  xn_star = max(x)
  n = length(x)
  slope = lm(seq(1:n)/n ~ sort(x)[1:n])$coefficients[[2]]
  
  res = list(
    theta_mean = 2 * mean(x) - 1,
    theta_med = 2 * median(x) - 1,
    theta_max = xn_star,
    theta_graph = 1/slope,
    theta_best = (xn_star^(n+1) - (xn_star - 1)^(n+1))/(xn_star^n - (xn_star - 1)^n)
  )
  return(res)
}

question7 <- function() {
  theta = 1000
  n = 20
  x = sample(1:theta, n, replace=T)
  
  draw_hist(x, 'Histogramme de U{1, ..., 1000}')
  
  # Graphe de probabilite
  plot(sort(x)[1:n], seq(1:n)/n, ylim=c(0, 1), main='Graphe de probabilité', xlab='Observations', ylab='i/n')
  title()
  
  estimates = get_estimates(x)
}

question7()

#
# 1.8
#

bias_error_estimators <- function(n, m, theta) {
  estimates = matrix(nrow = 5, ncol = m)
  biases = c(0, 0, 0, 0, 0)
  errors = c(0, 0, 0, 0, 0)
  
  for(i in 1:m) {
    x = sample(1:theta, n, replace=T)
    es = get_estimates(x)
    
    estimates[1, i] = es$theta_mean
    estimates[2, i] = es$theta_med
    estimates[3, i] = es$theta_max
    estimates[4, i] = es$theta_graph
    estimates[5, i] = es$theta_best
  }
  
  for(i in 1:5) {
    biases[i] = mean(estimates[i,]) - theta
    # EQM = variance + biais^2
    errors[i] = var(estimates[i,]) + biases[i]^2
  }
  
  res = list(
    biases = biases,
    errors = errors
  )
  return(res)
}

question8 <- function() {
  colors = c('red', 'green', 'blue')
  m_vals = c(10, 100, 1000)
  n_vals = c(10, 100, 1000)
  theta = 1000
  
  for(n in n_vals) {
    plot(m_vals, c(0,0,0,0,0), type="l", col="yellow")
    title(main='n = ' + toString(n))
    
    for(i in 1:length(m_vals)) {
      m = m_vals[i]
      mes = bias_error_estimators(n, m, theta)
      
      biases = mes$biases
      errors = mes$errors
      
      points(m, biases)
    }
  }
}

question8()

#
# 1.10
#

is_in_conf_int <- function(theta, n, alpha) {
  x = sample(1:theta, n, replace=T)
  ualpha = qnorm(1 - alpha/2)
  theta_tilde = get_estimates(x)$theta_mean
  
  a = 3 * n^2 - ualpha^2
  b = 6 * n^2 * theta_tilde
  delta = -4 * (3 * n^2 * ualpha^2 * (1 - theta_tilde^2) - ualpha^4)
  
  return((b - sqrt(delta))/(2 * a) <= theta && theta <= (b + sqrt(delta))/(2 * a))
}

question10 <- function() {
  theta = 1000
  n_vals = c(10000)
  m_vals = seq(1, 1100, 100)
  alpha_vals = c(0.1, 0.05, 0.01)
  
  freqs = vector(length=length(m_vals))
  
  for(alpha in alpha_vals) {
    for(n in n_vals) {
      for(m_index in 1:length(m_vals)) {
        m = m_vals[m_index]
        nb_in = 0
        
        for(i in 1:m) {
          if(is_in_conf_int(theta, n, alpha)) {
            nb_in = nb_in + 1
          }
        }
        freqs[m_index] = nb_in/m * 100
      }
      
      plot(m_vals, freqs)
      title(main=paste('alpha=', toString(alpha), ' ; n=', toString(n)))
    }
  }
}

question10()
