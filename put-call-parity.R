#initial variables:
s = 100
u = 1.01
d = 0.99
t = 15
RR = 0.05/365
k = 95
type = "C"

######################################################################################################
#European Option
######################################################################################################


  #1. Calculate stock prices on the binomial tree
  stock_price <- matrix(0, t, t)
  stock_price[1, 1] = s
  
  for (i in 2:ncol(stock_price)) {
    stock_price[i, i] = 100 * d^ (i - 1) 
  }
  
  for (i in 1:(nrow(stock_price)- 1)) {
    for (j in (i + 1):nrow(stock_price)) {
      stock_price[i , j] = stock_price[i, i] * u ^ (j - i) 
    }
  }
  
  #2. a. Calculate the option value at end of binomial tree
  call_options_price <- matrix(0, t, t)
  put_options_price <- matrix(0, t, t)
  call_options_price[,ncol(stock_price)] = pmax(stock_price[,ncol(stock_price)] - k, 0) #change here for put option  
  put_options_price[,ncol(stock_price)] = pmax(k - stock_price[,ncol(stock_price)], 0) #change here for put option  
  
  
  
  #2. b. backward recursion
  q = (1 + RR - d) / (u - d) #probability
  for (i in (ncol(call_options_price) - 1):1) {
    for (j in (i):1) {
      call_options_price[j, i] = (q * call_options_price[j, i + 1] + (1 - q) * call_options_price[j + 1, i + 1]) / (1 + RR)  
    }
  }
  
  for (i in (ncol(put_options_price) - 1):1) {
    for (j in (i):1) {
      put_options_price[j, i] = (q * put_options_price[j, i + 1] + (1 - q) * put_options_price[j + 1, i + 1]) / (1 + RR)  
    }
  }

  strikes <- matrix(k, t, t)
  
  for (i in ncol(strikes):1) {
    strikes[,i] = strikes[,i] / (1 + RR) ^ (t - i)
  }
  
  parity = stock_price + put_options_price - call_options_price - strikes
