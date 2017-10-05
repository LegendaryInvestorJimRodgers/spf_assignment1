#initial variables:
s = 100
u = 1.01
d = 0.99
RR = 0.05/365
k = 95
t = 15

######################################################################################################
#European Option
######################################################################################################

european <- function(s, u, d, RR, k, t, type) {
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
options_price <- matrix(0, t, t)
if (type == "C") {
  options_price[,ncol(stock_price)] = pmax(stock_price[,ncol(stock_price)] - k, 0) #change here for put option  
} else {
  options_price[,ncol(stock_price)] = pmax(k - stock_price[,ncol(stock_price)], 0) #change here for put option  
}


#2. b. backward recursion
q = (1 + RR - d) / (u - d) #probability
#q = 0.5068493151
for (i in (ncol(options_price) - 1):1) {
  for (j in (i):1) {
    options_price[j, i] = (q * options_price[j, i + 1] + (1 - q) * options_price[j + 1, i + 1]) / (1 + RR)  
  }
}

returned_things <- list("V0" = options_price, "stock" = stock_price)
return(returned_things)
}

########################################################################################################
#Put call parity
########################################################################################################
#print(european(s, u, d, RR, k, t, "C") - european(s, u, d, RR, k, t, "P"))
stocks = european(s, u, d, RR, k, t, "P")[2]
call_optionion = european(s, u, d, RR, k, t, "C")[1]
put_option = european(s, u, d, RR, k, t, "P")[1]

#TODO: turn 1 + RR ^ t into an array!
#parity <- stocks + put_option - call_option - k * (1 + RR)^(t)
