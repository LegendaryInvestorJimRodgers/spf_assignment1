#initial variables:
s = 100
#u = 1.01
d = 0.95
t = 15
RR = 0.05/365
k = 95


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
print(options_price)
return(options_price[1,1])
}

#ups = c(1.01, 1.02, 1.05, 1.1)
ups = seq(1.01,1.5, 0.001)
#downs = seq(0.5,0.999,0.01)
call_values = c()

for (u in ups) {
    call_values =  c(call_values, european(s, u, d, RR, k, t, "P"))
}

plot(ups,call_values, xlab = "Up Values", ylab = "Call Option Prices", log ="")


#p <- plot_ly(x = ups, y = downs, z = call_values) %>% add_surface()


#persp(ups, downs, call_values, phi = 45, theta = 45,
#      xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
#      main = "Surface elevation data"
#)


########################################################################################################
#Put call parity
########################################################################################################
#print(european(s, u, d, RR, k, t, "C") - european(s, u, d, RR, k, t, "P"))
#stocks = european(s, u, d, RR, k, t, "P")[2]
#call_optionion = european(s, u, d, RR, k, t, "C")[1]
#put_option = european(s, u, d, RR, k, t, "P")[1]

#TODO: turn 1 + RR ^ t into an array!
#parity <- stocks + put_option - call_option - k * (1 + RR)^(t)
