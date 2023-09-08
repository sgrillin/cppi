library(quantmod)
library(ggplot2)
library(PerformanceAnalytics)

getSymbols("BTC-USD", from = "2020-01-06", to = Sys.Date(), auto.assign = TRUE)

bitcoin <- `BTC-USD`

# Simple application
# Addition using drawdown (dynamic floor)
# Backtesting
# Graph with comparison of results with 60/40 strategy

# Calculate monthly returns of the SP500
btc_monthly <- to.monthly(bitcoin)[, 6]
btc_monthly_returns <- monthlyReturn(btc_monthly)

# Case 1: Fixed rfr, then use a Vasicek model for the path or a time-series of existing prices
# Define the risk-free rate
r <- 0.02
risk_free_rate <- rep(r/12, length(btc_monthly_returns)) # Can be replaced by a bond index or a series of bond returns

# Set the parameters for the CPPI strategy
floor <- 0.8 # This means that the loss should be no more than 20% of the original investment
multiplier <- 3 # so the investment is (1-0.8)*3 of the initial value, which is equivalent to a starting portfolio with 60/40 strategy
start_value <- 1000 # Initial investment

date <- index(btc_monthly_returns)
n_steps <- length(date)


account_value <- cushion <- risky_weight <- safe_weight <- risky_allocation <- safe_allocation <- numeric(n_steps)
account_value[1] <- start_value
floor_value <- floor*account_value
cushion[1] <- (account_value[1]-floor_value[1])/account_value[1]
risky_weight[1] <- multiplier * cushion[1]
safe_weight[1] <- 1-risky_weight[1]

risky_allocation[1] <- account_value[1] * risky_weight[1]  
safe_allocation[1] <- account_value[1] * safe_weight[1]

risky_return <- cumprod(1+btc_monthly_returns)

# Static floor: horizontal line
for (s in 2:n_steps) {
  account_value[s] = risky_allocation[s-1]*(1+btc_monthly_returns[s]) + safe_allocation[s-1]*(1+risk_free_rate[s-1])
  floor_value[s] <- account_value[1] * floor
  cushion[s] <- (account_value[s]-floor_value[s])/account_value[s]
  risky_weight[s] <- multiplier*cushion[s]
  risky_weight[s] <- min(risky_weight[s], 1)
  risky_weight[s] <- max(0, risky_weight[s])
  safe_weight[s] <- 1-risky_weight[s]
  risky_allocation[s] <- account_value[s] * risky_weight[s]
  safe_allocation[s] <- account_value[s] * safe_weight[s]
}

# Buy and hold strategy 60/40
buy_and_hold <- cumprod(1+(0.6*btc_monthly_returns + 0.4*risk_free_rate))

z_static <- cbind(account_value/1000, risky_return, buy_and_hold, floor_value/1000)

# Rename the columns
colnames(z_static) <- c("CPPI", "Bitcoin", "Mixed", "Floor_Value")

ggplot(z_static, aes(x = index(z_static))) +
  geom_line(aes(y = CPPI, color = "CPPI")) +
  geom_line(aes(y = Bitcoin, color = "Bitcoin")) +
  geom_line(aes(y = Mixed, color = "60-40")) +
  geom_line(aes(y = Floor_Value, color = "Floor Value")) +
  labs(title = "CPPI Strategy with Static Floor",
       x = "Time",
       y = "Cumulated Return",
       color = "Strategy") +
  theme_bw() 


# Annualized returns
cppi_aret_st <- (account_value[length(account_value)]/start_value)^(12/n_steps)-1
bh_aret_st <- as.numeric((buy_and_hold[length(buy_and_hold)])^(12/n_steps)-1)
bitcoin_aret_st <- as.numeric((risky_return[length(risky_return)])^(12/n_steps)-1)

# Volatility and annualized volatility
cppi_vol_st <- sd(risky_weight*btc_monthly_returns + safe_weight*risk_free_rate)
bh_vol_st <- sd(0.6*btc_monthly_returns + 0.4*risk_free_rate)
bitcoin_vol_st <- sd(btc_monthly_returns)

cppi_avol_st <- cppi_vol_st*sqrt(12)
bh_avol_st <- bh_vol_st*sqrt(12)
bitcoin_avol_st <- bitcoin_vol_st*sqrt(12)

# Sharpe Ratios
cppi_sr_st <- (cppi_aret_st-r)/cppi_avol_st
bh_sr_st <- (bh_aret_st-r)/bh_avol_st
bitcoin_sr_st <- (bitcoin_aret_st-r)/bitcoin_avol_st

# Drawdown
btc_dd <- min(Drawdowns(btc_monthly_returns))
bh_dd <- min(Drawdowns(0.6*btc_monthly_returns + 0.4*risk_free_rate))
cppi_dd_st <- min(Drawdowns(risky_weight*btc_monthly_returns + safe_weight*risk_free_rate))


# Dynamic Floor: step wise update of the floor
peak <- dynamic_floor <- numeric(n_steps)

peak[1] <- start_value
dynamic_floor <- floor*account_value

for (s in 2:n_steps) {
  account_value[s] = risky_allocation[s-1]*(1+btc_monthly_returns[s]) + safe_allocation[s-1]*(1+risk_free_rate[s-1])
  
  peak[s] <- max(start_value, cummax(account_value[1:s]))
  dynamic_floor[s] <- floor*peak[s]
  
  cushion[s] <- (account_value[s]-dynamic_floor[s])/account_value[s]
  risky_weight[s] <- multiplier*cushion[s]
  risky_weight[s] <- min(risky_weight[s], 1)
  risky_weight[s] <- max(0, risky_weight[s])
  safe_weight[s] <- 1-risky_weight[s]
  risky_allocation[s] <- account_value[s] * risky_weight[s]
  safe_allocation[s] <- account_value[s] * safe_weight[s]
}

z_dynamic <- cbind(account_value/start_value, risky_return, buy_and_hold, dynamic_floor/start_value)

# Rename the columns
colnames(z_dynamic) <- c("CPPI", "Bitcoin", "Mixed", "Floor_Value")

ggplot(z_dynamic, aes(x = index(z_dynamic))) +
  geom_line(aes(y = CPPI, color = "CPPI")) +
  geom_line(aes(y = Bitcoin, color = "Bitcoin")) +
  geom_line(aes(y = Mixed, color = "60-40")) +
  geom_line(aes(y = Floor_Value, color = "Dynamic Floor")) +
  labs(title = "CPPI Strategy with Dynamic Floor",
       x = "Time",
       y = "Cumulated Return",
       color = "Strategy") +
  theme_bw() 

# Build annualized returns and volatility and construct Sharpe ratios
# Annualized returns
cppi_aret_dyn <- (account_value[length(account_value)]/start_value)^(12/n_steps)-1
# Volatility and annualized volatility
cppi_vol_dyn <- sd(risky_weight*btc_monthly_returns + safe_weight*risk_free_rate)

cppi_avol_dyn <- cppi_vol_dyn*sqrt(12)

# Sharpe Ratios
cppi_sr_dyn <- (cppi_aret_dyn-r)/cppi_avol_dyn

# Drawdown 
cppi_dd_dyn <- min(Drawdowns(risky_weight*btc_monthly_returns + safe_weight*risk_free_rate))

summary <- c(bitcoin_aret_st, bitcoin_avol_st, bitcoin_sr_st, bh_aret_st, bh_avol_st, bh_sr_st, cppi_aret_st, cppi_avol_st, cppi_sr_st, cppi_aret_dyn, cppi_avol_dyn, cppi_sr_dyn)
summary <- matrix(summary, nrow=3,ncol=4,byrow=FALSE, dimnames=list(c("Average return","Annualised Vol","Sharpe Ratio"), c("Bitcoin", "Buy-and-hold", "CPPI Static Floor", "CPPI Dynamic Floor")))
round(summary, 2)

