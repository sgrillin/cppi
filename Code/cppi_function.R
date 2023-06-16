
cppi <- function(x, r=0, floor, multiplier, start_value, static_floor = TRUE, frequency = 1) {
  date <- index(x)
  n_steps <- length(date)
  if (frequency = 365){
    rf <- rep(r/365, length(x))
  } else if(frequency = 12) {
    rf <- rep(r/12, length(x))
  } else {
    rf <- rep(r, length(x))
  }
  rf <- rep(r/12, length(x))
  
  account_value <- cushion <- risky_weight <- safe_weight <- risky_allocation <- safe_allocation <- numeric(n_steps)
  account_value[1] <- start_value
  
  peak <- dynamic_floor <- numeric(n_steps)
  peak[1] <- start_value
  dynamic_floor <- floor*account_value
  floor_value <- floor * account_value
  
  
  cushion[1] <- (account_value[1] - floor_value[1]) / account_value[1]
  risky_weight[1] <- multiplier * cushion[1]
  safe_weight[1] <- 1 - risky_weight[1]
  
  risky_allocation[1] <- account_value[1] * risky_weight[1]
  safe_allocation[1] <- account_value[1] * safe_weight[1]
  
  risky_return <- cumprod(1 + x)
  
  for (s in 2:n_steps) {
    account_value[s] <- risky_allocation[s - 1] * (1 + x[s]) + safe_allocation[s - 1] * (1 + rf[s - 1])
    if  (static_floor == FALSE) {
      peak[s] <- max(start_value, cummax(account_value[1:s]))
      dynamic_floor[s] <- floor*peak[s]
      cushion[s] <- (account_value[s] - dynamic_floor[s]) / account_value[s]
    } else {
      floor_value[s] <- account_value[1] * floor
      cushion[s] <- (account_value[s] - floor_value[s]) / account_value[s]
    }
    risky_weight[s] <- multiplier * cushion[s]
    risky_weight[s] <- min(risky_weight[s], 1)
    risky_weight[s] <- max(0, risky_weight[s])
    safe_weight[s] <- 1 - risky_weight[s]
    risky_allocation[s] <- account_value[s] * risky_weight[s]
    safe_allocation[s] <- account_value[s] * safe_weight[s]
  }
  if(static_floor == FALSE){
    list(Date = date, Account_Value = account_value, Floor = dynamic_floor, risky_weight = risky_weight
         , safe_weight = safe_weight, risky_allocation = risky_allocation, safe_allocation = safe_allocation)
  } else {
    list(Date = date, Account_Value = account_value, Floor = floor, risky_weight = risky_weight
         , safe_weight = safe_weight, risky_allocation = risky_allocation, safe_allocation = safe_allocation)
  }
}
