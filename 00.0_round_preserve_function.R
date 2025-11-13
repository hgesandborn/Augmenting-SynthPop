# =========================================================
# Title: Rounding Function
# Author: Paul Delamater
# Date: 2025-10-24
# Description: Defines a round function that preserves the overall sum
# =========================================================


round_preserve_sum <- function(x, digits = 0) {
  
  # Account for zeros right from the start
  if (all(x == 0)) {
    
    return(x)
    
  } else {
    
    # Raise to appropriate power, e.g., 0 equals whole numbers
    up <- 10 ^ digits
    
    # Multiply original values by exponent
    x <- x * up
    
    # Remove the remainder from each obs
    y <- floor(x)
    
    # Get the number required to preserve sum
    rem_sum <- round(sum(x) - sum(y))
    
    # Get remainders to use as probs
    rem <- x - y
    
    # Test all remainders equal zero
    if (all(rem == 0)) {
      
      return(x)
      
    } else {
      
      # Sample
      ind <- sample(1:length(x), rem_sum, prob = rem / sum(rem))
      
      # Add one to values in "floor"
      y[ind] <- y[ind] + 1
      
      # Divide by exponent
      y <- y / up
      
      # Return
      return(y)
      
    }
    
  }
  
}
