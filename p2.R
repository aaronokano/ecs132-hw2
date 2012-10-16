flips <- function( value ) {
  return( ifelse( value < 0.5,  sum(sample(c(0,1),2,TRUE,c(0.1,0.9))),
                    sum( sample( c( 0, 1 ), 2, TRUE, c( 0.9, 0.1 ) ) ) ) )
}

trickCoins <- function( nreps ) {
  choice <- runif( nreps )
  results <-  sapply( choice, flips )
  ex2 <- sum( results ) / nreps
  print( ex2 )
  varx2 <- (sum( results^2 ) / nreps) - ex2^2
  return( varx2 )
}

trickCoins( 10000 )
