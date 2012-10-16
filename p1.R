pXk <- function( k ) {
  return( (1/385)*(k^2) )
}

power <- function( nreps ) {
  values <- sample( 1:10, size=nreps, replace=TRUE, sapply( 1:10, pXk )  )
  ex <- sum( values ) / nreps
  varx <- sum( values^2 ) / nreps - ex^2
  print( ex )
  print( varx )
}

power( 10000 )
