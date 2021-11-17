#' @title Binary base
#' @description It provides a matrix which is the basis of a TU game in the binary order.
#' @param n Number of players.
#' @return A matrix \eqn{n \times (2^n-1)}{n x 2^n-1}.
#' @details The basis of a TU game in the binary order is a matrix with \eqn{2^n-1} rows and \eqn{n} columns where
#' we order the coalitions of a TU game with the binary order and, in each row we
#' assign 1 if the player is in the coalition and 0 otherwise.
#' @examples
#' n=5
#' Basebin(n)
#' @seealso \link{Baselex}, \link{Bin2lex}, \link{Lex2bin}
#' @export
#'
Basebin=function(n){
  if(n<=0)
    stop('n must be positive',call.=F)
  coalitions=matrix(0,2^n-1,n)
  for (ii in 1:2^n-1){
    coalitions[ii,]=as.numeric(intToBits(ii)[1:n])
  }
return(coalitions)}
