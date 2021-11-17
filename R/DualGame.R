#' @title Dual Game
#' @description It returns the characteristic function of the dual game of a TU game.
#' @param v Characteristic function in binary order.
#' @return The characteristic function of the dual game of \eqn{(N,v)}.
#' @details Given a TU game \eqn{(N,v)}, its dual game, \eqn{(N,v^c)}{(N,vc)}, is defined, for all \eqn{S} in \eqn{N} by, \deqn{v^c(S)=v(N)-v(N\backslash S)}{vc(S)=v(N)-v(N\S)}
#' @examples
#' v=seq(1:31)
#' DualGame(v)
#' @export


DualGame=function(v){
  Characteristicdata(v)
  A=c()
  nC=Characteristicdata(v)$nC
  AN=v[nC]
  for(ii in 1:(nC-1)){
    A[ii]=AN-v[nC-ii]
  }
  A[nC]=AN
return(A)}
