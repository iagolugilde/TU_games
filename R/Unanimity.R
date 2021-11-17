#' @title Unanimity games
#' @description The unanimity games of a TU game.
#' @param n Number of players.
#' @return A \eqn{(2^n-1)\times(2^n-1)}{(2^n-1)x(2^n-1)} matrix whose rows are the unanimity games of the coalitions ordered by binary order.
#' @details Let \eqn{(N,v)} a TU game, we define, for each coalition \eqn{T} in \eqn{N}, the unanimity game of the coalition game, \eqn{(N,u_T)}{(N,uT)},
#' as \eqn{u_T(S)=1}{uT(S)=1} if \eqn{T} in \eqn{S}, and \eqn{u_T(S)=0}{uT(S)=0} otherwise for all \eqn{S} in \eqn{N}.
#' @examples
#' n=4
#' Unanimity(n)
#' @seealso \link{HarsanyiDividends}
#' @export
Unanimity=function(n){
  if(n<=0)
    stop('n must be positive',call.=F)
  nC=2^n-1
  U=matrix(0,nC,nC)
  for (ii in 1:nC){
    for(jj in 1:nC){
      if(bitwAnd(ii,jj)==ii){
        U[ii,jj]=1
      }
    }
  }
return(U)}
