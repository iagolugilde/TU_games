#' @title Monotonic Game
#' @description Checks if a TU game is monotonic.
#' @param v Characteristic function in binary order.
#' @return If the TU game given by vector \eqn{v} is monotonic, then it returns 1,  otherwise it returns 0 and provides the players who don't satisfy the monotonic conditions.
#' @details Given a TU game \eqn{(N,v)} it is monotonic if \eqn{v(S) \le v(T)} for all coalitions \eqn{S, T} such that \eqn{S} is in \eqn{T}.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' MonotonicGame(v)
#' @seealso \link{ZeroMonotonicGame}
#' @export
MonotonicGame=function(v){
  Characteristicdata(v)
  nC=Characteristicdata(v)$nC
  info=c();
  Mon=1
  for (ii in 1:nC){
    for (jj in 1:nC){
      if (ii==bitwAnd(ii,jj)){
        if (v[ii]>v[jj]){
          info=c(info,paste("Fails the coalition S={",toString(Getplayers(ii)$S),"} and the coalition T={",toString(Getplayers(jj)$S),"}",sep=""))
         Mon=0;
        }
      }
    }
  }
  if(Mon==0){return(list(Mon=Mon,info=info))}else{
  return(Mon=Mon)}}
