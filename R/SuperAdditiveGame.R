#' @title Super Additive Game
#' @description Checks if a TU game is superadditive.
#' @param v Characteristic function in binary order.
#' @return If the TU game given by vector \eqn{v} is superadditive
#' then it returns 1, otherwise it returns 0 and provides an instance where superadditivity breaks down.
#' @details Given a TU game \eqn{(N,v)}, it is superadditive if \eqn{v(S \cup T)\ge v(S)+v(T)}{v(S v T)\ge v(S)+v(T)}, for all disjoint coalitions \eqn{S,T} in \eqn{N}.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' SuperAdditiveGame(v)
#' @seealso \link{AdditiveG}
#' @export
SuperAdditiveGame=function(v){
  Characteristicdata(v)
  nC=Characteristicdata(v)$nC
  info=c()
  SA=1;
  for (ii in 1:nC){
    for (jj in 1:nC){
      if (bitwAnd(ii,jj)==0){
        if (v[bitwOr(ii,jj)]<(v[ii]+v[jj])){
          SA=0;
          info=c(info,paste("Fails the coalition S={",toString(Getplayers(ii)$S),"} and the coalition T={",toString(Getplayers(jj)$S),"}",sep=""))
          }
      }
    }
  }
  if(SA==0){return(list(SA=SA,info=info))}else{
  return(SA=SA)}}
