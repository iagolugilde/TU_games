#' @title Convex Game
#' @description Checks if a TU game is convex.
#' @param v Characteristic function in binary order.
#' @return If the TU game given by vector \eqn{v} is convex, then the function returns 1, otherwise it returns 0 and it provides the coalitions which don't satisfy the convex conditions.
#' @details A game \eqn{(N,v)} is convex if \eqn{v(S \cap T) + v(S \cup T) \ge v(S)+v(T)}{v(S v T) + v(S ^ T) \ge v(S)+v(T)}, for all
#' \eqn{S,T} in \eqn{N}.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' ConvexGame(v)
#' @export

ConvexGame=function(v){
  Characteristicdata(v)
  nC=Characteristicdata(v)$nC
  info=c()
  convex=1;

  for (ii in 1:nC){
    for (jj in 1:nC){
      if (bitwAnd(ii,jj)==0){
        if (v[bitwOr(ii,jj)]<(v[ii]+v[jj])){
          info=c(info,paste("Fails the coalition S={",toString(Getplayers(ii)$S),"} and the coalition T={",toString(Getplayers(jj)$S),"}",sep=""))
          convex=0
          }
        }else{
      if ((v[bitwOr(ii,jj)]+v[bitwAnd(ii,jj)])<(v[ii]+v[jj])){
        info=c(info,paste("Fails the coalition S={",toString(Getplayers(ii)$S),"} and the coalition T={",toString(Getplayers(jj)$S),"}",sep=""))
        convex=0;
        }
        }
    }
  }
  if(convex==0){return(list(convex=convex,info=info))}else{
  return(convex=convex)}}
