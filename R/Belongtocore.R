#' @title belong to core.
#' @description Checks if a point belongs to the core of a TU game.
#' @param v Characteristic function in binary order.
#' @param x A point.
#' @return If a point x belongs to the core of the TU game defined by
#'  vector v,  the function returns 1, otherwise it returns 0 and it gives which coalition doesn't satisfies the conditions.
#' @details The core of a TU game \eqn{(N,v)} is the set of those imputations \eqn{x} in \eqn{R^n}, \eqn{x_1+...+x_n=v(N)}{x1+...+xn=v(N)},
#'  such that \eqn{x(S)\ge v(S)} for all coalition \eqn{S} in \eqn{N}, where,
#'  \eqn{x(S)=\sum_{i\in S} x_i}{x(S)=\sum xi, i in S}.
#'
#'  \deqn{C(N,v)=\{x\in I(N,v):x(S)\ge v(S)\; \forall S\in N\}}{C(N,v)={x in I(N,v):x(S)\ge v(S) for all S in N}}
#'
#'  A vector \eqn{x} belongs to the core if no coalition \eqn{S} has an
#'  incentive to split off if \eqn{x} is the proposed reward allocation for \eqn{N},
#'  because the total amount \eqn{x(S)} allocated to S is not smaller than the
#'  amount \eqn{v(S)} which they can obtain by forming their own coalition.
#' @examples
#' v=c(0,0,0,2,1,1,6)
#' x=c(0,0,1)
#' Belongtocore(v,x)
#' @references Edgeworth, F. Y. (1881). Mathematical psychics: An essay on the application of mathematics to the moral sciences (No. 10). CK Paul.
#' @references Gillies, D. B. (1953). Some theorems on n-person games. Princeton University.
#' @seealso \link{CoreSet}, \link{CoreVertices}
#' @export
Belongtocore=function(v,x){
  Characteristicdata(v)
  nC=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
  if(length(x)!=n)
  stop('The point does not have the same dimension as the core',call.=F)
  info=c()
  R=1

  for(ii in 1:(nC-1)){
    S=Getplayers(ii)$S
    if(sum(x[S])<v[ii]){

      R=0;info=c(info,paste("Coalition {",toString(Getplayers(ii)$S),"} fails",sep=""))
    }
  }
  if(sum(x)!=v[nC]){
    R=0;info=c(info,paste("Coalition {",toString(Getplayers(nC)$S),"} fails",sep=""))
  }
if(R==0){
  return(list(belong=R,info=info))
}else{
  return(belong=R)}}
