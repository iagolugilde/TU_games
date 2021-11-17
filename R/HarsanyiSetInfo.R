#' @title Harsanyi set info
#' @description This function returns the mingame and maxgame of the TU game.
#' @param v Characteristic function in binary order.
#' @return Returns the Harsanyi mingame and maxgame of the game \eqn{(N,v)}.
#' @details Given a TU game, \eqn{(N,v)}, its Harsanyi set, \eqn{H}, also called the selectope, is the set of payoffs vectors
#' obtained by all possible distributions of the Harsanyi dividends of all
#' coalitions among its members. The Harsanyi set coincides with the core
#' of a convex game known as the Harsanyi mingame.  The mingame \eqn{(N,m)} and the maxgame \eqn{(N,M)} are defined, for each coalition \eqn{S} in \eqn{N}, by \deqn{m(S)=min\{x(S): x \in H\}}{m(S)=min{x(S): x in H}} \deqn{M(S)=max\{x(S): x \in H\}}{M(S)=max{x(S): x in H}} where,
#'  \eqn{x(S)=\sum_{i\in S} x_i}{x(S)=\sum xi, i in S}.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' HarsanyiSetInfo(v)
#' @references Hammer, P.J., Peled, U.N. and Sorensen, S. (1977) Pseudo-boolean function and game theory I. Core elements and Shapley value, Cahiers du Centre d'Etudes de Recherche Opérationnelle 19, 156-176.
#' @references Dehez, P. (2020). On Harsanyi dividends and asymmetric values. In game theoretic analisys (pp. 523-558).
#' @seealso \link{HarsanyiDividends}, \link{HarsanyiSet}
#' @export
HarsanyiSetInfo=function(v){
  Characteristicdata(v)
  n=Characteristicdata(v)$n
  M=Unanimity(n)

  nC=Characteristicdata(v)$nC
  HD=HarsanyiDividends(v)
  positivos=which(HD>0);
  if (length(positivos)>=1){
    Aplus=HD[positivos]%*%M[positivos,];
  }else{
    Aplus=rep(0,nC);
  }
  negativos=which(HD<0);
  if (length(negativos)>=1){
    Aminus=-HD[negativos]%*%M[negativos,];
  }else{
    Aminus=rep(0,nC);
  }

  B=Basebin(n)
  mingame=rep(0,nC)
  maxgame=rep(0,nC)
  for(ii in 1:(nC-1)){
    for(jj in 1:(nC-1)){
      if(sum((B[jj,]+B[ii,])==1)==n){
        mingame[ii]=v[ii]-(Aminus[nC]-Aminus[ii]-Aminus[jj]);
        maxgame[ii]=v[ii]+(Aplus[nC]-Aplus[ii]-Aplus[jj]);
      }
    }
  }
  mingame[nC]=v[nC]
  maxgame[nC]=v[nC]

  return(list(mingame=mingame,maxgame=maxgame))}
