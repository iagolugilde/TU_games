#' @title Dividends of Harsanyi
#' @description Computes the Harsanyi dividends of a TU game.
#' @param v Characteristic function in binary order.
#' @return It returns the Harsanyi dividends of game \eqn{v}.
#' @details Given a TU game \eqn{(N,v)}, its Harsanyi dividends are the coordinates of the game
#' in the base formed by the unanimity games. For a coalition \eqn{S} in \eqn{N},
#' \deqn{c_S=\sum_{S'\subset S}(-1)^{|S|-|S'|}v(S')}{cS=\sum(-1)^(|S|-|S'|)v(S') for all the coalitions S' in S}
#' A game is totally positive if all the Harsanyi dividends are non negative.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' HarsanyiDividends(v)
#' @references Hammer, P.J., Peled, U.N. and Sorensen, S. (1977) Pseudo-boolean function and game theory I. Core elements and Shapley value, Cahiers du Centre d'Etudes de Recherche Opérationnelle 19, 156-176.
#' @references Dehez, P. (2020). On Harsanyi dividends and asymmetric values. In game theoretic analisys (pp. 523-558).
#' @seealso \link{HarsanyiSet}, \link{HarsanyiSetInfo}, \link{Unanimity}
#' @export
HarsanyiDividends=function(v){
  Characteristicdata(v)
  n=Characteristicdata(v)$n
  nC=Characteristicdata(v)$nC
  v=as.matrix(v)
  M=Unanimity(n)
  I=diag(1,nrow=nC)
  HD=t(solve(M,I))%*%v
  HD=t(HD)

return(HD)}
