#' @title Utopia payoffs
#' @description The utopia payoffs of a TU game.
#' @param v Characteristic function of a TU game given by the binary order.
#' @return M. The utopia payoffs M of the players of a TU
#' game given by vector \eqn{v}.
#' @return m. The minimum rights vector of the TU game given by vector \eqn{v}.
#' @details Given a TU game \eqn{(N,v)}, the utopia payoff for the player \eqn{i} is given by
#' \eqn{M_i(N,v)=v(N)-v(N\backslash i)}{Mi(N,v)=v(N)-v(N\ i)}, i.e., it is the marginal contribution of player \eqn{i} in
#' the grand coalition.
#'
#' The minimum right for player \eqn{i} is the maximun between 0 and the rest if we assign to the other players his maximun in the core.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' UtopiaPayoffs(v)
#' @importFrom stats na.omit
#' @seealso \link{CoreCoverSet}, \link{Tau}
#' @export
UtopiaPayoffs=function(v){
  Characteristicdata(v)
  nC=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
 # v=Bin2lex(v)
  M=c()
  N=c()
  m=c()
  for (jj in 1:n){
    Ni=bitwXor(nC,2^(jj-1));
    M[jj]=v[nC]-v[Ni];
  }
  for (jj in 1:n){
    Amax=c();
    for (ii in 1:nC){
      if (2^(jj-1)==bitwAnd(ii,2^(jj-1))){
        Si=bitwXor(ii,2^(jj-1));
        PSi=Getplayers(Si)$S
        Amax=c(Amax, v[ii]-sum(M[PSi]));
      }
    }
  Amax=na.omit(Amax)
  m[jj]=max(Amax);
  }
#m[m<0]=0
return(list(M=M,m=m))}
