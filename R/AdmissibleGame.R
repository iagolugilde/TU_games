#' @title Admissible Game
#' @description Checks if a TU game is compromise admissible.
#' @param v Characteristic function in binary order.
#' @return If the TU game given by vector v is  compromise admissible then it returns 1, otherwise it returns 0 and the condition that no satisfies.
#' @details Let \eqn{(N,v)} a TU game, \eqn{m(N,v)} is the vector of minimal rights and \eqn{M(N,v)} is the vector of utopia payoffs. The game \eqn{(N,v)} is said to be compromise admissible if the core-cover is a not empty set, that is, if it satisfies the conditions:
#'
#' 1) \eqn{m(N,v)\leq M(N,v)}{The minimal rights are smaller than the utopia payoffs}.
#'
#' 2) \eqn{\sum_{i}m_{i}(N,v)\leq \sum_{i}M_i(N,v)}{The sum of the minimal rights is smaller than the sum of the utopia payoffs}.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' AdmissibleGame(v)
#' @seealso \link{CoreSet}, \link{CoreVertices}, \link{UtopiaPayoffs}
#' @export
AdmissibleGame=function(v){
  Characteristicdata(v)
  nC=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
    Ad=1;
    M=round(UtopiaPayoffs(v)[[1]],2)
    m=round(UtopiaPayoffs(v)[[2]],2)
    B=c(m>M , sum(m)>v[nC] , v[nC]>sum(M));
    if(sum(B)!=0){
      Ad=0;
      fallo=as.numeric(B)
      Info=c()
      for(ii in 1:n){
        if(fallo[ii]==1){
          Info=c(Info,paste('At least a minimal right is greater than a uttopia payoff'))}
      }
      if(fallo[n+1]==1){
        Info=c(Info,paste('The sum of minimal rights is greater than v(N)'))}
      if(fallo[n+2]==1){
        Info=c(Info,paste('v(N) is greater than the sum of uttopia payoffs'))
      }
      return(list(Ad=Ad,Info=Info))}
  return(Ad=Ad)}
