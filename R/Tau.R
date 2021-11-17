#' @title \eqn{\tau}-value
#' @description The \eqn{\tau}-value of a TU game.
#' @param v Characteristic function in binary order.
#' @param name A logical value.
#' @return Computes the \eqn{\tau}-value of a TU game given by
#' vector \eqn{v}.
#' @details Given a TU game \eqn{(N,v)}, the \eqn{\tau}-value is defined as the efficient allocation \eqn{\tau}, such that
#' \eqn{\tau}=m+\eqn{\alpha} (M-m), for some \eqn{\alpha} such that \eqn{\sum_{i\in N}\tau_i=v(N)}{\sum \tau i=v(N), i in N}, where \eqn{M} and \eqn{m} are the utopia payoffs and the minimum
#' rights vectors of \eqn{(N,v)} respectively. The \eqn{\tau}-value can only be computed if the game is of admissible compromise.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' Tau(v)
#' @references Tijs, S. H. (1981). Nash equilibria for noncooperative n-person games in normal form. Siam Review, 23(2), 225-237.
#' @seealso \link{PlotSolution}, \link{UtopiaPayoffs}
#' @export
Tau=function(v,name=FALSE){
  if(name==TRUE){
    solution="Tau value"
    return(solution)
  }
  Characteristicdata(v)
  nC=Characteristicdata(v)$nC
  M=UtopiaPayoffs(v)$M
  m=UtopiaPayoffs(v)$m
  AD=AdmissibleGame(v)
  if(AD==0){
    stop('The core-cover is empty',call.=F)
  return(tau=tau)}
  if(abs(sum(M)-sum(m))<10^-9){
    tau=m
  }else{
  alfa=(v[nC]-sum(m))/(sum(M)-sum(m));
  tau=m+alfa*(M-m);}
return(tau)}
