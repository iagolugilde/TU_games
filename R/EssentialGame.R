#' @title Essential Game
#' @description Checks if a TU game is essential.
#' @param v Characteristic function in binary order.
#' @return Essential. If the game \eqn{(N,v)} is essential, Essential is 1, otherwise Essential is 0.
#' @return Degenerate. If the game \eqn{(N,v)} is degenerate, Degenerate is 1, otherwise Degenerate is 0.
#' @details  A game \eqn{(N,v)} is essential if the imputation set is non-empty, or equivalently
#' if \eqn{v(N)\ge v(1)+\dots+v(n)}. When \eqn{v(N)=v(1)+\dots+v(n)} the game is called
#' degenerate or inessential.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' EssentialGame(v)
#' @export
EssentialGame=function(v){
  Characteristicdata(v)
  nC=Characteristicdata(v)$nC
  P=Getplayers(nC)$P
  vi=sum(v[P])
  if(vi>v[nC]){
    E=0
  }else{
    E=1
  }
  if(vi==v[nC]){
    D=1
  }else{
    D=0
  }
return(list(Essential=E,Degenerate=D))}
