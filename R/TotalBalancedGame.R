#' @title Total Balanced Game
#' @description Checks if a TU game is Total Balanced.
#' @param v Characteristic function in binary order.
#' @return If the TU game given by vector v is total balanced
#' then it returns 1, otherwise it returns 0 and provides the coalition where total balanced breaks down.
#' @details Given a TU game \eqn{(N,v)} it is total balanced if all the subgames are balanced. A game is said to be balanced if the core is a not empty set. The core consists of all the stable imputations, that is,
#' \eqn{C(N,v)=\{x\in I(N,v) : x(S)\ge v(S)\; \forall S \in N\}}{C(N,v)={x in I(N,v) : x(S)\ge v(S), for all S in N}} where,
#'  \eqn{x(S)=\sum_{i\in S} x_i}{x(S)=\sum xi, i in S}.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' TotalBalancedGame(v)
#' @seealso \link{BalancedGame}, \link{SubGames}
#' @export

TotalBalancedGame=function(v){
  Characteristicdata(v)
  n=Characteristicdata(v)$n
  nC=Characteristicdata(v)$nC
  TB=1
  base=Basebin(n)
  valor=cbind(base,v)
  info=c()
  for(ii in 1:nC){
    if(sum(base[ii,])>1){
    A=SubGames(v,ii)
    CV=CoreVertices(A)
    if(dim(CV)[1]==0){
      TB=0
      info=c(info,paste("The subgame of the coalition S={",toString(Getplayers(ii)$S),"} is not balanced",sep=""))
      }
    }}
  if(TB==0){return(list(TB=TB,info=info))}else{
return(TB)}}
