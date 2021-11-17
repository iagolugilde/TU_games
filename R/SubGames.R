#' @title Subgames
#' @description Subgames of a TU game.
#' @param v Characteristic function in binary order.
#' @param S Number of the coalition of the subgame.
#' @return  The subgame of the coalition \eqn{S}.
#' @details Given a TU game \eqn{(N,v)}, for each coalition \eqn{S} in \eqn{N}, the characteristic function, \eqn{v_S}{vS}, of the subgame of the coalition
#' is given by \eqn{v_S(T)=v(T)}{vS(T)=v(T)} for each coalition \eqn{S} in \eqn{T}.
#' @examples
#' v=c(0,0,1,1,1,1,2)
#' S=3
#' SubGames(v,S)
#' @seealso \link{TotalBalancedGame}
#' @export
SubGames=function(v,S){
  if(S<=0)
    stop('S must be possitive',call.=F)
  Characteristicdata(v)
n=Characteristicdata(v)$n
nC=Characteristicdata(v)$nC
if(S>nC)
  stop('The coalition is not in the game',call.=F)
base=Basebin(n)
if(sum(base[S,])==1){
  stop('There are not subgames for individual coalitions',call.=F)
return()}
valor=cbind(base,v)
subjuego=NULL

  coalition=base[S,]
  for(jj in 1:(S-1)){
    if(sum(base[jj,][which(coalition==1)]==1)>0 & sum(base[jj,][which(coalition==0)]==1)==0){
      subjuego=rbind(subjuego,valor[jj,])
    }
  }
  subjuego=rbind(subjuego,valor[S,])
  vs=subjuego[,n+1]
return(vs)}
