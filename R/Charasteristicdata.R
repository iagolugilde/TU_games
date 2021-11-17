#' @title Characteristic data
#' @description Given a characteristic function of a TU game, it provides his number of players and the number of coalitions.
#' @param v A characteristic function of a TU game.
#' @return n. Number of players.
#' @return nC. Number of coalitions.
#' @examples
#' v=seq(1:31)
#' Characteristicdata(v)
#' @export
Characteristicdata=function(v){
  nC=length(v)
  n=log(nC+1)/log(2)

  if(n>floor(n)){
    stop('Wrong characteristic function',call.=F)
  }
return(list(n=n,nC=nC))}
