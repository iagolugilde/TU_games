#' @title Get players
#' @description It provides which players are in the coalition in the position num given by the binary order.
#' @param num Number of the coalition in binary order.
#' @return S. The players who are in the coalition.
#' @return P. The position of the players who are in the coalition num given by binary order.
#' @return Schain. The players who are in the coalition as character.
#' @return Pchain. The position of the players who are in the coalition num given by the binary order as character.
#' @examples
#' num=5
#' Getplayers(num)
#' @seealso \link{Coalitionnumber}
#' @export
Getplayers=function(num){
  if(num<0)
    stop('num must be positive',call.=F)
  S=which(as.numeric(intToBits(num)[1:32])==1)
  P=2.^(S-1)
  t=length(S)
  Scadena=c()
  Pcadena=c()
  for (ii in 1:t){
    Scadena=cbind(Scadena,as.character(S[ii]))
  }
  for (ii in 1:t){
    Pcadena=cbind(Pcadena,as.character(P[ii]))
  }
return(list(S=S,P=P,Schain=Scadena,Pchain=Pcadena))}
