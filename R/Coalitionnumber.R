#' @title Coalition number
#' @description It gives the position of a coalition in a TU game given by the binary order.
#' @param S Coalition.
#' @param n Number of players.
#' @return Position of the coalition \eqn{S} in the binary order.
#' @examples
#' S=c(1,2,3)
#' n=3
#' Coalitionnumber(S,n)
#' @seealso \link{Getplayers}
#' @export

Coalitionnumber=function(S,n){
  A=Basebin(n)
  if(sum(S>n)>=1)
    stop('At least one of the players in the coalition is not in the game',call.=F)
  if(sum(S<=0)>0)
    stop('The players of the coalition S must be positive',call.=F)
p=rep(0,n)
for(i in 1:n){
p[i]=any(S==i)
}
position=NULL
for(i in 1:(2^n-1)){
if(sum(A[i,]==p)==n){position=i}
}
return(position)}
