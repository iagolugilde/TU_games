#' @title Non negative Game
#' @description Checks if a TU game is non negative.
#' @param v Characteristic function in binary order.
#' @return If the TU game given by vector \eqn{v} is non negative, then it returns 1,  otherwise it returns 0 and provides the coalition \eqn{S} such that \eqn{v(S)<0}.
#' @details Given a TU game \eqn{(N,v)} it is non negative if \eqn{v(S) \le 0} for any coalition \eqn{S} in \eqn{N}.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' NonnegativeGame(v)
#' @export

NonnegativeGame=function(v){
  Characteristicdata(v)
  N=1
  if(sum(v<0)>0){N=0
  negative=which(v<0)
  col=c()
  for(i in 1:length(negative)){
  col=c(col,paste("v({",toString(Getplayers(negative[i])$S),"})<0",sep=""))}
  return(list(N=N,coalition=col))}
return(N)}
