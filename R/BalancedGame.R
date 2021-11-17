#' @title Balanced Game
#' @description Checks if a TU game is balanced.
#' @param v Characteristic function in binary order.
#' @return If the TU game given by vector v is balanced
#' then it returns 1, otherwise it returns 0.
#' @details Given a TU game \eqn{(N,v)}, it is said to be balanced if the core is a nonempty set. The core consists of all the stable imputations, that is,
#' \eqn{C(N,v)=\{x\in I(N,v) : x(S)\ge v(S)\; \forall S \in N\}}{C(N,v)={x in I(N,v) : x(S)\ge v(S), for all S in N}} where,
#'  \eqn{x(S)=\sum_{i\in S} x_i}{x(S)=\sum xi, i in S}.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' BalancedGame(v)
#' @seealso \link{CoreSet}, \link{CoreVertices}, \link{TotalBalancedGame}
#' @export

BalancedGame=function(v){
  Characteristicdata(v)
B=1
      CV=CoreVertices(v)
      if(dim(CV)[1]==0){
        B=0

      }
      return(B=B)}
