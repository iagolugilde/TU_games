#' @title Lexicographic base
#' @description It provides a matrix which is the basis of a TU game in the lexicographic order.
#' @param n Number of players.
#' @return A matrix \eqn{n \times (2^n-1)}{n x 2^n-1}.
#' @details The basis of a TU game in the lexicographic order is a matrix with \eqn{2^n-1} rows and \eqn{n} columns where
#' we order the coalitions of a TU game with the lexicographic order and, in each row we
#' assign 1 if the player is in the coalition and 0 otherwise.
#' @examples
#' n=5
#' Baselex(n)
#' @importFrom pracma sortrows
#' @seealso \link{Basebin}, \link{Bin2lex}, \link{Lex2bin}
#' @export
Baselex=function(n){
  if(n<=0)
    stop('n must be positive',call.=F)
  c=Basebin(n)
  suma4fila=rowSums(c)
  sumas=cbind(suma4fila,c)
  coalitions=c()
  for(i in 1:(n-1)){
    t=which(sumas[,1]==i)
    m=sumas[t,]
    ms=sortrows(1-m[,2:(n+1)])
    coalitions=rbind(coalitions,1-ms)
  }
  coalitions=rbind(coalitions,c[2^n-1,])
  coalitions=matrix(coalitions,ncol=n)
return(coalitions)}
