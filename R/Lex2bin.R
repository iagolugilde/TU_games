#' @title Change from lexicographical to binary
#' @description It changes the characteristic function of a TU game with lexicographic order to the binary order.
#' @param vold Characteristic function in lexicographic order.
#' @return The characteristic function in binary order.
#' @examples
#' vold=seq(1:31)
#' Lex2bin(vold)
#' @seealso \link{Basebin}, \link{Baselex}, \link{Lex2bin}
#' @export
Lex2bin=function(vold){
  Characteristicdata(vold)
  vnew=vold
  m=length(vold)
  n=Characteristicdata(vold)$n;
  bin=Basebin(n)
  suma4fila=rowSums(bin)
  bin=cbind(suma4fila,bin)
  lex=Baselex(n)
  suma4fila=rowSums(lex)
  lex=cbind(suma4fila,lex)
  for (players in 1:(n-1)){
    B = which(bin[,1]==players); L=which(lex[,1]==players);
    for(ii in 1:length(B)){
      for(iii in 1:length(L)){
        if(sum(bitwAnd(bin[B[ii],2:(n+1)],lex[L[iii],2:(n+1)]))==players){
          vnew[B[ii]]=vold[L[iii]]
        }

      }
    }

  }
  return(vnew)}
