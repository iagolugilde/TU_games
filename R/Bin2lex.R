#' @title Change from binary to lexicographical
#' @description It changes the characteristic function of a TU game with binary order to the lexicographic order.
#' @param vold Characteristic function in binary order.
#' @return The characteristic function in lexicographic order.
#' @examples
#' vold=seq(1:31)
#' Bin2lex(vold)
#' @seealso \link{Basebin}, \link{Baselex}, \link{Lex2bin}
#' @export

Bin2lex=function(vold){
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
          vnew[L[iii]]=vold[B[ii]]
        }

      }
    }

  }
return(vnew)}
