#' @title Average marginal contribution vectors
#' @description This function returns the average marginal contribution vectors of a TU game \eqn{(N,v)}.
#' @param v Characteristic function in binary order.
#' @return Matrix with the average marginal contribution vectors.
#' @details Given a TU game \eqn{(N,v)}, we define the average marginal contribution vectors for each coalition \eqn{S} in \eqn{N} by,
#' \deqn{A(S)=\sum_{k\in S}\frac{1}{|S|}(v(S)-v(S\backslash \{k\}))}{A(S)=\sum(v(S)-v(S\ {k})),k in S}
#'
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' AverageMarginal(v)
#' @references Nowark,AS. & Radzik,T. (1994). A Solidarity Value for n-Person Transferable Utility Games. International Journal of Game Theory, vol. 23; 43-48.
#' @seealso \link{Marginal}, \link{Solidarity}
#' @export
AverageMarginal=function(v){
  Characteristicdata(v)
  n=Characteristicdata(v)$n
  m=Characteristicdata(v)$nC
  base=Basebin(n)
  AM=rep(0,m)
  for(i in 1:m){
    for(j in which(rowSums(base)==1)){
      if(sum(base[j,][which(base[i,]==0)])==0){
        if(i==j){AM[i]=AM[i]+(v[i])}else{
        k=i-j
        AM[i]=AM[i]+(v[i]-v[k])}
      }
    }
  }
AM=AM/rowSums(base)
  return(AM)}
