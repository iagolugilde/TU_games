#' @title Shapley value
#' @description The Shapley value of a TU game.
#' @param v Characteristic function in binary order.
#' @param name A logical value.
#' @return  Computes the Shapley value of a TU game given by vector \eqn{v}.
#' @details Given a TU game \eqn{(N,v)} and let \eqn{\pi} an order of the players of \eqn{N}, the distribution of the marginal contributions associated with the order \eqn{\pi} is defined as,
#' \eqn{m_i^{\pi}=v(Pre^{\pi}(i)\cap i)-v(Pre^{\pi}(i))}{the value of the characteristic function for the coalition of the players who precede the player i in the order \pi and the union of the player i minus the value of the characteristic function for the coalition of the players who precede the player i}
#' for all \eqn{i} in \eqn{N} \eqn{Pre^{\pi}(i)=\{j:\pi(j)<\pi(i)\}}{}.
#'
#' The Shapley value is calculated by averaging the vectors of marginal contributions associated with all possible arrivals of the players.
#' @importFrom pracma mod
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' Shapley(v)
#' @references Shapley, L. S. (2016). 17. A value for n-person games (pp. 307-318). Princeton University Press.
#' @seealso \link{Marginal}, \link{PlotSolution}
#' @export

Shapley=function(v,name=FALSE){
  if(name==TRUE){
    solution="Shapley"
    return(solution)
  }
  Characteristicdata(v)
  m=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
  if(n==2){
    m=c(max(0,v[3]-v[2]),max(0,v[3]-v[1]))
    V=matrix(c(m[1],v[3]-m[1],v[3]-m[2],m[2]),ncol=2,byrow=T)
    Sh=colSums(V)/2
    return(Sh)}
metade = ceiling(n/2);
valores = 1/n*rep(1,metade);

for (ii in 2:metade){
valores[ii]=valores[ii-1]*(ii-1)/(n-ii+1);
}
#valores
if (mod(n,2)==0){
vshap=c(valores,sort(valores));
}else{
  vshap=c(valores,sort(valores[1:(length(valores)-1)]));
}
#vshap
#################################
Sh= rep(0,n);
for (ii in 1:(2^(n-1)-1)){
  coalicions=as.numeric(intToBits(ii)[1:n])
  a = sum(coalicions);
  coalicions = coalicions*vshap[a];
  coalicions[coalicions==0]=-vshap[a+1];
  Sh = Sh+(v[ii]-v[2^n-1-ii])*coalicions;
}
Sh=Sh+v[m]/n;
return(Sh)}
