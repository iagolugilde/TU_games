#' @title Marginal contribution vectors
#' @description This function returns the marginal contribution vectors of a TU game \eqn{(N,v)}.
#' @param v Characteristic function in binary order.
#' @return Matrix with the marginal contribution vectors.
#' @details Given a TU game \eqn{(N,v)} and let \eqn{\pi} be an order of the players of \eqn{N}, the distribution of the marginal contributions associated with the order \eqn{\pi} is defined as,
#' \eqn{m_i^{\pi}=v(Pre^{\pi}(i)\cap i)-v(Pre^{\pi}(i))}{the value of the characteristic function for the coalition of the players who precede the player i in the order \pi and the union of the player i minus the value of the characteristic function for the coalition of the players who precede the player i}
#' for all \eqn{i} in \eqn{N} \eqn{and}{} \eqn{Pre^{\pi}(i)=\{j:\pi(j)<\pi(i)\}}{}.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' Marginal(v)
#' @importFrom pracma perms
#' @seealso \link{Shapley}, \link{WeberSet}, \link{WeberVertices}
#' @export
Marginal=function(v){
  Characteristicdata(v)
  n=Characteristicdata(v)$n
f=factorial(n)
WP=matrix(0,f,n);
Perms=perms(c(1:n))
for (k in 1:f){
P=Perms[k,]
w=NULL
   for (ii in 1:n){
         pos=which(P==ii)
         if (pos==1){
           Si=Coalitionnumber(P[1:pos],n)
             w[ii]=v[Si]}else{
             Si=Coalitionnumber(P[1:pos],n)
             S=Coalitionnumber(P[1:(pos-1)],n)
             w[ii]=v[Si]-v[S]
		}
   }
WP[k,]=w}

return(WP)}
