#' @title Owen value
#' @description This function returns the owen value of a TU game \eqn{(N,v)}.
#' @param v Characteristic function in binary order.
#' @param partition Partition of N with a priori coalitions.  A matrix with the priori unions where each row represent a union with 1 if the player is in the union and 0 if not. By default, partition=diag(log(length(v)+1)/log(2)), that is, there aren't priori unions.
#' @return Matrix with the marginal contribution vectors.
#' @details Given a TU game \eqn{(N,v)} where  there are a priori unions between some coalitions.
#' Let \eqn{P=\{P_1,\ldots,P_m\}}{P={P1,...,Pm}} a system of a priori coalitions, we define the game quotient, \eqn{(M,v_P)}
#' by the game with players \eqn{M} and characteristic function,
#'
#' \deqn{v_P(S)=v(\cap_{k\in S} P_k)}{vP(S)=v(U Pk), k in S}
#' The quotient game has as many players as there are elements in the partition.
#'
#' The Owen value can be obtained as follows: first, we calculate the game quotient induced by the partitioning of the players; second, we obtain the Shapley value of the quotient game that makes a distribution between the priori coalitions; Finally, the value assigned to each coalition is distributed among its members, taking into account the possibilities that these players will become part of other coalitions.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' partition=rbind(c(1,1,0),c(0,0,1))
#' Owen(v,partition)
#' @references Owen, G. (1977). Values of games with a priori unions. In Mathematical economics and game theory (pp. 76-88). Springer, Berlin, Heidelberg.
#' @importFrom pracma perms
#' @seealso \link{Shapley}
#' @export
Owen=function(v,partition=diag(log(length(v)+1)/log(2))){
  Characteristicdata(v)
  n=Characteristicdata(v)$n
  if(sum(colSums(partition)==1)!=n)
    stop('There is a player who is repeated or does not appear in the partition',call.=F)
  if(sum(partition>1)>0 | sum(partition<0)>0)
    stop('The parameter partition must be a matrix with ones and zeros',call.=F)
  f=factorial(n)
  WP=matrix(0,f,n);
  A=partition[which(rowSums(partition)>1),]
  if(sum(rowSums(partition)>1)==0){
    O=Shapley(v)
  return(O)}
  if(length(A)==n){
  A=as.matrix(t(A))}
  Perms=perms(c(1:n))
  for (k in 1:f){
    P=Perms[k,]
    w=seq(1:n)
    for(ii in 1:dim(A)[1]){
      if(sum(w)!=0){
      B=A[ii,]
      l=NULL
      for(jj in 1:length(B[B>0])){
        l[jj]=which(P==which(B==1)[jj])
      }
      if(length(seq(min(l),max(l)))!=length(l)){w=rep(0,n)}else{
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
      }
      }
    }
    WP[k,]=w
    }



  WP=WP[rowSums(WP)!=0,]
  O=colSums(WP)/dim(WP)[1]
  return(O)}
