#' @title Ponderated Shapley value
#' @description This function calculates the ponderated Shapley value of a game.
#' @param v Characteristic function in binary order.
#' @param w Vector of weights.
#' @return  Computes the poderated Shapley value of a TU game given by vector \eqn{v} and system of weigths \eqn{p=(w,\Omega)}.
#' @details Given a TU game \eqn{(N,v)} and an ordered system of weigths \eqn{p=(w,\Omega)} and let \eqn{S} in \eqn{N}, \eqn{(N,uS)} the unanimity game of the coalition \eqn{S}.
#' The ponderated Shapley value for the unanimity game for the player \eqn{i} is \eqn{Sh_{i}^{(w,\Omega)}(N,u_S)=\frac{w_i}{\sum_{j\in S\cap S_K}w_j}}{}  \eqn{if}{}  \eqn{i\in S\cap S_K}{}  \eqn{and}{} \eqn{zero}{} \eqn{otherwise}{} \eqn{with}{}  \eqn{k=max\{j:S\cap S_j\not=\emptyset\}}{} \eqn{and}{} \eqn{i\in N}{ the weight of the player i divided by the sum of the weights of the players who are in the coalition S and in the coalition Sk being k the maximal coalition with not empty intersection with S}.
#'
#' The ponderated Shapley value is the sum of all the ponderated Shapley values for the unanimity games and the Harsanyi Dividends of the game \eqn{(N,v)}.
#' \eqn{Sh_{i}^{(w,\Omega)}(N,v)=\sum_{S\in N}c_{S}Sh_{i}^{(w,\Omega)}(N,u_S)}{} \eqn{,beeing}{} \eqn{c_S, S\in N}{}  \eqn{the}{} \eqn{Harsanyi}{} \eqn{dividends.}{}
#' @examples
#' v=c(0,0,0,0,0,0,1,0,0,1,3,4,6,8,10)
#' w=c(0.5, 0.2 ,0.2 ,0.1)
#' ShapleyPond(v,w)
#' @seealso \link{Shapley}
#' @references Kalai, E., & Samet, D. (1987). On weighted Shapley values. International journal of game theory, 16(3), 205-222.
#' @export

ShapleyPond=function(v,w){
  Characteristicdata(v)
  m=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
  if(sum(w)!=1){
    stop('The weigths do not sum 1',call.=F)
  }
  if(length(w)!=n){
    stop('There must be a weigth for each player',call.=F)
  }
  if(sum(w<=0)>0){
    stop('The weigths must be possitive',call.=F)
  }
  P=matrix(0,factorial(n),n)
  for(ii in 1:factorial(n)){
  P[ii,] = Permutation(n,ii);}
  elementos = dim(P)[1];
  pagos = matrix(c(0),nrow=elementos,ncol=n)
  pond=rep(1,elementos);
  for (i in 1:elementos){
    orden=P[i,];
    p=w[orden];
    a=1;
    b=UtopiaPayoffs(v)$M;
    for (j in 1:n){
      b[orden[j]]=0;
      jug=max(v[m]-sum(b),0);
      pagos[i,orden[j]]=jug-sum(pagos[i,]);
      a=a*sum(p[1:j]);
    }
    pond[i]=prod(p)/a;
  }


  SP=colSums(pagos*pond);
  return(SP)}
