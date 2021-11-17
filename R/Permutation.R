#' @title Permutation
#' @description It generates all permutations of of n numbers.
#' @param n First n numbers permuted.
#' @param num Number of the permutation.
#' @return The permutation.
#' @examples
#' n=4
#' num=2
#' Permutation(n,num)
#' @export

Permutation=function(n,num){
if(n<=0)
  stop('n must be possitive',call.=F)
  if(num<=0)
    stop('num must be possitive',call.=F)
  mod=function(x,y){
    (abs(x/y)%%1)*y*sign(x)}
  num=mod(num,factorial(n))
  if (num==0){
    num=factorial(n);
  }

  num=num-1;

  N=rep(0,n);
  pos=floor(num/factorial(n-1))+1;
  N[pos]=1;
  resto=mod(num,factorial(n-1));
  resto=round(resto)
  for(i in c(seq(2,n),seq(n,2))){
    pos=floor(resto/factorial(n-i)+0.0000000001)+1
    resto=mod(resto,factorial(n-i))
    resto=round(resto)
    ind=which(N==0)
    N[ind[pos]]=i
  }
  return(N)}
