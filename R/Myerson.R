#' @title Myerson value
#' @description This function returns the Myerson value of a TU game \eqn{(N,v)}.
#' @param v Characteristic function in binary order.
#' @param communication Players of N where there are direct communication. A matrix where each row is a communication between two players with 1 if the player is communicated and zero if not.
#' @return  Computes the Myerson value of a TU game given by vector \eqn{v} and the communications given by the matrix comunication.
#' @details Given a TU game \eqn{(N,v)} where some of the players cannot communicate with each other. The matrix comunication provides
#' us which players can communicate directly.
#' The game with restricted communication, \eqn{v^{A}}{vA} is defined by,
#'\eqn{v^{A}(S)=v(S)}{vA(S)=v(S)} if the players of S can communicate and zero otherwise.
#'
#' The Myerson value is the Shapley value of the game with restricted communication.
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' communication=rbind(c(1,1,0),c(1,0,1))
#' Myerson(v,communication)
#' @references Myerson, R. B. (1977). Graphs and cooperation in games. Mathematics of operations research, 2(3), 225-229.
#' @seealso \link{Shapley}
#' @export
Myerson=function(v,communication){
  Characteristicdata(v)
  n=Characteristicdata(v)$n
  if(sum(rowSums(communication)!=2)>0)
    stop('The communications must be introduced two by two',call.=F)
  if(sum(communication>1)>0 | sum(communication<0)>0)
    stop('The parameter communications must be a matrix with ones and zeros',call.=F)
  if(dim(communication)[2]!=n)
    stop('The dimension of the matrix and the game is not the same',call.=F)
  m=Characteristicdata(v)$nC
base=Basebin(n)
vA=rep(0,m)
com=communication
unions=2
while(unions<n){
for(ii in 1:(dim(communication)[1]-1)){
  for(jj in (ii+1):dim(communication)[1]){
    if(sum(com[jj,][com[ii,]==1])>0){
    com=rbind(com,colSums(rbind(communication[ii,],communication[jj,])))}
  }
}
  unions=unions+1
  com[com==2]=1
  communication=com
}
com=unique(communication)
for(i in 1:m){
  if(sum(base[i,])>1){
    for(j in 1:dim(com)[1]){
      if(sum(base[i,]==com[j,])==n){vA[i]=v[i]}
    }

  }else{vA[i]=v[i]}
}
M=Shapley(vA)
  return(M)}
