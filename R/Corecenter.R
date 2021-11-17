#' @title Core-center
#' @description The core-center of a TU balanced game.
#' @param v Characteristic function in binary order.
#' @param name A logical value.
#' @return It returns the core-center of a TU game given by
#' vector \eqn{v}.
#' @details The core of a TU game \eqn{(N,v)} is the set of those imputations \eqn{x} in \eqn{R^n}, \eqn{x_1+...+x_n=v(N)}{x1+...+xn=v(N)},
#'  such that \eqn{x(S)\ge v(S)} for all coalition \eqn{S} in \eqn{N}, where,
#'  \eqn{x(S)=\sum_{i\in S} x_i}{x(S)=\sum xi, i in S}.
#'
#'  \deqn{C(N,v)=\{x\in I(N,v):x(S)\ge v(S)\; \forall S\in N\}}{C(N,v)={x in I(N,v):x(S)\ge v(S) for all S in N}}
#'
#' The core-center (CC) is defined as the centroid (center of gravity) of
#' the core, so it can only be computed if the game is balanced. It is the expectation of the uniform distribution over the
#' core of the game. Let \eqn{\mu} be the
#' \eqn{(n-1)}-dimensional Lebesgue measure and denote \eqn{V(C)=\mu(C(N,v))} the volume (measure) of the core. If \eqn{V(C)>0} then for each \eqn{i} in \eqn{N},
#' \deqn{CC_i(N,v)=\frac{1}{V(C)}\int_{C(N,v)}x_i d\mu}{1/V(C)*[S xd\mu], where S is the integral and it is taken over C(v).}
#' @importFrom geometry delaunayn
#' @examples
#' v=c(0,0,0,1,1,1,2)
#' Corecenter(v)
#' @references Gonzalez-Díaz, J., & Sánchez-Rodríguez, E. (2007). A natural selection from the core of a TU game: the core-center. International Journal of Game Theory, 36(1), 27-46.
#' @seealso \link{BalancedGame}, \link{CoreSet}, \link{CoreVertices}, \link{PlotSolution}
#' @export
Corecenter=function(v,name=FALSE){
  if(name==TRUE){
    solution="Core center"
    return(solution)
  }
  Characteristicdata(v)
  m=Characteristicdata(v)$nC
  n=Characteristicdata(v)$n
  additive=v-NormalicedGame(v)$norm0#Si existe parte aditiva la sumamos al final
  v=NormalicedGame(v)$norm0#Calculamos el core-center del juego sin la parte aditiva
if(BalancedGame(v)==0)
  stop('The core is empty',call.=F)

if(n==2){
  m=c(max(0,v[3]-v[2]),max(0,v[3]-v[1]))
  V=matrix(c(m[1],v[3]-m[1],v[3]-m[2],m[2]),ncol=2,byrow=T)
  CC=colSums(V)/2
  CC=CC+additive[1:2]
return(CC)}
  CC=rep(0,n)
  V=CoreVertices(v)
  if(dim(V)[1]==1){
   CC=as.vector(V)
  }else if(dim(V)[1]==2){
    CC=colSums(V)/2
  }else{
  A=V[,colSums(V)!=0]#Me quedo con las columnas distintas de cero.
  if(sum(colSums(V)==0)>0){#Marco las columnas que valen cero.
  if(which(colSums(V)==0)==n){#Si es la última pierdo una dimensión, ya que hago la proyección sobre n-1.
    CC[n]=0
    n1=n-1
  }else{
  CC[colSums(V)==0]="x"
  n1=n}}else{
    n1=n
  }
  n2=n-sum(colSums(V)==0)
  A=A[,1:(dim(A)[2]-1)]
  A=unique(A)
  P=delaunayn(A,output.options=T)
  areas=P$areas
  vol=sum(areas)*factorial(n2-1)
  tri=P$tri
  r=rep(0,dim(A)[2])
  for(i in 1:length(areas)){
    triangle=A[tri[i,],]
    r=r+colSums(triangle)*areas[i]/n2#Para juegos de asignación, dejamos la mitad de las columnas y n=columnas+1
  }
  r=r/(sum(areas))
  j=0
  for(i in 1:(n1-1)){#Recupero las columnas que quité al principio.
    if(CC[i]=="x"){CC[i]=0
    j=j+1}else{
      CC[i]=r[i-j]
    }
  }
  CC=as.numeric(CC)
  CC[n1]=v[m]-sum(CC)}
if(sum(additive)!=0){
  CC=CC+additive[rowSums(Basebin(n))==1]
}
  return(CC)}
