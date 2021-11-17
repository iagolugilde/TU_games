#' @title Polygon order
#' @description Orders the rows of a matrix to draw a polygon.
#' @param  C A \eqn{n\times 2}{n x 2} o \eqn{n\times 3}{n x 3} matrix whose rows are points that belong to a
#' same hyperplane.
#' @param P A \eqn{1\times n}{1 x n} matrix of integer numbers. By default P=c().
#' @return CO. Matrix obtained by ordering the rows of C.
#' @return PO. Is a vector that represents the faces to
#' where the rows of C belong.
#' @examples
#' C=matrix(c(0, 1, 2/3, -1, 3, -1/3,1, 0, 1, 7, 7, -17/3, 2, 2, -2/3),ncol=3,byrow=TRUE);
#' P=c(5,8,3,2,7)
#' Polygonorder(C,P)
#' @importFrom RSEIS xprod
#' @export
#'
Polygonorder=function(C,P=c()){

  tamanho=dim(C);
  Csin=unique(C);
  if (dim(Csin)[1]<tamanho[1]){
  print('C has repeated rows that have being deleted')
  C=Csin;
  tamanho=dim(C);
  P=c(1:tamanho[1]);
  }

  if (tamanho[2]<2 | tamanho[2]>3){
    print('Matrix C must have 2 or 3 columns')
    on.exit()
  }

  if(sum(P)==0){
  P=c(1:tamanho[1]);
  }else{
  if (length(P)<tamanho[1]){
  P=c(1:tamanho[1]);
  }else{
    P=round(abs(P[1:tamanho[1]]))
  }
}

  if(tamanho[2]==2){

    CO=C;PO=P;
    minimox=min(CO[,1]);
    II=which(CO[,1]==minimox);
    if (length(II)==1){
    reorden=II;
    }else{
    minimoy=min(CO[II,2]);
    JJ=which(CO[,2]==minimoy & CO[,1]==minimox)
    reorden=JJ;
    }
    minimolex=CO[reorden,]
    minimoP=PO[reorden];
    CO=CO[-reorden,]
    PO=PO[-reorden]
    CO=rbind(minimolex,CO);
    PO=c(minimoP,PO);

    direccion=c(CO[1,1],C[1,2]-1)-CO[1,];

    angulo=c();
    for (fila in 2:dim(CO)[1]){
    vector=CO[fila,]-CO[1,];
    angulo[fila-1]=acos((direccion%*%t(t(vector)))/(sum(direccion*direccion)^0.5*sum(vector*vector)^0.5));
}

orden=sort(angulo);
permuta=sort(angulo,index.return=T)$ix
CO=rbind(CO[1,],CO[permuta+1,]);
PO=c(PO[1],PO[permuta+1]);

  }else{
    CO=C;PO=P;tamanho=dim(CO);

    normal=xprod(CO[2,]-CO[1,],CO[3,]-CO[1,]);
    if(tamanho[1]>3){
    for (ii in 4:tamanho[1]){
    vector= CO[ii,]-CO[1,];
    prodescalar=normal%*%t(t(vector));
if (abs(prodescalar)>10^(-6)){
    print('Row vectors in C do not belong to the same plane')
}
    }
      }
    minimox=min(CO[,1])
    II=which(CO[,1]==minimox);
    if (length(II)==1){
    reorden=II;
    }else{
      minimoy=min(C[II,2])
      JJ=which(CO[,2]==minimoy & CO[,1]==minimox);
    if (length(JJ)==1){
    reorden=JJ;
    }else{
      minimoz=min(CO[JJ,3]);
      ZZ=which(CO[,2]==minimoy & CO[,1]==minimox & CO[,3]==minimoz)
      reorden=ZZ;
    }
    }
    minimolex=CO[reorden,]
    minimoP=PO[reorden];
    CO=CO[-reorden,]
    PO=PO[-reorden]
    CO=rbind(minimolex,CO);
    PO=c(minimoP,PO);
    direccion=CO[2,]-CO[1,];
    angulo=c();
    for (fila in 3:dim(CO)[1]){
    vector=CO[fila,]-CO[1,];
    perp=xprod(direccion,vector);
    if (perp%*%normal>0){
      angulo[fila-1]=acos((direccion%*%t(t(vector)))/(sum(direccion*direccion)^0.5*sum(vector*vector)^0.5));
    }else{
      angulo[fila-1]=-acos((direccion%*%t(t(vector)))/(sum(direccion*direccion)^0.5*sum(vector*vector)^0.5));
    }
    }
angulo[1]=0;
  orden=sort(angulo);
  permuta=sort(angulo,index.return=T)$ix
  CO=rbind(CO[1,],CO[permuta+1,]);
  PO=c(PO[1],PO[permuta+1]);
  }
  return(list(C=CO,P=PO))}
