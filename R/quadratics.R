makequadstring <- function(coeffs)
{

  A=coeffs[1]
  B=coeffs[2]
  C=coeffs[3]

  s=""
  if(A==-1) s = paste0(s, "-")
  if(abs(A) != 1) s = paste0(A)
  s = paste0(s,"x\u00B2 ")
  if (B<0) s = paste0(s, "- ") else if (B>0) s = paste0(s, "+ ")
  if(B != 0) {if(abs(B)!=1) s = paste0(s, abs(B), "x") else s = paste0(s, "x ")}
  if (C<0) s = paste0(s, "- ") else s = paste0(s, "+ ")
  s = paste0(s, abs(C))

  s
}

gcd <- function(x, y){while(y) {t = y;y = x %% y;x = t};return(x)}


#' getfactorisingproblem()
#'
#' @name getfactorisingproblem
#' @author Chris Casey
#' @description Get Problem to Factorise
#' @return List containing Coefficients, String Representation and Solution
#' @examples
#' x = getfactorisingproblem()
#' cat(x[[2]])
#' @export
#'
getfactorisingproblem <- function()
{

  repeat
  {
    x=c(sample(10,1),sample(10,1),sample(10,1),sample(10,1))
    A=x[1]*x[3]
    B=x[1]*x[4]+x[2]*x[3]
    C=x[2]*x[4]

    if (prod(x)<5000 & x[1] <=5 & x[3]<=5 ) break
  }

  nneg=sample(c(0,0,0,0,0,1,1,1,1,1,2,2,3),1)
  if(nneg>0)
  {
    ii = sample(4,nneg)
    x[ii]=-x[ii]
  }

  a=x[1];b=x[2];c=x[3];d=x[4]

  gcd1=gcd(abs(a),abs(b));a=a/gcd1;b=b/gcd1
  gcd2=gcd(abs(c),abs(d));c=c/gcd2;d=d/gcd2

  if(a<0 & b<0){ a = -a;b = -b}
  if(c<0 & d<0){ c = -c;d = -d}

  A=a*c
  B=a*d+b*c
  C=b*d

  s=makequadstring(c(A,B,C))

  sa1="("
  if(a==-1)sa1 = paste0(sa1, "-")
  if(abs(a) != 1) sa1 = paste0(sa1,a)
  sa1 = paste0(sa1,"x ")
  if (b<0){ sa1 = paste0(sa1, "- ",abs(b),")")} else if (b>0) {sa1 = paste0(sa1, "+ ",abs(b),")")}

  sa2=paste0("(")
  if(c==-1)sa2 = paste0(sa2, "-")
  if(abs(c) != 1) sa2 = paste0(sa2,c)
  sa2 = paste0(sa2,"x ")
  if (d<0){ sa2 = paste0(sa2, "- ",abs(d),")")} else if (d>0) {sa2 = paste0(sa2, "+ ",abs(d),")")}

  if(sa1!=sa2) sa = paste0(sa1,sa2) else sa = paste0(sa1,"\u00B2")

  ret = list()
  ret[[1]]=c(A,B,C)
  ret[[2]]=s
  ret[[3]]=sa

  ret
}

#' solvefactorisingproblem(coeffs)
#'
#' @name solvefactorisingproblem
#' @author Chris Casey
#' @description Solve Factorising Problem
#' @param coeffs vector of 3 Coefficients
#' @return List containing  String Representation of Solution
#' @examples
#' x = solvefactorisingproblem(c(1,2,1))
#' cat(x[[1]])
#' @export
#'
solvefactorisingproblem <- function(coeffs)
{
  linestr = "________________________________________"
  A=coeffs[1]
  B=coeffs[2]
  C=coeffs[3]
  mm="    "
  #ret=paste0("\n",makequadstring(coeffs),"\n")
  ret=paste0("\n")
  ret=paste0(ret,mm,"Solving...","\n")
  ret=paste0(ret,mm,"a \u00D7 c = ",A*C,"\n")
  ret=paste0(ret,mm,"Need factors of ",A*C," that sum to b (",B,")\n")

  f1=NA
  f2=NA
  if(A*C>0)
  {
    for(i in 1:floor(sqrt(A*C)))
    {
      if((A*C) %% i == 0){ret=paste0(ret,mm,sprintf("%4d \u00D7 %-4d  Sum = %-4d",sign(B)*i,sign(B)*A*C/i,sign(B)*(i+A*C/i)))
      if((i+A*C/i) == abs(B)){ret=paste0(ret," <<<<<\n");f1=sign(B)*i;f2=sign(B)*A*C/i} else ret=paste0(ret,"\n")}
    }
  }else
  {
    for(i in 1:abs(A*C))
    {
      if(abs(A*C) %% i == 0){ret=paste0(ret,mm,sprintf("%4d \u00D7 %-4d Sum = %-4d",i,A*C/i,i+A*C/i))
      if(i+A*C/i == B){ret=paste0(ret," <<<<<\n");f1=i;f2=A*C/i}else ret=paste0(ret,"\n")}
    }
  }

  if(!is.na(f1) & !is.na(f2))
  {
    Z1=abs(gcd(f1,A))*sign(A)
    Z2=abs(gcd(f2,C))*sign(f2)
    X1=Z1
    X2=Z2
    X3=f2/Z2
    X4=C/Z2

    z=""
    if(A==-1)z = paste0(z, "-")
    if(abs(A) != 1) z = paste0(A)
    z = paste0(z,"x\u00B2 ")
    if (f1<0){ z = paste0(z, "- ")} else if (f1>0) {z = paste0(z, "+ ")}
    if(f1 != 0)
    {if(abs(f1)!=1) z = paste0(z, abs(f1), "x ") else z = paste0(z, "x ") }
    if (f2<0){ z = paste0(z, "- ")} else if (f2>0) {z = paste0(z, "+ ")}
    if(f2 != 0)
    {if(abs(f2)!=1) z = paste0(z, abs(f2), "x ") else z = paste0(z, "x ") }
    if (C<0){ z = paste0(z, "- ")} else {z = paste0(z, "+ ")}
    z = paste0(z, abs(C))

    ret=paste0(ret,mm,"Split x term using the factors ",f1," and ",f2,"\n")
    ret=paste0(ret,mm,z,"\n")
    ret=paste0(ret,mm,"Factorise (I)\n")

    q1="("
    if(X3==-1)q1 = paste0(q1, "-")
    if(abs(X3) != 1) q1 = paste0(q1,X3)
    q1 = paste0(q1,"x ")
    if(X4<0)q1 = paste0(q1, "- ")else q1=paste0(q1, "+ ")
    q1 = paste0(q1,abs(X4),")")

    q=""
    if(X1<0)q="-"
    if(abs(X1)!=1) q=paste0(q,abs(X1))
    q=paste0(q,"x")
    q=paste0(q,q1)
    if(X2<0)q=paste0(q," - ") else q=paste0(q," + ")
    if(abs(X2)!=1) q=paste0(q,abs(X2))
    q=paste0(q,q1)
    ret=paste0(ret,mm,q,"\n")
    ret=paste0(ret,mm,"Factorise (II)\n")
    v="("
    if(X1<0)v=paste0(v,"-")
    if(abs(X1)!=1) v=paste0(v,abs(X1))
    v=paste0(v,"x")
    if(X2<0)v=paste0(v," - ") else v=paste0(v," + ")
    v=paste0(v,abs(X2),")")
    if(q1!=v) v=paste0(v,q1) else  v=paste0(v,"\u00B2")
    ret=paste0(ret,mm,v,"\n")
    ret=paste0(ret,mm,linestr,"\n")
  }else
  {
    ret=paste0(mm,ret,"No Integer Solution\n")
    #ret=paste0(mm,"No Integer Solution\n")
    v=NA
  }

  allret=list()
  allret[[1]]=ret
  allret[[2]]=v

  allret
}

#' getproblems(n)
#'
#' @name getproblems
#' @author Chris Casey
#' @description Get list of factorising problems
#' @param n Number of problems
#' @return None - printed to screen
#' @examples
#' getproblems(10)
#'
#' @export
#'
getproblems <- function(n)
{
for(i in 1:n)
  {
    p = getfactorisingproblem()
    s=solvefactorisingproblem(p[[1]])
    cat(sprintf("%02d. %s\n%s",i,p[[2]],s[[1]]))
  }
}
