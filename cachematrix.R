## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library(MASS)
makeCacheMatrix<-function(x=matrix()){  #make a new matrix
  ma<-NULL
  set<-function(y){
    x<<-y
    ma<<-NULL
  }
  get<-function()x
  setma<-function(inverse)ma<<-inverse
  getma<-function(){
    inver<-ginv(x)
    inver%*%x    #make inverse matrix
  }
  list(set=set,get=get,
       setma=setma,
       getma=getma)
}

cacheSolve<-function(x,...){
  ma<-x$getma()
  if(!is.null(ma)){
    message("getting cached data!")
    return(ma)
  }
  data<-x$get()
  ma<-solve(data,...)
  x$setma(ma)
  ma
}
