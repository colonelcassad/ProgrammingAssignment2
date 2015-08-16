## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     
     setmatrix <- function (y=matrix()){
       x <<- y
       inverse <<- NULL
     }
       
     getmatrix <- function()x
     
     setinverse <- function (inv=matrix()) inverse <<-inv
     
     getinverse <- function()inverse
     
     list(getmatrix=getmatrix,setmatrix=setmatrix,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverse <-x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data<-x$getmatrix()
  inverse <- solve(data,...)
  x$setinverse(inverse)
  inverse
}
