## This is a pair of functions that cache the inverse of a matrix
## 

## This function creates a special "matrix" object
#that can cache its inverse.

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


## This function computes the inverse of the special
#"matrix" returned by `makeCacheMatrix` above. If the inverse has
#already been calculated (and the matrix has not changed), then
#`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      inverse <- x$getinverse()
      
       if(!is.null(inverse)){
       message("getting cached data")
      return(inverse)
       }
      
     data <- x$getmatrix()
      inverse <- solve(data,...)
      x$setinverse(inverse)
      inverse
}
