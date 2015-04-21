## The following two functions are used to cache the inverse of matrix
## as matrix inversion is costly computation.

## makeCache creates a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invmatrix<- NULL
    set <- function(y){
      x <<-y
      invmatrix<<-NULL
    }
    get <- function() x
    setinverse <- function(inverse) invmatrix<<-inverse
    getinverse <- function() invmatrix
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## CacheSolve function gives the inverse of the matrix.
## It first checks whether the inverse is previously computed if not,
## then compute the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
  
        invmatrix <- x$getinverse()
        if(!is.null(invmatrix)){
          message("getting cached data")
          return(invmatrix)
        }
        data <- x$get()
        invmatrix <-solve(data, ...)
        x$setinverse(invmatrix)
        invmatrix
}
