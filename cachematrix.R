## The functions are used to cache the inverse of an invertible matrix since it is an 
## expensive calculation. 

## Creates a “special” matrix which caches the inverse of an invertible matrix. 
## The set function sets the inverse; get function returns the inverse, if saved.
## Output is a list which contains all the methods in the special matrix
makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
       x <<- y
       m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Input: special matrix created from makeCacheMatrix function.
## If inverse has been saved by the helper function then returns that to the user
## Else computes the inverse using solve() and saves that in the cache using the helper functions

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    ## Return a matrix that is the inverse of 'x'
    x$setinverse(m)
    m

}
