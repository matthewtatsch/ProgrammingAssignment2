## The following functions can be used to compute and cache the inverse of a
## matrix.  Caching the inverse allows avoidance of having to re-compute if
## the inverse already exists in the cache.

## The makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.  This is implemented by assigning the various functions to slots
## in a list.

makeCacheMatrix <- function(x = matrix()) {
     
     ## initialize m
     m <- NULL
     
     ## Sets the value of x; resets m to NULL
     set <- function(y) {
          x <<- y         
          m <<- NULL      
     }
     
     get <- function() x                        ## Gets value of x
     setinverse <- function(solve) m <<- solve  ## Assigns inverse to m
     getinverse <- function() m                 ## Gets value assigned to m
     list(set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse
     )
}


## The cacheSolve function computes the inverse of the special "matrix" returned 
## by the makeCacheMatrix function. If the inverse has already been calculated  
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     ## Assign current value stored in cache to m (via makeCacheMatrix)
     m <- x$getinverse()
     
     ## If a value already exists in cache, then return it along with message
     ## advising that the value is being retrieved from the cache
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     ## If no value already exists in cache, then get the matrix and compute
     ## and return its inverse
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
