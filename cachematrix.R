## This script has a pair of functions that allow the user to cache the results of a 
## matrix inversion operation and then later re-use the cached inverse. 

## Given a matrix x, this function will return an object that is able 
## to cache its inverse once this inverse has been calculated. In order
## to be able to access the inverse use cacheSolve() 

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


## given a 'caching' matrix calculate the inverse if it has not been calculated, otherwise fetch the inverse from
## the cache.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    mat <- x$get()
    m <- solve(mat)
    x$setinverse(m)
    m
}
