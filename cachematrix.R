## These functions cache the value of inverse of a matrix
## so that it doesn't need to be recomputed if the matrix
## is unchanged.  

## makeCacheMatrix just sets up the cache.  cacheSolve actually
## performs the computation of the inverse.

## makeCacheMatrix(x = matrix())
## Cache a matrix and its inverse.
## 
## Parameters:   x - the matrix to be cached
##
## Return value: a list containing the labeled getter and setter methods
##              for x and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve(x, ...)
## Look up the inverse of a matrix in the cache and return it.  If it has
## not been cached, compute it and save it in the cache first.  The matrix
## is passed to solve() without checking first to see if it is invertible.
## Any errors thrown by solve() are not caught.
##
## Parameters:  x - a list of getter and setter methods as returned
##                  by makeCacheMatrix
##              ... - further arguments are passed to solve function
##
## Return value: a matrix that is the inverse of the matrix cached in x    
 
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    inverse <- solve(x$get(), ...)
    x$setinverse(inverse)
    inverse
}
