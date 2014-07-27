##  R Programming Assignment 2

##  cacheMatrix.R contains two functions, 'makeCacheMatrix' and 'cacheSolve'.
##  Computing the inverse of a large square matrix is a potentially costly computation.
##  This R script contains two functions, 'makeCacheMatrix' and 'cacheSolve', which may be used to
##  cache the inverse matrix.


##  The function 'makeCacheMatrix' returns a list containing functions
##  to set the matrix, get the matrix, set the inverse of the matrix, and
##  get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {

    cachedMatrix <- NULL
    
    ##  Set the matrix anew, and therefore the inverse 'cacheMatrix' is not yet 
    ##  cached.
    set <- function(y) {
        x <<- y
        cachedMatrix <<- NULL
    }
    
    get <- function() x
    
    ##  Invert the matrix and place it in the cache.
    setInverse <- function(solve) cachedMatrix <<- solve
    
    getInverse <- function() cachedMatrix
    
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


##  The function cacheSolve takes a list created by makeCacheMatrix,
##  returns the inverse from the cache if it has already been computed,
##  and otherwise computes the inverse and places it in the cache.

##  ASSUMPTION: matrix set in makeCacheMatrix is invertible!

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    theInverse <- x$getInverse()
  
    ## If the matrix is not NULL, the inverse was previously computed.
    if(!is.null(theInverse)) {
        message("getting cached data")
        return(theInverse)
    }
  
    ## Otherwise, compute the inverse for the first time.
    ## Get the matrix using the get() function.
    data <- x$get()
    
    theInverse <- solve(data, ...)

    ##  Compute the inverse and place it in the cache.
    x$setInverse(theInverse)
    
    theInverse
    
}
