##  R Programming Assignment 2

##  cacheMatrix.R contains two functions, 'makeCacheMatrix' and 'cacheSolve'.
##  Computing the inverse of a large square matrix is a potentially costly computation.
##  This R script contains two functions, 'makeCacheMatrix' and 'cacheSolve', which may be used to
##  cache the inverse matrix.


## The function 'makeCacheMatrix' returns a list containing functions
##  to:
##  1.  Set the matrix.
##  2.  Get the matrix.
##  3.  Set the inverse of the matrix.
##  4.  Get the inverse of the matrix.
##
##  To use this function to create 'x', issue the R command
##  x<-makeCacheMatrix().
##  1.  x$set(matrix(...)) sets the matrix.
##  2.  x$get() gets displays the matrix.
##  3.  x$setInverse() should not be used on its own; this should be
##      used only in conjunction with the function cacheSolve().
##  4.  x$getInverse() displays the inverse of the matrix once
##      it has been set by cacheSolve() and returns NULL otherwise.

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


##  The function cacheSolve calculates the inverse of the matrix 
##  stored using the function makeCacheMatrix.  
##  ASSUMPTION: matrix set in makeCacheMatrix is invertible!
##  It takes, as a parameter, the list created by the makeCacheMatrix
##  function. 
##  It first checks to see if the inverse has already been computed.  
##  If so, it retrieves the inverse from the cache and avoids a 
##  new computation.
##  Otherwise, it computes the inverse of the matrix and places the 
##  inverse in the cache via the setInverse function.

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
