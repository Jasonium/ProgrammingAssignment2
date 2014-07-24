makeMatrix <- function(theMatrix = matrix()) {
  cachedMatrix <- NULL
  set <- function(y) {
    theMatrix <<- y
    cachedMatrix <<- NULL
  }
  get <- function() theMatrix
  setInverse <- function(solve) cachedMatrix <<- solve
  getInverse <- function() cachedMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheInverse <- function(x, ...) {
  theInverse <- x$getInverse()
  if(!is.null(theInverse)) {
    message("getting cached data")
    return(theInverse)
  }
  data <- x$get()
  theInverse <- solve(data, ...)
  x$setInverse(theInverse)
  theInverse
}