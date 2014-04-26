## Functions to create a special matrix using makeCacheMatrix() that
## can have its inverse computed and cached using the function cacheSolve().
##
## A fist call to cacheSolve() compute the inverse and caches it.
## A second call to cacheSolve() do not compute again the inverse but
## retrieve it from the cache.

## Create a special matrix which allows its inverse to be cached and fetched.
## The inverse is computed and cached by calling the support function
## cacheSolve().
makeCacheMatrix <- function(x = matrix()) {
  x.inv <- NULL
  set <- function(y) {
    x <<- y
    inv.x <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(inverse){
    x.inv <<- inverse
  }
  getInverse <- function() {
    x.inv
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of matrix 'x'. If the inverse is
## already computed, it is retrieved from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x.inv <- x$getInverse()
  if(!is.null(x.inv)) {
    message("getting cached data")
    return(x.inv)
  }
  data <- x$get()
  x.inv <- solve(data)
  x$setInverse(x.inv)
  x.inv
}
