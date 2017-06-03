 ## caching the inversr of a matrix:
## the following functions creates a new object that
## stores a matrix and caches its inverse

## a function that creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function computes an inversr matrix of the one created by makeCachMatrix
## if the inverse has been created it should retrieve it form cache

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
              message("getting cached data")
              return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
