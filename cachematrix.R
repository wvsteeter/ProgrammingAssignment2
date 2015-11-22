## makeCacheMatrix() works in conjunction with cacheSolve()
## to accept a matrix, and return its inverse.  The inverse
## matrix is also cached so it can be retrieved again without
## recomputation.


## Accept a matrix and place its inverse into cache
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInvert <- function(invert) m <<- invert
  getInvert <- function() m
  list(set = set, get = get,setInvert = setInvert,getInvert = getInvert)
}


## Check for the existance of an inverted matrix in the cache.
## If it exists then return it, otherwise compute the inverse
##of the matrix.
## e.g. cacheSolve( makeCacheMatrix( matrix( c(2,3,8,9,12,15,15,20,34,56,67,34,123,456,678,890),ncol=4 ) ) )
cacheSolve <- function(x, ...) {
  m <- x$getInvert()
  if(!is.null(m)) {
    return(m) ## inverse matrix has already cached...return it
    m
  }
  res <- x$get()
  m <- solve(res)
  x$setInvert(m)
  m  
}