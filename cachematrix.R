##These functions illustrates cashing of an inverse matrix 
##of the given matrix and thus demonstrates the concept of lexical scoping.

##The function "makeCacheMatrix"builds a set of functions 
##and returns the functions within a list to the parent environment.
makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) invmat <<- inv
  getInverse <- function() invmat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



##cacheSolve() is required to populate or retrieve 
##the inverse matrix from an object of type makeCacheMatrix()
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmat <- x$getInverse()
  if (!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
}