## Makes a list of functions to get and set input matrix and 
## cached matrix inverse.
## Functions called by cacheSolve() function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(yval) {
      xval <<- yval
      inv <<- NULL
    }
    get <- function() xval
    setinv <- function(xval) inv <<- solve(xval)
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
