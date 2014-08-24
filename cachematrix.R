## makeCacheMatrix makes a list of functions to get and set input matrix and 
## cached matrix inverse.
##
## Input matrix ASSUMED to be invertable, per instructions (so no checks in code)
## makeCacheMatrix() functions called by cacheSolve() function
##
## NOTE: Instructions do not call for loading the input matrix into cache, so 
##  program cacheSolve() throws ERROR unless x$set() function is executed
##  AFTER run of makeCacheMatrix() and BEFORE cacheSolve() is run.
##  Command like to do this: myfun$set(input_matrix),
##  assuming prior run of: myfun <- makeCacheMatrix(input_matrix).
##
## Usage: 
##  input_matrix <- matrix(c(5.5, 15:22), ncol=3) (an invertable 3x3 matrix)
##  myfun <- makeCacheMatrix(input_matrix)   ##creates list of cache functions like myfun$set

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(yval) {
      xval <<- yval
      inv <<- NULL
    }
    get <- function() xval
    ## setinv <- function(xval) inv <<- solve(xval)
    setinv <- function() inv <<- solve(get())
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }

## cacheSolve returns cached inverse of input matrix, or calculates one if none exists
## Usage: 
## myfun$set(input_matrix)  ## This loads the input matrix in the cache
## cacheSolve(myfun)  ##  . .  and repeat this call to demo cache retrieval
## To demo function notices change of input, repeat myfun$set with a DIFFERENT input matrix
##     and cacheSOlve will invert the new input, report on the newly computed inverse,
###    and update the inverted matrix cache to service the next call. 

cacheSolve <- function(x, ...) {
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("returning cached inverse")
    return(inv)
  }
  data <- x$get()  ## NOTE: Throws error if x$set() has not been previously run
  inv <- solve(data, ...)
  ## x$setinv(inv)
  x$setinv()
  inv
}
