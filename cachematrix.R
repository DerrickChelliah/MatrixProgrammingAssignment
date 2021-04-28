## The first function is to make a a vector containing a list of functions to set a function, 
## get a function, set the inverse of a matrix and get the inverse of a matrix.  
## The second function checks to see if there is an inverse matrix in the cache and, if not, calculates the
## inverse of the matrix, stores it in the cache and returns the value of the inverse matrix.

## This function is to make a a vector containing a list of functions to set a function, 
## get a function, set the inverse of a matrix and get the inverse of a matrix.  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solve) m <<- solve
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)

}


## This function checks to see if there is an inverse matrix in the cache and, if not, calculates the
## inverse of the matrix, stores it in the cache and returns the value of the inverse matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinversematrix()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversematrix(m)
  m
}