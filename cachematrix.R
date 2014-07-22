## There are two functions in this R file. They can be used to calculate the inverse of a matrix.
## Also, if the inverse of a matrix has already been calculated, the same need not be calculated again 
## as the inverse is cached as soon as it is calculated. This saves a lot of compute time for large matrices.

## the makeCacheMatrix function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## the cacheSolve matrix function can be used to compute the inverse of a matrix. for matrices whose 
## inverse has already been calculated, this function also retreives the cached value from the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
        ## Return a matrix that is the inverse of 'x'
}
