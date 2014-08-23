## Computing an inverse of a matrix is an expensive operation.
## The purpose of the functions below is to optimise it by
## caching the inverses along with the matrix object.

## Create and return a matrix object that has attributes to store 
## and retrieve the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function checks if the inverse for a given matrix has been computed.
## If so, it returns the cached object. Else, it computes, stores as well as
## returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}