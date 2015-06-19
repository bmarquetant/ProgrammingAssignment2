## makeCacheMatrix() function creates a special matrix object that can cashe its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv,getinv=getinv)
}


## cacheSolve() function computes the inverse of the matrix returned 
## by makeCacheMatrix() function. If the inverse has already been
## computed and thematrix has not changed, it returns the inverse
## from the cashe directly.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cashed data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setinverse(inv)
  return(inv)
}
