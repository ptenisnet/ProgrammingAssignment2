## The following functions support the caching of a matrix and calculating and caching of
## of its inverse for subsequent use.


##  creates a cached copy of matrix x.  Exposes setter and getter functions for the matrix
##  and for its inverse. If the cached matrix is updated, then the inverse is set to NULL
##  in order to force recalculation of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL # initialize inverse to null.
  set <- function(y) {
    x <<- y
    invX <<- NULL  # matrix has changed. set inverse to null so it can be recalculated.
  }
  get <- function() x
  setInv <- function(inv) invX <<- inv
  getInv <- function() invX
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {   #inverse has already been calculated.
    message("getting cached value")
    return(inv)
  }
##  matrix has been updated. Recalculated inverse
  inv<-solve(x$get()) 
  x$setInv(inv)
  inv
}
