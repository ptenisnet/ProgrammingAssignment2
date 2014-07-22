## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
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
  if(!is.null(inv)) {
    message("getting cached value")
    return(inv)
  }
  inv<-solve(x$get())
  x$setInv(inv)
  inv
}
