## 'set' sets my matrix's value. 'get' returns my matrix's value. 'setinverse'
## takes an input i and assigns variable 'inverse' equal to this (gets called by cacheSolve). 
## 'getinverse' is also called by cacheInverse and is what returns the inverse.
## It was created using assignment's provided 'makeVector' and 'cachemean' as guides
## as commands and overall structure were very similar.

## The following creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() {
    return(x)
  }
  setinverse <- function(i) { 
    inverse <<- i
  }
  getinverse <- function() {
    return(inverse)
  }
  list(set = set, get= get, setinverse = setinverse, getinverse = getinverse)
}

## This checks to see if inverse is NULL and if not (!) returns the invserse. Otherwise
## the inverse is calculated with solve(), which gives inverse of a square matrix.
## Messages are used to tell whether result was cached or not

## This computes the inverse of a matrix returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("This was cached:")
    return(inverse)
  }
  m <- x$get()
  inverse <- solve(m, ...)
  x$setinverse(inverse)
  message("This was not cached:")
  return(inverse)
}
