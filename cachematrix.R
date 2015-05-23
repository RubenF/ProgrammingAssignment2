## These are functions that are able to cache potentially time-consuming computations.
## Here we build functions for creating and using inverted matrices with the ability
## to cache the result in order to look it up rather than recomputed it again.

## This function creates an special type object (matrix type) that can cache its result
## (its inverse)

makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setCache <- function(matinv) inv <<- matinv
  getCache <- function() inv
  list(set = set, get = get, 
		setCache = setCache, 
		getCache = getCache)
}

## This function computes the inverse of the special type matrix.
## Note that first it looks up if the result has already been calculated.
## In this case, it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  inv <- x$getCache()
  if(!is.null(inv)) {
    message("Getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setCache(inv)
  return(inv)
}