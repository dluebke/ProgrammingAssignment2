###############################################################################
## This module offers two functions to efficiently (computation-wise) work with
## the inversion of matrices at the downside of using more memory for caching
## the result.
## Use the makeCacheMatrix() function to create a cachedMatrix object, which
## internally is a list with four methods as described in the documentation of
## the makeCacheMatrix() documentation. This object can be used together with
## the cacheSolve() function, which will compute the inverse of the matrix only
## once and return the cached result at all subsequent calls.

###############################################################################
# makeCacheMatrix creates an object wrapper around a matrix. Technically, it
# represents the cached matrix object as a list with four functions:
# set/get:           set and get the underlying matrix object on which is
#                    operated
# setsolve/getsolve: set and get the cached inverse of the matrix computed via 
#                    solve()
# When the underlying matrix is changed by a call to the set function, the
# cached inverse is discarded. The first subsequent call to cacheSolve() will
# newly calculate the inverse for the updated matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL                              # initially empty cached result
  set <- function(y) {                   # set function which also clears cache
    x <<- y
    s <<- NULL
  }
  get <- function()                      # get function to return matrix
  setsolve <- function(solve) s <<- solve # setter for cached value
  getsolve <- function() s                # getter for cached value
  list(set = set, get = get,              # return object as list of functions
       setsolve = setsolve,
       getsolve = getsolve)
}

###############################################################################
# cacheSolves takes a cachedMatrix object and returns the inverse matrix as 
# computed by solve().
# In order to speed up compution, the cached element of the cachedMatrix object
# is inspected first. If it is set (i.e., not null), it is returned immediatly.
# Otherwise, the inverse matrix is computed, the result stored back to the 
# cachedMatrix object and the freshly computed result is returned.
cacheSolve <- function(x, ...) {
  ## Look up cache
  s <- x$getsolve()
  if(!is.null(s)) {
    # If cache is hit, return cached copy
    message("getting cached data")
    return(s)
  }
  # otherwise calculate inverse, save it and return it
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
