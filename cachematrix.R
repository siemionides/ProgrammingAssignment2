# This file contains two fuctions that overall serve the purpose of inverting given matrix
# and storing it's solved version in the cache, so it doesn't have to be computed each time 
# the solved version is requested. Please read the comments above each function for examples of use.


# This function contains variables that store cached matrix as well as it's solved (inverted) version.
# it can be used like 
#  m = makeCacheMatrix()
# m$set(your_matrix_here).
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize variable for storing solved matrix
  s = NULL

  # declare and initialize functions for getting and setting matrix and it's solved version
  set = function(y){
    x <<- y
    s <<- NULL
  }
  get = function() x
  setSolve = function(solve) s <<- solve
  getSolve = function() s

  # expose the methods by returning a list of them
  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}


# This functions used the method of matrix created by makeCacheMatrix() in order to perform 
# inverting matric given as argument x. If matrix was already cached, it's value is read from the cache, 
# otherwise - it's calculated and saved in cache for future use
cacheSolve <- function(x, ...) {

  # get value from cache
  s = x$getSolve()

  # if value was in cache, return it instead of computing an inversion
  if (!is.null(s)){
    return(s)
  }

  # if cache was emtpy, get the raw array from x object...
  data = x$get()

  #... and calculate it's value
  s = solve(data, ...)

  # save the solved value in the cache
  x$setSolve(s)
}
