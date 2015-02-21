## Functions used to cache the calculation of inversing matrices. 
## Use makeCacheMatrix to create a cachable matrix. This returns a list of functions
##   (an object of sorts). Use $set(x) to set a new matrix
## Use cacheSolve with this list to return the inverse of the matrix previously set
##   by $set(x)


## Creates a cacheable matrix. The matrix is accessible using the $set(x) and $get()
## properties of the list 

makeCacheMatrix <- function(x = matrix()) {
  # the cached inverse
  inv <- NULL
  
  # function that sets the "current" matrix
  set <- function(mtx) {
    x <<- mtx
    # reset the cached inverse since it is no longer valid
    # TODO: if mtx is the same as x we don't need to invalidate the cache
    inv <<- NULL
  }
  # function to get the "current" matrix
  get <- function() x
  
  # function to set the cached inverse once computed
  setInverse <- function(newInv)
    inv <<- newInv
  
  # function to get the cached inverse (if available)
  getInverse <- function() inv
  
  # return a list of named functions to be used mainly by the solver
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Returns the inverse of a cacheable matrix. If the inverse has been previously
## calculated, it is simply returned. Otherwise it is calculated, cached, 
## and returned

cacheSolve <- function(x, ...) {
  # get the cached solution
  inv <- x$getInverse()
  
  # if we had it, just return it and we're done
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # get the "current" matrix so we can solve it
  data <- x$get()
  
  # solve the matrix passing all of the parameters along with it
  inv <- solve(data, ...)
  
  # cache the solution we just calculated
  x$setInverse(inv)
  
  # return the solution
  inv
}
