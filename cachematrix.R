### rprog-010 Programming Assignment2: Caching the Inverse of a Matrix
# For this assignment, assume that the matrix supplied is always invertible.

# makeCacheMatrix: This function creates a special "matrix" object
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # initially the cache is not set
  cachedInverse <- NULL  
  
  # the set function keeps a copy of the matrix and invalidates the cache
  set <- function(y) {
    x <<- y  
    cachedInverse <<- NULL # invalidate cached inverse matrix
  }
  
  # return the cached matrix
  get <- function() x
  
  # sets the inverse matrix cache
  # the assumption is made that it is correct for this assignment
  setInverse <- function(inverse){
    cachedInverse <<- inverse
  } 
  
  # returns the cached inverse matrix
  # expect NULL if the cache has not been set yet
  getInverse <- function(){
    cachedInverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# `cacheSolve`: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  # look in the cache first and return it if valid
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    # cache hit!
    message("getting cached data")
    return(inverseMatrix)
  }
  
  # not available in cache, compute the inverse matrix, cache it and return it.
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setInverse(inverseMatrix) # set the inverse in the cache for later re-use
  inverseMatrix
}
