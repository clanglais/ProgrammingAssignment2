### rprog-010 Programming Assignment2: Caching the Inverse of a Matrix
# For this assignment, assume that the matrix supplied is always invertible.

# makeCacheMatrix: This function creates a special "matrix" object
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL # invalidate cached inverse matrix
  }
  get <- function() x
  setInverse <- function(inverse){
    cachedInverse <<- inverse
  } 
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
    message("getting cached data")
    return(inverseMatrix)
  }
  
  # not present in cache, compute the inverse matrix, cache it and return it.
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
