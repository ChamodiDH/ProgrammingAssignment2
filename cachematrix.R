makeCacheMatrix <- function(x = numeric()) {
  # cache value is hold in here
  # initially cache value set to non
  
  cache <- NULL
  # matrix is stored in here
  setMatrix <- function(newValue) {
    x <<- newValue
    # cache is flushed
    cache <<- NULL}
  
  # stored matrix is returened
  getMatrix <- function() {
    x
  }
  # caching the argument
  cacheInverse <- function(solve) {
    cache <<- solve }
  # get the cached value
  getInverse <- function() {
    cache
  }
  # Returns a list
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# makeCacheMatrix

cacheSolve <- function(y, ...) {
  # get the cached value
  inverse <- y$getInverse()
  # if a cached value exists return it
  if(!is.null(inverse)) {
    message("get cached data")
    return(inverse)
  }
  # else get the matrix, and  caclulate the inverse and store it in the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  # return the inverse
  inverse
}