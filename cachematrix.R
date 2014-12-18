# makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special “matrix” returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then cacheSolve should retrieve the inverse from the cache.

## Creates a cacheable matrix inverse object. It contains methods
## to set/get the matrix and setInverse/getInverse for its inverse

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setInverse <- function(matrixInverse = matrix()) xInv <<- matrixInverse
  getInverse <- function() xInv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function operates on a cacheMatrix object, first checking for the existence of a cached
## matrix inverse, otherwise creating its inverse, caching and then returning the inverted 
## matrix (of data type matrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInv <- x$getInverse()
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setInvers(xInv)
  xInv
}

## Extra copy of template vector functions to examine their operation as reference

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
