## Handles matrix and storing of inverted variant
makeCacheMatrix <- function(matrixWithCachedInverse = matrix()) {
  invertedMatrixCached <- NULL # Empty placeholder
 
  set <- function(m) { # Set new matrix, with empty inverted cache
    matrixWithCachedInverse <<- m
    invertedMatrixCached <<- NULL
  }
  
  get <- function() { matrixWithCachedInverse } # Get our normal matrix
  setInverse <- function(inverse) { invertedMatrixCached <<- inverse } # Store an inverted matrix 
  getInverse <- function() { invertedMatrixCached } # Return cached inverted matrix
  
  
  # Properties that our makeCacheMatrix supports
  list(
      set = set, 
      get = get,
      setInverse = setInverse,
      getInverse = getInverse
      )  
}


## Invert a matrix, using cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #Get cached inverse, if it exists
  result <- x$getInverse()
  #If we got a cached result, return it. 
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  #No cached version, get empty matrix
  m <- x$get()
  #Invert input
  result <- solve(m)
  #Store in cache
  x$setInverse(result)
  result
}


# Test:
mm <- makeCacheMatrix(matrix(c(18,2,1980,40), 2, 2))
mm$get()
#[,1] [,2]
#[1,]   18 1980
#[2,]    2   40
cacheSolve(mm)
#[,1]         [,2]
#[1,] -0.012345679  0.611111111
#[2,]  0.000617284 -0.005555556
cacheSolve(mm)
cacheSolve(mm)
