## to store or cache the result of matrix
  
  makeCacheMatrix <- function(x = matrix()) {
    ##set inv value to null 
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
	 ##setting inverse result of matrix
	 ##getting inverse result of matrix
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
  }
  
  
  ## first it will check if the inverse of matrix is already present or not
  ## if yes it will return directly or else it will call solve function
  
  cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    ##for checking inverse matrix is present or not
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    ##solve function to find inverse of matrix
    x$setInverse(inv)
    inv
    ## Return a matrix that is the inverse of 'x'
  }
