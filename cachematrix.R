@@ -4,18 +4,6 @@
  ## to cache the result of matrix
  
  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    
  }
  
  @@ -23,14 +11,5 @@ inv <- NULL
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