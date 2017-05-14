## Assignment: Caching the Inverse of a Matrix

## This function creates a matix 
## It rerurn a list of set-ers and get-ers to the parent environment
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ##solve function reverses a matrix; 
  ##the requirement said that we should assime that matrix can be reversed
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  
  ##return a list of function
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function retrieves the data from cach
## or does the inversion
cacheSolve <- function(x, ...) {
        
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    
    ##returns the inverse of 'x' from cash
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  
  ## return a matrix that is the inverse of 'x'
  m
}
