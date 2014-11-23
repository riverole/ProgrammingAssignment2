## These functions they allow us to set the inverse of a matrix in a different environment and cache it 
## such as the next time that is called is used from the cached value and not calculated again. In order to do that we use m variable that is used in both functions using the <<- parameters

## This function creates a list from a matrix parameter that includes functions such as get the value of the matrix, set the inverse or get the inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function tries to get the inverse of the matrix from the parent environment and if it is, it uses that instead of calculating again the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
