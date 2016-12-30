##These two functions together compute the inverse of a matrix.


##This function set and gets the matrix, and sets and gets the inverse of it.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

##This function takes in the matrix, checks if the inverse of it is stored in the cache,
##if yes, gives out that inverse, else computes it


cacheSolve <- function(x, ...) {
 
         ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  }
