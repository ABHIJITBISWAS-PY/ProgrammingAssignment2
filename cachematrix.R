## The following funtions cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set_matrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  get_matrix <- function() x
  setinverse_matrix <- function(inverse) m <<- inverse
  getinverse_matrix <- function() m
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       setinverse_matrix = setinverse_matrix,
       getinverse_matrix = getinverse_matrix)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the inverse 
##from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse_matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get_matrix()
  m <- solve(data, ...)
  x$setinverse_matrix(m)
  m
}

