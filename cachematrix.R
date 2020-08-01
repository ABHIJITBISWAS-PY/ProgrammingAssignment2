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
