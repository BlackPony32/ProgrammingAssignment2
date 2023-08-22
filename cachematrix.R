## Creates a "matrix" object with the ability to cache the inverse matrix
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

## Computes and caches the inverse of the input matrix 'x'
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached inverse.")
    return(inv)
  }
  
  inv <- solve(x$get())
  x$setInverse(inv)
  inv
}
# Create a matrix (example)
matrix_data <- matrix(c(2, 2, 3, 4, 7, 10, 1, 1, 3), nrow = 3, ncol = 3)

# Create a cacheMatrix object
cacheMatrix <- makeCacheMatrix(matrix_data)

# Compute the inverse using cacheSolve
inverse_matrix <- cacheSolve(cacheMatrix)

# Print the computed inverse
print("Inverse Matrix:")
print(inverse_matrix)

