# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(mat = matrix()) {
  # Initialize the inverse matrix cache
  inverse <- NULL
  
  # Function to set the matrix
  set <- function(matrix) {
    mat <<- matrix
    inverse <<- NULL  # Invalidate the cached inverse when the matrix is set
  }
  
  # Function to get the matrix
  get <- function() {
    mat
  }
  
  # Function to get the cached inverse or calculate it if not cached
  getInverse <- function() {
    if (!is.null(inverse)) {
      message("Getting cached inverse")
      return(inverse)
    }
    
    message("Calculating inverse and caching")
    inverse <- solve(mat)
    return(inverse)
  }
  
  # Return a list of functions
  list(set = set, get = get, getInverse = getInverse)
}

# Function to compute the inverse of the cached matrix
cacheSolve <- function(cacheMatrix) {
  cacheMatrix$getInverse()
}

# Example usage:

# Create a cache matrix
myCacheMatrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))

# Get the original matrix
print("Original Matrix:")
print(myCacheMatrix$get())

# Compute and cache the inverse
print("Inverse Matrix:")
print(cacheSolve(myCacheMatrix))

# Get the cached inverse without recomputing
print("Cached Inverse:")
print(cacheSolve(myCacheMatrix))
