# The first function, makeCacheMatrix creates a special "matrix",
# which is really a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  # initialize
  x_inverse <- NULL
  
  # set the value of the matrix
  set_matrix <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  
  # get the value of the matrix
  get_matrix <- function() {
    return(x)
  }
  
  # set the value of the inverse
  set_inverse_matrix <- function(y_inverse) {
    x_inverse <<- y_inverse
  }
  
  # get the value of the inverse
  get_inverse_matrix <- function() {
    return(x_inverse)
  }
  
  # return a list of the functions
  return(
    list(set_matrix = set_matrix, 
         get_matrix = get_matrix,
         set_inverse_matrix = set_inverse_matrix,
         get_inverse_matrix = get_inverse_matrix)
  )
}


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see
# if the inverse has already been calculated. If so, it gets the inverse
# from the cache and skips the computation. Otherwise, it calculates
# the inverse of the data and sets the value of the inverse in the cache
# via the "set_inverse_matrix" function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inverse <- x$get_inverse_matrix()
  if (is.null(x_inverse)) {
    message("Getting cached data....")
    return(x_inverse)
  }
  x_matrix <- x$get_matrix()
  x_inverse <- solve(x_matrix, ...)
  x$set_inverse_matrix(x_inverse)
  return(x_inverse)
}


