## Following pair of functions cache the inverse of a matrix.

## makeCacheMatrix creates a matrix interface
## which is a list containing a functions to:
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse of the matrix
##     get the value of the inverse of the matrix

makeCacheMatrix <- function(data = matrix())
{
  inverse_data <- NULL
  
  set <- function(rhs)
  {
    data <<- rhs
    inverse_data <<- NULL
  }
  
  get <- function() data
  
  setinverse <- function(solve) inverse_data <<- solve
  
  getinverse <- function() inverse_data
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns a inverse of the matrix 'x'
## using the cached inverse value, if possible

cacheSolve <- function(x, ...)
{
  matrix_inverse <- x$getinverse()
  if (!is.null(matrix_inverse))
  {
    return(matrix_inverse)
  }

  result <- solve(x$get(), ...)
  x$setinverse(result)
  result
}
