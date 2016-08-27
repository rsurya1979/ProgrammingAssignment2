# Description of function:
# Matrix inversion is usually a costly computation. Caching the inverse of a matrix 
# rather than compute it repeatedly has huge benefit on run time. The    
# following two functions are used to accomplish the above task.           
          
# Preview

# makeCacheMatrix creates a list containing a function to                      
# 1. set the value of the matrix                                               
# 2. get the value of the matrix                                               
# 3. set the value of inverse of the matrix                                    
# 4. get the value of inverse of the matrix        

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get, setmat = setmat, getmat = getmat)
}
# The following function returns the inverse of the matrix. It first checks if 
# the inverse has already been computed. If so, it gets the result and skips the 
# computation. If not, it computes the inverse, sets the value in the cache via 
# setinverse function. 

# This function assumes that the matrix is always invertible. 
                            
cacheSolve <- function(x, ...) { 
  m <- x$getmat() 
  if(!is.null(m)) { 
    message("getting cached data.") 
    return(m) 
  } 
  data <- x$get() 
  m <- solve(data) 
  x$setmat(m) 
  m 
} 
