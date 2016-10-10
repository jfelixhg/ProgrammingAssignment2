## These functions allows to calculate the inverse of a matrix, or if already calculated, gets the values from cache. 

## functions do
## 1. set the value of the matrix
## 2. get the value of the matrix
makeCacheMatrix <- function (x = matrix()) 
{

  #initialize the variables to store the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #define the functions to set or get the matrix 
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  
}


## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
cacheSolve <- function(x, ...) {
  
##get the inverse matrix from cache  
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
##if matrix inverse is not in cache, then calculate the inverse and set it to cache.
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


## it can be tested with next scenario: 
# 1. m<-makeCacheMatrix(rnorm(9,10,1),3,3)
# 2. cacheSolve(m)