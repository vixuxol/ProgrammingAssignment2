## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" 
##object that can cache its inverse

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  ##initialize the inverse property
  m <- NULL
  
  ##set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## get the matrix
  get <- function() x
  
  ##set the inverse matrix
  setInverse <- function(inverse) m <<- inverse
  
  ##get the inverse matrix
  getInverse <- function() m
  
  ##return list of all methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
       
        m <- x$getInverse()
        
        ##if the inverse has already been calculated
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        ##getting matrix from object
        data <- x$get()
        
        ## finding the inverse 
        m <- solve(data) %*% data
        
        ##set the inverse
        x$setInverse(m)
        
        ##return the matrix
        m
}
