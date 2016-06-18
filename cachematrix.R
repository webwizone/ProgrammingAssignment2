## Put comments here that give an overall description of what your
## functions do

## 1. set the matrix 
## 2. get the matrix 
## 3. set the inverse of the matrix 
## 4. get the inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  
  
  setinverse <- function(inverse) inv <<- inverse 
  
  getinverse <- function() inv 
  
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x' 
## Following the same format as the assignment example 
## Get the current state of the inverse and see if it 
## has been computed yet 


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  
  inv<-solve(data, ...)
  
  x$setinverse(inv)
  
  inv
}
