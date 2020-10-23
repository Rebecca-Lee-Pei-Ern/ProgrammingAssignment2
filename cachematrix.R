## Put comments here that give an overall description of what your functions do
## makeCacheMatrix - to create a matrix and cache its inverse
## cacheSolve - to return inverse of the matrix if already exist on cache returned by makeCacheMatrix

## Write a short comment describing this function
## to store a returned matrix in the global environment
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## to check if the inverse matrix already exist and retrieve the stored matrix from previous function, then return the inverse matrix
## if not, it will return the incerse matrix and store in makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
