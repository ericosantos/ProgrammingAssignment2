## Functions calculate the inverse of a matrix if it hasn't been calculated before
## if it has the function cacheSolve returns the cached value, saving the time to solve again

## makeCacheMatrix receives a matrix and returns a list of functions
## to set/get the matrix and to set/get the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invMatrix) inv <<- invMatrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve receives an object of the type makeCacheMatrix and returns its inverse
## if the inverse has been calculated previously it returns the pre-calculated value

cacheSolve <- function(x = makeCacheMatrix(matrix())) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}

#test...
m<-matrix(c(1,2,3,4),nrow = 2)
x<-makeCacheMatrix(m)
cacheSolve(x)

