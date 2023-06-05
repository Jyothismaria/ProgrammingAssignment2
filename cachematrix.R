## Put comments here that give an overall description of what your
## functions do

##  function creates a special "matrix" object that can cache its inverse
## list contain get()set() getinverse and inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  isInvertible <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
    isInvertible <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  setisInvertible <- function(is) isInvertible <<- is 
  getisInvertible <- function() isInvertible 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       getisInvertible = getisInvertible,
       setisInvertible  = setisInvertible )
}


## cacheSolve if the data is cached if not it calculates the inverse

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  is <- x$getisInvertible()
  if(!is.null(is)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  if (det(data) != 0){
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    x$setisInvertible(T)
  }
  else {
    x$setinverse(NULL)
    x$setisInvertible(F)
  }
  inverse
}
