## The makeCacheMatrix and cacheSolve functions are used together to
## create a caching function for a matrix and its inverse, and to calculate that inverse.
## You can find convenient testing code at the end of the file to assist in your review.

## This function creates an object that serves as the storage repository
## for a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function uses the object created by the makeCacheMatrix function to
## check to see if is currently storing a cached Inverse; and, if it is, returns it.
## If it's not already storing an inverse, it will calculate the inverse of the matrix,
## and preserve it in the storage object.

cacheSolve <- function(x, ...){
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data <- x$get()
  i <-solve(data, ...)
  x$setInverse(i)
  i
}

## Sample Testing Code -- copy the below code into your console, remove the "#", and execute to test.

##  set.seed(1)
##  testMatrix <- matrix(runif(16,0,100),4,4)
##  cachedTestMatrix <- makeCacheMatrix(testMatrix)
##  cacheSolve(cachedTestMatrix)
##  cacheSolve(cachedTestMatrix)


