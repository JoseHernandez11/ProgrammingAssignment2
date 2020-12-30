

#The funcion makeCacheMatrix create a matrix object than can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inverse<- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


#The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  inverse
    
}


#we see if the function works 
test<- matrix(runif(25,0,50),5,5)
test_matrix<- makeCacheMatrix(test)
cacheSolve(test_matrix)
