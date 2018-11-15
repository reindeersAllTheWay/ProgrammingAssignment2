## We have to write two functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){

  inv <- NULL
  set <- function(y) {
        x <<- y
        inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}	

cacheSolve <- function(x, ...) {
##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}	

## x1 <- makeCacheMatrix(matrix(rnorm(16),4,4))
## cacheSolve(x1)
## [,1]       [,2]         [,3]       [,4]
## [1,] -0.08308237 -0.4682020  0.004272563  1.4686616
## [2,] -0.98655438 -2.2206506  1.930828384  2.2915784
## [3,] -0.08335486 -1.1883472  2.186197038  1.5036010
## [4,] -0.10433477  0.7259153 -0.356970720 -0.3960493
## > cacheSolve(x1)
## getting cached result
## [,1]       [,2]       [,3]       [,4]
## [1,] -0.50012564  0.2218774 -0.2811647 -0.5670937
##[2,]  0.07900369 -0.3132251 -0.6209163 -0.3116894
## [3,]  0.61741466  0.4383896  0.7339295  0.5870293
## [4,]  0.16682664  0.1386740 -0.4358524  0.5497003

