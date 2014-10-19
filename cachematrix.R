## the following functions enable you to take advantage of caching the inverse
## of a matrix after it has first been calculated so that subsequent inversions
## of the matrix retrieve the cached value if the value of the matrix has not
## changed.  If the value of the matrix has changed then the inverse is caculated again.

## the makeCacheMatrix function turns a matrix into a 'special matrix' which is
## really a list containing functions to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse [note that the function 'solve' is used for inversion]
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
     m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## the cacheSolve function computes the inverse of a matrix 'x'
## if the matrix has previously been inverted using cacheSolve
## then the inverse is retreived from the cache and a message printed

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    
}

## Example to illustrate the operation:
## create the 2x2 matrix 'testm'
## create the special version of this 'testmspecial' 
## apply cacheSolve to invert the matrix the first time
## apply cacheSolve again and it retieves the value from cache
## here is the code:
## testm<-matrix(c(1,2,2,1), 2, 2)
## testmspecial<-makeCacheMatrix(testm)
## cacheSolve(testmspecial)
##            [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
## cacheSolve(testmspecial)
## getting cached data
## [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333