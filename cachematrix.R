## cachematrix will save castly computing time by caching the inverse of a matrix.
## These functions will cache a matrix and then cache its inverse

## The function makeCacheMatrix creates a special matix object that can cache its inverse
## The result will be a list of four functions that can be called
## 1. set to create a cache of received matrix
## 2. get to return the matrix from the cache
## 3. setInverse to create a cache of the inversed matrix
## 4. getInverse to return the cached matrix

makeCacheMatrix <- function(x = matrix()) {

      m <- NULL
      
      set <- function(y) {
          
             x <<- y
             
             m <<- NULL
      }

      get <- function () x

      setInverse <- function(matrixIn) m <<- matrixIn

      getInverse <- function() m

      list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## The function cacheSolve computes the inverse of a cached matrix.
## If the inverse has already been calculated, the funtion will retrieve the inverse from the cache
## The function receives as an argument a list of functions created by makeCacheMatrix

cacheSolve <- function(x, ...) {
       
      m <- x$getInverse()

      if(!is.null(m)){
           
           print ("getting cached data")

           return(m)
     
      }

      data <- x$get()

      m <- solve(data, ...)

      x$setInverse(m)

      m


}
