## The functions create a special matrix and caches its values 

## This is a list that sets and gets the value of the matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This computes the inverse of the matrix or returns it from the cache if it has already been computed 

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if (!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}

