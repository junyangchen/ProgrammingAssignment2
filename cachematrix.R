## The following two functions will be used to cache the inverse of a matrix.

## Creating a special "matrix" --- a list containing 4 functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse)  inv <<- inverse
    getinverse <- function() inv
    
    list(set = set, get = get,
      setinverse = setinverse,
      getinverse = getinverse)
}


## CacheSolve function will calculate the inverse of of the special "matrix"
## created by makeCacheMatrix function.  If the inverse is existing in the 
## cache, in other words, the inverse has been calculated, and the value of 
## the matrix has not changed, then the function will "get" the value from the
## cache. Otherwise, the inverse of the matrix will be calculated, stored in 
## the cache and returned.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
        
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
