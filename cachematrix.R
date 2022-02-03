##The first function, makeVector creates a special "vector",
##which is really a list containing a function to 1) set the value of the vector
## 2) get the value of the vector 3) set the value of the inverse 4) get the
## value of the Inverse

makeVector <- function(x = matrix()) {
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

## The second function cacheSolve finds the inverse of the special matrix
## created with the above function. However, it first checks to see if the
##inverse has already been calculated. If so, it gets the mean from the cache
## and skips the computation. Otherwise, it calculates the inverse
## of the data and gets the value of the inverse in the cache via the
## setinverse function.
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
## Test
B <- matrix(c(1,2,3,4),2,2)
B1 <- makeVector(B)
cacheSolve(B1)
