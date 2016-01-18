## the two functions allow the calculation of the inverse of a matrix and 
## the storage of the result in the cache 

## make a special matrix that can store the inverse in the cache

makeCacheMatrix <- function (x = matrix()) {
    inverse <- NULL
    set <- function (y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function () x
    set_inverse <- function(inv) inverse <<- inv
    get_inverse <- function() inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## take a matrix made with makeCahceMatrix; check whether its inverse has been calculated 
## or not; if yes, return the inverse; if not, calculate the inverse and store the result
## in the cache, so next time it can be taken out directly without the computation

cacheSolve <- function(x, ...) {
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$set_inverse(inverse)
    inverse
}

## here is an example
mat <- makeCacheMatrix (matrix(rnorm(25), c(5,5))) # define a matrix using makeCacheMatrix
mat$get() # take a look at the matrix
cacheSolve(mat) # calculate the inverse for the first time
cacheSolve(mat) # the second time, it loads the inverse from the cache