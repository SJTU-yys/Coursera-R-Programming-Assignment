## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- null
    set <- function(y){
        x <<- y
        inverse <<- null
    }
    get <- function() x
    set_inverse <- function(inv_matrix) inverse <<- inv_matrix
    get_inverse <- function() inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$get_inverse()
        if(!is.null(inv_matrix)) {
            message("getting cached data")
            return(inv_matrix)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inverse(inv)
        inv
}
