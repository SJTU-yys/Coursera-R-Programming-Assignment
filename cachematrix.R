## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix, which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix

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


## This function is used to get the inverse matrix of a special matrix,
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache. Otherwise, it 
## calculates the inverse matrix of the data and sets the inverse matrix in the cache via the set_inverse function.

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
