## Pair of functions to calculate and save in cache the inverse of a matrix
## "x" provided by the user, this way if the matrix doesn't change the inverse
## only needs to be calculated once.
## NOTE: the matrix supplied must be invertible


## This function creates a list with 4 functions, each one an element of the list
## set: allows the user to change the matrix associated with an existing object
##      of the type created by the function
## get: gets the matrix associated with an existing object of the type 
##      created by the function
## set_invX: cache the inverse matrix for future reference
## get_invX: Retrieves the inverse matrix, saving the time needed for calculation

makeCacheMatrix <- function(x = matrix()) {
        invX <- NULL
        set <- function (y){
                x <<- y
                invX <<- NULL
        }
        get <- function() x
        set_invX <- function (inverse_X) invX <<- inverse_X
        get_invX <- function() invX
        list(set = set, get = get, set_invX = set_invX, get_invX = get_invX)
}


## This function accepts a list of the type created by the makeCacheMatrix 
## function, evaluate the cache associated with the list looking for the 
## inversed value of the matrix saved in the list, if the inverse matrix 
## have already been calculated then it is retrieved form cache, saving time in 
## calculation, if it has not been calculated the it solve for the inverse and 
## save it for future reference.

cacheSolve <- function(x, ...) {
        invX <- x$get_invX()
        
        if (!is.null(invX)) {
                message("getting cached data")
                x$get_invX
        }
        matrix2solve <- x$get()
        inverse_X <- solve(matrix2solve)
        x$set_invX(inverse_X)
        inverse_X
        ## Return a matrix that is the inverse of 'x'
}
