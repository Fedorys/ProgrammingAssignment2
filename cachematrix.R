## Put comments here that give an overall description of what your
## functions do

## This function creates object type matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){     # The function assings the specified matrix 
        x <<- y             # and warns if the matrix not square
        inv <<- NULL
        if(ncol(x) != nrow(x)) message("The matrix must be square.")
    }
    get <- function() x
    setInv <- function(par) inv <<- par
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The function  returns the inverse matrix if its already defined.
## Otherwise, the function checks that the matrix is a square, 
## non-singular and calculates inverse matrix, either returns NULL

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("Getting cached data")
        return(inv)
    }
    y<- x$get()
    if(ncol(y) != nrow(y)){
        message("The matrix is not square")
        inv <- NULL
    }
    else if(det(y) == 0){
        message("The matrix is singular")
        inv <- NULL
    }
    else{
        inv <- solve(y)
    }
    x$setInv(inv)
    inv
}