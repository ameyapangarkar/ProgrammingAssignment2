## makeCacheMatrix:
## This function creates a list of functions containing:
## setMatrix = Function to set new Matrix
## getMatrix = Function to get current Matrix
## setinvMatrix = Function to set inv of Matrix
## getinvMatrix = Function to get current inv of Matrix

makeCacheMatrix <- function(M = matrix()) {

    # Initialize inv Matrix to NULL
    iM <- NULL
    
    ## Function to set new Matrix, make inverse NULL
    setMatrix <- function (x) {
        M <<- x
        iM <<- NULL
    }
    
    ## Function to return current Matrix
    getMatrix <- function () {
        M
    }
    
    ## Function to set inv Matrix
    setinvMatrix <- function (ix) {
        iM <<- ix
    }
    
    ## Function to return current inv Matrix
    getinvMatrix <- function () {
        iM
    }
    
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setinvMatrix = setinvMatrix, getinvMatrix = getinvMatrix)
    
}


## cacheSolve:
## This function returns inverse of matrix from cache if available
## If not available the function sets inv of matrix and returns it

cacheSolve <- function(fl, ...) {
    
    ## Get current value of inv Matrix
    iX <- fl$getinvMatrix()
    
    ## If current value is not null, then retun the value
    if(!is.null(iX)) {
        message("Found inverse matrix in cache")
        return(iX)
    }
    
    ## If current value is null, then calculate new value of inv Matrix
    X <- fl$getMatrix()
    iX <- solve(X, ...)
    fl$setinvMatrix(iX)
    iX
}
