## Two Functions that cache the inverse of a Matrix


## Creation of a Temporary Matrix which will be used to calculate inverse
makeCacheMatrix <- function(mat = matrix()) 
{
    ## Initialize inversion
    i <- NULL

    ## Method to set Matrix
    set <- function(matrix) {
            mat <<- matrix
            i <<- NULL
    }

    ## Method the get Matrix
    get <- function() {
    	## Return the Matrix
    	mat
    }

    ## Method to set the inverse of the Matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the Matrix
    getInverse <- function() {
        ## Return the inversion
        i
    }

    ## Return a list of the methods
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## If the inverse has already been calculated and the matrix stays same, then "cachesolve" will retrieve the inverse from Cache.
cacheSolve <- function(x, ...) {

    ## Returns Matrix that is inverse of x
    mat <- x$getInverse()

    ## Returns Inverse if already set
    if( !is.null(mat) ) 
    {
            message("Getting Cached Data....")
            return(mat)
    }

    ## Get the matrix from object
    data <- x$get()

    ## Calculate inverse using matrix multiplication
    mat <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(mat)

    ## Return the matrix
    mat
}