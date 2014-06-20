## This function is like a class wrapper that internally caches a computed
## inverse matrix. It has "methods" that are getters and setters for
## the cached computed inverse matrix.


## The input to this function is a matrix object. We will default it to
## an empty matrix object.
makeCacheMatrix <- function(x = matrix()) {
    ## First check if the input is a valid matrix and not empty.
    ## if it is, return message to the user.
    if (!is.matrix(x) || !is.null(x)) {
        message("Input object cannot be NULL and must a matrix object")
    }
    
    
    ## inverseMatrix is an internal variable that caches the matrix
    ## that is passed in.
    inverseMatrix <- NULL
    
    ## This "SetMatrix" function serves to purposes:
    ##      1) Redefines the "x" matrix that is passed in to
    ##         makeCacheMatrix to the input matrix "y".
    ##      2) Clears the cache (i.e. sets inverseMatrix to NULL)
    ## 
    ## The "<<-" assignment operator must be used because "x" is outside
    ## the scope of this function. 
    SetMatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## This "GetMatrix" function simply returns the input matrix "x"
    GetMatrix <- function() { x }
    
    ## This "SetInvMatrix" function caches a computed matrix.
    ## It uses the internal "inverseMatrix" variable as the cache.
    ## The "<<-" assignment operator must be used because "inverseMatrix"
    ## is outside the scope of this function
    SetInvMatrix <- function(mean) { inverseMatrix <<- mean }
    
    ## This "GetInvMatrix" function returns the cached matrix object
    ## from the "inverseMatrix" internal variable.
    GetInvMatrix <- function() { inverseMatrix }
    
    ## We return a list as the last statement to make the above 
    ## functions accessible via the "$" extraction notation
    ## e.g. You can get the cached matrix via makeCacheMatrix$GetInvMatrix
    list (SetMatrix = SetMatrix,
          GetMatrix = GetMatrix,
          SetInvMatrix = SetInvMatrix,
          GetInvMatrix = GetInvMatrix)
}



## This function takes a matrix "x" and calculates it's inverse,
## then caches the result. The cache ensures that subsequent calls
## to cacheSolve does not impact system peformance if the inverse
## matrix was previously calculated.
cacheSolve <- function(x, ...) {
    ## Validate that "x" is a matrix before compute the inverse.
    if (!is.matrix(x) || !is.null(x)) {
        print("Input object cannot be NULL and must a matrix object")
    }
    
    ## NOTE: Future improvement - Ideally there would be an
    ## inexpensive way to check if a matrix is actually invertible.
    ## For this assignment, we are assuming that "x" is invertible.
    
    ## First we attempt to retrieve the matrix. 
    ## If the returned matrix IS NULL, then we compute its inverse.
    ## if the returned matrix IS NOT NULL, we simply return the matrix.
    matrix <- x$GetInvMatrix()
    if (!is.null(matrix)) {
        message("Returning cached data.")
        return(matrix)
    }
    
    ## Data is not cached, so we compute the inverse and save it
    ## to the cache for future use.
    data <- x$GetMatrix()
    matrix <- solve(x)
    x$SetInvMatrix(matrix)
    matrix
}
