## These two functions create, and store inverse matrix for future purposes.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the variable with NULL(to store inverse matrix in later step)
        im <- NULL
        
        ## Method to modify the matrix, and set inverse matrix back to NULL
        set <- function(y ) {
                x <<- y
                im <<- NULL
        }
        
        ## Method the get the (not yet inverted) matrix
        get <- function() x
        
        ## Method to set the inverse matrix
        setinverse <- function(inverse) im <<- inverse 
        
        ## Method to get the inverse matrix
        getinverse <- function() im
        
        ## Return a list of the methods
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function pulls the inverse matrix from 'makeCacheMatrix' function if already computed.
## If not, this function compute the inverse matrix and cache it to 'makeCacheMatrix' function.       
cacheSolve <- function(x, ...) {
        
        ## Assign inverse matrix(iff already made) to im
        im <- x$getinverse()
        
        ## Check if inverse matrix has been made. If so, return the inverse matrix
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        
        ## Following steps are executed when inverse matrix hasn't been made
        
        ## Get (not yet inverted) matrix
        data <- x$get()
        ## Make inverse matrix using built-in solve() function
        im <- solve(data, ...)
        ## set the inverse matrix
        x$setinverse(im)
        ## retrun inverse matrix
        im
}
