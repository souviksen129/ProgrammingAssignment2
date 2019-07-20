## This is to create two functions to cache inverse of a matrix


## Creating a special matrix object
makeCacheMatrix <- function( m = matrix() ) {
        
        ## Initialize the inverse property
        init <- NULL
        
        ## Method to set the matrix
        set <- function( matrix ) {
                m <<- matrix
                init <<- NULL
        }
        
        ## Method the get the matrix
        get <- function() {
                ## Return the matrix
                m
        }
        
        ## Method to set the inverse of the matrix
        setInverse <- function(inverse) {
                init <<- inverse
        }
        
        ## Method to get the inverse of the matrix
        getInverse <- function() {
                ## Return the inverse property
                init
        }
        
        ## Return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Computing the inverse of the matrix returned by "makeCacheMatrix"

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## return the inverse if its already set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculating inverse
        m <- solve(data) %*% data
        
        ## Setting the inverse to the object
        x$setInverse(m)
        
        ## Return matrix
        m
}