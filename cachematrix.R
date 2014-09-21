## The overall goal of this code is to save time in lengthy operations such as 
## calculating the inverse of a matrix. This objective is achieved through 
## two different functions: one that will cache the result of the operation we want 
## to do, and one that performs the actual operation or retrieves the cached result.

## The following function requires a square matrix as input and 
## creates a list of 4 elements (i.e. functions) that will:
## a) set the value of the matrix
## b) get the value of the matrix
## c) set the value of the inverse of the matrix
## d) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) im <<-inverse
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function applied to the list created with "makeCacheMatrix"
## returns the inverse of the matrix used as input in the above function, either
## by calculating it right away or by fetching the cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if (!is.null(im)) {
                message("getting cached inversed matrix")
                return(im)
        } else {
        ## Calcultating the inverse of 'x'        
                im <- solve(x$get())
                x$setinverse(im)
                return(im)
        }
}
