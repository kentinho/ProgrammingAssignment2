## Put comments here that give an overall description of what your
## functions do

## If you substitute matrix x for the makeCacheMatrix function, this make inverse of x and contain it.
## After that, you put this result on the cachematrix function, you take the result of what is the inverse of x.

makeCacheMatrix <- function(x = matrix()) { #substitute matrix x for the function
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, # make the outputs the list, so we use them available  
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(x, ...) { #... through x another functions
        m <- x$getinverse()
        if(!is.null(m)) { #when mean is cached 
                message("getting cached data") #this message print
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
