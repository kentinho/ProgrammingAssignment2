
## If you put matrix x on the makeCacheMatrix function, this make inverse of x and contain it.
## After that, you put this result on the cacheSolve function, you take the result of what is the inverse of x.

## Step1: You make a matrix which have inverse.
## ex.) x <- matrix(runif(9),3,3)

## Step2: Put x on the makeCacheMatrix function
## ex.) aa <- makeCacheMatrix(x)

## Step3: Put aa on the cacheSolve function
## ex.) bb <- cacheSolve(aa)

## Step4: You execute bb, and you get inverse of x matrix. 

## Step5: You can realize that it's matrix is correctly inverse of x.
## x %*% bb or bb %*% x

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
        if(!is.null(m)) { #when inverse of x is cached 
                message("getting cached data") #this message print
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
