##@Kyle Main: https://github.com/kamain87
## 08/13/2015

## Functions that cache the inverse of a matrix

## Example usage:
## source('cachematrix.R')
## m <- makeCacheMatrix(matrix(c(1, 0, 0, 1), c(2, 2)))
## cacheSolve(m)
##[,1] [,2]
##[1,]    1    0
##[2,]    0    1

## Create a special "matrix", which is a list containing
## a function to
##   - set <- set the value of the matrix
##   - get <- get the value of the matrixoptions(error = recover)
##   - set inverse <- set the value of the inverse matrix
##   - getinverse <- get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
        
}


## Take the inverse of the matrix that was created using the above function. If cached result is stored from makeCacheMatrix above, return it.If not,
##compute the inverse, and set it in the cache using setinverse() function from above.

cacheSolve <- function(x, ...) {
      
        i <- x$getinverse()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
          ## Return a matrix that is the inverse of 'x'
        i
}
