## This is minimalist approach, as far as I understood the assignment
## it is about proper comments proving undestanding of subject, 
## as the code provided is sufficient to get a solution with only minor changes


#Reads in a matrix and returns a list of functions:
#returning given matrix, changing saved matrix, setting inverse matrix and getting inverse matrix
#as values are set in function's environment, they can be easily approached

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y) {
                x <<- y ## when used sets value of x for parent:whole makeCacheMatrix function      
                inv <<- NULL #when new matrix is set, resets inverse matrix to NULL
        }
        get <- function() x #returns x which is matrix set for this functions instance
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## works on lists of functions like makeCacheMatrix. 
#Reads saved inverse matrix, in case it is not empty returns message and the matrix
#in case there isn't any, reads matrix, finds inverse matrix solution and sets it in makeCaheMatrix type object

cacheSolve <- function(x, ...) {
        
                inv <- x$getinverse()
                if(!is.null(inv)) {
                        message("getting cached inverse matrix")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data, ...)
                x$setinverse(inv)
                inv        ## Return a matrix that is the inverse of 'x'
 }
