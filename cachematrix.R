#Example: Caching matrix object

#Description: This function creates a special "matrix" object that 
#can cache its inverse.

#makeCacheMatrix creates a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of inverse of the matrix
#4. get the value of inverse of the matrix

#Usage:
#   makeCacheMatrix(x)

#Arguments:
# x -> A matrix object.

#See Also:
# cacheSolve()     

#Exemple:
#  > x <- rbind(c(1, 1/2), c(1/2, 1))
#  > cm <- makeCacheMatrix(x)
#  > cm$get()
#     [,1] [,2]
#[1,]  1.0  0.5
#[2,]  0.5  1.0

makeCacheMatrix <- function(x = matrix()) {
    mx_inverse <- NULL
    set <- function(y) {
        x <<- y
        mx_inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mx_inverse <<- inverse
    getinverse <- function() mx_inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

#Calculate the inverse of the matrix

#Description: 
#This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix. 

#Usage:

#Arguments:
# x   -> A matrix object.
# ... -> extra arguments for future methods.

#Values:
#If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve should retrieve the inverse 
#from the cache.

# See also:
#   makeCacheMatrix()

#Exemple:
# Note there is no cache in the first run
# > cacheSolve(cm)
# [,1]       [,2]
# [1,]  1.3333333 -0.6666667
# [2,] -0.6666667  1.3333333

## Retrieving from the cache in the second run
# > cacheSolve(cm)
# getting cached data.
# [,1]       [,2]
# [1,]  1.3333333 -0.6666667
# [2,] -0.6666667  1.3333333

#Note: This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
    mx_inverse <- x$getinverse()
    if(!is.null(mx_inverse)) {
        message("getting cached data.")
        return(mx_inverse)
    }
    data <- x$get()
    mx_inverse <- solve(data)
    x$setinverse(mx_inverse)
    mx_inverse
}
