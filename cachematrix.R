## Put comments here that give an overall description of what your
## functions do



#Creates an object that represents a matrix. Takes a matrix as parameter
#and saves it in x. This value can be re-set using get function: x$set(matrix)
#and retrieved using get function: x$get().
#Also saves the value of its inverse, this value is set calling setinv function:
#x$setinv(inverse), and retrieved calling getinv fuction: x$getinv().
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(mtx){
                x <<- mtx
                inv <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse
        
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function recieves a makeCacheMatrix object as a parameter and retrieves
# its data. In case tha inverse operation of that object is NULL it is
# calculated it using solve function and saves the result in the parameter data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        inv <- x$setinv(solve(x$get()))
        inv
}
