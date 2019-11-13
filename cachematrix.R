## caches inverse of matrix
##second function checks if inverse is in cache, if it is then return it, if not then calc inverse


makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        #set the value of the Matrix
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        #get the value of the matrix       
         getMatrix <- function() {
                x
         }
         
         #set the value of the invertible matrix
        setInverse <- function(inverse) {
                invMatrix <<- inverse  
        }
        
        #get the value of invertible matrix
        getInverse <- function() {
                invMatrix                     
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setInverse = setInverse, 
             getInverse = getInverse)
        }
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #get the value of the invertible matrix from the makeCacheMatrix function
        invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL
                message("Getting Cached Invertible Matrix")   
                return(invMatrix)                             #return the invertible matrix
        }
        
        #if value of the invertible matrix is NULL then  
        MatrixData <- x$getMatrix()                     #get the original Matrix Data 
        invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
        x$setInverse(invMatrix)                         
        return(invMatrix)                               
}

TestMatrix <- matrix(1:16, 4, 4)
CacheMatrix <- makeCacheMatrix(TestMatrix)
cacheSolve(CacheMatrix)
