## Put comments here that give an overall description of what your
## functions do

## This function caches a matrix in a list, along with functions to 
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(emm = matrix()) {
    # check that matrix 'square', since only square matrices are invertible
    dimEmm <- dim(emm)
    if (dimEmm[1] != dimEmm[2])
    {
        message("Matrix is not square. Cannot solve for inverse")
    }
    ivrse <- NULL
    set <- function(y) {
        emm <<- y
        ivrse <<- NULL
    }
    get <- function() emm
    setiVerse <- function(solve) ivrse <<- solve
    getiVerse <- function() ivrse
    list(set = set, get = get,
         setiVerse = setiVerse,
         getiVerse = getiVerse)
}


## This function attempts to retrieve the inverse of a matrix from a supplied list
## If the inverse was retrieved, an appropriate message is displayed, and the inverse returned
## If the inverse was not stored, the matrix is retrieved from the list, the inverse is calculated, and returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the matrix stored in 'x'
        ivrse <- x$getiVerse()
        if(!is.null(ivrse)) {
           message("getting cached data")
           return(ivrse)                  
        }
        data <- x$get()
        ivrse <- solve(data,...)
        x$setiVerse(ivrse)
        ivrse
}
