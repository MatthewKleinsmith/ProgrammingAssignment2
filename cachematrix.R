## These functions together create a matrix and return its inverse.
## If the inverse has already been computed, the inverse is returned from a cache.
## Otherwise, the inverse is computed, cached, and returned.

## Creates a special matrix that which can be thought of as a matrix with functions
## attached to it.

makeCacheMatrix <- function(matrix = matrix()) {
    
    inverse <- NULL
    
    set_matrix <- function(new_matrix) { ## Modifies the matrix and clears the cache.
        matrix <<- new_matrix
        inverse <<- NULL
    }
    
    get_matrix <- function() { ## Retreives the matrix.
        matrix
    }
    
    set_inverse <- function(new_inverse) { ## Modfies what's in the cache.
        inverse <<- new_inverse
    }
    
    get_inverse <- function() { ## Retrieves what's in the cache.
        inverse
    }
    
    list(set_matrix = set_matrix,
         get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)  
}

## Returns a matrix that is the inverse of the matrix in 'special_matrix'.

cacheSolve <- function(special_matrix, ...) {
    
    inverse <- special_matrix$get_inverse() ## Retrieves what's in the cache.
    
    if(!is.null(inverse)) { ## If the cache has something in it, return that thing.
        message("getting cached data")
        return(inverse)
    }
    
    ## If the cache is empty, do the following:
    
    matrix <- special_matrix$get_matrix() ## Retrieve the matrix from the special matrix object.
    
    inverse <- solve(matrix, ...) ## Compute the inverse.
    
    special_matrix$set_inverse(inverse) ## Cache the inverse.
    
    inverse ## Return the inverse.
}
