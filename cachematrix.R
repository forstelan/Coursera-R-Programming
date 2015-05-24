## These functions construct, store and compute matrix inverses.  Data caching is used to reduce
## resource and time consumption for repeated matrix inversions

## Creates and stores a cached matrix with its inverse and provides a collection of accessor and
## mutator functions for matrix data.
makeCacheMatrix <- function(data = matrix())
    {
    cached_data <- data;
    cached_inverse <- NULL;

    get_data <- function()
        {
        return(cached_data);
        }

    get_inverse <- function()
        {
        return(cached_inverse);
        }

    set_data <- function(new_data)
        {
        cached_data <<- new_data;
        cached_inverse <<- NULL;
        }

    set_inverse <- function(new_inverse)
        {
        cached_inverse <<- new_inverse;
        }

    return(list(get_data = get_data,
                get_inverse = get_inverse,
                set_data = set_data,
                set_inverse = set_inverse));
    }

## Return the inverse matrix of a given cached matrix.  If the inverse has already been computed
## and stored, then the value of the stored inverse is returned. Otherwise, the inverse is computed
## and stored with the given matrix, and the inverse matrix is returned.
cacheSolve <- function(matrix, ...)
    {
    ## Return a matrix that is the inverse of 'matrix'
    matrix_data <- matrix$get_data();
    matrix_inverse <- matrix$get_inverse();

    if(!(is.null(matrix_inverse)))
        {
        print("Accessing cached inverse data...");
        }
    else
        {
        print("Computing inverse data");
        matrix_inverse <- solve(matrix_data);
        matrix$set_inverse(matrix_inverse);
        }

    return(matrix_inverse);
    }
