# Functions for math operations


def round_up(number, nearest):
    """Round a number up to an integer.

    Args:
        number: float. The value to be rounded up.
        nearest: Integer. The precision to round up to.
            e.g. 10, 100, 1000

    Returns:
        The number rounded up to the nearest integer
        with precision "nearest".
    """
    import math
    if number == 0:
        return nearest
    return int(math.ceil(number / nearest) * nearest)


def round_down(number, nearest):
    """Round a number down to an integer.

    Args:
        number: Float. The value to be rounded down.
        nearest: Integer. The precision to round down to.
            e.g. 10, 100, 1000

    Returns:
        The number rounded down to the nearest integer
        with precision "nearest".
    """
    import math
    if number == 0:
        return nearest
    return int(math.floor(number / nearest) * nearest)


def transpose_expr_matrix(filename, outfile=None, **kwargs):
    """Transpose expression matrix text file.

    Takes a text file expression matrix and transposes it, then
    writes it to a new tab-separated-value file.

    Args:
        filename: String path to file to be transposed.
        outfile: String filename to write the transposed matrix to.
            Defaults to filename with '_TRANSPOSED' appended before
            the file extension.
        **kwargs: Variable keyword arguments passed to pandas.read_csv().
            Suggested args include sep, header, and index_col.

    Returns:
        None. Writes a tab-separated-value file containing the transposed
        matrix to disk at the outfile location.
    """
    import pandas as pd

    data = pd.read_csv(filename, **kwargs)
    data = data.transpose()
    if outfile is None:
        filename = filename.split('.')
        if len(filename) == 1:
            outfile = filename[0] + '_TRANSPOSED'
        else:
            filename[-2] += '_TRANSPOSED'
            outfile = '.'.join(filename[:-1].append(filename[-1]))
    data.to_csv(path_or_buf=outfile, sep='\t')
    return


def pairwise_dist(pts, full=False):
    """Calculate pairwise distances from vector array.

    Takes a n x d numpy array of vectors, where n is the
    number of vectors and d is the number of dimensions, and
    calculates pairwise Euclidean distances.

    About as fast as sklearn.metrics.pairwise_distances and
    scipy.spatial.distance.squareform(scipy.spatial.distance.pdist)
    but if you only take the condensed distance matrix from pdist
    without squareform, pdist is WAY faster.

    Args:
        pts: numpy array containing vectors. Type must be some
            kind of numeric.
        full: bool indicating whether or not to return a full
            square distance array. The results are always
            symmetric and so only the upper triangular is
            returned by default.

    Returns:
        res: numpy array with n x n dimensions containing pairwise
            distances for the vectors in pts.
    """
    import numpy as np

    res = np.zeros((pts.shape[0], pts.shape[0]))
    for i in range(pts.shape[0]):
        res[i, i + 1:] = np.sqrt(np.sum(((pts[i + 1:, ] - pts[i, ]) ** 2),
                                        axis=1))
    if not full:
        return res
    else:
        return res.T + res


def compare_sparse(mat1, mat2, verbose=False):
    """Compare two scipy.sparse matrices for equivalency.

    Compare two scipy.sparse matrices for equivalency,
    short-circuits if they have different shapes or nnz,
    before comparing directly.

    Args:
        mat1: scipy.sparse matrix. First matrix to be compared.
        mat1: scipy.sparse matrix. Second matrix to be compared.
        verbose: bool. If True, will print a description of
            the results in addition to returning the result.

    Returns:
        bool indicating if the two matrices are equivalent or not.

    Raises:
        ValueError: if the two matrices are not scipy.sparse matrices.
    """
    import scipy.sparse
    if not (isinstance(mat1, scipy.sparse.spmatrix)
            or isinstance(mat2, scipy.sparse.spmatrix)):
        raise ValueError('Both mat1 and mat2 must be scipy sparse matrices.')
    if mat1.shape != mat2.shape:
        if verbose:
            print('mat1 and mat2 do not have the same shape.')
        return False
    elif mat1.nnz != mat2.nnz:
        if verbose:
            print('mat1 and mat2 do not have the same '
                  'number of non-zero entries.')
        return False
    else:
        if verbose:
            print('mat1 and mat2 have the same shape and number '
                  'of non-zero entries, but are not equivalent.')
        return (mat1 != mat2).nnz == 0


def is_sym(a, **kwargs):
    """Test if 2D array is symmetric.

    Args:
        a: object that can be coerced into a
            numpy 2D array.
        **kwargs: named arguments passed to numpy.allclose

    Returns:
        bool indicating if a is symmetric or not.
    """
    import numpy
    a = numpy.array(a)
    if len(a.shape) != 2:
        raise ValueError('a must be a 2D array')
    if a.shape[0] != a.shape[1]:
        raise ValueError('a must be a square array')
    return numpy.allclose(a, a.T, **kwargs)
