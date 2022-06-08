import numpy as np
import scipy.linalg as scipy


def dot(A, B, C, message=False):
    """
    Implements fast matrix multiplication of 3 matrices X = ABC
    From left: X = (AB)C. From right: X = A(BC). This function
    calculates which is faster, and outputs the result.

    Parameters
    -----------
    A:          First matrix
    B:          Multiplied with 2nd matrix
    C:          Multiplied with 3rd matrix
    message:    Default = False. If True, doesn't output result, but
                outputs TRUE if left to right, else FALSE right to left.
    Returns
    -----------
    (A@B@C or message)
    """
    n, a_b = A.shape    # A and B share sizes. Size of A determines
    # final number of rows
    b_c = B.shape[1]
    c = C.shape[1]      # final columns

    left, right = dot_left_right(n, a_b, b_c, c)

    if message:
        return left <= right

    if left <= right:
        return A @ B @ C
    return A @ (B @ C)


def transpose(X, overwrite=True, dtype=None):
    """
    Provides X.T if dtype == float, else X.H (Conjugate transpose)

    Parameters
    -----------
    X :         Matrix to be decomposed. Has to be symmetric.
    Overwrite:  If overwritten, then inplace operation.
    Returns
    -----------
    X.T or X.H: Conjugate Tranpose (X)
    """
    if dtype is None:
        dtype = X.dtype
    if isComplex(dtype):
        if overwrite:
            return np.conjugate(X, out=X).T
        return X.conj().T
    return X.T
