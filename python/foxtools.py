"""
General collection of Python classes and methods that may be generally useful
across specific project settings. Because this is a collection including
methods for any field, the imports are distributed to the methods that need
them to reduce import time to only the functions that are used. Python is
good about caching and not doubling imports for different namespaces, so this
shouldn't be a problem. If a list of dependencies is needed, run the shell
command 'grep "^\s*import" foxtools.py | sort -u'.

General
-------
  smart_dir
  print_globals
  get_timestamp
  get_uuid
  gunzip
  extract_tar
  bytes_to_human
  human_to_bytes
  invalid_arg_err_msg
  get_exception_string
  convert_seconds

Math
----
  round_up
  round_down
  transpose_expr_matrix
  pairwise_dist
  compare_sparse

Plotting
--------
  year_to_num
  get_group_plot_data

Tabular Data
------------
convert_dict_to_dataframe
mult_value_counts

CLI Output
----------
  propeller
  pretty_str_list
  pretty_str_text
  get_ruler
  color_print
  print_timedelta
  print_h5_tree

CLI Input
---------
  get_int
  get_float
  get_yes_or_no
  get_command
"""

__version__ = '0.9'
__author__ = 'Nathan Fox'

# Global Imports
import sys


################################################################################
#                                   General                                    #
################################################################################

def smart_dir(obj):
    """Sorts dir() output intelligently.

    dir(obj) returns a list of attributes for an object. However, it
    is generally unusable because there are many hidden attributes
    that clog the output and the desired user-facing attributes are
    hard to find. smart_dir uses standard naming conventions to sort
    the output and present it in a human-readable way. It sorts
    attributes into 6 categories based on the following rules:

      Python Reserved Methods:
          Name starts and ends with '__' and has 'function' or
          'method' in the description.
      Python Reserved Other:
          Name starts and ends with '__' and does not have
          'function' or 'method' in the description.
      Internal Methods:
          Name starts with '_' and has 'function' or 'method'
          in the description.
      Internal Other:
          Name starts with '_' and does not have 'function'
          or 'method' in the description.
      Public Methods:
          Name does not meet any of the above criteria
          and has 'function' or 'method' in the description.
      Public Other:
          Name does not meet any of the above criteria
          and has 'function' or 'method' in the description.

    Args:
        obj: Any python object.

    Returns:
        Nothing is returned, however the sorted output
        is printed to the standard out.
    """
    python_reserved_methods = []
    python_reserved_other = []
    internal_methods = []
    internal_other = []
    public_methods = []
    public_other = []
    attr = dir(obj)
    max_name_width = 0
    for a in attr:
        a_type = str(type(getattr(obj, a))).split('\'')[1]
        if len(a) > max_name_width:
            max_name_width = len(a)
        if a[:2] == '__' and a[-2:] == '__':
            if 'method' in a_type or 'function' in a_type:
                python_reserved_methods.append((a, a_type))
            else:
                python_reserved_other.append((a, a_type))
        elif a[:1] == '_':
            if 'method' in a_type or 'function' in a_type:
                internal_methods.append((a, a_type))
            else:
                internal_other.append((a, a_type))
        else:
            if 'method' in a_type or 'function' in a_type:
                public_methods.append((a, a_type))
            else:
                public_other.append((a, a_type))
    python_reserved_methods.sort(key=lambda x: x[0])
    python_reserved_other.sort(key=lambda x: x[0])
    internal_methods.sort(key=lambda x: x[0])
    internal_other.sort(key=lambda x: x[0])
    public_methods.sort(key=lambda x: x[0])
    public_other.sort(key=lambda x: x[0])
    obj_type = str(type(obj)).split('\'')[1]
    print('------' + ('-' * (len(obj_type) + 4)))
    print(f'  Type: {obj_type}')
    print('------' + ('-' * (len(obj_type) + 4)))
    print()
    if len(python_reserved_methods) != 0:
        print('Python Reserved Methods')
        print('=======================')
        for a, a_type in python_reserved_methods:
            print(f'  {a:{max_name_width}s} : {a_type}')
        print()
    if len(python_reserved_other) != 0:
        print('Python Reserved Non-Methods')
        print('===========================')
        for a, a_type in python_reserved_other:
            print(f'  {a:{max_name_width}s} : {a_type}')
        print()
    if len(internal_methods) != 0:
        print('Internal Methods')
        print('================')
        for a, a_type in internal_methods:
            print(f'  {a:{max_name_width}s} : {a_type}')
        print()
    if len(internal_other) != 0:
        print('Internal Non-Methods')
        print('====================')
        for a, a_type in internal_other:
            print(f'  {a:{max_name_width}s} : {a_type}')
        print()
    if len(public_methods) != 0:
        print('Public Methods')
        print('==============')
        for a, a_type in public_methods:
            print(f'  {a:{max_name_width}s} : {a_type}')
        print()
    if len(public_other) != 0:
        print('Public Non-Methods')
        print('==================')
        for a, a_type in public_other:
            print(f'  {a:{max_name_width}s} : {a_type}')
        print()
    return


def print_globals(printed=True, width=16):
    """Summarizes non-internal global variables.

    Iterates over all variables in the global namespace,
    drops all variables with a leading underscore (typically
    indicates internal, protected, or private variables),
    and prints or returns them.

    Args:
        print: bool. Indicates if the name/type tuples
            should be returned in a list, or printed.
        width: int. Passed to the format string to
            to provide field width for the variable name.
            Only valid if printing.

    Returns:
        list of name/type tuples for all the found variables
        if print is not True. None otherwise.
    """
    import inspect
    caller_globals = dict(inspect.getmembers(
                              inspect.stack()[1][0])
                     )["f_globals"]
    variables = [(g[0], type(g[1]))
                 for g in caller_globals.items() if not g[0].startswith('_')]
    if print:
        for n in [f'{g[0]:{width}s} : {g[1]}' for g in variables]:
            print(n)
    else:
        return variables


def get_timestamp(mode='both', long=True):
    """Generates string timestamp.

    Generates a string version of a datestamp, a timestamp,
    or both combined. The three versions are in the strftime()
    formats:
        date: '%y%m%d'
        time: '%H%M%S'
        both: '%y%m%d_%H%M%S'

    Args:
        mode: String. Must be one of {'both', 'date', 'time'}
        long: Boolean indicating whether to do a long format
            appropriate for log files, or a short format
            appropriate for filenames.

            Examples
            --------
            long  : 2019-01-01 13:03:24
            short : 190101_130324

    Returns:
        String timestamp of the date, time, or both. See above
        for format.

    Raises:
        ValueError: Invalid value passed as mode argument
    """
    import datetime as dt

    now = dt.datetime.now()
    if long:
        date_stamp = now.strftime('%Y-%m-%d')
        time_stamp = now.strftime('%H:%M:%S')
        datetime_stamp = date_stamp + ' ' + time_stamp
    else:
        date_stamp = now.strftime('%y%m%d')
        time_stamp = now.strftime('%H%M%S')
        datetime_stamp = date_stamp + '_' + time_stamp
    if mode == 'both':
        return datetime_stamp
    elif mode == 'date':
        return date_stamp
    elif mode == 'time':
        return time_stamp
    else:
        raise ValueError('mode must be one of {\'both\', \'date\', \'time\'}!')


def get_uuid():
    """Generates string version of a new UUID4."""
    import uuid

    return str(uuid.uuid4())


def gunzip(files, remove=True):
    """Gunzips files

    Given a list of file paths, gunzips them, and optionally
    removes the .gz files.

    Args:
        files: Either a string, or a list of strings. Each string
            should be a path to a gzipped file to be gunzipped.
        remove: Boolean. If True, the gzipped files will be
            removed, leaving only the gunzipped files.
    """
    import os
    import gzip
    import shutil

    if isinstance(files, str):
        file = files
        files = []
        files.append(file)
    for f in files:
        path, filename = os.path.split(f)
        if filename[-3:] == '.gz':
            new_filename = filename[:-3]
        else:
            new_filename = filename
        with gzip.open(os.path.join(path, filename), 'rb') as f_in:
            with open(os.path.join(path, new_filename), 'wb') as f_out:
                shutil.copyfileobj(f_in, f_out)
        if remove:
            os.remove(os.path.join(path, filename))
    return


def extract_tar(tar_file, outpath="", remove=True):
    """Extracts a tar archive.

    Extracts a tar archive to the given path and filters for
    problematic file names. If a file has a problematic file
    name (e.g. '/' or '..' in the name), it will not be extracted
    and a notice will be printed to standard out.

    Args:
        tar_file: String. The path to the tar archive to be extracted.
        outpath: String. The path to the folder where the extracted
            files should be written. If default (''), the files will
            be extract to the Current Working Directory, NOT the folder
            holding the tar archive.
        remove: Boolean. If True, the original tar archive will be
            deleted.
    """
    import os
    import tarfile

    with tarfile.open(tar_file) as tar:
        tar_member = tar.next()
        skipped_members = []
        while True:
            if tar_member is None:
                break
            forbidden_strings = ('..', '/')
            if any(forb in tar_member.name for forb in forbidden_strings):
                skipped_members.append(tar_member.name)
                continue
            else:
                tar.extract(tar_member, path=outpath)
            tar_member = tar.next()
    if remove:
        os.remove(tar_file)
    for sm in skipped_members:
        print(f'WARNING: {sm} not extracted due to potentially dangerous '
              'filename. Please extract manually!')
    return


def bytes_to_human(n, format='%(value).1f %(symbol)s', symbols='customary'):
    """
    Convert n bytes into a human readable string based on format.
    symbols can be either "customary", "customary_ext", "iec" or "iec_ext",
    see: http://goo.gl/kTQMs

      >>> bytes_to_human(0)
      '0.0 B'
      >>> bytes_to_human(0.9)
      '0.0 B'
      >>> bytes_to_human(1)
      '1.0 B'
      >>> bytes_to_human(1.9)
      '1.0 B'
      >>> bytes_to_human(1024)
      '1.0 K'
      >>> bytes_to_human(1048576)
      '1.0 M'
      >>> bytes_to_human(1099511627776127398123789121)
      '909.5 Y'

      >>> bytes_to_human(9856, symbols="customary")
      '9.6 K'
      >>> bytes_to_human(9856, symbols="customary_ext")
      '9.6 kilo'
      >>> bytes_to_human(9856, symbols="iec")
      '9.6 Ki'
      >>> bytes_to_human(9856, symbols="iec_ext")
      '9.6 kibi'

      >>> bytes_to_human(10000, "%(value).1f %(symbol)s/sec")
      '9.8 K/sec'

      >>> # precision can be adjusted by playing with %f operator
      >>> bytes_to_human(10000, format="%(value).5f %(symbol)s")
      '9.76562 K'

    Bytes-to-human / human-to-bytes converter.
    Based on: http://goo.gl/kTQMs
    Working with Python 2.x and 3.x.

    Author: Giampaolo Rodola' <g.rodola [AT] gmail [DOT] com>
    License: MIT

    Args:
        n: int. Number of bytes to be converted.
        format: str. Format string for the output. "%(value)"
            and "%(symbol)" cannot be changed.
        symbols: str. The label set to use. Options are below.
            'customary'     : ('B', 'K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y'),
            'customary_ext' : ('byte', 'kilo', 'mega', 'giga', 'tera', 'peta',
                               'exa', 'zetta', 'iotta'),
            'iec'           : ('Bi', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi',
                               'Ei', 'Zi', 'Yi'),
            'iec_ext'       : ('byte', 'kibi', 'mebi', 'gibi', 'tebi',
                               'pebi', 'exbi', 'zebi', 'yobi')
    Raises:
        ValueError: n < 0 or symbols is not a valid options.
        AssertionError: Invalid symbols option got past the validation check.
    """
    SYMBOLS = {
        'customary'     : ('B', 'K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y'),
        'customary_ext' : ('byte', 'kilo', 'mega', 'giga', 'tera', 'peta',
                           'exa', 'zetta', 'iotta'),
        'iec'           : ('Bi', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi',
                           'Ei', 'Zi', 'Yi'),
        'iec_ext'       : ('byte', 'kibi', 'mebi', 'gibi', 'tebi',
                           'pebi', 'exbi', 'zebi', 'yobi')
    }
    n = int(n)
    if n < 0:
        raise ValueError("n < 0")
    if symbols not in SYMBOLS.keys():
        raise ValueError("symbols is invalid")
    symbols = SYMBOLS[symbols]
    prefix = {}
    if symbols.startswith('customary'):
        for i, s in enumerate(symbols[1:]):
            prefix[s] = 1000 ** (i+1)
    elif symbols.startswith('iec'):
        for i, s in enumerate(symbols[1:]):
            prefix[s] = 1024 ** (i+1)
    else:
        raise AssertionError("invalid symbols arg got past input validation")
    for symbol in reversed(symbols[1:]):
        if n >= prefix[symbol]:
            value = float(n) / prefix[symbol]
            return format % locals()
    return format % dict(symbol=symbols[0], value=n)


def human_to_bytes(s):
    """
    Attempts to guess the string format based on default symbols
    set and return the corresponding bytes as an integer.
    When unable to recognize the format ValueError is raised.

      >>> human_to_bytes('0 B')
      0
      >>> human_to_bytes('1 K')
      1024
      >>> human_to_bytes('1 M')
      1048576
      >>> human_to_bytes('1 Gi')
      1073741824
      >>> human_to_bytes('1 tera')
      1099511627776

      >>> human_to_bytes('0.5kilo')
      512
      >>> human_to_bytes('0.1  byte')
      0
      >>> human_to_bytes('1 k')  # k is an alias for K
      1024
      >>> human_to_bytes('12 foo')
      Traceback (most recent call last):
          ...
      ValueError: can't interpret '12 foo'

    Bytes-to-human / human-to-bytes converter.
    Based on: http://goo.gl/kTQMs
    Working with Python 2.x and 3.x.

    Author: Giampaolo Rodola' <g.rodola [AT] gmail [DOT] com>
    License: MIT

    Args:
        s: str. Human readable byte-type string to be converted
            to an integer number of bytes.

    Returns:
        An integer number of bytes equivalent to the input string.

    Raises:
        ValueError: When the input string cannot be interpreted.
        AssertionError: If an invalid SYMBOLS key was found internally.
    """
    SYMBOLS = {
        'customary'     : ('B', 'K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y'),
        'customary_ext' : ('byte', 'kilo', 'mega', 'giga', 'tera', 'peta',
                           'exa', 'zetta', 'iotta'),
        'iec'           : ('Bi', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi',
                           'Ei', 'Zi', 'Yi'),
        'iec_ext'       : ('byte', 'kibi', 'mebi', 'gibi', 'tebi',
                           'pebi', 'exbi', 'zebi', 'yobi')
    }
    init = s
    num = ""
    while s and s[0:1].isdigit() or s[0:1] == '.':
        num += s[0]
        s = s[1:]
    num = float(num)
    letter = s.strip()
    for name, sset in SYMBOLS.items():
        if letter in sset:
            break
    else:
        if letter == 'k':
            # treat 'k' as an alias for 'K' as per: http://goo.gl/kTQMs
            sset = SYMBOLS['customary']
            letter = letter.upper()
        else:
            raise ValueError("can't interpret %r" % init)
    prefix = {sset[0]:1}
    if name.startswith('customary'):
        for i, s in enumerate(sset[1:]):
            prefix[s] = 1000 ** (i+1)
    elif name.startswith('iec'):
        for i, s in enumerate(sset[1:]):
            prefix[s] = 1024 ** (i+1)
    else:
        raise AssertionError("unexpected key in SYMBOLS")
    return int(num * prefix[letter])


def invalid_arg_err_msg(post_validation=False, **kwargs):
    """Construct error message for invalid argument.

    Takes a named argument and constructs an error message for it
    for the case that the passed value is invalid. Can target one
    of two audiences: user and dev.

    Examples
    --------
    >>> print(invalid_arg_err_msg(post_validation=FALSE, integer='abc'))
    Invalid value "abc" for the "integer" argument.

    >>> invalid_arg_err_msg(post_validation=TRUE, integer='abc')
    Invalid value "abc" for the "integer" argument got past input validation.

    Args:
        post_validation: Boolean flag indicating if the message should
            include " got past input validation" at the end.
        **kwargs: Must be a single named argument. The name and value
            will be substituted into the error message.

    Returns:
        A string containing the error message.

    Raises:
        ValueError: If **kwargs is not a single named argument.
    """
    import inspect
    function_name = inspect.getframeinfo(inspect.currentframe()).function
    if len(kwargs) != 1:
        raise ValueError(f'{function_name} requires a single named argument.')
    arg = list(kwargs.keys())[0]
    val = kwargs[arg]
    if post_validation:
        end_of_msg = ' got past input validation'
    else:
        end_of_msg = ''
    error_msg = (f'Invalid value "{val} for the "{arg}" '
                 'argument{end_of_msg}.')
    return error_msg


def get_exception_string(exc):
    """Convert an Exception object to a string.

    Converts an Exception object to a string, equivalent to
    the output when an uncaught Exception is raised during execution.

    Args:
        exc: Exception or subclass of Exception. The Exception to
            be converted.

    Returns:
        A string identical to the output when the Exception is uncaught.
    """
    import traceback
    tb_str = ''.join(traceback.format_exception(etype=type(exc),
                                                value=exc,
                                                tb=exc.__traceback__))
    return tb_str


def convert_seconds(seconds, days=False):
    """Convert seconds to hours, minutes, and seconds.

    Args:
        seconds: integer. Number of seconds to be converted.
        days: boolean. If True, a 4-member tuple is returned
            with days.

    Returns:
        A tuple of 3 integers. Seconds converted to
        hours, minutes, seconds. If days=True, then days is
        included and a 4 member tuple is returned.
    """
    m, s = divmod(seconds, 60)
    h, m = divmod(m, 60)
    if days:
        d, h = divmod(h, 24)
        return d, h, m, s
    else:
        return h, m, s


################################################################################
#                                     Math                                     #
################################################################################

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
    if outfile == None:
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
                                        axis = 1))
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

################################################################################
#                                   Plotting                                   #
################################################################################

def year_to_mdate(year):
    """Convert years to matplotlib dates.

    Convert years (e.g. 2002) to matplotlib dates. Matplotlib's
    support for plotting datetime objects is generally horrible
    and so this is a useful function for manually plotting them.

    Args:
        year: Integer, String, or iterable of either. All
            arguments must be a 4-digit year. This means
            that years before 1000 cannot be passed as an
            integer.

    Returns:
        Either a single float or a numpy array of floats,
        depending on the dimensions of the input. The floats
        are the matplotlib date versions of the years
        given as arguments.
    """
    import numpy as np
    import matplotlib.dates as mdates

    if type(year) is str:
        year = int(year)
        year = f'{year}-01-01'
        return mdates.datestr2num(year)
    elif type(year) is int:
        year = f'{year}-01-01'
        return mdates.datestr2num(year)
    else:
        year = list(year)
        year = np.array(list(map(lambda x: f'{x}-01-01', year)))
        year = mdates.datestr2num(year)
        return year


def get_group_plot_data(df, groupby, value, convert_date=True):
    """Group a column by another column.

    Given a pandas DataFrame, split the values of one column
    by the values of another, typically categorical, column.
    This is helpful for plotting with matplotlib methods,
    but not for seaborn.

    Args:
        df: Pandas DataFrame. The source for the columns.
        groupby: The name of the column containing the categorical
            variable. The other column will be split by the unique
            values of this column.
        value: The name of the column containing the values to
            split. This column will be divided into groups based
            on the corresponding values of groupby.
        convert_date: If True, and the value column is a datetime
            dtype, the returned values will be floats resulting
            from a conversion to matplotlib dates. If False, they
            will be left as numpy.datetime64 objects.

    Returns:
        A 2-member tuple:
            1. A ragged array with N columns, where N is the number
               of unique values in the groupby column. Each column
               of the ragged array contains all of the values of
               value that had the same corresponding value of
               groupby.
            2. A list of Strings, containing the names of the
               N columns in the returned ragged array.
    """
    import numpy as np
    import pandas as pd
    import matplotlib.dates as mdates

    df = df.pivot(columns=groupby, values=value)
    vectors = []
    names = []
    for col in df.columns:
        if convert_date and pd.api.types.is_datetime64_any_dtype(df[col]):
            vectors.append(mdates.date2num(df[col].dropna()))
        else:
            vectors.append(np.array(df[col].dropna()))
        names.append(col)
    return (vectors, names)


################################################################################
#                                 Tabular Data                                 #
################################################################################
def convert_dict_to_dataframe(dict_of_dicts):
    """Convert dict of dicts to a Pandas DataFrame.

    Convert a dictionary of dictionaries to a Pandas
    DataFrame, where the keys of the outer dict become
    the Index for the resultant DataFrame. The keys
    of the inner dicts must all be identical and
    become the columns of the resultant DataFrame.

    Example:
        >>> test_dict
        {'Sample_1': {'sex': 'F', 'age': 13},
         'Sample_2': {'sex': 'M', 'age': 14}}
        >>> convert_dict_to_dataframe(test_dict)
                  age sex
        Sample_1   13   F
        Sample_2   14   M

    Args:
        dict_of_dicts: Dict. The keys of this dict
            will be the Index of the dataframe. The values
            must be dicts with identical keys. The inner keys
            will be the column names of the dataframe. The
            inner values will be the values of the dataframe.

    Returns:
        A Pandas DataFrame holding the converted dict of dicts.

    Raises:
        ValueError: If all inner dicts don't have identical keys.
    """
    import pandas as pd

    first_dict = True
    index = []
    for k, v in dict_of_dicts.items():
        if first_dict:
            ref_keys = pd.Series(sorted(list(v.keys())))
            new_dict = {ref_key:[] for ref_key in ref_keys}
            first_dict = False
        index.append(k)
        test_keys = pd.Series(sorted(list(v.keys())))
        if (test_keys.shape[0] != ref_keys.shape[0]
            or (test_keys != ref_keys).any()):
            raise ValueError('Not all inner dicts have the same keys!')
        for k2, v2 in v.items():
            new_dict[k2].append(v2)
    new_df = pd.DataFrame(new_dict, index = index)
    return new_df


def mult_value_counts(df, cols=[], printed=True, **kwargs):
    """Print the value_counts output for given columns in a DataFrame.

    pandas.DataFrame.value_counts() gives the counts for all combinations
    of unique values in the columns given. This simply prints the
    value_counts output for each column sequentially.

    Args:
        df: pandas DataFrame. Contains columns for output.
        cols: list. The columns to be printed. Empty list defaults to all.
        printed: bool. If True, output will be printed. Otherwise, returned
            as a string.
        **kwargs: Passed to the pandas.Series.value_counts() method.

    Returns:
        None if printed; the output string otherwise.
    """
    if not cols:
        cols = df.columns
    outputs = []
    for col in cols:
        vc = df[col].value_counts()
        if vc.shape[0] == 0:
            continue
        output = str(df[col].value_counts(**kwargs)).splitlines()
        metadata_line = output[-1]
        output = '  ' + '\n  '.join(output[:-1])
        metadata = {}
        for item in metadata_line.split(','):
            k, v = item.split(':')
            metadata[k.strip().lower()] = v.strip()
        # metadata = {k.strip(): v.strip() for k, v in item.split(':') in metadata.split(',')}
        header = []
        if 'name' in metadata.keys():
            header.append(metadata['name'])
        header.append(f'({str(df[col].dtype)}) |')
        header.append(f'Length: {vc.shape[0]}')
        header = ' '.join(header)
        header_sep = '-' * len(header)
        outputs.append('\n'.join([header, header_sep, output]))
    final_output = '\n' + '\n\n\n'.join(outputs)
    if printed:
        print(final_output)
        return
    else:
        return final_output


################################################################################
#                                  CLI Output                                  #
################################################################################

def propeller(duration, frequency):
    """Spin a propeller on the command line.

    Displays a single character oscillating propeller
    on the command line.

    The characters it displays in sequence are:
    |
    /
    \
    |
    /
    \

    Args:
        duration: Float. Number of seconds the propeller will spin.
        frequency: Float. Number of full rotations per second.
    """
    import time

    step_duration = 1 / (8 * frequency)
    chars = ['|', '/', '—', '\\', '|', '/', '—', '\\']
    counter = 0
    start = time.perf_counter()
    while time.perf_counter() < (start + duration):
        print('\r' + chars[counter], end = '')
        counter = (counter + 1) % 8
        time.sleep(step_duration)
    print('\rDone!')


def pretty_str_list(list, width=50, indent='', sep=', ', one_per_line=False):
    """Creates pretty string of a list.

    Creates a pretty string of the items in a list within
    the given width, with the given indent in front of each
    line. Unlike pprint, this method does not quote or
    annotate items in any way. This makes it better for
    printing lists for display, rather than for examination.
    Will produce unexpected behavior if the string version
    of a list element contains a new line character.

    For example:
        >>> test = ['apple', 'banana', 'cherry', 'date',
                   'elderberry', 'fig', 'grapefruit']
        >>> test
        ['apple', 'banana', 'cherry', 'date', 'elderberry', 'fig', 'grapefruit']
        >>> print(pprint.pformat(test, width = 30, indent = 4))
        [    'apple',
             'banana',
             'cherry',
             'date',
             'elderberry',
             'fig',
             'grapefruit']
        >>> print(pretty_str_list(test, width = 30, indent = '    '))
            apple, banana, cherry,
            date, elderberry, fig,
            grapefruit

    Args:
        list: The list to convert into a pretty string. Items are
            automatically converted to their string representation,
            so, within reason, the list items do not have to be
            strings.
        width: Integer. The maximum character width of each line,
            including the indent.
        indent: A string to prepend to each line. For example, to
            indent the whole output, use '    ' (4 whitespaces).
        sep: A string to use to separate entries on a single line.
        one_per_line: Boolean. If True, all elements will be printed
            on their own lines, without commas, regardless of
            element length.

    Returns:
        A single string containing appropriate new lines and indents
        that can be printed. Contains a pretty string version of the
        list.
    """
    import string
    # Building a string in a loop via concatenation is bad practice.
    # Any operations that add to the end of the pretty string are
    # done by adding substrings to a list, then joining at the end.
    # This operation takes linear time, rather than quadratic.
    # However, string concatenations that take place on a single
    # item, such as adding the ", " are left as string concatenations
    # for clarity, since they do not involve the main pretty string.

    line_counter = 0
    line_width = 0
    str_out = []
    str_out.append(indent)
    width -= len(indent)
    # sep_end is all trailing whitespace in sep
    # sep_beg is everything before that
    sep_beg = ''
    sep_end = sep
    for i in range(len(sep) - 1, -1, -1):
        if sep[i] in string.whitespace:
            continue
        else:
            sep_beg = sep[:i + 1]
            if i == len(sep) - 1:
                sep_end = ''
            else:
                sep_end = sep[i + 1:]
            break
    for idx, item in enumerate(list):
        item = str(item)
        if idx < len(list) - 1 and (not one_per_line):
            item += sep_beg
        if one_per_line:
            str_out.append(item)
            if idx < len(list) - 1:
                str_out.append('\n' + indent)
            continue
        # Only runs if one_per_line is False
        if len(item) >= width:
            if line_counter != 0:
                str_out.append('\n' + indent)
            str_out.append(item)
            str_out.append('\n' + indent)
            line_counter += 2
            line_width = 0
        else:
            if line_width == 0:
                # Because of above if statement, I know that the
                # len(item) is currently < width
                str_out.append(item)
                line_width += len(item)
            elif len(sep_end) + len(item) + line_width > width:
                str_out.append('\n' + indent)
                str_out.append(item)
                line_counter += 1
                line_width = len(item)
            else:
                if line_counter == 0 and line_width == 0:
                    pass
                else:
                    item = sep_end + item
                str_out.append(item)
                line_width += len(item)
    str_out = ''.join(str_out)
    return str_out


def pretty_str_text(text, width=80, indent=''):
    """Returns pretty string of string.

    Given a string, returns a prettified, printable
    version of the same string. This is a special case
    of pretty_str_list().

    Args:
        text: string. The string to be converted to a
            pretty printable string.
        width: integer. The maximum character width of
            the pretty string.
        indent: string. A string to prepend to every line
            of the pretty string.

    Returns:
        A printable version of the text string. It conforms
        to the width restriction and adds the indent to the
        beginning of each line.
    """
    pretty_str = pretty_str_list(text.split(' '), width=width, indent=indent,
                                 sep=' ', one_per_line=False)
    return pretty_str


def get_ruler(m, n=None, ticks=True, indent='', ticks_above=True):
    """Get a width ruler for the command line.

    Get a string that, when printed on the command line, provides
    a ruler to measure width in units of characters.

    For example, a ruler of length 12 could look like this:
        123456789012
    or like this:
                10
        123456789012

    Args:
        m: Integer. If only m is passed, a ruler starting at 1 and
            ending at m is provided.
        n: Integer. If n is passed, then a ruler starting at m and
            ending at n is provided.
        ticks: Boolean. If True, the ruler is 2 lines tall. The top
            line has major ticks at multiples of 10. The second
            line contains the minor ticks, like normal.
        indent: String. This string will be prepended to all lines
            of the ruler.
        ticks_above: Boolean. If True and ticks is True, the major ticks
            will be place above the minor ticks. If False, the two
            lines will be switched.

    Returns:
        A string suitable for printing to the command line to
        produce a ruler with the desired dimensions.

    Raises:
        AssertionError: If the ruler is not an increasing range.
    """
    if n is None:
        start = 1
        end = m
    else:
        start = m
        end = n
    if start > end:
        raise AssertionError('The range must be increasing!')
    if start == end:
        if ticks:
            return(f'\n{str(start)[-1]}')
        else:
            return(f'{str(start)[-1]}')
    if ticks:
        major_ticks = ''
        if start > 0 and end > 0:
            start_lab = (int(start / 10) + 1) * 10
            end_lab = int(end / 10) * 10
            if start_lab == end_lab:
                labels = [start_lab]
            else:
                labels = list(range(start_lab, end_lab + 1, 10))
            for i, lab in enumerate(labels):
                if i == 0 and (start % 10 != 1):
                    if (10 - int(str(start)[-1])) < len(str(lab)) - 1:
                        major_ticks += f'{" " * (10 - int(str(start)[-1]) + 1)}'
                    else:
                        major_ticks += f'{" " * ((10 - (start % 10) + 1) - len(str(lab)))}{lab}'
                else:
                    major_ticks += f'{" " * (10 - len(str(lab)))}{lab}'
        elif start < 0 and end < 0:
            start_lab = int(start / 10) * 10
            end_lab = (int(end / 10) - 1) * 10
            if start_lab == end_lab:
                labels = [start_lab]
            else:
                labels = list(range(start_lab, end_lab + 1, 10))
            for i, lab in enumerate(labels):
                if i == 0 and (start % 10 != 1):
                    if int(str(start)[-1]) < len(str(lab)) - 1:
                        major_ticks += f'{" " * (int(str(start)[-1]) + 1)}'
                    else:
                        major_ticks += f'{" " * ((10 - (start % 10) + 1) - len(str(lab)))}{lab}'
                else:
                    major_ticks += f'{" " * (10 - len(str(lab)))}{lab}'
        elif start < 0 and end > 0:
            if start > -10:
                major_ticks += f'{" " * int(str(start)[-1])}'
            else:
                neg_start_lab = int(start / 10) * 10
                neg_end_lab = -10
                if neg_start_lab == neg_end_lab:
                    neg_labels = [neg_start_lab]
                else:
                    neg_labels = list(range(neg_start_lab, neg_end_lab + 1, 10))
                for i, lab in enumerate(neg_labels):
                    if i == 0 and (start % 10 != 1):
                        if int(str(start)[-1]) < len(str(lab)) - 1:
                            major_ticks += f'{" " * (int(str(start)[-1]) + 1)}'
                        else:
                            major_ticks += f'{" " * ((10 - (start % 10) + 1) - len(str(lab)))}{lab}'
                    else:
                        major_ticks += f'{" " * (10 - len(str(lab)))}{lab}'
                major_ticks += '         '
            major_ticks += '0'
            if end < 10:
                major_ticks += f'{" " * int(str(start)[-1])}'
            else:
                pos_start_lab = 10
                pos_end_lab = int(end / 10) * 10
                if pos_start_lab == pos_end_lab:
                    pos_labels = [pos_start_lab]
                else:
                    pos_labels = list(range(pos_start_lab, pos_end_lab + 1, 10))
                for i, lab in enumerate(pos_labels):
                    major_ticks += f'{" " * (10 - len(str(lab)))}{lab}'
        elif start == 0:
            major_ticks += '0'
            if end < 10:
                major_ticks += f'{" " * int(str(end)[-1])}'
            else:
                start_lab = 10
                end_lab = int(end / 10) * 10
                if start_lab == end_lab:
                    labels = [start_lab]
                else:
                    labels = list(range(start_lab, end_lab + 1, 10))
                for i, lab in enumerate(labels):
                    major_ticks += f'{" " * (10 - len(str(lab)))}{lab}'
        elif end == 0:
            if start > -10:
                major_ticks += f'{" " * int(str(start)[-1])}'
            else:
                start_lab = int(start / 10) * 10
                end_lab = -10
                if start_lab == end_lab:
                    labels = [start_lab]
                else:
                    labels = list(range(start_lab, end_lab + 1, 10))
                for i, lab in enumerate(labels):
                    if i == 0 and (start % 10 != 1):
                        if int(str(start)[-1]) < len(str(lab)) - 1:
                            major_ticks += f'{" " * (int(str(start)[-1]) + 1)}'
                        else:
                            major_ticks += f'{" " * ((10 - (start % 10) + 1) - len(str(lab)))}{lab}'
                    else:
                        major_ticks += f'{" " * (10 - len(str(lab)))}{lab}'
                major_ticks += '         '
            major_ticks += '0'
    minor_ticks = []
    for i in range(start, end + 1, 1):
        minor_ticks.append((str(i)[-1]))
    minor_ticks = ''.join(minor_ticks)
    if ticks:
        if ticks_above:
            ruler = indent + major_ticks + '\n' + indent + minor_ticks
        else:
            ruler = indent + minor_ticks + '\n' + indent + major_ticks
    else:
        ruler = indent + minor_ticks
    return ruler


def color_print(string, color, file=sys.stdout):
    """Print a string with a certain color.

    Assumes that the color before and after this string should
    be the default black and white. Handles:
        White (normal - not sure why it's white and not black)
        Red
        Green
        Orange
        Blue
        Purple

    Args:
        string: String. The text to be printed.
        color: String. The color to print the text in.
            Case-insensitive. Spelling-sensitive.
        file: IO object allowing output. Passed to
            the file argument of print().

    Returns:
        None. Prints the colored string to file.
    """
    color = color.lower()
    if color == 'white':
        color = '\033[0m'
    elif color == 'red':
        color = '\033[31m'
    elif color == 'green':
        color = '\033[32m'
    elif color == 'orange':
        color = '\033[33m'
    elif color == 'blue':
        color = '\033[34m'
    elif color == 'purple':
        color == '\033[35m'
    else:
        color_print('ERROR: Color not supported', 'red')
    print(color + string + '\033[0m', file=file)


def print_timedelta(timedelta, short=False):
    """Convert a datetime.timedelta object to a pretty string.

    Convert a datetime.timedelta or an integer number
    of seconds to a human readable string in the format:
        0 days, 0 hours, 0 minutes, 0 seconds
    If short is True, the returned string will be in
    short format:
        00:00:00:00
    Leading zero units are stripped. For example:
        2 hours, 0 minutes, 10 seconds
          or
        02:00:10

    Adapted from
        https://gist.github.com/thatalextaylor/7408395
    on 2020-09-09.

    Args:
        timedelta: Either a datetime.timedelta object or
            a number of seconds that can be converted to
            an integer. The time interval to be converted
            to a string.

    Returns:
        String. The human readable version of timedelta
        in the format listed above.
    """
    import datetime
    if type(timedelta) is datetime.timedelta:
        seconds = timedelta.total_seconds()
    else:
        seconds = timedelta
    if seconds < 0:
        sign_string = '-'
    else:
        sign_string = ''
    seconds = abs(int(seconds))
    days, seconds = divmod(seconds, 86400)
    hours, seconds = divmod(seconds, 3600)
    minutes, seconds = divmod(seconds, 60)
    unit = {}
    if short:
        unit['d'] = ':'
        unit['h'] = ':'
        unit['m'] = ':'
        unit['s'] = ''
    else:
        unit['d'] = ' day, ' if days == 1 else ' days, '
        unit['h'] = ' hour, ' if hours == 1 else ' hours, '
        unit['m'] = ' minute, ' if minutes == 1 else ' minutes, '
        unit['s'] = ' second' if seconds == 1 else ' seconds'
    if short:
        if days > 0:
            return (f'{sign_string}'
                    f'{days:02d}{unit["d"]}'
                    f'{hours:02d}{unit["h"]}'
                    f'{minutes:02d}{unit["m"]}'
                    f'{seconds:02d}{unit["s"]}')
        elif hours > 0:
            return (f'{sign_string}'
                    f'{hours:02d}{unit["h"]}'
                    f'{minutes:02d}{unit["m"]}'
                    f'{seconds:02d}{unit["s"]}')
        elif minutes > 0:
            return (f'{sign_string}'
                    f'{minutes:02d}{unit["m"]}'
                    f'{seconds:02d}{unit["s"]}')
        else:
            return (f'{sign_string}'
                    f'{seconds}{unit["s"]}')
    else:
        if days > 0:
            return (f'{sign_string}'
                    f'{days}{unit["d"]}'
                    f'{hours}{unit["h"]}'
                    f'{minutes}{unit["m"]}'
                    f'{seconds}{unit["s"]}')
        elif hours > 0:
            return (f'{sign_string}'
                    f'{hours}{unit["h"]}'
                    f'{minutes}{unit["m"]}'
                    f'{seconds}{unit["s"]}')
        elif minutes > 0:
            return (f'{sign_string}'
                    f'{minutes}{unit["m"]}'
                    f'{seconds}{unit["s"]}')
        else:
            return (f'{sign_string}'
                    f'{seconds}{unit["s"]}')


def print_h5_tree(h5_item, show_dtypes=False, prefix='', file=sys.stdout):
    """Pretty print a tree view of an HDF5 file.

    Prints a tree view of an HDF5 file, showing the
    hierarchy of groups and datasets.

    Args:
        h5_item: h5py.File - open. The HDF5 file to be printed.
        show_dtypes: bool. Whether or not the dtypes of the Datasets
            should be printed.
        prefix: str. The character string to prefix the tree. e.g.
            '    ' to move the tree off the left gutter.
        file: IO object allowing output. Tree will be printed to it.

    Returns:
        None. Prints the tree to the file argument.

    Raises:
        AssertionError: If the HDF5 file contains an object
            that isn't an h5py.{File,Group,Dataset}.
    """
    vert = '\u2502'
    horz = '\u2500'
    corn = '\u2514'
    fork = '\u251c'
    if str(type(h5_item)).endswith('File\'>'):
        print(prefix, end='', file=file)
    print(h5_item.name.split('/')[-1]
            if not h5_item.name == '/' else h5_item.filename, end='', file=file)
    if str(type(h5_item)).endswith('Dataset\'>'):
        # Base Case
        if show_dtypes:
            print(f' <{str(h5_item.dtype)}>', file=file)
        else:
            print('', file=file)
        return
    elif not (str(type(h5_item)).endswith('Group\'>') or
              str(type(h5_item)).endswith('File\'>')):
        raise AssertionError(f'h5py object "{h5_item.name}" is not a '
                             'File, Group, or Dataset and is not supported. '
                             f'type is "{type(h5_item)}".')
    else:
        print('', file=file)
    # Recursive Case
    total = len(h5_item)
    for i, k in enumerate(h5_item.keys()):
        if i < total - 1:
            print(f'{prefix}{fork}{horz} ', end='', file=file)
            print_h5_tree(h5_item[k], show_dtypes, prefix + f'{vert}  ', file)
        else:
            print(f'{prefix}{corn}{horz} ', end='', file=file)
            print_h5_tree(h5_item[k], show_dtypes, prefix + '   ', file)


################################################################################
#                                  CLI Input                                   #
################################################################################

def get_int(prompt, nonnegative=False):
    """Reads an integer input with error checking.

    Args:
        prompt: String. The prompt to get an integer.
        nonnegative: Boolean. If True, the integer must be
            nonnegative.

    Returns:
        An integer from the user.
    """
    repeat = True
    while repeat:
        value = input(prompt)
        try:
            value = int(value)
        except ValueError:
            print('ERROR: Please enter a valid integer!')
            repeat = True
            continue
        repeat = False
        if nonnegative == True and value < 0:
            print('ERROR: Please enter an integer >= 0!')
            repeat = True
            continue
    return value


def get_float(prompt, nonnegative=False):
    """Reads an float input with error checking.

    Args:
        prompt: String. The prompt to get a float.
        nonnegative: Boolean. If True, the float must be
            nonnegative.

    Returns:
        A float from the user.
    """
    repeat = True
    while repeat:
        value = input(prompt)
        try:
            value = float(value)
        except ValueError:
            print('ERROR: Please enter a valid number!')
            repeat = True
            continue
        repeat = False
        if nonnegative == True and value < 0:
            print('ERROR: Please enter a positive number!')
            repeat = True
            continue
    return value


def get_yes_or_no(input_string):
    """Get a yes or no response from the user.

    Get a yes or no response from the user in a way
    that handles invalid responses. If this code
    doesn't produce a 'y' or a 'n', then the prompt
    prints an error and loops.

    Code: INPUT_FROM_USER[0].lower()

    Args:
        input_string: string. Prompt fed to input()

    Returns:
        bool. True if answer was yes. False if no.
    """
    yn_loop = True
    while yn_loop:
        user_input = input(input_string)
        if user_input is None:
            print('ERROR: Please enter yes or no!')
            continue
        if len(user_input) == 0:
            print('ERROR: Please enter yes or no!')
            continue
        user_input = user_input[0].lower()
        if user_input == 'y':
            yn_loop = False
            return True
        elif user_input == 'n':
            yn_loop = False
            return False
        else:
            print('ERROR: Please enter yes or no!')
            continue


def get_command(options, prompt, first_letter=False,
                case_sensitive=True, error_message=None,
                empty_str_error_msg=None):
    """Get a command from the user in a CLI.

    In a command line interactive application, get a command from
    the user from preset list of options. Provides a stable loop
    that handles bad input.

    Args:
        options: List of Strings. Each string is a valid input option
            to receive from the user. The empty string can be included
            here as a valid option.
        prompt: String. The string used as the prompt for input()
        first_letter: Boolean. If True and all options start with a
            different letter, valid input can be given as the first
            letter of any option. To avoid confusing interfaces,
            'a' and 'A' are not considered different first letters,
            even if the case_sensitive flag is set.
        case_sensitive: Boolean. If True, the input does not have
            to match case to the given options.
        error_message: String. If not None, this will be used as
            a generic error message for all invalid input except
            empty string input. If None, the following will be used:
                'ERROR: Invalid option!'
        empty_str_error_msg: String. If not None, this will be used
            as the error message for invalid empty string input.
            If None, the following will be used:
                'ERROR: Empty string is not a valid option!'

    Returns:
        A single string matching one of the options passed in
        options, even if a first_letter abbreviation was
        given by the user or if input is not case-sensitive.

    Raises:
        Assertion Error: If first_letter is set True and there are
            non-unique first letters. This ignores case and the
            case_sensitive flag.
    """
    if error_message is None:
        error_message = 'ERROR: Invalid option!'
    if empty_str_error_msg is None:
        empty_str_error_msg = 'ERROR: Empty string is not a valid option!'
    if first_letter:
        first_letters = set()
        for opt in options:
            if opt == '':
                first_letters.add(opt)
            else:
                first_letters.add(opt[0].lower())
        if len(first_letters) != len(options):
            raise AssertionError('first_letter cannot be True if there are '
                                 'non-unique first letters in the options! '
                                 'Case does not count and there cannot be '
                                 'more than one empty string.')
        del first_letters
    opt_set = set(options)
    if len(opt_set) != len(options):
        raise AssertionError('options has non-unique entries!')
    options = opt_set
    del opt_set
    if '' in options:
        empty_string = True
        options.remove('')
    else:
        empty_string = False
    while True:
        user_input = input(prompt)
        # Take care of empty string case to simplify later code
        if user_input == '':
            if empty_string:
                return ''
            else:
                print(empty_str_error_msg)
                continue
        if user_input in options:
            return(user_input)
        elif not case_sensitive:
            user_input_lower = user_input.lower()
            for opt in options:
                opt_lower = opt.lower()
                if user_input_lower == opt_lower:
                    return opt
                elif first_letter and len(user_input) == 1:
                    if user_input_lower == opt_lower[0]:
                        return opt
            print(error_message)
            continue
        elif first_letter and len(user_input) == 1:
            for opt in options:
                if user_input == opt[0]:
                    return opt
            print(error_message)
            continue
        else:
            print(error_message)
            continue


def main():
    pass


if __name__ == '__main__':
    main()
