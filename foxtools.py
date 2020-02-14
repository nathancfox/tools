"""
General collection of Python classes and methods that may be generally useful
across specific project settings. Because this is a collection including
methods for any field, the imports are distributed to the methods that need
them to reduce import time to only the functions that are used. Python is
good about caching and not doubling imports for different namespaces, so this
shouldn't be a problem. If a list of dependencies is needed, run the shell
command 'grep "^import" foxtools.py'.

General
-------
  smart_dir
  get_timestamp
  get_uuid
  gunzip
  extract_tar
  convert_dict_to_dataframe

Math
----
  round_up
  round_down
  transpose_expr_matrix
  pairwise_dist
  
Plotting
--------
  year_to_num
  get_group_plot_data

CLI Output
----------
  propeller
  pretty_str_list
  pretty_str_text
  get_ruler
  color_print

CLI Input
---------
  get_int
  get_float
  get_yes_or_no
  get_command
"""

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

    Raises: None
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
    python_reserved_methods.sort(key = lambda x: x[0])
    python_reserved_other.sort(key = lambda x: x[0])
    internal_methods.sort(key = lambda x: x[0])
    internal_other.sort(key = lambda x: x[0])
    public_methods.sort(key = lambda x: x[0])
    public_other.sort(key = lambda x: x[0])
    obj_type = str(type(obj)).split('\'')[1]
    print('------'+ ('-' * (len(obj_type) + 4)))
    print(f'  Type: {obj_type}')
    print('------'+ ('-' * (len(obj_type) + 4)))
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

def get_timestamp(mode = 'both', long = True):
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
        return(datetime_stamp)
    elif mode == 'date':
        return(date_stamp)
    elif mode == 'time':
        return(time_stamp)
    else:
        raise ValueError('mode must be one of {\'both\', \'date\', \'time\'}!')

def get_uuid():
    """Generates string version of a new UUID4."""
    import uuid
    
    return(str(uuid.uuid4()))

def gunzip(files, remove = True):
    """Gunzips files

    Given a list of file paths, gunzips them, and optionally
    removes the .gz files.

    Args:
        files: Either a string, or a list of strings. Each string
            should be a path to a gzipped file to be gunzipped.
        remove: Boolean. If True, the gzipped files will be
            removed, leaving only the gunzipped files.
    
    Returns: None
    Raises: None
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
        
def extract_tar(tar_file, outpath = "", remove = True):
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

    Returns: None

    Raises : None
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
                tar.extract(tar_member, path = outpath)
            tar_member = tar.next()
    if remove:
        os.remove(tar_file)
    for sm in skipped_members:
        print(f'WARNING: {sm} not extracted due to potentially dangerous '
               'filename. Please extract manually!')
    
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
    return(new_df)

def convert_seconds(seconds, days=False):
    """Converts seconds to hours, minutes, and seconds.

    Args:
        seconds: Integer. Number of seconds to be converted.
        days: Boolean. If True, a 4-member tuple is returned
            with days.

    Returns:
        Tuple of 3 integers. Seconds converted to
        hours, minutes, seconds. If days=True, then days is
        included and a 4 member tuple is returned.

    Raises: None
    """
    m, s = divmod(seconds, 60)
    h, m = divmod(m, 60)
    if days:
        d, h = divmod(h, 24)
        return(d, h, m, s)
    else:
        return (h, m, s)

################################################################################
#                                     Math                                     #
################################################################################

def round_up(number, nearest):
    """Rounds a number up to an integer.

    Args:
        number: Float. The value to be rounded up.
        nearest: Integer. The precision to round up to.
            e.g. 10, 100, 1000

    Returns:
        The number rounded up to the nearest integer
        with precision "nearest".

    Raises: None
    """
    import math
    if number == 0:
        return nearest
    return(int(math.ceil(number / nearest) * nearest))

def round_down(number, nearest):
    """Rounds a number down to an integer.

    Args:
        number: Float. The value to be rounded down.
        nearest: Integer. The precision to round down to.
            e.g. 10, 100, 1000

    Returns:
        The number rounded down to the nearest integer
        with precision "nearest".

    Raises: None
    """
    import math
    if number == 0:
        return nearest
    return(int(math.floor(number / nearest) * nearest))

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
        Writes a tab-separated-value file containing the transposed matrix
        to disk at the outfile location.
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

def pairwise_dist(pts, full = False):
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
        res[i, i + 1:] = np.sqrt(np.sum(((pts[i + 1:, ] - pts[i, ]) ** 2), axis = 1))
    if not full:
        return res
    else:
        return res.T + res

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
        
    Raises: None
    """
    import numpy as np
    import matplotlib.dates as mdates
    
    if type(year) is str:
        year = int(year)
        year = f'{year}-01-01'
        return(mdates.datestr2num(year))
    elif type(year) is int:
        year = f'{year}-01-01'
        return(mdates.datestr2num(year))
    else:
        year = list(year)
        year = np.array(list(map(lambda x: f'{x}-01-01', year)))
        year = mdates.datestr2num(year)
        return(year)
    
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
    
    Raises: None
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
    return((vectors, names))

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
    —
    \
    |
    /
    —
    \

    Args:
        duration: Float. Number of seconds the propeller will spin.
        frequency: Float. Number of full rotations per second.

    Returns: None
    Raises: None
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
    
def pretty_str_list(list, width = 50, indent = '',
                    sep = ', ', one_per_line = False): 
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

    Raises: None
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
    return(str_out)

def pretty_str_text(text, width=80, indent=''):
    """Returns pretty string of string.

    Given a string, returns a prettified, printable
    version of the same string. This is a special case
    of pretty_str_list().

    Args:
        text: String. The string to be converted to a
            pretty printable string.
        width: Integer. The maximum character width of
            the pretty string.
        indent: String. A string to prepend to every line
            of the pretty string.
    
    Returns:
        A printable version of the text string. It conforms
        to the width restriction and adds the indent to the
        beginning of each line.

    Raises: None
    """
    pretty_str = pretty_str_list(text.split(' '), width=width, indent=indent,
                                 sep=' ', one_per_line=False)
    return(pretty_str)

def get_ruler(m, n=None, ticks=True, indent=''):
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
        ruler = indent + major_ticks + '\n' + indent + minor_ticks
    else:
        ruler = indent + minor_ticks
    return(ruler)

def color_print(string, color):
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

    Returns:
        Doesn't actually return anything. Just prints to stdout.

    Raises: None
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
        colorprint('ERROR: Color not supported', 'red')
    print(color + string + '\033[0m')

################################################################################
#                                  CLI Input                                   #
################################################################################

def get_int(prompt, nonnegative = False):
    """Reads an integer input with error checking.

    Args:
        prompt: String. The prompt to get an integer.
        nonnegative: Boolean. If True, the integer must be
            nonnegative.

    Returns:
        An integer from the user.

    Raises: None
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
    return(value)

def get_float(prompt, nonnegative = False):
    """Reads an float input with error checking.

    Args:
        prompt: String. The prompt to get a float.
        nonnegative: Boolean. If True, the float must be
            nonnegative.

    Returns:
        A float from the user.

    Raises: None
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
        input_string: String. Prompt fed to input()
    
    Returns:
        Boolean. True if answer was yes. False if no.

    Raises: None
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
            return(True)
        elif user_input == 'n':
            yn_loop = False
            return(False)
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
                return('')
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
                    return(opt)
                elif first_letter and len(user_input) == 1:
                    if user_input_lower == opt_lower[0]:
                        return(opt)
            print(error_message)
            continue
        elif first_letter and len(user_input) == 1:
            for opt in options:
                if user_input == opt[0]:
                    return(opt)
            print(error_message)
            continue
        else:
            print(error_message)
            continue

def main():
    pass

if __name__ == '__main__':
    main()
