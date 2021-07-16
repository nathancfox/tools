# Functions for plotting, mostly in matplotlib


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
