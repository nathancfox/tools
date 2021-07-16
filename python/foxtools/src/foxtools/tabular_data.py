# Functions for working with tables and other data.


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


def get_iris():
    """Retrieve the commonly used test dataset: iris.

    Fisher's iris dataset is extremely commonly used for testing
    and examples that use tabular data. This function returns it
    as a pandas DataFrame.

    Returns:
        pandas DataFrame. Containing the dataset with a header and
        a standard integer index range [0..]
    """
    return get_dataset('iris')


def get_penguins():
    """Retrieve the commonly used test dataset: penguins.

    The Palmer Archipelago penguins dataset is commonly used for testing
    and examples that use tabular data. This function returns it
    as a pandas DataFrame.

    Returns:
        pandas DataFrame. Containing the dataset with a header and
        a standard integer index range [0..]
    """
    return get_dataset('penguins')

def get_dataset(dataset):
    """Retrieve one of several test datasets.

    There are a number of standard datasets used for testing or
    examples, e.g. Fisher's iris dataset. This function exposes
    easy access to them.

    VALID_DATASETS = {
        'iris',
        'penguins'
    }

    Args:
        dataset: str. Name of the dataset to be retrieved. Valid values are
            listed above.

    Returns:
        pandas DataFrame. Containing the dataset with a header and
        a standard integer index range [0..]

    Raises:
        ValueError. If the dataset is not a valid name.
    """
    from pandas import read_csv

    VALID_DATASETS = {
        'iris',
        'penguins'
    }
    if dataset not in VALID_DATASETS:
        raise ValueError(f'{dataset} is not a valid dataset. Use one of:\n'
                         f'{", ".join(sorted(list(VALID_DATASETS)))}')
    baseurl = 'https://raw.githubusercontent.com/mwaskom/seaborn-data/master'
    dataset = read_csv(f'{baseurl}/{dataset}.csv',
                       header=0, index_col=False)
    return dataset
