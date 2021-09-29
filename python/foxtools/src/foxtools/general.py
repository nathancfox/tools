# General purpose


def smart_dir(obj, output=['p']):
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
        output: list-like. Contains which of the 6 categories
            to display. Agnostic to case and underscore/dash/
            whitespace. Acceptable outputs are:
            ['reserved methods', 'rm', 'reserved other', 'ro',
             'internal methods', 'im', 'internal other', 'io',
             'public methods', 'pm', 'public other', 'po',
             'reserved', 'r', 'internal', 'i', 'public', 'p',
             'methods', 'm', 'other', 'o', 'all', 'a']

    Returns:
        Nothing is returned, however the sorted output
        is printed to the standard out.
    """
    categs = {'rm': False, 'ro': False, 'im': False,
              'io': False, 'pm': False, 'po': False}
    flags = set()
    if type(output) == str:
        output = [output]
    try:
        output = list(map(str, output))
    except:
        print('output must be a list-like of strings.')
        return
    for cat in output:
        cat = cat.replace('_', ' ').replace('-', ' ').lower().split()
        cat = ''.join([word[0] for word in cat])
        if cat == 'a':
            flags.update(['rm', 'ro', 'im', 'io', 'pm', 'p'])
        elif cat == 'r':
            flags.update(['rm', 'ro'])
        elif cat == 'i':
            flags.update(['im', 'io'])
        elif cat == 'p':
            flags.update(['pm', 'po'])
        elif cat == 'm':
            flags.update(['rm', 'im', 'pm'])
        elif cat == 'o':
            flags.update(['ro', 'io', 'po'])
        elif cat in categs:
            flags.update([cat])
        else:
            print(f'"{cat}" is not a valid option for output.')
            return
    categs = {k: True if k in flags else False for k in categs}
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
    if len(python_reserved_methods) != 0 and categs['rm']:
        print('Python Reserved Methods')
        print('=======================')
        for a, a_type in python_reserved_methods:
            print(f'  {a:{max_name_width}s} : {a_type}')
        print()
    if len(python_reserved_other) != 0 and categs['ro']:
        print('Python Reserved Other')
        print('===========================')
        for a, a_type in python_reserved_other:
            print(f'  {a:{max_name_width}s} : {a_type}')
        print()
    if len(internal_methods) != 0 and categs['im']:
        print('Internal Methods')
        print('================')
        for a, a_type in internal_methods:
            print(f'  {a:{max_name_width}s} : {a_type}')
        print()
    if len(internal_other) != 0 and categs['io']:
        print('Internal Other')
        print('====================')
        for a, a_type in internal_other:
            print(f'  {a:{max_name_width}s} : {a_type}')
        print()
    if len(public_methods) != 0 and categs['pm']:
        print('Public Methods')
        print('==============')
        for a, a_type in public_methods:
            print(f'  {a:{max_name_width}s} : {a_type}')
        print()
    if len(public_other) != 0 and categs['po']:
        print('Public Other')
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
    caller_globals = dict(inspect.getmembers(inspect.stack()[1][0]))
    caller_globals = caller_globals['f_globals']
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
        'customary': ('B', 'K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y'),
        'customary_ext': ('byte', 'kilo', 'mega', 'giga', 'tera', 'peta',
                          'exa', 'zetta', 'iotta'),
        'iec': ('Bi', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi',
                'Ei', 'Zi', 'Yi'),
        'iec_ext': ('byte', 'kibi', 'mebi', 'gibi', 'tebi',
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
        'customary': ('B', 'K', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y'),
        'customary_ext': ('byte', 'kilo', 'mega', 'giga', 'tera', 'peta',
                          'exa', 'zetta', 'iotta'),
        'iec': ('Bi', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi',
                'Ei', 'Zi', 'Yi'),
        'iec_ext': ('byte', 'kibi', 'mebi', 'gibi', 'tebi',
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
    prefix = {sset[0]: 1}
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


def setup_loggers(global_level=10, logfile='',
                  splitouterr=False, reset=[]):
    """Set up standard and clean loggers.

    I don't know what will happen if you call this from an external
    library (foxtools) from a different module. Mostly intended
    for copy and paste.

    Args:
        global_level: int. Constant from logging indicating the maximum
            level of logging for both loggers.
            Defined as:
                CRITICAL: 50
                ERROR   : 40
                WARNING : 30
                INFO    : 20
                DEBUG   : 10
                NOTSET  : 0
        logfile: str. Path to the file used for logging. If empty string,
            will default to the __file__ of this script, with any
            extension replace with '.log'. If logfile is an existing
            directory, It will default to the same as an empty string,
            but placed in that directory.
        splitouterr: bool. If true, the typical *.log file will instead
            be split into *.out and *.err. The former will contain
            INFO and DEBUG messages. The latter will contain CRITICAL,
            ERROR, and WARNING messages.
        reset: list of str. The names of the loggers to be reset before
            configuration. Empty list indicates all loggers. None
            indicates no loggers.

    Raises:
    """
    import logging
    import os
    import sys

    # Functions and Classes
    def reset_logging(loggers=[]):
        """Reset loggers of given names, completely.

        Stolen from somewhere online.
        """
        manager = logging.Logger.manager
        manager.disabled = logging.NOTSET
        if type(loggers) is str:
            loggers = [loggers]
        if len(loggers) == 0:
            lkeys = list(manager.loggerDict.keys())
        else:
            lkeys = []
            for lkey in loggers:
                if lkey not in manager.loggerDict.keys():
                    print(f'{lkey} is not a valid logger. Will not be reset.',
                          file=sys.stderr)
                else:
                    lkeys.append(lkey)
        for lkey in lkeys:
            logger = manager.loggerDict[lkey]
            if isinstance(logger, logging.Logger):
                logger.setLevel(logging.NOTSET)
                logger.propagate = True
                logger.disabled = False
                logger.filters.clear()
                handlers = logger.handlers.copy()
                for handler in handlers:
                    # Copied from `logging.shutdown`.
                    try:
                        handler.acquire()
                        handler.flush()
                        handler.close()
                    except (OSError, ValueError):
                        pass
                    finally:
                        handler.release()
                    logger.removeHandler(handler)

    class Out_Filter(logging.Filter):
        """Filter for logging messages of INFO and DEBUG."""
        def filter(self, rec):
            if rec.levelno <= logging.INFO:
                return(True)
            else:
                return(False)

    class Err_Filter(logging.Filter):
        """Filter for logging messages of CRITICAL, ERROR, and WARNING."""
        def filter(self, rec):
            if rec.levelno >= logging.WARNING:
                return(True)
            else:
                return(False)

    if reset is not None:
        reset_logging(loggers=reset)
    # Formatters
    std_format = logging.Formatter(fmt=('%(asctime)s [%(levelname)s] '
                                        '%(message)s'),
                                   datefmt='%Y-%m-%d %H:%M:%S')
    clean_format = logging.Formatter(fmt='')

    # Parse logfile
    if logfile == '':
        logfile = os.path.basename(__file__).splitext()[0] + '.log'
    elif os.path.isdir(logfile):
        logdir = logfile
        logfile = os.path.basename(__file__).splitext()[0] + '.log'
        logfile = os.path.join(logdir, logfile)
    else:
        # No effect, here for readability
        logfile = logfile

    # Build Handlers
    std_handlers = {}
    clean_handlers = {}
    if splitouterr:
        logfile_name = os.path.splitext(os.path.basename(logfile))[0]
        logout_file = os.path.join(os.path.dirname(logfile),
                                   f'{logfile_name}.out')
        logerr_file = os.path.join(os.path.dirname(logfile),
                                   f'{logfile_name}.err')

        std_handlers['logout'] = logging.FileHandler(filename=logout_file,
                                                     mode='a')
        std_handlers['logerr'] = logging.FileHandler(filename=logerr_file,
                                                     mode='a')
        std_handlers['logout'].addFilter(Out_Filter())
        std_handlers['logerr'].addFilter(Err_Filter())

        clean_handlers['logout'] = logging.FileHandler(filename=logout_file,
                                                       mode='a')
        clean_handlers['logerr'] = logging.FileHandler(filename=logerr_file,
                                                       mode='a')
        clean_handlers['logout'].addFilter(Out_Filter())
        clean_handlers['logerr'].addFilter(Err_Filter())
    else:
        std_handlers['logfile'] = logging.FileHandler(filename=logfile,
                                                      mode='a')
        clean_handlers['logfile'] = logging.FileHandler(filename=logfile,
                                                        mode='a')
    for hdler in std_handlers:
        std_handlers[hdler].setFormatter(std_format)
    for hdler in clean_handlers:
        clean_handlers[hdler].setFormatter(clean_format)

    # Building loggers
    logger = logging.getLogger('standard')
    if splitouterr:
        logger.addHandler(std_handlers['logout'])
        logger.addHandler(std_handlers['logerr'])
    else:
        logger.addHandler(std_handlers['logfile'])
    logger.setLevel(global_level)

    logger = logging.getLogger('clean')
    if splitouterr:
        logger.addHandler(clean_handlers['logout'])
        logger.addHandler(clean_handlers['logerr'])
    else:
        logger.addHandler(clean_handlers['logfile'])
    logger.setLevel(global_level)


def ipyprompt(prompt=None):
    """Set custom IPython prompt.

    Add the current time as a prefix line to the IPython prompt.
    """
    from IPython.core.getipython import get_ipython
    from IPython.terminal.prompts import Prompts, Token
    from time import localtime, strftime

    class MyPrompt(Prompts):
        def in_prompt_tokens(self):
            start_time = strftime('%H:%M:%S', localtime())
            return [
                (Token.PromptNum, f'{start_time}\n'),
                (Token.Prompt, self.vi_mode()),
                (Token.Prompt, 'In ['),
                (Token.PromptNum, str(self.shell.execution_count)),
                (Token.Prompt, ']: '),
            ]

    ip = get_ipython()
    old_prompt = ip.prompts
    if prompt is None:
        ip.prompts = MyPrompt(ip)
    else:
        ip.prompts = prompt
    return old_prompt


def for_loop_progress(generator):
    """Decorate for loops with progress updates.

    A decorator to print timestamped %-completed updates
    every 10%. This decorator is designed for generic for
    loops that take a single iterable. To allow decoration,
    the for loop must utilize enumerate(), be wrapped in
    a generator, and yield i at each step. Because it is
    wrapped in a generator, the for loop can only affect
    variables by reference.

    Example
    -------
    Filling a list with perfect squares.

        Undecorated
        -----------
        >>> squares = []
        >>> for root in range(1, 10):
        ...    squares.append(root ** 2)

        Decorated
        ---------
        >>> squares = []
        >>> @for_loop_progress
        >>> def generator(roots):
        ...     for i, root in enumerate(roots):
        ...         squares.append(root ** 2)
        ...     yield i
        >>> generator(range(1, 10))


    """
    from datetime import datetime as dt

    def inner(input_list):
        total = len(input_list)
        progress = [0, 0]
        print(f'{dt.now().strftime("%Y-%m-%d %H:%M:%S")} | Starting')
        for i in generator(input_list):
            progress[1] = int(i / total * 100)
            if progress[1] % 10 == 0 and progress[1] > progress[0]:
                print(f'{dt.now().strftime("%Y-%m-%d %H:%M:%S")} | '
                      f'{progress[1]}%')
                progress[0] = progress[1]
        print(f'{dt.now().strftime("%Y-%m-%d %H:%M:%S")} | Finished')
    return inner
