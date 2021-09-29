# Useful for interaction with the command line


###############################################################################
#                                  CLI Output                                 #
###############################################################################

def propeller(duration, frequency=0.5):
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
        duration: float. Number of seconds the propeller will spin.
        frequency: float. Number of full rotations per second.
    """
    import time

    step_duration = 1 / (8 * frequency)
    chars = ['|', '/', '—', '\\', '|', '/', '—', '\\']
    counter = 0
    start = time.perf_counter()
    while time.perf_counter() < (start + duration):
        print('\r' + chars[counter], end='')
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
                   'elderberry', 'fig', 'grape']
        >>> test
        ['apple', 'banana', 'cherry', 'date', 'elderberry', 'fig', 'grape']
        >>> print(pprint.pformat(test, width = 30, indent = 4))
        [    'apple',
             'banana',
             'cherry',
             'date',
             'elderberry',
             'fig',
             'grape']
        >>> print(pretty_str_list(test, width = 30, indent = '    '))
            apple, banana, cherry,
            date, elderberry, fig,
            grape

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
                    pos_labels = list(range(pos_start_lab,
                                            pos_end_lab + 1,
                                            10))
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


def color_print(string, color, file=None):
    """Print a string with a certain color.

    Assumes that the color before and after this string should
    be the default black and white. Handles:
        White (normal - not sure why it's white and not black)
        Red
        Green
        Orange
        Blue

    Args:
        string: String. The text to be printed.
        color: String. The color to print the text in.
            Case-insensitive. Spelling-sensitive.
        file: IO object allowing output. Passed to
            the file argument of print().

    Returns:
        None. Prints the colored string to file.
    """
    import sys
    if file is None:
        file = sys.stdout
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
    else:
        color_print('ERROR: Color not supported', 'red')
        return
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


def print_h5_tree(h5_item, show_dtypes=False, prefix='', file=None):
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
    import sys
    if file is None:
        file = sys.stdout
    vert = '\u2502'
    horz = '\u2500'
    corn = '\u2514'
    fork = '\u251c'
    if str(type(h5_item)).endswith('File\'>'):
        print(prefix, end='', file=file)
    if '/' in h5_item.name and len(h5_item.name) > 1:
        print(h5_item.name.split('/')[-1], end='', file=file)
    else:
        print(h5_item.filename, end='', file=file)
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


###############################################################################
#                                  CLI Input                                  #
###############################################################################

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
        if nonnegative and value < 0:
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
        if nonnegative and value < 0:
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
