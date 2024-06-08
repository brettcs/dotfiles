import collections
import datetime
import decimal
import functools
import io
import itertools
import operator
import os
import pprint as pprintmod
import random
import re
import shutil
import subprocess
import sys
import tempfile
import traceback
import unicodedata

try:
    import pathlib
except ImportError:
    pass
else:
    Path = pathlib.Path

Date = datetime.date
DateTime = datetime.datetime
TimeDelta = datetime.timedelta
TZInfo = datetime.tzinfo
Decimal = decimal.Decimal
StringIO = io.StringIO

try:
    TimeZone = datetime.timezone
    UTC = datetime.timezone.utc
except AttributeError:
    pass

printer_config = {
    'indent': 2,
    'sort_dicts': False,
    'width': shutil.get_terminal_size((100, 24)).columns,
}
if sys.version_info > (3, 10):
    printer_config['underscore_numbers'] = True
pprinter = pprintmod.PrettyPrinter(**printer_config)
p = pprinter.pprint
del printer_config

# JSON compatibility constants
null = None
false = False
true = True
