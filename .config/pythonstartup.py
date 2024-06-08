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
pprint = pprintmod.pprint

try:
    TimeZone = datetime.timezone
    UTC = datetime.timezone.utc
except AttributeError:
    pass

# JSON compatibility constants
null = None
false = False
true = True
