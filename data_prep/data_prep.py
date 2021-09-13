'''SWISSSTORE INPUT DATA PREPARATION '''

'''The following script prepares the input data for the residential actor model 
of the SwissStore project'''

#################################################################################

'Importing packages'

import gdxtools as gt
import pandas as pd
import numpy as np
import itertools


#Module that provides a portable way of using operating system dependent functionality
import os

from itertools import permutations, repeat

#Module for character encoding auto-detection 
#https://chardet.readthedocs.io/en/latest/index.html
import chardet 

#Plotting modules
import matplotlib
from matplotlib import pyplot as plt

#Data visualization module based on matplotlib
#https://seaborn.pydata.org/
import seaborn as sns

#Cross-platform colored terminal text 
#https://pypi.org/project/colorama/
from colorama import Fore
from colorama import Style

from pandas.io.parsers import ParserError

from itertools import product

#################################################################################

'Importing data and creating dataframes'

dir_source = '\\wwz-jumbo.storage.p.unibas.ch\wwz-home01$\xigeby65\git_repo\SwissStore\input_data'
dir_parsed = './Results'