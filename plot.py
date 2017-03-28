#!/usr/bin/env python

import numpy as np

import matplotlib
matplotlib.rcParams['backend'] = 'Qt5Agg'

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

from collections import namedtuple
import glob

Params = namedtuple(
    'Params', (
        'num_bots',
        'level_1',
        'level_2',
        'efficiency',
        'exponent',
        'lethality',
    ),
)

FILE = 'data.txt'

def parse_level_1(s):
    return s[s.find('1-')+2:s.find('),')]

def parse_level_2(s):
    return s[s.find('2,')+2:s.find(')]')]

def parse_filename(f):
    # drop extension
    parts = f[:-4].split('_')
    n = int(parts[1])
    level_1 = parse_level_1(parts[2])
    level_2 = parse_level_2(parts[2])
    efficiency = float(parts[3])
    exponent = float(parts[4])
    lethality = float(parts[5])
    return Params(
        n,
        level_1,
        level_2,
        efficiency,
        exponent,
        lethality,
    )

def parse_results(f):
    """
    returns a dict sending the property to the result string
    input is a list of strings, one per line.
    """
    d = {}
    prop = None
    for line in f:
        line = line.strip()
        if not line:
            continue # skip empty lines
        if prop is None:
            prop = line[1:-1] # drop the quotes
            continue
        if line == 'Result': # skip result line
            continue
        if prop is not None:
            d[prop] = line
            prop = None
            continue
        # empty lines get skipped
    return d

def parse_data():
    with open(FILE) as f:
        data = f.readlines()

    return [
        (
            float(line[line.find('1-')+2:line.find('),')]),
            float(line[line.find('2,')+2:line.find(')]')]),
            float(line.split('\t')[-1]),
        )
        for line
        in data
    ]

def get_2d_data(param_name, prop, pattern):
    """
    parameter name that's varying
    property that identifies the result corresponding to that parameter
    the pattern to glob for in the results dir

    return list is sorted in ascending order by the first parameter
    """
    tups = []
    files = glob.glob(pattern)
    print('matched files', files)
    for p in files:
        params = parse_filename(p)
        x = getattr(params, param_name)
        with open(p) as f:
            data = parse_results(f.readlines())
        y = data[prop]
        tups.append( (float(x), float(y)) )
    return sorted(tups, key=lambda t: t[0])

def main():
    data = get_2d_data(
        'exponent',
        'P=? [ F s_1=4 ]',
        # 'results/*',
        'results/model_50_[[](1-50),(2,50)[]]_1.0_*_0.25.csv',
    )

    # X = np.array(sorted([t[0] for t in data]))
    # Y = np.array(sorted([t[1] for t in data]))
    # X, Y = np.meshgrid(X, Y)

    # table = dict(
    #     ((t[0], t[1]), t[2])
    #     for t
    #     in data
    # )

    # Z = np.array([table[(x, y)] for x, y in zip(np.ravel(X), np.ravel(Y))])
    # Z = Z.reshape(X.shape)

    fig = plt.figure()
    ax = fig.add_subplot(111) #, projection='3d')
    # ax.plot_surface(X,Y,Z)
    ax.plot([t[0] for t in data], [t[1] for t in data])

    plt.ylabel('success probability')
    plt.xlabel('exponent')

    plt.show()
    fig.savefig('asdf.png')

if __name__ == '__main__':
    main()
