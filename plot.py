#!/usr/bin/env python

from sys import exit

import pandas as pd

import numpy as np

import matplotlib
matplotlib.rcParams['backend'] = 'Qt5Agg'

import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

from collections import namedtuple
from numpy import isclose
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

def load_data():
    d = pd.read_csv("results-4.csv")
    return d

def main():
    data = load_data()
    print(data)

    # Plot prob of eventual win against exponent

    # Sort by exponent
    data = data.sort_values(by="exponent")

    props = [# "P=? [ F some_battle_won ]",
            # "P=? [ F N=0 ]",
            # "P=? [ F s_1=3 ]",
            # "P=? [ F s_1=4 ]",
            "P=? [ F s_1=4&N>=0.1*initialN ]",
            "P=? [ F s_1=4&N>=0.2*initialN ]",
            "P=? [ F s_1=4&N>=0.3*initialN ]",
            "P=? [ F s_1=4&N>=0.4*initialN ]",
            "P=? [ F s_1=4&N>=0.5*initialN ]",
            "P=? [ F s_1=4&N>=0.6*initialN ]",
            "P=? [ F s_1=4&N>=0.7*initialN ]",
            "P=? [ F s_1=4&N>=0.8*initialN ]",
            "P=? [ F s_1=4&N>=0.9*initialN ]",
            # "P=? [ G (s_1=1=>(X s_1=2)) ]"
            ]
    for p in props:
        plt.plot(data.exponent, data[p], label=p)
    plt.legend()
    plt.show()

    print(data["P=? [ F N=0 ]"])

    '''
    # DataFrame with
    # numbots
    # enemies 25, 200,
    # prob that a proportion survive
    print(set(data.efficiency))
    size = len(data.efficiency)
    eff_leth_vary = data[
            (data.num_enemies == 2) &
            isclose(data.enemy_level_1, np.repeat(25, size)) &
            isclose(data.enemy_level_2, np.repeat(200, size)) &
            isclose(data.exponent,
                np.repeat(1.15, size),
                rtol = 0.0001) &
            isclose(data.efficiency,
                np.repeat(1.85, size),
                rtol = 0.0001) &
            isclose(data.lethality,
                np.repeat(0.1, size),
                rtol = 0.0001)
            ]
    
    eff_leth_vary = eff_leth_vary.sort_values(by="num_bots")

    survival_indices = [
        ("P=? [ F s_1=4&N>=0.1*initialN ]", "10% survive"),
        ("P=? [ F s_1=4&N>=0.2*initialN ]", "20% survive"),
        ("P=? [ F s_1=4&N>=0.3*initialN ]", "30% survive"),
        ("P=? [ F s_1=4&N>=0.4*initialN ]", "40% survive"),
        ("P=? [ F s_1=4&N>=0.5*initialN ]", "50% survive"),
        ("P=? [ F s_1=4&N>=0.6*initialN ]", "60% survive"),
        ("P=? [ F s_1=4&N>=0.7*initialN ]", "70% survive"),
        ("P=? [ F s_1=4&N>=0.8*initialN ]", "80% survive"),
        ("P=? [ F s_1=4&N>=0.9*initialN ]", "90% survive")
    ]

    Ns = list(eff_leth_vary.num_bots.values)

    plot_lines = []

    for i in survival_indices:
        print("Currently on property %s" % i[1])
        ls = []
        for n in Ns:
            ls.append(eff_leth_vary[
                eff_leth_vary.num_bots == n
                ][i[0]].values[0]
                )
        print(ls)
        plot_lines.append(ls)

    for i, l in zip(survival_indices, plot_lines):
        plt.plot(Ns, l, label=i[1])

    plt.legend()
    plt.title("Probability of survival given proportion")
    plt.xlabel("N (Initial number of bots)")
    plt.ylabel("Probability of survival")
    plt.show()
    '''


    '''
    # DataFrame with
    # numbots
    # enemies 25, 200,
    # efficiency 1.15
    # 
    # varying exponent, lethality
    print(set(data.efficiency))
    size = len(data.efficiency)
    exp_leth_var = data[(data.num_bots == 20) &
            (data.num_enemies == 2) &
            isclose(data.enemy_level_1, np.repeat(25, size)) &
            isclose(data.enemy_level_2, np.repeat(200, size)) &
            isclose(data.efficiency,
                np.repeat(1.35, size),
                rtol = 0.0001)
            ]

    exp_leth_size = len(exp_leth_var)
    print(len(exp_leth_var))

    # Probability of eventual victory
    X = np.array(sorted(list(exp_leth_var.exponent)))
    Y = np.array(sorted(list(exp_leth_var.lethality)))
    X, Y = np.meshgrid(X, Y)

    Z = []

    i = 1
    total = len(np.ravel(X))
    for x, y in zip(np.ravel(X), np.ravel(Y)):
        i += 1
        z = exp_leth_var[
                isclose(exp_leth_var.exponent,
                    np.repeat(x,
                        exp_leth_size,
                        ),
                    rtol = 0.001
                    ) &
                isclose(exp_leth_var.lethality,
                    np.repeat(y,
                        exp_leth_size
                        ),
                    rtol = 0.001
                    )
                ]["P=? [ F some_battle_won ]"].values[0]
        Z.append(z)

    print(Z)

    Z = np.array(Z).reshape(X.shape)

    print("print shape")
    print(Z)
    print("printed shape")

    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    ax.plot_surface(X,Y,Z)
    plt.title("Probability of winning at least one battle")
    plt.xlabel("a-Parameter")
    plt.ylabel("Lethality")
    ax.set_zlabel("Probability of winning at least one battle")

    plt.show()
    '''

    # Yay pandas dataframe

    #data = get_2d_data(
    #    'exponent',
    #    'P=? [ F s_1=4 ]',
    #    # 'results/*',
    #    'results/model_50_[[](1-50),(2,50)[]]_1.0_*_0.25.csv',
    #)

    # data = parse_data()

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

    # fig = plt.figure()
    # ax = fig.add_subplot(111, projection='3d')
    # ax.plot_surface(X,Y,Z)
    # # ax.plot([t[0] for t in data], [t[1] for t in data])

    # plt.ylabel('level 1')
    # plt.xlabel('level 2')

    # plt.show()
    # fig.savefig('surface.png')

if __name__ == '__main__':
    main()
