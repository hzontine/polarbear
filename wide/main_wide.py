#!/usr/bin/env python3

import igraph
import random
import os
import sys
import logging
import tempfile
import numpy as np

from wide import *
from wide_sim import *
from suite import *
from sweep import *


def print_usage():
    print('Usage: main_wide.py [env_openness=#|range]\n' +
        '                    [homophily=#|range]\n' +
        '                    [suite=#]\n' +
        '                    [num_iter=#]\n' +
        '                    [plot_graphs=True|False]\n' +
        '                    [plot_suite=True|False]\n' +
        '                    [plot_sweep=True|False]\n' +
        '                    [seed=#]\n' +
        '                    [N=#]\n' +
        '                    [MIN_FRIENDS_PER_NEIGHBOR=#]\n' +
        '                    [NUM_IDEOLOGIES=#]\n' +
        '                    [log_level=level|NONE].\n' +
        '(where "range" is #-#-#, for start-stop-step.)')


# https://stackoverflow.com/questions/17602878/ ##############################
import sys
import contextlib

@contextlib.contextmanager
def smart_open(filename=None):
    if filename and filename != 'stdout':
        fh = open(filename, 'w')
    else:
        fh = sys.stdout
    try:
        yield fh
    finally:
        if fh is not sys.stdout:
            fh.close()
##############################################################################


if (len(sys.argv) < 2 or sys.argv[1].startswith('usage') 
        or sys.argv[1].startswith('-usage')):
    print_usage()
    sys.exit(1)

this_module = sys.modules[__name__]

sweepable_params = [ 'env_openness', 'homophily' ]
params = [
    ('env_openness',.5),
    ('homophily',.5),
    ('suite',0),
    ('seed',0),
    ('num_iter',200),
    ('plot_graphs',True),
    ('plot_suite',True),
    ('plot_sweep',True),
    ('N',50),
    ('MIN_FRIENDS_PER_NEIGHBOR',3),
    ('NUM_IDEOLOGIES',3),
    ('log_level','INFO')]

for arg in sys.argv[1:]:
    if not '=' in arg:
        print("Malformed argument '{}'.".format(arg))
        print_usage()
        sys.exit(2)
    arg_name, arg_val = arg.split('=')
    if arg_name not in [ x for x,_ in params ]:
        print("Unknown argument '{}'.".format(arg_name))
        print_usage()
        sys.exit(3)
    try:
        setattr(this_module, arg_name, int(arg_val))
    except:
        try:
            setattr(this_module, arg_name, float(arg_val))
        except:
            setattr(this_module, arg_name, arg_val)

param_dict = {}
for (param,default) in params:
    if not hasattr(this_module, param):
        setattr(this_module, param, default)
    param_dict[param] = getattr(this_module, param)

if log_level == 'NONE':
    logging.getLogger().setLevel(logging.CRITICAL + 1)
else:
    logging.getLogger().setLevel(log_level)

if seed in (0,None):
    seed = random.randrange(10000)



if any([ type(param_dict[x]) == str for x in sweepable_params ]):
    # Parameter sweep (over different parameter values).
    to_sweep = [ (x, param_dict[x]) for x in sweepable_params 
                                        if type(param_dict[x]) == str ]
    sweep_params = {}
    for s_p, s_vals in to_sweep:
        start, stop, step = [ float(x) for x in s_vals.split('-') ]
        sweep_params[s_p] = list(np.arange(start, stop+step, step))
    param_dict['plot_suite'] = False
    param_dict['plot_graphs'] = False
    if param_dict['suite'] == 0:
        # If they want a sweep but didn't specify, use size-1 suites so that
        # we get data output (instead of just a single-run plot).
        param_dict['suite'] = 1
    Sweep(sweep_params, param_dict).run()
    if plot_sweep in [True,'True']:
        os.system('./plotSweep.R')
elif suite:
    # Suite of runs (with all the same parameters).
    if plot_suite in [True,'True']:
        _,filename = tempfile.mkstemp(suffix=".csv",dir="/tmp")
    else:
        filename = 'stdout'
        # Turn off logging to not interfere with stdout.
        logging.getLogger().setLevel(logging.CRITICAL + 1)
    suite_results = Suite(range(seed,seed+suite)).run(param_dict)
    with smart_open(filename) as f:
        print('seed,iteration,' + ','.join(sweepable_params) + 
            ',assortativity', file=f)
        for seed,results in suite_results.items():
            for i,r in enumerate(results):
                if r == None:
                    print(('{},{},' + 
                            '{:.2f},'*len(sweepable_params) + 
                            'NA').format(seed,i,
                            *[ param_dict[x] for x in sweepable_params ]),
                    file=f)
                else:
                    print(('{},{},' + 
                            '{:.2f},'*len(sweepable_params) + 
                            '{:.4f}').format(seed,i,
                                *[ param_dict[x] for x in sweepable_params ],r),
                    file=f)
                    
    if plot_suite in [True,'True']:
        os.system('./plotSuite.R --args {}'.format(filename))
else:
    # Single run.
    print('=== Using seed {}.'.format(seed))
    print("=== homophily={}, env_openness={}, N={}, MIN_FRIENDS={}.".format(
        homophily, env_openness, N, MIN_FRIENDS_PER_NEIGHBOR))
    print("=== num_iter={}, NUM_IDEOLOGIES={}.".format(num_iter, 
                                                            NUM_IDEOLOGIES))
    random.seed(seed)

    associates_graph = generate_associates_graph(N, MIN_FRIENDS_PER_NEIGHBOR,
        NUM_IDEOLOGIES)
    graph = generate_friends_graph(associates_graph, env_openness, homophily,
        MIN_FRIENDS_PER_NEIGHBOR)
    results = run_bvm(graph, num_iter, True if plot_graphs=='True' else False)
    _,filename = tempfile.mkstemp(suffix=".csv",dir="/tmp")
    with open(filename,'w') as f:
        print('iteration,assortativity',file=f)
        for i,r in enumerate(results):
            print('{},{:.4f}'.format(i,r),file=f)
    print('results at {}.'.format(filename))
    os.system('./plotSingle.R --args {}'.format(filename))

