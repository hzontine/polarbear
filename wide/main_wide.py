#!/usr/bin/env python3

import igraph
import random
import os
import sys
import logging
import tempfile

from wide import *
from wide_sim import *
from suite import *


def print_usage():
    print('Usage: main_wide.py [env_openness=#|range]\n' +
        '                    [tolerance=#|range]\n' +
        '                    [suite=#]\n' +
        '                    [num_iter=#]\n' +
        '                    [plot_graphs=True|False]\n' +
        '                    [plot_suite=True|False]\n' +
        '                    [seed=#]\n' +
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


if len(sys.argv) > 8:
    print_usage()
    sys.exit(1)

this_module = sys.modules[__name__]

for arg in sys.argv[1:]:
    if not '=' in arg:
        print("Malformed argument '{}'.".format(arg))
        print_usage()
        sys.exit(2)
    arg_name, arg_val = arg.split('=')
    try:
        setattr(this_module, arg_name, int(arg_val))
    except:
        try:
            setattr(this_module, arg_name, float(arg_val))
        except:
            setattr(this_module, arg_name, arg_val)

sweepable_params = [ 'env_openness', 'tolerance' ]
params = [
    ('env_openness',.5),
    ('tolerance',.5),
    ('suite',0),
    ('seed',0),
    ('num_iter',200),
    ('plot_graphs',True),
    ('plot_suite',True),
    ('log_level','INFO')]
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



# Other configuration parameters.
N = 50
MIN_FRIENDS_PER_NEIGHBOR = 3
NUM_IDEOLOGIES = 3
param_dict['N'] = N
param_dict['MIN_FRIENDS_PER_NEIGHBOR'] = MIN_FRIENDS_PER_NEIGHBOR
param_dict['NUM_IDEOLOGIES'] = NUM_IDEOLOGIES


if any([ type(param_dict[x]) == str for x in sweepable_params ]):
    # Parameter sweep (over different parameter values).
    print("sweep.")
    pass
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
        print('seed,iteration,assortativity', file=f)
        for seed,results in suite_results.items():
            for i,r in enumerate(results):
                print('{},{},{:.4f}'.format(seed,i,r),file=f)
    if plot_suite in [True,'True']:
        os.system('./plotSuite.R --args {}'.format(filename))
else:
    # Single run.
    print('=== Using seed {}.'.format(seed))
    print("=== tolerance={}, env_openness={}, N={}, MIN_FRIENDS={}.".format(
        tolerance, env_openness, N, MIN_FRIENDS_PER_NEIGHBOR))
    print("=== num_iter={}, NUM_IDEOLOGIES={}.".format(num_iter, 
                                                            NUM_IDEOLOGIES))
    random.seed(seed)

    associates_graph = generate_associates_graph(N, MIN_FRIENDS_PER_NEIGHBOR,
        NUM_IDEOLOGIES)
    graph = generate_friends_graph(associates_graph, env_openness, tolerance,
        MIN_FRIENDS_PER_NEIGHBOR)
    results = run_bvm(graph, num_iter, True if plot_graphs=='True' else False)
    _,filename = tempfile.mkstemp(suffix=".csv",dir="/tmp")
    with open(filename,'w') as f:
        print('iteration,assortativity',file=f)
        for i,r in enumerate(results):
            print('{},{:.4f}'.format(i,r),file=f)
    print('results at {}.'.format(filename))
    os.system('./plotSingle.R --args {}'.format(filename))

