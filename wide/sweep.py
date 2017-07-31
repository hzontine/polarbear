#!/usr/bin/env python3

import subprocess
import os
import sys
from functools import reduce
import operator
import itertools


class Sweep():

    NUM_CORES = 8

    def __init__(self, sweep_params, param_dict):
        '''
        sweep_params is a dictionary mapping parameter names to lists of
        values to test. param_dict is a dictionary mapping parameter names to
        single values (i.e., not swept.)
        '''
        self.sweep_params = sweep_params
        self.param_dict = param_dict
        # Get rid of dups.
        for sweep_param in self.sweep_params:
            if sweep_param in self.param_dict:
                del self.param_dict[sweep_param]

    def run(self, start_seed=100):
        num_distinct_vals = reduce(operator.mul,
            [ len(vals) for _,vals in self.sweep_params.items() ])
        total_num_runs = num_distinct_vals * self.param_dict['suite']
        suites_per_core = num_distinct_vals // self.NUM_CORES

        procs = []
        output_files = []
        seed = start_seed

        annotated_vals = []
        for k,vals in self.sweep_params.items():
            annotated_vals.append([ (k,v) for v in vals ])
        to_sweep = itertools.product(*annotated_vals)
        for param_set in to_sweep:
            output_file = '/tmp/output'+str(seed)+'.csv'
            output_files.append(output_file)
            self.param_dict['seed'] = seed
            with open(output_file, 'w') as f:
                cmd_line = ['./main_wide.py']
                for param in param_set:
                    cmd_line.append('{}={}'.format(*param))
                for p,v in self.param_dict.items():
                    cmd_line.append('{}={}'.format(p,v))
                print('going to run: {}'.format(' '.join(cmd_line)))
                procs.append(subprocess.Popen(cmd_line, stdout=f))
            seed += self.param_dict['suite']

        print('Waiting for completion...')
        [ p.wait() for p in procs ]
        print('...done.')

        cmd_line = 'cat ' + ' '.join(output_files) 
        with open('/tmp/output.csv','w') as f:
            subprocess.Popen(['cat'] + output_files, stdout=f)
        os.system('grep -v "^#" /tmp/output.csv > /tmp/output2.csv')
        os.system('grep -v "seed" /tmp/output2.csv > /tmp/output3.csv')
        os.system('head -1 /tmp/output.csv > /tmp/outputheader.csv')
        os.system('cat /tmp/outputheader.csv /tmp/output3.csv > ' +
            '/tmp/final_output.csv')
        os.system('rm /tmp/output*.csv')
        print('Output in /tmp/final_output.csv.')
