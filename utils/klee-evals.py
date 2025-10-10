#!/usr/bin/env python3

import sys, os, argparse, time, subprocess, shutil
import pandas as pd
import numpy as np
import fileinput
from itertools import product

debug = 3
na_repr = '\\nan'
timeout_repr = '\\timeout'

crit_col = '\\criterionName'
time_col = 'time (s)'
nlabels_col = '\\#labels'
ntests_col = '#tests /gen'

include = {
  'tritype.c'                       :  'tritype',
  'tcas.c'                          :  'tcas',
  'Fourballs.c'                     :  'fourballs',
  'replace.c'                       :  'replace',
  'modulus.c'                       :  'modulus',
  'selection_sort.c'                :  'sel_sort',
  'checkutf8-5.c'                   :  'checkutf8',
  # 'checkutf8-7.c'                   :  'checkutf8-7',
  'power.c'                         :  'power',
  'full_bad.c'                      :  'full_bad',
  'iter1_prefixLong_arr_bad.c'      :  'get_tag',
  'gd_full_bad-5.c'                 :  'gd_full_bad',
  # # ---
  # 'modulus_fixed.c'                 :  'modulus*',
  # 'full_bad_fixed.c'                :  'full_bad*',
  # 'iter1_prefixLong_arr_bad_fixed.c':  'get_tag*',
}

loc_map = {
  'full_bad.c': 65,
  'full_bad_fixed.c': 68,
  'iter1_prefixLong_arr_bad.c': 111,
  'iter1_prefixLong_arr_bad_fixed.c': 116,
  'checkutf8-5.c': 74,
  'checkutf8-7.c': 74,
  'Fourballs.c': 30,
  'gd_full_bad-5.c': 156,
  'modulus.c': 25,
  'modulus_fixed.c': 27,
  'power.c': 18,
  'replace.c': 96,
  'selection_sort.c': 24,
  'tcas.c': 110,
  'tritype.c': 22,
}

def timeout_of_cfile (cfile):
  return 60

criteria = (
  'DC',
  'CC',
  'MCC',
  'WM',
  'LIMIT',
)

def criterion_prefix_key (c):
  return (0 if c.startswith ('DC') else
          1 if c.startswith ('CC') else
          2 if c.startswith ('MCC') else
          3 if c.startswith ('WM') else
          4)

klee_modes = (
  ('ignored',
   "label-handling = 'ignore'          \nreplay = 'delayed'"),
  ('naive',
   "label-handling = 'naive'           \nreplay = 'delayed'"),
  ('optimised+noreplay',
   "label-handling = 'optimize'        \nreplay = 'delayed'"),
  ('optimised',
   "label-handling = 'optimize'        \nreplay = 'yes'"),
  # ('optimised+dfs',
  #  "label-handling = 'optimize'        \nreplay = 'yes'      \nsearch = ['dfs']"),
  # ('optimised+bfs',
  #  "label-handling = 'optimize'        \nreplay = 'yes'      \nsearch = ['bfs']"),
)

klee_mode_renames = {
  'ignored': 'ignore',
  'naive': 'naive',
  'optimised+noreplay': 'tight',
  'optimised': 'optim',
}

# Data items reported for each experiment
items = (
  'coverage', '#covered',
  'total instructions', '#instr.',
  'total paths', '#paths',
  '#completed paths',
  '#partially completed paths',
  'generated tests', 'num final tests', ntests_col,
  'klee time', time_col,
)

# ---

def all_files_with_suffix (d, suff):
  for dir, _dirs, files in os.walk (d):
    for f in files:
      if f.endswith (suff):
        yield dir, f


def replace_in_file (f, **repls):
  with fileinput.input (f, inplace = True) as f:
    for l in f:
      for k, v in repls.items ():
        l = l.replace (k, v)
      print (l, end = '')


def setup_out_dir (d, verbose = 0, **kwds):
  if verbose > 0:
    print (f'Outputing into directory `{d}\'...')
  os.makedirs (d, exist_ok = True)


def setup_experiments (indir, cfile, outdir, verbose = 0,
                       criteria = criteria,
                       klee_modes = klee_modes):
  exp_c = os.path.join (indir, cfile)
  toml_template = os.path.join (indir, 'seacoral.toml.in')
  expdir = os.path.join (outdir, indir)

  print (f'{exp_c}... ', end = '\n' if verbose > 1 else '\r')
  if not os.path.exists (toml_template):
    print (toml_template, 'missing')
    return

  if verbose > 0:
    print (f'Setting up directory `{expdir}\'...')
  os.makedirs (expdir, exist_ok = True)
  shutil.copy (exp_c, expdir)

  with open (toml_template, 'r') as template:
    firstline = template.readline ().rstrip ()
    entrypoints, toml = \
      ((firstline[1:].split (), '') if firstline.startswith ('#') else \
       (['main'], firstline))
    toml += template.read ()

  if verbose > 0:
    print ('Entrypoint(s):', *entrypoints)

  timeout = timeout_of_cfile (cfile)
  runs = []
  for crit, klee_mode in product (criteria, klee_modes):
    full_toml = toml
    for kv in (('__CRITERION__', crit),
               ('__KLEE_MODE__', klee_mode[1]),
               ('__TIMEOUT__', str (timeout))):
      full_toml = full_toml.replace (*kv)

    real_toml = os.path.join (expdir, f'{crit}-{klee_mode[0]}.toml')
    with open (real_toml, 'w') as config:
      config.write (full_toml)
    runs += [dict (criterion = crit, klee_mode = klee_mode[0], toml = real_toml)]

  return dict (expname = cfile[:-len ('.c')],
               expdir = expdir,
               cfile = os.path.join (expdir, cfile),
               entrypoints = entrypoints,
               runs = runs,
               timeout = timeout)


def read_klee_infos (kleedir):
  klee_infos = dict ()
  with open (os.path.join (kleedir, 'info'), 'r') as ic:
    for l in ic.readlines ():
      l = l.rstrip ()
      if l.startswith ('KLEE: done: '):
        for k in ('total instructions',
                  'completed paths',
                  'partially completed paths',
                  'generated tests'):
          kk = f'KLEE: done: {k} = '
          if l.startswith (kk):
            klee_infos[k] = int (l[len (kk):])
            break
  with open (os.path.join (kleedir, 'kleetime'), 'r') as ic:
    klee_infos['klee time'] = float (ic.read ().rstrip ())
  return klee_infos


def perform_experiments (df, indir, outdir, verbose = 0, debug = 3, **kwds):
  for f in all_files_with_suffix (indir, '.c'):
    exp = setup_experiments (*f, outdir, verbose = verbose, **kwds)
    for entrypoint in exp['entrypoints']:
      expname, expdir, cfile = exp['expname'], exp['expdir'], exp['cfile']
      cfilename = os.path.basename (cfile)

      if cfilename not in include:
        continue

      if verbose > 0:
        print (f'Considering experiment {expname}...')
        
      for run in exp['runs']:
        criterion, klee_mode = run['criterion'], run['klee_mode']

        if verbose > 0:
          print (f'Using klee mode {klee_mode}...')

        # if klee_mode == 'optimised+keepall' and \
        #    entrypoint == 'get_tag' and \
        #    criterion == 'MCC':
        cmd = [
          'seacoral',
          '--configuration-file', run['toml'],
          '--inputs', cfile,
          '--entrypoint', entrypoint,
          '--tools', 'klee',
          '--log-level', str (debug),
          '--clean-start',
        ]
        if verbose > 0:
          print ('$ ', *cmd)
        # Run the experiment
        subprocess.run (cmd)
        # else:
        #   continue

        # Register some vars about the experiment
        rundir = os.path.join ('_sc', f'{cfilename}-{criterion}-last')
        kleedir = os.path.join (rundir, 'klee', f'klee-out.{entrypoint}.0')
        covinfo = os.path.join (rundir, 'covinfo.csv')

        # Read coverage info as reported by seacoral
        covinfo = pd.read_csv (covinfo, sep = '\t')
        covinfo.pop ('# run')
        covinfo[['criterion', 'klee mode', 'program', 'timeout']] = \
          criterion, klee_mode, cfilename, exp['timeout']

        # Grab some infos written by klee
        klee_infos = read_klee_infos (kleedir)

        dfx = covinfo.assign (**klee_infos)
        dfx.rename (columns = {'num_tests_gen': 'num final tests',
                               'time': 'total time'}, inplace = True)
        dfx['num labels'] = dfx['covered'] + dfx['uncoverable'] + dfx['unknown']
        dfx.pop ('uncoverable')
        dfx.pop ('unknown')

        # Append all this to the full stats dataframe
        df = pd.concat ((df, dfx))

  return df


def load_stats (outdir, verbose = 0, **kwds):
  stats_csv = os.path.join (outdir, 'stats.csv.gz')
  return pd.read_csv (stats_csv).rename_axis ('index')


def save_stats (df, outdir, verbose = 0, **kwds):
  stats_csv = os.path.join (outdir, 'stats.csv.gz')
  if verbose > 0:
    print (f'Saving stats into `{stats_csv}\'...')
  df.to_csv (stats_csv, index = False)



def stats_to_latex (df, outdir, basename = 'stats', verbose = 0, **kwds):
  sum_paths = False
  rotate_prog_names = False

  df.rename (columns = {'covered': '#covered',
                        'program': 'prog',
                        'total instructions': '#instr.',
                        'klee time': time_col,
                        'num labels': nlabels_col},
             inplace = True)
  critLabelColName = f'{crit_col} ({nlabels_col})'

  prog_loc = { include[p]: loc_map[p]
               for p in df['prog'].unique ()
               if p in include }

  df.loc[:,critLabelColName] = \
    df[['criterion', nlabels_col]].astype (str).agg (' ('.join, axis = 1) + ')'
  df.pop ('criterion')
  df.pop (nlabels_col)

  df[ntests_col] = \
    df[['num final tests', 'generated tests']] \
    .map ('{:.0f}'.format).agg (' /'.join, axis = 1) # + ')'
  df.pop ('generated tests')
  df.pop ('num final tests')

  df.loc[:,'klee mode'] = df['klee mode'].map (klee_mode_renames)
  df.loc[:,'prog'] = df['prog'].map (include)

  index_cols = ['prog', critLabelColName, 'klee mode']
  df = df.drop_duplicates (subset = index_cols, keep = 'last')

  if sum_paths:
    paths = ('#paths',)
    df = df.assign (**{'#paths': df['completed paths'] +
                       df['partially completed paths']})
    df.pop ('completed paths')
    df.pop ('partially completed paths')
  else:
    paths = ('#completed paths', '#partially completed paths')
    df = df.rename (columns = {'completed paths': '#completed paths',
                               'partially completed paths': '#partially completed paths'})

  df[time_col] = df[time_col].where (df[time_col] < df['timeout'], other = np.nan)

  # Use a hierarchical index for nicer tables
  numvals = ('#covered', '#instr.', time_col) + paths
  xl = df.pivot_table (index = index_cols,
                       values = numvals + (ntests_col,),
                       aggfunc = {ntests_col: ' '.join,
                                  **{k: 'mean' for k in numvals}})
  
  # select and reorder columns
  xl = xl[[i for i in items if i in xl.columns]]

  # reorder klee modes
  xl = xl.reindex (level = 'klee mode',
                   index = [klee_mode_renames[k] for k, _ in klee_modes])

  # Do not print too many decimals on the console (for clarity)
  pd.options.display.float_format = '{:,.1f}'.format
  print (xl)

  # transpose "deepest" table (and preserve rows with NaN during this
  # operation)
  xlT = xl.stack (future_stack = True).unstack (level = -2)

  xlT.sort_values (by = ['prog', critLabelColName],
                   key = lambda x: (x.map (prog_loc) if x.name =='prog' else
                                    x.map (criterion_prefix_key)),
                   inplace = True)

  xlT.index = \
    xlT.index.set_levels ([f'{p} ({prog_loc[p]} loc)' for p in xlT.index.levels[0] ],
                          level = 0)
  print (xlT)
  # xlT.loc['prog'] = xlT['prog'].map (lambda x: x)

  # Output the results in LaTeX files
  
  stats_tex = os.path.join (outdir, f'{basename}.tex')
  stats_html = os.path.join (outdir, f'{basename}.html')
  statsT_tex = os.path.join (outdir, f'{basename}T.tex')
  statsT_html = os.path.join (outdir, f'{basename}T.html')

  hl_min = None# [time_col# , 'generated tests', 'num final tests']
  hl_max = None# ['#covered']
  ints = ['#instr.', '#covered'] + list (paths)
  floats = [time_col]
  scis = ['#instr.'] + list (paths)
  hl_args = hl_min, hl_max, ints, floats, scis

  for xl, tex, html, colsfmt, hl_axis, hl_min, hl_max, ints, floats, scis in (
      (xl, stats_tex, stats_html,
       ''.join ('r' * (len (ints) + len (floats) + len (scis) + 1)), 'index')
      + hl_args,
      (xlT, statsT_tex, statsT_html,
       ''.join ('r' * len (klee_modes)), 'columns')
      + tuple (pd.IndexSlice[:,:,x] for x in hl_args),
  ):
    if verbose > 0:
      print (f'Saving LaTeX table into `{tex}\'...')
    styler = xl.style \
      .format_index (escape = 'latex',
                     formatter = {'prog': lambda p: p.replace (' (', '} {(')}) \
      .format (escape = 'latex')
    # if hl_max is not None:
    #   styler.highlight_max (
    #     subset = hl_max, axis = hl_axis,
    #     props = 'color:{red};bfseries:;',
    #   )
    # if hl_min is not None:
    #   styler.highlight_min (
    #     subset = hl_min, axis = hl_axis,
    #     props = 'color:{red};bfseries:;',
    #   )
    styler \
      .format (
        subset = ints,
        precision = 0,
      ) \
      .format (
        subset = floats,
        precision = 1,
        na_rep = timeout_repr,
      ) \
      .map_index (
        lambda v: ("rotatebox:[origin=r]{90}--rwrap;textsf:--rwrap;" \
                   if rotate_prog_names and v != '' else \
                   "textsf:--rwrap;" if v != '' else v),
        level = 0, axis = 0,
      ) \
      .hide (names = True, axis = hl_axis)
    # export = styler.export ()

    styler \
      .format (
        '\\num[scientific-notation=false]{{{}}}',
        # '≈\\,\\num[round-mode=figures,round-precision=1,scientific-notation=false]{{{}}}',
        na_rep = na_repr,
        escape = 'latex',
        subset = scis,
      ) \
      .format (
        '\\num[zero-decimal-to-integer,scientific-notation=false]{{{}}}',
        na_rep = na_repr,
        escape = 'latex',
        subset = ints,
      ) \
      .to_latex (
        tex,
        hrules = True,
        clines = 'skip-last;data',
        multirow_align = 't',
        multicol_align = 'r',
        column_format = '@{}lll' + colsfmt + '@{}',
        environment = 'longtable',
        # siunitx = True
      )

    # Final hacked sed-like adjustments:
    replace_in_file (tex, **{'{tabular}': '{xtabular}', '/nan': r'/\nan'})

# ---

def add_io_args (ap):
  ap.add_argument ('--output-dir', '--output', '-O', metavar = 'DIR',
                   help = 'output directory', type = str, required = True)
  ap.add_argument ('--input-dirs', '--inputs', '-I', metavar = 'DIR',
                   nargs='+', type = str, required = True)
def add_behavioral_args (ap):
  ap.add_argument ('--compute', '-c', action = 'store_true',
                   help = 'actually perform evaluations')
  ap.add_argument ('--debug', type = int, default = 3, metavar = 'N',
                   help = 'Seacoral debug level')
def add_verbosity_flags (ap):
  ap.add_argument ('--verbose', '-v', action = 'count', default = 0)

# ---

ap = argparse.ArgumentParser \
  (description = 'Seacoral/Klee evaluation script',
   formatter_class = argparse.ArgumentDefaultsHelpFormatter)
add_io_args (ap)
add_behavioral_args (ap)
add_verbosity_flags (ap)

# ---

def main ():
  args = ap.parse_args ()
  common_args = dict (verbose = args.verbose, debug = args.debug)
  outdir = args.output_dir

  setup_out_dir (outdir, **common_args)
  try:
    df = load_stats (outdir, **common_args)
  except:
    df = pd.DataFrame ()

  if args.compute:
    for indir in args.input_dirs:
      df = perform_experiments (df, indir, outdir, **common_args)

  if df.empty:
    print (f'Nothing to do on empty stats (did you forget `--compute`?)')
  else:
    stats = pd.DataFrame (df)
    save_stats (stats, outdir, **common_args)
    stats_to_latex (stats, outdir, **common_args)

    # simple_progs = {'power', 'sort', 'modulus'}

    # dc = df.query ('criterion == "DC"')
    # dc = dc[dc['function'].isin (simple_progs)]
    # selected = pd.concat ((pd.DataFrame (df.query ('criterion == "LIMIT" | '
    #                                                'criterion == "WM" | '
    #                                                'criterion == "MCC"')),
    #                        dc))

    # # stats_to_latex (pd.DataFrame (selected[selected['function'].isin (simple_progs)]),
    # #                 outdir, basename = 'selected0', **common_args)
    # # stats_to_latex (pd.DataFrame (selected[~selected['function'].isin (simple_progs)]),
    # #                 outdir, basename = 'selected1', **common_args)
    # stats_to_latex (selected, outdir, basename = 'selected', **common_args)

    # # df = pd.DataFrame (df.query ('criterion == "LIMIT"'))
    # # stats_to_latex (df, outdir, basename = 'limit', **common_args)

if __name__=="__main__":
  main ()
