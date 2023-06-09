# mk_funcs_procs.py --- Generate name-list of functions and procedures
#
# Usage:
#   $ cd jal-mode     <-- Root directory of this project
#   $ python3 tools/mk_funcs_procs.py
#
#   output --> jal-mode/jallib-funcs-procs.el
#
# Workflow:
#
#  jallib/include/jal/*.jal    jal-mode/tools/         jal-mode/
#  +-----------+              o===================o    +---------------------------+
#  |  XXX.jal  |-+           /                   /     |   jallib-funcs-procs.el   |
#  +-----------+ |-+   -->  / mk_funcs_procs.py /  --> |------>8-----//---->8------|
#    ------------+ |       /                   /       | (defvar jal-mode-builtins |
#      ------------+      o===================o        |        '( "foo"           |
#                                                      |             :             |
#                                                      |------8<-----//----8<------|
#                             jal-mode/                              |
#                             +-------------------------------+      |
#                             |         jal-mode.el           |      |
#                             |-->8------>8---//--->8----->8--|      |
#                             |    :                          |      |
#                             | (require 'jallib-funcs-procs) |  <---+
#                             |    :      ^^^^^^^^^^^^^^^^^^  |
#                             |--8<------8<--//----8<-----8<--|
import os

home = os.getenv('HOME')
include_dir = os.path.join(home, 'git-clone/jallib/include/jal')

jals = []
for _dir, _, _files in os.walk(include_dir):
    for x in _files:
        if x.endswith('.jal'):
            jals.append(os.path.join(_dir, x))

# print(jals)

names = []
for jal in jals:
    with open(jal, encoding='ISO-8859-1') as f:
        for line in f:
            try:
                comment_col = line.index('--')
            except ValueError:
                comment_col = -1

            if(comment_col > -1):
                line = line[0:comment_col]

            line = line.strip()

            if line == '':
                continue

            if line.startswith('function') or line.startswith('FUNCTION') or line.startswith('procedure') or line.startswith('PROCEDURE'):
                # print(line)
                sa = line.split()
                # print(sa)
                name = sa[1].split('(')[0]
                names.append(name)


names = list(dict.fromkeys(names))  # remove duplicates


out_path = 'jallib-funcs-procs.el'
header_lines = ''';;; This file was generated by mk_funcs_procs.py,
;;; and is intended to be `require'd from jal-mode.el.

(defvar jal-mode-builtins
  \'(;; from jalv2.pdf
    "_usec_delay"

    ;; from jallib/include/jal/*.jal
'''

from pathlib import Path
footer_lines = f''')
  "JAL\'s functions and prodedures")

(provide \'{Path(out_path).stem})
;;; {out_path} ends here
'''

with open(out_path, mode='w') as f:
    f.write(header_lines)

    names = ['    "' + x + '"\n' for x in names]
    names[-1] = names[-1][:-1]
    f.writelines(names)

    f.write(footer_lines)
