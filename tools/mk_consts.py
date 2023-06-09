# mk_consts.py --- Generate name-list of constants
#
# Usage:
#   $ cd jal-mode     <-- Root directory of this project
#   $ python3 tools/mk_consts.py
#
#   output --> jal-mode/jallib-consts.el
#
# Workflow:
#
#  jallib/include/jal/      jal-mode/tools/    jal-mode/
#  +-------------+         o==============o    +----------------------------+
#  | constants   |        /              /     |      jallib-consts.el      |
#  | _jallib.jal |  -->  / mk_consts.py /  --> |------->8-----//---->8------|
#  +-------------+      /              /       | (defvar jal-mode-constants |
#                      o==============o        |   '( "BAR"                 |
#                                              |        :                   |
#                                              |-------8<-----//----8<------|
#                      jal-mode/                              |
#                      +-------------------------------+      |
#                      |         jal-mode.el           |      |
#                      |-->8------>8---//--->8----->8--|      |
#                      |    :                          |      |
#                      | (require 'jallib-consts)      |  <---+
#                      |    :      ^^^^^^^^^^^^^       |
#                      |--8<------8<--//----8<-----8<--|
import os

home = os.getenv('HOME')
const_src = os.path.join(home, 'git-clone/jallib/include/jal/constants_jallib.jal')

names = []
with open(const_src, encoding='ISO-8859-1') as f:
    for line in f:
        try:
            comment_col = line.index('--')
        except ValueError:
            comment_col = -1

        if(comment_col > -1):
            line = line[0:comment_col]

        line = line.rstrip()

        if line == '':
            continue
        sa = line.split()
        names.append(sa[-3])
        # print('"' + sa[-3] + '"')

names = list(dict.fromkeys(names))  # remove duplicates


out_path = 'jallib-consts.el'
header_lines = ''';;; This file was generated by jallib_consts.py,
;;; and is intended to be `require'd from jal-mode.el.

(defvar jal-mode-constants
  \'('''

from pathlib import Path
footer_lines = f''')
  "JAL\'s constants from jallib/include/jal/constants_jallib.jal")

(provide \'{Path(out_path).stem})
;;; {out_path} ends here
'''

with open(out_path, mode='w') as f:
    f.write(header_lines)

    names = ['    "' + x + '"\n' for x in names]
    names[0] = names[0].lstrip()
    names[-1] = names[-1][:-1]
    f.writelines(names)

    f.write(footer_lines)
