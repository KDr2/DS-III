#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Copyright © 2009-2011 Alexander Kojevnikov <alexander@kojevnikov.com>
# Copyright © 2011-2012 KDr2 <killy.draw@gmail.com>
#
# hilite.py is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# hilite.py is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.

# You should have received a copy of the GNU Affero General Public License
# along with hilite.me.  If not, see <http://www.gnu.org/licenses/>.
#
# ==== run hilite.py -h to see the usage. =====
#

import re
import sys
import optparse

from pygments import highlight
from pygments.lexers import get_lexer_by_name
from pygments.formatters import HtmlFormatter

default_div_style=";".join(['color:black',
                            'background:white',
                            'border:solid gray',
                            'border-width:.1em .1em .1em .8em',
                            'padding:.2em .6em'])+";"

def hilite_me(code, lexer=None, style=None, linenos=True, divstyles=''):
    lexer = lexer or 'python'
    style = style or 'default'
    defstyles = 'overflow:auto;width:auto;'

    formatter = HtmlFormatter(style=style,
                              linenos=False,
                              noclasses=True,
                              cssclass='',
                              cssstyles=defstyles + divstyles,
                              prestyles='margin: 0')
    html = highlight(code, get_lexer_by_name(lexer), formatter)
    if linenos:
        html = insert_line_numbers(html)
    html = "<!-- HTML generated using hilite.py -->\n" + html
    return html

def update_styles(style, divstyles):
    common_styles = 'border:solid gray;border-width:.1em .1em .1em .8em;padding:.2em .6em;'
    bw_styles = 'color:black;background:white;' + common_styles
    wb_styles = 'color:white;background:black;' + common_styles
    if not divstyles: divstyles = bw_styles

    if style in ('fruity', 'native'):
        if divstyles == bw_styles:
            divstyles = wb_styles
    else:
        if divstyles == wb_styles:
            divstyles = bw_styles
    return divstyles

def insert_line_numbers(html):
    match = re.search('(<pre[^>]*>)(.*)(</pre>)', html, re.DOTALL)
    if not match: return html

    pre_open = match.group(1)
    pre = match.group(2)
    pre_close = match.group(3)

    html = html.replace(pre_close, '</pre></td></tr></table>')
    numbers = range(1, pre.count('\n') + 1)
    format = '%' + str(len(str(numbers[-1]))) + 'i'
    lines = '\n'.join(format % i for i in numbers)
    html = html.replace(pre_open, '<table><tr><td>' + pre_open + lines + '</pre></td><td>' + pre_open)
    return html

def process_opt():
    opt=optparse.OptionParser()
    opt.add_option("-s","--source",
                   dest="source",
                   default=None,
                   help="source file, ignore this option to use stdin")
    opt.add_option("-l","--lexer",
                   dest="lexer",
                   default='text',
                   help="source type, like c/c++/python/diff ..., " + \
                       "see http://pygments.org/languages/ for more details")
    opt.add_option("-n","--lineno",
                   action="store_true",
                   dest="lineno",
                   default=False,
                   help="show line numbers")
    opt.add_option("-t","--style",
                   dest="style",
                   default='default',
                   help="highlight style:default/native/colorful...")
    (ret,tmp)=opt.parse_args()
    return ret

def main():
    opt=process_opt()
    src=sys.stdin
    if opt.source:src=file(opt.source)
    code=src.read()
    ret=hilite_me(code,
                  lexer=opt.lexer,
                  style=opt.style,
                  linenos=opt.lineno,
                  divstyles=default_div_style)
    print ret


if __name__=='__main__':
    main()

    
