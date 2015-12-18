#!/bin/bash

cpanm -n Devel::REPL Term::ReadLine::Gnu || exit 1

PERL=$(which perl)
REPL=$(which re.pl)

[ -e $REPL.bak ] || cp $REPL $REPL.bak

chmod u+w $REPL
cat > $REPL <<EOF
#!$PERL

use Devel::REPL;
my \$repl = Devel::REPL->new;
\$repl->load_plugin(\$_) for qw(
                                 History
                                 LexEnv
                                 MultiLine::PPI
                                 Completion
                                 CompletionDriver::INC
                                 CompletionDriver::LexEnv
                                 CompletionDriver::Keywords
                                 CompletionDriver::Methods
                         );
#\$|=1;

\$repl->run()

EOF

chmod u-w $REPL
