#!/usr/bin/env perl

# cpanm install Devel::REPL
# cpanm install Term::ReadLine::Gnu

use Devel::REPL;
my $repl = Devel::REPL->new;
$repl->load_plugin($_) for qw(
                                 History
                                 LexEnv
                                 MultiLine::PPI
                                 Completion
                                 CompletionDriver::INC
                                 CompletionDriver::LexEnv
                                 CompletionDriver::Keywords
                                 CompletionDriver::Methods
                         );
#$|=1;

$repl->run()
