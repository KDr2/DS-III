package Command::Private;

use v5.16;
use strict;
use warnings;

use YAML;
use File::Slurp;
use Encode;
use IO::CaptureOutput qw(capture qxx qxy);

use Utils;
use Command::TechTopic;
use Command::Note;

sub do_cmd {
    my ($class, $cmd, $droid, $message) = @_;
    my $text = $message->{body};
    my $method_name = qq/do_cmd_$cmd/;
    return __PACKAGE__->$method_name($droid, $message);
}

sub do_cmd_run {
    my ($class, $droid, $message) = @_;
    my $text = $message->{body};
    return $droid->forkit(
        run => \&run_cmd,
        who => $message->{who},
        channel => $message->{channel},
        arguments => [$text, 0]
    )
}

sub do_cmd_srun {
    my ($class, $droid, $message) = @_;
    my $text = $message->{body};
    return run_cmd(undef, $text, 1);
}

sub run_cmd { # cmd
    my ($body, $cmd, $sync) = @_;
    my $output;
    $cmd =~ s/^,s?run\b\s*//i;
    if ($cmd =~ m/^\s*$/) {
        $output = "ERROR: can not run empty command.";
    } else {
        #open(my $proc_cmd, $cmd . "|") or return "ERROR: invalid command `$cmd'.";
        #my $output = read_file($proc_cmd);
        my ($stdout, $stderr, $success) = qxx( $cmd );
        $stdout = decode('utf-8', $stdout);
        $stderr = decode('utf-8', $stderr);
        $output = "=Result for command `$cmd':\n";
        $output .= "--- STDOUT ---:\n$stdout\n" if $stdout;
        $output .= "--- STDERR ---:\n$stderr\n" if $stderr;
        $output .= "..............=OUTPUT-END"
    }
    if ((scalar (split "\n", $output, 10)) > 9) {
        $output = "=Result for command `$cmd' is pasted at: " . Utils->paste($output);
    }
    return $output if $sync;
    print $output, "\n";
}

sub do_cmd_tech_topic {
    my ($class, $droid, $message) = @_;
    return Command::TechTopic->do_cmd($droid, $message);
}

sub do_cmd_note {
    my ($class, $droid, $message) = @_;
    return Command::Note->do_cmd($droid, $message);
}

1;
