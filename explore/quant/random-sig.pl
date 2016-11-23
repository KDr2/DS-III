#!/usr/bin/perl

use strict;
use warnings;

use Date::Calc;
use Text::CSV;

sub randsig {
    my $n = int(rand(10));
    return 1 if $n >= 7;
    return -1 if $n <=2;
    return 0;
}

my @start_date = (2015, 1, 1);
my @end_date = (2016, 1, 1);
my @symbols = ("000001.XSHE","000002.XSHE");

print STDERR "$_\n" for (@symbols);

my $csv = Text::CSV->new ( { binary => 1 } )  # should set binary attribute.
    or die "Cannot use CSV: ".Text::CSV->error_diag ();
$csv->eol("\n");

do{
    my $tdate = sprintf("%04d%02d%02d", @start_date);
    for (@symbols) {
        $csv->print(*STDOUT, [$tdate, $_, randsig]);
    }
    @start_date = Date::Calc::Add_Delta_Days(@start_date, 1);
} while (Date::Calc::Delta_Days(@start_date, @end_date) > 0);

__END__
