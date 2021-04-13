#!/usr/bin/perl

use strict;
use warnings;

use Text::CSV;
use Kx;

# get data from http://depot.kdr2.com/data/quant/data-cnstock-quote-2016.csv.zip

my $k = Kx->new(host=>'localhost', port=>2222);
$k->connect() or die "Can't connect to Kdb+ server";

sub one_line {
    my $row = shift;
    my $data = <<EOF;
           @{[$row->{TDATE}=~ s/(\d{4})(\d{2})(\d{2})/$1.$2.$3/r]};
           `\$"@{[$row->{SYMBOL}]}";
           0x@{[$row->{EXCHANGE} eq "CNSESH" or 0]};
           @{[$row->{LCLOSE} or 0]}f;
           @{[$row->{TOPEN} or 0]}f;
           @{[$row->{TCLOSE} or 0]}f;
           @{[$row->{HIGH} or 0]}f;
           @{[$row->{LOW} or 0]}f;
           @{[$row->{VOTURNOVER} or 0]}f;
           @{[$row->{VATURNOVER} or 0]}f;
           @{[$row->{NDEALS} or 0]}i;
           @{[$row->{AVGPRICE} or 0]}f;
           @{[$row->{AVGVOLPD} or 0]}f;
           @{[$row->{AVGVAPD} or 0]}f;
           @{[$row->{CHG} or 0]}f;
           @{[$row->{PCHG} or 0]}f;
           @{[$row->{PRANGE} or 0]}f;
           @{[$row->{MCAP} or 0]}f;
           @{[$row->{TCAP} or 0]}f;
           @{[$row->{TURNOVER} or 0]}f
EOF
    $data =~ s/[\s\n]//mg;
    my $insert_cmd = "insert[`cnstkqt]($data)";
    print($insert_cmd, "\n");
    $k->cmd($insert_cmd);
}

sub main {
    my $csv = Text::CSV->new ( { binary => 1 } )  # should set binary attribute.
        or die "Cannot use CSV: ".Text::CSV->error_diag ();

    $csv->eol("\n");
    my $fh = *STDIN;
    $csv->column_names(@{$csv->getline($fh)});

    while (my $row = $csv->getline_hr($fh)) {
        one_line($row);
    }
}

main();
