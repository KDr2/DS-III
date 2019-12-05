package DB;

use v5.16;
use strict;
use warnings;

use DBI;

use Utils;

sub get_dbh {
    state $dbh = undef;
    if (!$dbh) {
        my $dbfile = Utils->path("data/c3p0.db");
        $dbh = DBI->connect("dbi:SQLite:dbname=$dbfile","","");
    }
    return $dbh;
}

1;
