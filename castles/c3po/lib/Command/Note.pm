package Command::Note;

use v5.16;
use strict;
use warnings;

use DB;
use Utils;

use constant {
    NT_ST_NORMAL  => 1,
    NT_ST_DELETED  => 2,
};


sub do_cmd {
    my ($class, $droid, $message) = @_;
    my $text = (split /\s+/, $message->{body}, 2)[1];
    $text //= "";
    chomp $text;

    if ($text =~ m/^(?:c|new)\s+(.+)$/i) {
        return __PACKAGE__->new_note($1);
    } elsif ($text =~ m/^e(?:dit)?\s+(\d+)\s+(.+)$/i) {
        return __PACKAGE__->edit_note($1, $2);
    } elsif ($text =~ m/^del(?:ete)?\s+(\d+)$/i) {
        return __PACKAGE__->del_note(int($1));
    } elsif ($text =~ m/^(?:list\s+recent|l)(?:\s+(\d+))?$/i) {
        return __PACKAGE__->list_recent_notes($1);
    }
    return "ERROR: bad agrument for note";
}

sub new_note {
    my ($class, $text) = @_;
    my $dbh = DB->get_dbh;
    my $today = Utils->date_ymd;
    my $sth = $dbh->prepare("INSERT INTO note(date, content, due_to, status) VALUES(?, ?, ?, ?)");
    $sth->execute($today, $text, $today, NT_ST_NORMAL);
    return "Note saved.";
}

sub edit_note {
    my ($class, $id, $regexp) = @_;
    my $dbh = DB->get_dbh;
    my $sth = $dbh->prepare("SELECT * FROM note WHERE id = ?");
    $sth->execute($id);
    my $row = $sth->fetchrow_hashref;
    if ($row) {
        my $content = $row->{content};
        eval '$content =~ ' . $regexp;
        return "ERROR: note edit error: $@" if $@;
        $sth = $dbh->prepare("UPDATE note SET content = ? WHERE id = ?");
        $sth->execute($content, $id);
        return "Note <$id> edited.";
    }
    return "No such note <$id>.";
}

sub del_note {
    my ($class, $id) = @_;
    my $dbh = DB->get_dbh;
    my $sth = $dbh->prepare("UPDATE note SET  status = ? WHERE id = ?");
    $sth->execute(NT_ST_DELETED, $id);
    if ($sth->rows) {
        return "Note <$id> deleted.";
    }
    return "No such note <$id>.";
}

sub list_recent_notes {
    my ($class, $num) = @_;
    $num //= 3;
    my ($ret, $cnt) = ("", 0);
    my $dbh = DB->get_dbh;
    my $sth = $dbh->prepare("SELECT * FROM note WHERE status = ? ORDER BY id DESC LIMIT ?");
    $sth->execute(NT_ST_NORMAL, $num);
    while (my @row = $sth->fetchrow_array) {
        $cnt++;
        $ret .= "\t" . $row[0] . "\t" . $row[1] . "\t" . $row[2] . "\n";
    }
    $ret = "Recent $cnt note(s):\n" . $ret;
    return $ret;
}

1;
