package Command::TechTopic;

use v5.16;
use strict;
use warnings;
use experimental 'autoderef';

use YAML;
use File::Slurp;

use DB;
use Utils;

sub do_cmd {
    my ($class, $droid, $message) = @_;
    my $text = (split /\s+/, $message->{body}, 2)[1];
    $text //= "";
    chomp $text;

    if ($text =~ m/^pick(\s+reset)?$/i) {
        my $reset = $1 // 0;
        return __PACKAGE__->pick_topic($reset);
    } elsif ($text =~ m/^s(?:earch)?-?p(?:ick)?(\s+reset)?\s+(.+)$/i) {
        my $reset = $1 // 0;
        return __PACKAGE__->search_pick_topic($2, $1);
    } elsif ($text =~ m/^cont(?:inue)?$/i) {
        return __PACKAGE__->continue_topic;
    } elsif ($text =~ m/^reload$/i) {
        __PACKAGE__->load_topics(1);
        return "Tech topic list is reloaded.";
    } elsif ($text =~ m/^set\s+(\d+)\s+(?:pts|points?)$/i) {
        return __PACKAGE__->set_pts_for_topic($1);
    } elsif ($text =~ m/^set\s+(\d+)\s+(?:pts|points?)\s+for\s+(\d+)$/i) {
        return __PACKAGE__->set_pts_for_topic($1, $2);
    } elsif ($text =~ m/^(?:list\s+recent|l)(?:\s+(\d+))?$/i) {
        return __PACKAGE__->list_recent_topics($1);
    } elsif ($text =~ m/^r(?:eport)?(?:\s+(\S+))?$/i) {
        return __PACKAGE__->topic_pts_report($1);
    }
    return "ERROR: bad agrument for tech-topic";
}

sub _flatten_tree {
    my ($target_ref, $srcref, $parent_path) = @_;
    if (ref $srcref eq "HASH") {
        while (my($key, $value) = each %{$srcref}) {
            my $full_path = $parent_path ? "$parent_path/$key" : $key;
            _flatten_tree($target_ref, $value, $full_path);
        }
    } elsif (ref $srcref eq "ARRAY") {
        _flatten_tree($target_ref, $_, $parent_path) for @$srcref;
    } else {
        push $target_ref, "$parent_path/$srcref";
    }
}

sub load_topics {
    my ($class, $force_reload) = @_;
    state $topics = undef;
    if ((!$topics) || $force_reload) {
        $topics = [];
        my $yaml_content = read_file(Utils->path("data/tech-topic.yaml"));
        my ($hashref, $arrayref, $string) = Load($yaml_content);
        _flatten_tree($topics, $hashref);
    }
    return $topics;
}

sub pick_topic_randomly {
    my $topics = __PACKAGE__->load_topics;
    my $idx = int(rand($#$topics));
    return $topics->[$idx];
}

sub pick_topic {
    my ($class, $reset) = @_;
    my $today = Utils->date_ymd;
    my $dbh = DB->get_dbh;
    my $sth = $dbh->prepare("SELECT * FROM tech_topic WHERE DATE = ? LIMIT 1");
    $sth->execute($today);
    my @row = $sth->fetchrow_array;
    if (@row) {
        return "Your tech topic for today was already picked ($row[2]) !" if !$reset;
    }
    my $topic = __PACKAGE__->pick_topic_randomly;
    if (@row) {
        return "I can't reset your tech topic for today ($row[2]) which has a non-zero points!" if $row[4] > 0;
        $sth = $dbh->prepare("UPDATE tech_topic SET topic = ? , short_name = ?, points = 0 WHERE id = ?");
        $sth->execute($topic, $topic =~ s/(^.+\/)//gr, $row[0]);
    } else {
        $sth = $dbh->prepare("INSERT INTO tech_topic(date, topic, short_name, points) VALUES(?, ?, ?, 0)");
        $sth->execute($today, $topic, $topic =~ s/(^.+\/)//gr);
    }
    return "Tech topic for today: [$topic].";
}

sub continue_topic {
    my $today = Utils->date_ymd;
    my $dbh = DB->get_dbh;
    my $sth = $dbh->prepare("SELECT * FROM tech_topic WHERE DATE = ? LIMIT 1");
    $sth->execute($today);
    my @row = $sth->fetchrow_array;
    if (@row) {
        return "Your tech topic for today was already picked ($row[2]) !";
    }

    $sth = $dbh->prepare("SELECT * FROM tech_topic ORDER BY id DESC LIMIT 1");
    $sth->execute();
    @row = $sth->fetchrow_array;
    if (@row) {
        my $topic = $row[2];
        $sth = $dbh->prepare("INSERT INTO tech_topic(date, topic, short_name, points) VALUES(?, ?, ?, 0)");
        $sth->execute($today, $topic, $topic =~ s/(^.+\/)//gr);
        return "Your tech topic for today is set to ($topic) by cont.!";
    }
    return "No tech topic to be continued.";
}

sub search_pick_topic {
    my ($class, $regexp, $reset) = @_;
    my $today = Utils->date_ymd;
    my $dbh = DB->get_dbh;
    my $sth = $dbh->prepare("SELECT * FROM tech_topic WHERE DATE = ? LIMIT 1");
    $sth->execute($today);
    my @row = $sth->fetchrow_array;
    if (@row) {
        return "Your tech topic for today was already picked ($row[2]) !" if !$reset;
    }

    my @search_result = grep { eval '$_ =~ m/' . $regexp . "/i" } @{load_topics()};
    if ($#search_result > 0) {
        return "ERROR: more than one item found: " . join(",", @search_result);
    } elsif ($#search_result < 0) {
        return "Error: no item found with keyword `$regexp'";
    }
    my $topic = $search_result[0];
    if (@row) {
        return "I can't reset your tech topic for today ($row[2]) which has a non-zero points!" if $row[4] > 0;
        $sth = $dbh->prepare("UPDATE tech_topic SET topic = ? , short_name = ?, points = 0 WHERE id = ?");
        $sth->execute($topic, $topic =~ s/(^.+\/)//gr, $row[0]);
    } else {
        $sth = $dbh->prepare("INSERT INTO tech_topic(date, topic, short_name, points) VALUES(?, ?, ?, 0)");
        $sth->execute($today, $topic, $topic =~ s/(^.+\/)//gr);
    }
    return "Tech topic for today: [$topic].";
}

sub set_pts_for_topic {
    my ($class, $pts, $tid) = @_;
    my ($ret, $sth);
    my $dbh = DB->get_dbh;
    if ($tid) {
        $sth = $dbh->prepare("UPDATE tech_topic SET points = ? WHERE id = ?");
        $sth->execute($pts, $tid);
        $ret = "Points of topic $tid is set to $pts.";
    } else {
        my $today = Utils->date_ymd;
        $sth = $dbh->prepare("UPDATE tech_topic SET points = ? WHERE date = ?");
        $sth->execute($pts, $today);
        $ret = "Points of today's topic is set to $pts.";
    }
    return $ret;
}

sub list_recent_topics {
    my ($class, $num) = @_;
    $num //= 3;
    my ($ret, $cnt) = ("", 0);
    my $dbh = DB->get_dbh;
    my $sth = $dbh->prepare("SELECT * FROM tech_topic ORDER BY date DESC LIMIT ?");
    $sth->execute($num);
    while (my @row = $sth->fetchrow_array) {
        $cnt++;
        $ret .= "\t" . $row[0] . "\t" . $row[1] . "\t" . $row[2] . "\t" . $row[4] . "\n";
    }
    return "No topic find." if $cnt < 1;
    $ret = "Recent $cnt tech topic(s):\n" . $ret;
    return $ret;
}

sub topic_pts_report {
    my ($class, $date_prefix) = @_;
    my ($where, $ret, $cnt) = ("", "", 0);
    $where = " WHERE date like ? " if defined($date_prefix);
    my $dbh = DB->get_dbh;
    my $sth = $dbh->prepare("SELECT topic, sum(points) AS pts FROM tech_topic $where GROUP BY topic HAVING pts > 0 ORDER BY pts DESC");
    if (defined($date_prefix)) {
        $sth->execute("$date_prefix%");
    } else {
        $sth->execute();
    }
    while (my @row = $sth->fetchrow_array) {
        $cnt++;
        $ret .= "\t" . $row[1] . "\t" . $row[0] . "\n";
    }
    return "No points to report." if $cnt < 1;
    $ret = "Points report for all $cnt tech topic(s):\n" . $ret;
    return $ret;
}

1;
