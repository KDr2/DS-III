package DS3Helpers;

use v5.22;
use Data::Dumper;

use base 'Mojolicious::Plugin';

sub register {
    my ($self, $app) = @_;

    $app->helper(check_login => sub {
                     my $c = shift;
                     my $username = $c->session->{USERNAME};
                     return $username ? 1 : 0;
                 });
}

1;
