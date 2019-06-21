package UserAction;

use v5.16;
use strict;
use warnings;

use Vault;

sub user_login {
    my $c = shift;
    return $c->redirect_to("/dashboard") if ($c->check_login);

    my $method = $c->req->method;
    if ($method eq "GET") {
        return $c->render("/welcome");
    }

    my $username = $c->param("username");
    my $password = $c->param("password");
    my $login_ok = ($username eq $DS3_USERNAME) && ($password eq $DS3_PASSWORD);
    if ($login_ok) {
        $c->session->{USERNAME} = $username;
        return $c->redirect_to("/dashboard");
    } else {
        $c->render("/welcome");
    }
}

sub user_logout {
    my $c = shift;
    delete $c->session->{USERNAME};
    return $c->redirect_to("/");
}

sub dashboard {
    my $c = shift;
    return $c->redirect_to("/") unless ($c->check_login);
    $c->render("/dashboard");
}

1;
