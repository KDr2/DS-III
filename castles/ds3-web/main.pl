#!/usr/bin/env perl
use Mojolicious::Lite;
use Encode qw(encode decode);
use JSON;

use TextSharing;
use MomentsQuip;
use OpenSanctum;

# Documentation browser under "/perldoc"
# plugin 'PODRenderer';

get '/' => sub {
  my $c = shift;
  $c->render(template => 'home');
};


get '/moments' => sub {
    my $c = shift;
    my $cb = $c->param("callback");
    my $moments = TextSharing->get_moments;
    # my $moments = MomentsQuip->get_moments;
    if (!$cb) {
        $c->render(json => $moments);
    } else {
        my $text = $cb . "(" . decode('UTF-8', encode_json($moments)) . ")";
        $c->render(text => $text);
    }
};


get '/open-sanctum/*page' => sub {
    my $c = shift;
    my $page = $c->stash('page');

    my $perm = OpenSanctum::permission($c, $page);
    if ($perm == 0) {
        $c->res->headers->www_authenticate('Basic realm="The Open Part of KDr2\'s Workspace"');
        $c->render(text => "", status => 401);
        return;
    } elsif ($perm == 2) {
        $c->render(text => "Forbidden", status => 403);
        return;
    } elsif ($perm == 3) {
        $c->render(text => "File Not Found", status => 404);
        return;
    }
    $c->render(text => OpenSanctum::file_content($page), format=> $page =~ s/.*\.([^.]+)/$1/r);
};


app->start;
__DATA__

@@ index.html.ep
% layout 'default';
% title 'Welcome';
<h1>Welcome to the DS3-WEB!</h1>

@@ layouts/default.html.ep
<!DOCTYPE html>
<html>
  <head><title><%= title %></title></head>
  <body>
  <%= content %>
  </body>
</html>
