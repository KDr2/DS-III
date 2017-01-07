#!/usr/bin/env perl
use Mojolicious::Lite;
use Encode qw(encode decode);
use TextSharing;
use JSON;

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
    if (!$cb) {
        $c->render(json => $moments);
    } else {
        my $text = $cb . "(" . decode('UTF-8', encode_json($moments)) . ")";
        $c->render(text => $text);
    }
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
