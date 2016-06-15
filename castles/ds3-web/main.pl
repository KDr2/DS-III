#!/usr/bin/env perl
use Mojolicious::Lite;
use Encode qw(encode decode);
use Tweet;
use JSON;

# Documentation browser under "/perldoc"
# plugin 'PODRenderer';

get '/' => sub {
  my $c = shift;
  $c->render(template => 'index');
};

get '/tweets' => sub {
    my $c = shift;
    my $cb = $c->param("callback");
    my $tweets = Tweet->get_tweets;
    if (!$cb) {
        $c->render(json => $tweets);
    } else {
        my $text = $cb . "(" . decode('UTF-8', encode_json($tweets)) . ")";
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
  <body><%= content %></body>
</html>
