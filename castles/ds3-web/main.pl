#!/usr/bin/env perl
use strict;
use warnings;

use lib "./lib";

use Mojolicious::Lite;
use Encode qw(encode decode);
use JSON;

use UserAction;
use Command;
use OpenSanctum;
use Moments;

# Documentation browser under "/perldoc"
# plugin 'PODRenderer';
plugin 'DS3Helpers';

any [qw/GET POST/] => '/' => \&UserAction::user_login;
get '/logout' => \&UserAction::user_logout;
get '/dashboard' => \&UserAction::dashboard;
any [qw/GET POST/] => '/board/command' => \&Command::run_command;
get '/open-sanctum/*page' => \&OpenSanctum::open_page;
get '/moments' => \&Moments::moments;


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
