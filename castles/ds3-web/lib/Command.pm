package Command;

use v5.16;
use strict;
use warnings;

my %COMMANDS = (
    Default => "free",
    Dir => "ls",
    GCP_List_Instances => "gcloud compute instances list",
);


sub run_command {
  my $c = shift;
  return $c->redirect_to("/") unless ($c->check_login);

  my $cmd = $c->param("command");
  my $method = $c->req->method;
  if ($method eq "GET") {
      $cmd = "Default"; #"GCP_List_Instances";
  }

  my $output = join '', `$COMMANDS{$cmd} 2>&1`;

  $c->stash(
      commands => {%COMMANDS},
      cmd_label => $cmd,
      command => $COMMANDS{$cmd},
      stdout => $output);
  $c->render("/board-command");
}

1;
