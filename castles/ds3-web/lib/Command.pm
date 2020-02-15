package Command;

use v5.16;
use strict;
use warnings;

my %COMMANDS = (
    Default => "uptime",
    Memory => "free -m",
    GCP_List_Instances => "gcloud compute instances list",
    GCP_List_Machine_Types => "gcloud compute machine-types list",
    GCP_Stop_Instance1 =>
    "gcloud compute instances stop instance-1 --zone=asia-east1-b",
    GCP_Boot_Instance1 =>
    "gcloud compute instances start instance-1 --zone=asia-east1-b",
    GCP_Resize_Instance1_f1_micro =>
    "gcloud compute instances set-machine-type instance-1 --machine-type=f1-micro --zone=asia-east1-b",
    GCP_Resize_Instance1_g1_small =>
    "gcloud compute instances set-machine-type instance-1 --machine-type=g1-small --zone=asia-east1-b",
    GCP_Resize_Instance1_n1_highcpu2 =>
    "gcloud compute instances set-machine-type instance-1 --machine-type=n1-highcpu-2 --zone=asia-east1-b",
    GCP_Resize_Instance1_n1_highcpu4 =>
    "gcloud compute instances set-machine-type instance-1 --machine-type=n1-highcpu-4 --zone=asia-east1-b",
    GCP_Resize_Instance1_n1_highmem2 =>
    "gcloud compute instances set-machine-type instance-1 --machine-type=n1-highmem-2 --zone=asia-east1-b",
    GCP_Resize_Instance1_n1_highmem4 =>
    "gcloud compute instances set-machine-type instance-1 --machine-type=n1-highmem-4 --zone=asia-east1-b",
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
      return_code => $?,
      stdout => $output,
  );
  $c->render("/board-command");
}

1;
