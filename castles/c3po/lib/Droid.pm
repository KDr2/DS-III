#!/usr/bin/perl

package DroidState;
use warnings;
use strict;

use POE::Kernel;
use POE::Session;

use base qw(POE::Component::IRC::State);

sub _send_login { # copied from PoCo::IRC.pm
    my ($kernel, $self, $session) = @_[KERNEL, OBJECT, SESSION];

    # Now that we're connected, attempt to log into the server.

    # for servers which support CAP, it's customary to start with that
    if (0) { # the hack
        $kernel->call($session, 'sl_login', 'CAP REQ :identify-msg');
        $kernel->call($session, 'sl_login', 'CAP REQ :multi-prefix');
        $kernel->call($session, 'sl_login', 'CAP LS');
        $kernel->call($session, 'sl_login', 'CAP END');
    }

    if (defined $self->{password}) {
        $kernel->call($session => sl_login => 'PASS ' . $self->{password});
    }
    $kernel->call($session => sl_login => 'NICK ' . $self->{nick});
    $kernel->call(
        $session,
        'sl_login',
        'USER ' .
            join(' ', $self->{username},
                 (defined $self->{bitmode} ? $self->{bitmode} : 8),
                 '*',
                 ':' . $self->{ircname}
             ),
    );

    # If we have queued data waiting, its flush loop has stopped
    # while we were disconnected.  Start that up again.
    $kernel->delay(sl_delayed => 0);

    return;
}


package Droid;

use warnings;
use strict;

use Encode;

use POE::Kernel;
use POE::Session;

use Data::Dumper;
use POE::Component::IRC;
use POE::Component::IRC::Constants qw(:ALL);
use POE::Component::IRC::Plugin::CTCP;
use Router;

use base qw(Bot::BasicBot);

# copied from Bot::BasicBot.pm hack for PoCo::IRC::State and CAP commands
sub start_state {
    my ($self, $kernel, $session) = @_[OBJECT, KERNEL, SESSION];
    $kernel->sig('DIE', 'die');
    $self->{session} = $session;

    # Make an alias for our session, to keep it from getting GC'ed.
    $kernel->alias_set($self->{ALIASNAME});
    $kernel->delay('tick', 30);

    $self->{IRCOBJ} = DroidState->spawn( #hack here, use a mod package
        alias => $self->{IRCNAME},
    );

    $self->{IRCOBJ}->plugin_add(
        'Connector',
        POE::Component::IRC::Plugin::Connector->new(),
    );
    $self->{IRCOBJ}->plugin_add(
        'CTCP',
        POE::Component::IRC::Plugin::CTCP->new(
            version => "xChat"
        ),
    );

    $kernel->post($self->{IRCNAME}, 'register', 'all');

    $kernel->post(
        $self->{IRCNAME},
        'connect',
        {
            Nick      => $self->nick,
            Server    => $self->server,
            Port      => $self->port,
            Password  => $self->password,
            UseSSL    => $self->ssl,
            Flood     => $self->flood,
            LocalAddr => $self->localaddr,
            useipv6   => $self->useipv6,
            $self->charset_encode(
                Nick     => $self->nick,
                Username => $self->username,
                Ircname  => $self->name,
            ),
        },
    );

    return;
}

# common message callback
sub said {
    my ($self, $message) = @_;
    $self->{IRCOBJ}->debug(1);
    return Router->dispatch($self, $message);
}

sub _fork_said {
    my ($self, $body, $wheel_id) = @_[OBJECT, ARG0, ARG1];
    chomp $body;    # remove newline necessary to move data;

    # pick up the default arguments we squirreled away earlier
    my $args = $self->{forks}{$wheel_id}{args};
    $args->{body} = decode('utf-8',$body);

    $self->say($args);
    return;
}

# help text for the bot
sub help { "I'm annoying, and do nothing useful." }

# reconnect
sub irc_disconnected_state {
    my ($self, $kernel, $session) = @_[OBJECT, KERNEL, SESSION];
    $self->log("server disconnected.");
    #$poe_kernel->delay('_start', 2);
    $kernel->post(
        $self->{IRCNAME},
        'connect',
        {
            Nick      => $self->nick,
            Server    => $self->server,
            Port      => $self->port,
            Password  => $self->password,
            UseSSL    => $self->ssl,
            Flood     => $self->flood,
            LocalAddr => $self->localaddr,
            useipv6   => $self->useipv6,
            $self->charset_encode(
                Nick     => $self->nick,
                Username => $self->username,
                Ircname  => $self->name,
            ),
        },
    );
}

1;
