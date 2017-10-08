package OpenSanctum;

use v5.22;
use utf8;

use MIME::Base64;
use Mojo::DOM;
use File::Slurp;

# Documentation browser under "/perldoc"
# plugin 'PODRenderer';

my @ROOT = qw{
                 /home/kdr2/Work/mine/sanctum/output/
                 /mnt/hgfs/D/work/mine/sanctum/output/
                 /home/kdr2/sanctum/
                 /mnt/d/work/mine/sanctum/
         };
my $ROOT = (grep { -d $_ } @ROOT)[0];


sub auth_info {
    my %data;
    my $permfile = $ROOT . "public.html";
    my $dom = Mojo::DOM->new(scalar read_file($permfile, {binmode => ':utf8'}));
    my $auth_blocks = $dom->find("pre.auth-info");
    for my $auth_block (@$auth_blocks) {
        my $auth_text = $auth_block->text;
        $auth_text =~ s/\s+//gi;
        $auth_text = encode_base64($auth_text);
        $auth_text =~ s/\s+//gi;
        my $pages = $auth_block->parent->find("li a")->map(attr=>"href");
        $data{$auth_text} = $pages;
    }
    return %data;
};


sub permission {
    # 0 - Auth Need, 1 - OK, 2 -Faileds, 3 - Not found
    my $c = shift;
    my $page = shift;
    my $pub64 = q/cHVibGlj/;
    my $item;
    return 3 if (! -f $ROOT . $page);
    return 1 if $page =~ m{^static/};
        my %auth_data = auth_info();
    for $item(@{$auth_data{$pub64}}) {
        return 1 if $item eq $page;
    }

    # auth header: Authorization: Basic base64_of_u:p
    my $auth_header = $c->req->headers->authorization;
    return 0 if not $auth_header;
    $auth_header =~ s/.*\s+(\S+)/$1/gi;
    for $item(@{$auth_data{$auth_header} // []}) {
        return 1 if $item eq $page;
    }
    return 0;
};


sub file_content {
    my $page = shift;
    my $text = scalar read_file($ROOT . $page, {binmode => ':utf8'});
    $text =~ s{"/static/}{"/open-sanctum/static/}gi;
    return $text;
};

1;
