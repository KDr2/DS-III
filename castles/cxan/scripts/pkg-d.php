<?

// curl http://cxan.kdr2.com/pkg-d.php -F dest=/path/to/dest.pkg -F token=${CXAN_TOKEN}

include_once("pkg.inc.php");

$dest = $_REQUEST['dest'];
$dest[0] != '/' and $dest = '/' . $dest;

!is_file(WEBSITE_ROOT . $dest) and die("error");
unlink(WEBSITE_ROOT . $dest) or die("error");
die("ok");
