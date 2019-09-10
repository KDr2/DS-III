<?php
// curl http://cxan.kdr2.com/scripts/file-d.php \
//     -F dest=/path/to/dest.pkg -F token=${CXAN_TOKEN}

include_once("file.inc.php");

function delete_()
{
    $dest = $_REQUEST['dest'];
    if(empty($dest)) return "ERROR: No `dest`.";
    $dest[0] != '/' and $dest = '/' . $dest;

    if(!is_file(WEBSITE_ROOT . $dest)) return "ERROR: `dest` doesn't exist.";
    if(!unlink(WEBSITE_ROOT . $dest)) return "ERROR: unlink failed.";
}

$result = delete_();
?>
<!doctype html>
<html lang=en>
    <head>
        <meta charset=utf-8>
        <title>CXAN</title>
    </head>
    <body>
        INFO: <b><?php echo $result; ?></b>
    </body>
</html>
