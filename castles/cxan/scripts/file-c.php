<?
// curl http://cxan.kdr2.com/scripts/file-c.php \
//   -F token=${CXAN_TOKEN} -F file=@file \
//   -F dest=/path/to/dest.pkg -F alias=a,b,c

include_once("file.inc.php");

function create_()
{
    $dest = $_REQUEST['dest'];
    if(empty($dest)) return "ERROR: No `dest`.";
    $dest[0] != '/' and $dest = '/' . $dest;

    $filename = basename($dest);
    $dir = dirname($dest);
    if(file_exists(WEBSITE_ROOT . $dir)) {
        if(!is_dir(WEBSITE_ROOT . $dir)){
            return "ERROR: `dest` is a directory.";
        }
    } else {
        mkdir(WEBSITE_ROOT . $dir, 0755, true);
    }

    if(empty($_FILES["file"])) return "ERROR: no file in request.";

    $file = $_FILES["file"];

    if ($file['error'] == UPLOAD_ERR_OK) {
        $tmp_name = $file["tmp_name"];
        $name = WEBSITE_ROOT . $dest;
        move_uploaded_file($tmp_name, $name);
        // alias
        $alias = $_REQUEST['alias'];
        if(!empty($alias)) {
            $aliases = explode(",", $alias);
            for ($i = 0; $i < count($aliases); $i++) {
                $alias = $aliases[$i];
                $alias[0] != '/' and $alias = '/' . $alias;
                $alias = WEBSITE_ROOT . $alias;
                if(file_exists($alias)) unlink($alias);
                symlink($name, $alias);
            }
        }
    } else {
        return "ERROR: UPLOAD(" . $file['error'] . ").";
    }
    return "ERROR: None.";
}

$result = create_();
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
