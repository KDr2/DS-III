<?

// curl http://cxan.kdr2.com/pkg-c.php -F file=@file -F dest=/path/to/dest.pkg -F token=${CXAN_TOKEN}

include_once("pkg.inc.php");

$dest = $_REQUEST['dest'];
$dest[0] != '/' and $dest = '/' . $dest;


$filename = basename($dest);
$dir = dirname($dest);
if(file_exists(WEBSITE_ROOT . $dir)) {
    if(!is_dir(WEBSITE_ROOT . $dir)){
        die("error");
    }
} else {
    mkdir(WEBSITE_ROOT . $dir, 0755, true);
}

empty($_FILES["file"]) and die("error");

$file = $_FILES["file"];

if ($file['error'] == UPLOAD_ERR_OK) {
    $tmp_name = $file["tmp_name"];
    $name = WEBSITE_ROOT . $dest;
    move_uploaded_file($tmp_name, $name);
} else {
    die("error");
}

echo "ok";
