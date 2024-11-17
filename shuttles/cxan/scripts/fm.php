<?
// curl http://cxan.kdr2.com/scripts/file-c.php \
//   -F token=${CXAN_TOKEN} -F file=@file \
//   -F dest=/path/to/dest.pkg -F alias=a,b,c \
//   -F unzip=/path/to/dir \
//   -F action=create|delete

include_once("file.inc.php");

function ok($msg="ok") {
    return ['status' => 'ok', 'message' => $msg];
}

function err($msg="unknown") {
    return ['status' => 'error', 'message' => $msg];
}

function expand_path_from_arg($arg)
{
    $val = $_REQUEST[$arg];
    return expand_path($val);
}

function expand_path($val)
{
    if (empty($val)) return "";
    if ($val[0] == ':') { // absolute path
       return substr($val, 1);
    }

    // prepend WEBSITE_ROOT
    $val[0] != '/' and $val = '/' . $val;
    return WEBSITE_ROOT . $val;
}

function unzip_file($input, $odir)
{
    $zip = new ZipArchive;
    $res = $zip->open($input);
    if ($res === TRUE) {
        $zip->extractTo($odir);
        $zip->close();
    }
}

function _create()
{
    if(!check_auth()) return err("Auth error.");
    $dest = expand_path_from_arg('dest');
    if(empty($dest)) return err("No `dest`.");

    $filename = basename($dest);
    $dir = dirname($dest);
    if(file_exists($dir)) {
        if(!is_dir($dir)){
            return err("EPATH-001");
        }
    } else {
        mkdir($dir, 0755, true);
    }

    if(empty($_FILES["file"])) return err("no file in request.");

    $file = $_FILES["file"];

    if ($file['error'] == UPLOAD_ERR_OK) {
        $tmp_name = $file["tmp_name"];
        move_uploaded_file($tmp_name, $dest);
        // alias
        $alias = $_REQUEST['alias'];
        if (!empty($alias)) {
            $aliases = explode(",", $alias);
            for ($i = 0; $i < count($aliases); $i++) {
                $alias = $aliases[$i];
                $alias = expand_path($alias);
                if(file_exists($alias)) unlink($alias);
                symlink($dest, $alias);
            }
        }

        $unzip = expand_path_from_arg("unzip");
        if (!empty($unzip)) {
            unzip_file($dest, $unzip);
        }
    } else {
        return err("UPLOAD(" . $file['error'] . ").");
    }
    return ok();
}

function _delete()
{
    if(!check_auth()) return err("Auth error.");
    $dest = expand_path_from_arg('dest');
    if(empty($dest)) return err("No `dest`.");
    if(!is_file($dest)) return err(" `dest` doesn't exist.");
    if(!unlink($dest)) return err("unlink failed.");
    return ok();
}


// --- entrance ---
$result = err("Nothing todo.");
if ($_REQUEST['action'] == 'delete') {
    $result = _delete();
} else {
    $result = _create();
}

header('Content-Type: application/json; charset=utf-8');
echo json_encode($result);
?>
