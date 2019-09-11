<?php

define("WEBSITE_ROOT", "PATH_TO_WEBSITE_ROOT");
define("UPLOAD_TOKEN", "a secure token");

function check_auth()
{
    return $_REQUEST['token'] == UPLOAD_TOKEN;
}
