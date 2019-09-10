<?php

define("WEBSITE_ROOT", "PATH_TO_WEBSITE_ROOT");
define("UPLOAD_TOKEN", "a secure token");

$_REQUEST['token'] != UPLOAD_TOKEN and die("error");
