grammar INIFile {
    token key { \w+ }
    token value { \N+ }
    token entry { \h* <key> \h* '=' \h* <value> \h*}
    token TOP {
        ^
        [
            | <entry> \n
            | \h* \n
        ]+
        \h*
        $
    }
}

INIFile.HOW.trace-on(INIFile);

my $m := INIFile.parse(
    Q{
        name = Animal Facts
        author = jnthn
    }
);

for $m<entry> -> $entry {
    say("Key: {$entry<key>}, Value: {$entry<value>}");
}
say($m)
