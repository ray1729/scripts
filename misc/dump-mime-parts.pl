#!/usr/bin/perl

use MIME::Parser;

my $parser = new MIME::Parser;

$parser->output_under(".");
$entity = $parser->parse(\*STDIN) or die "parse failed";
$entity->dump_skeleton();
