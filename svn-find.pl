#!/usr/bin/env perl

use strict;
use warnings FATAL => 'all';

use Getopt::Long;

GetOptions(
    'type=s'      => \my $type,
    'max-depth=i' => \my $max_depth,
    'min-depth=i' => \my $min_depth,
    'name=s'      => \my $name,
) and @ARGV or die "Usage: $0 [--type=f|--type=d] [--max-depth=N] [--min-depth=N] [--name=NAME] REPOS_URL ...\n";

for ( @ARGV ) {
    $_ .= '/' unless $_ =~ m{/$};
    svn_find( $_, 0 );
}

sub is_match {
    my $path = shift;

    my ( $basename, $is_dir ) = $path =~ m{([^/]+)(/?)$};                                           
    
    if ( defined $type ) {
        if ( $type eq 'f' ) {
            return if $is_dir;
        }
        elsif ( $type eq 'd' ) {
            return unless $is_dir;
        }
    }

    if ( defined $name and $name ne $basename ) {
        return;
    }

    return 1;    
}

sub svn_find {
    my ( $repos_url, $depth ) = @_;

    if ( defined $max_depth and $depth > $max_depth ) {
        return;
    }
    
    my @dirents = svn_ls( $repos_url );

    if ( ( not defined $min_depth ) or $depth >= $min_depth ) {
        print $repos_url . $_ . "\n" for grep { is_match($_) } @dirents;
    }
    
    for my $dir ( grep m{/$}, @dirents ) {
        svn_find( $repos_url . $dir, $depth + 1 );
    }    
}

sub svn_ls {
    my $svn_url = shift;
    
    open( my $ls, '-|', 'svn', 'ls', $svn_url )
        or die "svn ls $svn_url failed";
    map { chomp; $_ } <$ls>;
}



