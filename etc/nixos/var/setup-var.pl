use strict;
use File::Find;
use File::Copy;
use File::Path;
use File::Basename;
use File::Slurp;

my $var = $ARGV[0] or die;
my $static = "/var/static";

sub atomicSymlink {
    my ($source, $target) = @_;
    my $tmp = "$target.tmp";
    unlink $tmp;
    symlink $source, $tmp or return 0;
    rename $tmp, $target or return 0;
    return 1;
}


# Atomically update /var/static to point at the var files of the
# current configuration.
atomicSymlink $var, $static or die;

# Returns 1 if the argument points to the files in /var/static.  That
# means either argument is a symlink to a file in /var/static or a
# directory with all children being static.
sub isStatic {
    my $path = shift;

    if (-l $path) {
        my $target = readlink $path;
        return substr($target, 0, length "/var/static/") eq "/var/static/";
    }

    if (-d $path) {
        opendir DIR, "$path" or return 0;
        my @names = readdir DIR or die;
        closedir DIR;

        foreach my $name (@names) {
            next if $name eq "." || $name eq "..";
            unless (isStatic("$path/$name")) {
                return 0;
            }
        }
        return 1;
    }

    return 0;
}

# Remove dangling symlinks that point to /var/static.  These are
# configuration files that existed in a previous configuration but not
# in the current one.  For efficiency, don't look under /var/nixos
# (where all the NixOS sources live).
sub cleanup {
    if ($File::Find::name eq "/var/nixos") {
        $File::Find::prune = 1;
        return;
    }
    if (-l $_) {
        my $target = readlink $_;
        if (substr($target, 0, length $static) eq $static) {
            my $x = "/var/static/" . substr($File::Find::name, length "/var/");
            unless (-l $x) {
                print STDERR "removing obsolete symlink ‘$File::Find::name’...\n";
                unlink "$_";
            }
        }
    }
}

find(\&cleanup, "/var");


# Use /var/.clean to keep track of copied files.
my @oldCopied = read_file("/var/.clean", chomp => 1, err_mode => 'quiet');
open CLEAN, ">>/var/.clean";


# For every file in the var tree, create a corresponding symlink in
# /var to /var/static.  The indirection through /var/static is to make
# switching to a new configuration somewhat more atomic.
my %created;
my @copied;

sub link {
    my $fn = substr $File::Find::name, length($var) + 1 or next;
    my $target = "/var/$fn";
    File::Path::make_path(dirname $target);
    $created{$fn} = 1;

    # Rename doesn't work if target is directory.
    if (-l $_ && -d $target) {
        if (isStatic $target) {
            rmtree $target or warn;
        } else {
            warn "$target directory contains user files. Symlinking may fail.";
        }
    }

    if (-e "$_.mode") {
        my $mode = read_file("$_.mode"); chomp $mode;
        if ($mode eq "direct-symlink") {
            atomicSymlink readlink("$static/$fn"), $target or warn;
        } else {
            my $uid = read_file("$_.uid"); chomp $uid;
            my $gid = read_file("$_.gid"); chomp $gid;
            copy "$static/$fn", "$target.tmp" or warn;
            $uid = getpwnam $uid unless $uid =~ /^\+/;
            $gid = getgrnam $gid unless $gid =~ /^\+/;
            chown int($uid), int($gid), "$target.tmp" or warn;
            chmod oct($mode), "$target.tmp" or warn;
            rename "$target.tmp", $target or warn;
        }
        push @copied, $fn;
        print CLEAN "$fn\n";
    } elsif (-l "$_") {
        atomicSymlink "$static/$fn", $target or warn;
    }
}

find(\&link, $var);


# Delete files that were copied in a previous version but not in the
# current.
foreach my $fn (@oldCopied) {
    if (!defined $created{$fn}) {
        $fn = "/var/$fn";
        print STDERR "removing obsolete file ‘$fn’...\n";
        unlink "$fn";
    }
}


# Rewrite /var/.clean.
close CLEAN;
write_file("/var/.clean", map { "$_\n" } @copied);

# Create /var/NIXOS tag if not exists.
# When /var is not on a persistent filesystem, it will be wiped after reboot,
# so we need to check and re-create it during activation.
open TAG, ">>/var/NIXOS";
close TAG;
