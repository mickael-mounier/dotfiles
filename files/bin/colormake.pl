#!/usr/bin/perl

use Cwd;
use Env qw(HOME);

$col_norm       = "\033[00m";
$col_brighten   = "\033[01m";
$col_underline  = "\033[04m";
$col_blink      = "\033[05m";
$col_background = "\033[07m";

$col_black      = "\033[30m";
$col_red        = "\033[31m";
$col_green      = "\033[32m";
$col_brown      = "\033[33m";
$col_blue       = "\033[34m";
$col_purple     = "\033[35m";
$col_cyan       = "\033[36m";
$col_ltgray     = "\033[37m";

$col_path =         $col_purple;
$col_default =      $col_ltgray;
$col_gcc =          $col_purple.$col_brighten;
$col_make =         $col_cyan;
$col_makeinfo =     $col_green.$col_brighten;
$col_distcc =       $col_blue.$col_brighten;
$col_filename =     $col_brown;
$col_linenum =      $col_cyan;
$col_trace =        $col_brown;
$col_warning =      $col_brown.$col_brighten;
$col_error =        $col_red.$col_brighten;
$error_highlight =  $col_red;

$error = 0;
$cwd = getcwd;

$oldin = 'shit';
$in = '';

while (<>)
{
    $hide = 0;
    $oldin = $in;
    $in = '';

    $orgline = $thisline = $_;

    if ($thisline =~ s/^Making\ (\w+)\ in\ (.*)$/${col_make}Making $col_makeinfo$1$col_norm$col_make in $col_path$2$col_norm/x)
    {
        $in = 'make';
    }
    if ($thisline =~ s/^(make\[[0-9]+\]:\ *)/${col_make}/x)
    {
        $in = 'make';
    }
    if ($thisline =~ s/^(make)\ *([-\w]+)/${col_make}$1 $col_makeinfo$2$col_norm/x)
    {
        $in = 'make';
    }
    if ($thisline =~ s/^(make:\ *)/$col_make/x)
    {
        $in = 'make';
    }
    if ($thisline =~ s/^(Nothing.*)`(.*)'/Nothing'/x)
    {
        $in = 'make';
    }

    if ($thisline =~ s/^(distcc\[[0-9]+\]\ *)//x)
    {
        $thisline =~ s/\([^)]*\)\ *//x;
        $thisline =~ s/ERROR:/error:/x;
        $thisline =~ s/Warning:/warning:/x;
        $thisline =~ s/^(error:\ )|(warning:\ )?/${col_red}$1${col_brown}$2${col_distcc}/x;
        $in = 'distcc';
        $hide = 1;
    }
    if ($thisline =~ s/libtool:\ compile:\ //x)
    {
        if ($thisline =~ s/([^ ]+)(\.cc|\.hh|\.hxx|\.cpp|\.cxx|\.c|\.h|\.hpp)([ :])/${col_path}${col_brighten}$1$2${col_norm}$3/x)
        {
            $hide = 0;
        }
        else
        {
            $hide = 1;
        }
    }

    if ($in eq 'make')
    {
        if ($thisline =~ s/\*\*\*\ \[(.*)\]/$1/x)
        {
            # fixme detect type and put color
            #$thisline =~ s/[^ /]+//x
        }
        $thisline =~ s/\ directory\ / /x;
        $thisline =~ s/Error(\ [0-9]*)/${col_red}error$1${col_make}/x;
    }
    elsif ($in eq '')
    {
        $in = 'gcc';
    }

    if ($in eq 'gcc')
    {
        if ($thisline =~ s/(error:)/\1/x)
        {
            $error += 1;
        }

        $thisline =~ s/([^ ]+)(\.cc|\.hh|\.hxx|\.cpp|\.cxx|\.c|\.h|\.hpp)([ :])/${col_path}${col_brighten}$1$2${col_norm}$3/x;
        $thisline =~ s/:([0-9]+):/:${col_linenum}$1${col_norm}:/x;
        if ($thisline =~ s/error:/${col_error}error${col_norm}:${col_brown}/x ||
            $thisline =~ s/warning:/${col_warning}warning${col_norm}:${col_green}/x)
        {
        }
    }

    #$thisline =~ s/(\/[^ ]+)/$col_path$1$col_norm/x;
    $thisline =~ s/$cwd\/?//x;
    $thisline =~ s/$HOME/~/x;
    $thisline =~ s/`([^']*)'/$col_path$1$col_norm/x;
    $thisline =~ s/\ +/\ /x;
    $thisline =~ s/\b?\.$//x;
    $thisline =~ s/$/$col_norm/x;

    if ($hide == 0)
    {
        print "\n" if $oldin ne $in;
        print $col_green.$in.">".$col_norm."\t".$thisline;
    }
}

print $col_norm;
exit 255 if $error > 255;
exit $error;
