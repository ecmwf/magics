#!/webapps/bin/perl
# (C) Copyright 1996-2016 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.


##!/usr/local/bin/perl56 
use strict;

use Data::Dumper;
use CGI;
use XML::Parser;
use Image::Magick;
use IO::Socket;
use POSIX qw(:sys_wait_h);
#use HTML::Entities;
use Time::Local;
use Time::localtime;

my %X = (
	'AElig'		=>	'A',
	'Aacute'	=>	'A',
	'Abreve'	=>	'A',
	'Acirc'		=>	'A',
	'Agrave'	=>	'A',
	'Amacr'		=>	'A',
	'Aogon'		=>	'A',
	'Aring'		=>	'A',
	'Atilde'	=>	'A',
	'Auml'		=>	'A',
	'Cacute'	=>	'C',
	'Ccaron'	=>	'C',
	'Ccedil'	=>	'C',
	'Ccirc'		=>	'C',
	'Cdot'		=>	'C',
	'Dcaron'	=>	'D',
	'Dstrok'	=>	'D',
	'Dstroke'	=>	'D',
	'Eacute'	=>	'E',
	'Ecaron'	=>	'E',
	'Ecirc'		=>	'E',
	'Edot'		=>	'E',
	'Egrave'	=>	'E',
	'Emacr'		=>	'E',
	'Eogon'		=>	'E',
	'Euml'		=>	'E',
	'Gbreve'	=>	'G',
	'Gcedil'	=>	'G',
	'Gcirc'		=>	'G',
	'Gdot'		=>	'G',
	'Hcirc'		=>	'H',
	'Hstrok'	=>	'H',
	'IJlig'		=>	'IJ',
	'Iacute'	=>	'I',
	'Icirc'		=>	'I',
	'Idot'		=>	'I',
	'Igrave'	=>	'I',
	'Imacr'		=>	'I',
	'Iogon'		=>	'I',
	'Itilde'	=>	'I',
	'Iuml'		=>	'I',
	'Jcirc'		=>	'J',
	'Kcedil'	=>	'K',
	'Lacute'	=>	'L',
	'Lcaron'	=>	'L',
	'Lcedil'	=>	'L',
	'Lmidot'	=>	'L',
	'Lstrok'	=>	'L',
	'Lstroke'	=>	'L',
	'ENG'           =>      'N',
	'Nacute'	=>	'N',
	'Ncaron'	=>	'N',
	'Ncedil'	=>	'N',
	'Ntilde'	=>	'N',
	'OElig'		=>	'OE',
	'Oacute'	=>	'O',
	'Ocirc'		=>	'O',
	'Odblac'	=>	'O',
	'Ograve'	=>	'O',
	'Omacr'		=>	'O',
	'Oslash'	=>	'O',
	'Otilde'	=>	'O',
	'Ouml'		=>	'O',
	'Racute'	=>	'R',
	'Rcaron'	=>	'R',
	'Rcedil'	=>	'R',
	'Sacute'	=>	'S',
	'Scaron'	=>	'S',
	'Scedil'	=>	'S',
	'Scirc'		=>	'S',
	'Tcaron'	=>	'T',
	'Tcedil'	=>	'T',
	'Tstrok'	=>	'T',
	'Uacute'	=>	'U',
	'Ubreve'	=>	'U',
	'Ucirc'		=>	'U',
	'Udblac'	=>	'U',
	'Ugrave'	=>	'U',
	'Umacr'		=>	'U',
	'Uogon'		=>	'U',
	'Uring'		=>	'U',
	'Utilde'	=>	'U',
	'Uuml'		=>	'U',
	'Wcirc'		=>	'W',
	'Yacute'	=>	'Y',
	'Ycirc'		=>	'Y',
	'Yuml'		=>	'Y',
	'Zacute'	=>	'Z',
	'Zcaron'	=>	'Z',
	'Zdot'		=>	'Z',
	'aacute'	=>	'a',
	'abreve'	=>	'a',
	'acirc'		=>	'a',
	'aelig'		=>	'a',
	'agrave'	=>	'a',
	'amacr'		=>	'a',
	'Dstroke'	=>	'D',
	'Lstroke'	=>	'L',
	'cedilla'	=>	'c',
	'lstroke'	=>	'l',
	'aogon'		=>	'a',
	'aring'		=>	'a',
	'atilde'	=>	'a',
	'auml'		=>	'a',
	'cacute'	=>	'c',
	'ccaron'	=>	'c',
	'ccedil'	=>	'c',
	'ccirc'		=>	'c',
	'cdot'		=>	'c',
	'dcaron'	=>	'd',
	'dotlessi'	=>	'i',
	'dstrok'	=>	'd',
	'eacute'	=>	'e',
	'ecaron'	=>	'e',
	'ecirc'		=>	'e',
	'edot'		=>	'e',
	'egrave'	=>	'e',
	'emacr'		=>	'e',
	'eogon'		=>	'e',
	'euml'		=>	'e',
	'fnof'		=>	'f',
	'gacute'	=>	'g',
	'gbreve'	=>	'g',
	'gcirc'		=>	'g',
	'gdot'		=>	'g',
	'hcirc'		=>	'h',
	'hstrok'	=>	'h',
	'iacute'	=>	'i',
	'icirc'		=>	'i',
	'igrave'	=>	'i',
	'ijlig'		=>	'ij',
	'imacr'		=>	'i',
	'inodot'	=>	'i',
	'iogon'		=>	'i',
	'itilde'	=>	'i',
	'iuml'		=>	'i',
	'jcirc'		=>	'j',
	'kcedil'	=>	'k',
	'kgreen'	=>	'k',
	'lacute'	=>	'l',
	'lcaron'	=>	'l',
	'lcedil'	=>	'l',
	'lmidot'	=>	'l',
	'lstrok'	=>	'l',
	'lstroke'	=>	'l',
	'eng'           =>      'n',
	'nacute'	=>	'n',
	'napos'		=>	'n',
	'ncaron'	=>	'n',
	'ncedil'	=>	'n',
	'ntilde'	=>	'n',
	'oacute'	=>	'o',
	'ocirc'		=>	'o',
	'odblac'	=>	'o',
	'oelig'		=>	'oe',
	'ograve'	=>	'o',
	'omacr'		=>	'o',
	'oslash'	=>	'o',
	'otilde'	=>	'o',
	'ouml'		=>	'o',
	'racute'	=>	'r',
	'rcaron'	=>	'r',
	'rcedil'	=>	'r',
	'sacute'	=>	's',
	'scaron'	=>	's',
	'scedil'	=>	's',
	'scirc'		=>	's',
	'sup1'		=>	'1',
	'szlig'		=>	's',
	'tcaron'	=>	't',
	'tcedil'	=>	't',
	'tstrok'	=>	't',
	'uacute'	=>	'u',
	'ubreve'	=>	'u',
	'ucirc'		=>	'u',
	'udblac'	=>	'u',
	'ugrave'	=>	'u',
	'umacr'		=>	'u',
	'uogon'		=>	'u',
	'uring'		=>	'u',
	'utilde'	=>	'u',
	'uuml'		=>	'u',
	'wcirc'		=>	'w',
	'yacute'	=>	'y',
	'ycirc'		=>	'y',
	'yuml'		=>	'y',
	'zacute'	=>	'z',
	'zcaron'	=>	'z',
	'zdot'		=>	'z'
);





$ENV{MAGPLUS_HOME}    = $ARGV[1];
$ENV{LD_LIBRARY_PATH} = "$ENV{MAGPLUS_HOME}/lib:$ENV{LD_LIBRARY_PATH}";


my $ROOT = qw(/webapps/epsgrams/data/);
my $port = $ARGV[0];

sub REAPER {
	print "$$: SIG{CHLD}\n";
	1 until(waitpid(-1,WNOHANG) <= 0);
	$SIG{CHLD} = \&REAPER;
}



sub calcul_time {
    my ($basetime, $deltahour, $deltamin) = @_;
    my ($vyear, $vmonth, $vday, $vhour) =$basetime=~m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
    my $time = timelocal(0,0,$vhour, $vday, $vmonth-1, $vyear-1900);
    my $delta=              0+
                $deltamin*60 +
                $deltahour*60*60;
    my $newtime = $time+ $delta;
    my $tm = localtime($newtime);
    my $year = $tm->year+1900;
    my $month = $tm->mon+1;  if ($month<10) { $month="0$month";} ;
    my $day =$tm->mday; if ($day<10) { $day="0$day";} ;
    my $hour = $tm->hour; if ($hour<10) { $hour="0$hour";} ;
    my $min = $tm->min; if ($min<10) { $min = "0$min";};
    return "$year$month$day$hour";
}

# $SIG{CHLD} = \&REAPER;

my $server = IO::Socket::INET->new(
		LocalPort => $port,
		Reuse     => 1,
		Listen    => 10,
		Type      => SOCK_STREAM
		);


for(;;)
{
#	$SIG{CHLD} = "IGNORE";
	my $client = $server->accept();
#	$SIG{CHLD} = \&REAPER;

	next unless(defined $client);


	my $pid = fork;

	if($pid)
	{
		waitpid($pid,0);
		close($client);
		next;
	}

	unless(defined $pid)
	{
#	print $client "ERROR: fork: $!\n";
		close($client);
		warn "fork: $!";
		next;
	}	

	# Double fork
	if(fork) {
		exit(0);
	}

	close($server);

	alarm(15);

	eval {
		local $/;
		my $request = <$client>;
		my $VAR1;
		eval $request;
		#print Dumper($$,$VAR1);
		my $x = dispatch($VAR1,$client);
		print $client "REPLY\n";
		print $client Dumper($x);

	};

	close($client);
	exit(0);

}

sub dispatch {
	my ($request,$soc) = @_;

	my $result = {};

	eval {


# print Dumper($VAR1);
		$request->{expver} = "0001"    unless($request->{expver});
		$request->{database}   = "10_days" unless($request->{database});

		my $action = $request->{action};
		$result = main->$action($request,$soc);


	};


	if($@)
	{
		$result = { error => $@ }	;
		print STDERR $@;
	}

	#print Dumper($$,$result);

	return $result;

}



sub date_list {
	my ($self,$args,$soc) = @_;

	my $expver;
	my $database;
	my $list = {};

	opendir(ROOT,$ROOT);
	while($database = readdir(ROOT))
	{
		next  unless($database =~ /^\d\d_days/)  ;

		opendir(BASE,"$ROOT/$database");
		while($expver = readdir(BASE))
		{

			next  unless($expver =~ /^\d\d\d\d$/)  ;

			my %x;
			my $x;

			my $latest = readlink("$ROOT/$database/$expver/latest");
			$latest = 999999999999 unless($latest);

			opendir(DIR,"$ROOT/$database/$expver") or die "$ROOT/$args->{expver}: $!";
			while($x = readdir(DIR))
			{ 
				next unless($x =~ /^(\d\d\d\d\d\d\d\d)(\d\d)/)	;
				my $date = "$1$2";
				$x{$date}++ if($date <= $latest);
			}
			closedir(DIR);
			$list->{$database}->{$expver} = { dates => [sort keys %x], latest => $latest };
		}
		closedir(BASE);
	}
	closedir(ROOT);

	return $list;
}

sub last_thursday {
        my ($basedate) = @_;

    my ($vyear, $vmonth, $vday, $vhour) =$basedate=~m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)/;
        my $time = timelocal(0,0,$vhour, $vday, $vmonth-1, $vyear-1900);
    my $tm = localtime($time);
    my $wday = $tm->wday;
	my $delta;
    
    if ($wday < 4){
        $delta = ($wday-4)+7;
    }
    else {
        $delta = $wday-4;
    }
    $delta= $delta*24*60*60 + $vhour*60*60;
    

        my $newtime = $time-$delta;
        my $tm = localtime($newtime);

    
    my $year = $tm->year+1900;
        my $month = $tm->mon+1;  if ($month<10) { $month="0$month";} ;
        my $day =$tm->mday; if ($day<10) { $day="0$day";} ;
        my $hour = $tm->hour; if ($hour<10) { $hour="0$hour";} ;
        my $min = $tm->min; if ($min<10) { $min = "0$min";};

    
    return "$year$month$day$hour";
} 

sub make_eps {
	my ($self,$args,$soc) = @_;

	print Dumper($args);

	my $basetime = sprintf('%d%02d',$args->{date},$args->{time}/100);


	my $tmp = "/webapps/epsgrams/work/epsgram_wind.$$";
	my $ext = $args->{type};

	$ext = "ps" if($args->{type} eq "pdf");


	# $args->{title} =~ s/'/&#x39;/g;
	# $args->{title} =~ s/'/ /g;
	$args->{title} =~ s/'/&quot;/g;

	$args->{title}  = "" if($args->{title} eq "none");
	$args->{height} = "" if($args->{height} eq "none");

	my $fail;

	my $name = $args->{title};
	$args->{title} =~ s/\&(\w+);/$X{$1}/ge;

	my $database = $args->{database};
	my $databases = {};
    my $template="$database\_epsgram.xml";

	if ( $database eq '15_days_with_clim' ) { 
		$databases->{database_eps} = "$ROOT/15_days/$args->{expver}/$basetime";
		my $clim = last_thursday($basetime);
		$databases->{database_clim} = "$ROOT/clim_15_days/$args->{expver}/$clim";
	}

	elsif ( $database eq 'efi_distribution' ) { 
		$database = "15_days";
        $template='efi_distribution.xml';
		my $step = -12;
		$basetime = sprintf('%d%02d',$args->{date},12);
		my $validtime = sprintf('%d%02d',$args->{vdate},12);
		while ( $step  >= -120) {
			my $date = calcul_time($validtime, $step, 00);
			my $db_eps = "database_eps$step\h";
			my $db_efi = "database_efi$step\h";
		    $databases->{$db_eps} = "$ROOT/15_days/$args->{expver}/$date";
		    $databases->{$db_efi} = "$ROOT/efi/$args->{expver}/$date";
			$step = $step-12;
        }
		my $clim = last_thursday($basetime);
		$databases->{database_clim} = "$ROOT/clim_15_days/$args->{expver}/$clim";
	
    }
	else {
		$databases->{database} = "$ROOT/$database/$args->{expver}/$basetime";

  }		

	open(OUT,">$tmp.in.xml") or die "$tmp.in.xml: $!";
	print OUT <<"EOF";
	<eps  
		template='$template' 
EOF
	foreach my $db (keys %{$databases} )  {
		print OUT "\t\t$db='$databases->{$db}'\n";
		print "$db='$databases->{$db}'\n";
	}

	print OUT <<"EOF";
		>

		<station 
		name       = '$args->{title}'
		latitude   = '$args->{latitude}' 
		longitude  = '$args->{longitude}'
		height     = '$args->{height}'
		psfile     = '$tmp.ps' 
		metafile   = '$tmp.xml'
		/><name>$name</name>
		</eps>
EOF
		close(OUT) or die "$tmp.in.xml: $!";

 system("$ENV{MAGPLUS_HOME}/bin/metgram","$tmp.in.xml") or $fail++;
 system("cat $tmp.in.xml>e.xml");
#	system("$ENV{MAGPLUS_HOME}/bin/epsweb $tmp.in.xml 1>/dev/null 2>/dev/null");
#system("$ENV{MAGPLUS_HOME}/bin/epsweb $tmp.in.xml 1>&2");
		#system("cat $tmp.in.xml");

# if($args->{type} eq "pdf")
	{
		my $density = 72;
		my $rotate  = 90;
		my $file = "$tmp.ps";

		system("ps2pdf $tmp.ps $tmp.pdf");

if(1) {
		my $image = Image::Magick->new;
		$image->Read($file);
#$image->Set(Density => $density);
#    $image->Rotate(degrees => $rotate) unless $rotate == 0;
		#$image->Trim(background => '#FFFFFF');
		#$image->Border(width=>10,height=>10,color=>"#ffffff");
		$file =~ s/\.(\w+)$/\.gif/;
		$image->Write($file);
		}

	}

# system("scp","$tmp.$args->{type}","$args->{host}:$args->{path}.$args->{type}");

	foreach my $e ( qw(gif ps pdf xml) )
	{
		send_file("$tmp.$e","$args->{path}.$e",$soc);
	}

	my $x = { foo=> 'bar'};
	eval {
		#system("cat $tmp.xml");
		$x = XML::Parser->new(Style => 'Objects')->parsefile("$tmp.xml");
		#print Dumper($x);
	};


	unlink("$tmp.ps");
	unlink("$tmp.pdf");
	unlink("$tmp.gif");
	unlink("$tmp.xml");
	unlink("$tmp.in.xml");

	return $x;

}

sub send_file {
	my ($src,$target,$soc) = @_;
	
	my $size = -s $src;
	return unless($size);

	open(IN,"<$src") or return;

	print $soc "FILE\n$size\n$target\n";
	print  "FILE $size $target\n";

	my $buf;
	my $buflen = 64*1024;
	my $read; 
	my $total = 0;
	my $w;

	while(($read = sysread(IN,$buf,$buflen)) > 0)
	{
		 $w = syswrite($soc,$buf,$read);
		 print STDERR "WRITE $w $read\n";
		 $total += $read;
	}

	close(IN);
	print "SENT $total\n";
}



