use XML::Parser;
use Data::Dumper;


$in = shift;

$p1 = new XML::Parser(Style => "Tree");
$x = $p1->parsefile($in);

$y = process({}, $x->[0],$x->[1]);

$y = $y->{magics};


# Patche


if(exists $y->{page}) {
    if(exists $y->{page}->{nopageid}) {
        delete $y->{page}->{nopageid};
        $y->{page}->{_order} = [map { $_ eq "nopageid" ? "page_id" : $_ } @{$y->{page}->{_order}}];
        $y->{page}->{page_id} = "off";
    }
}

if(exists $y->{drivers}) {
    my @d;
    foreach my $d ( keys %{$y->{drivers}} )
    {
        unless($d =~ /^_/) {
            $y->{drivers}->{$d}->{format} = $d;
            unshift @{$y->{drivers}->{$d}->{_order}}, "format";
            push @d, $y->{drivers}->{$d};
    }
    }
    $y->{drivers} = \@d;
}

json($y);

sub json {
    my ($r, $depth) = @_;
    print "  " x $depth;
    print "{\n";

    my %seen  = ( "_order" => 1 );
    my @order = @{$r->{_order}};
    my @o;
    foreach my $k ( @order ) {
        unless($seen{$k})
        {
            $seen{$k}++;
            push @o, $k;
        }
    }

    my $count = @o;
    my $i   = 0;
    foreach my $k ( @o ) {
        print "  " x ($depth + 1);
        print "\"$k\":";
        my $x = $r->{$k};
        if(ref($x) =~ /ARRAY/)
        {
            print "[\n";
            my $cnt = @{$x};
            my $j   = 0;
            foreach my $o ( @{$x} )
            {
                json($o,$depth + 2);
                if($j++ != $cnt-1)
                {
                    #print "  " x ($depth + 2);
                    print ",\n";
                }
            }
            print "\n";
            print "  " x ($depth + 1);
            print "]\n";
        }
        elsif(ref($x) =~ /HASH/) {
            print "\n";
            json($x,$depth+1);
        }
        else
        {
            print "\"$x\"";
        }
        if($i++ != $count-1)
        {
            print ",\n";
        }
        else
        {
            print "\n";
        }
    } 
    print "  " x $depth;
    print "}";

}



sub process {
    my ($root, $tag,$kids, $depth) = @_;
    my %att = %{$kids->[0]};
    my $r;

    $root->{_order} = [] unless(exists $root->{_order});
    push @{$root->{_order}}, $tag;

    if($root->{$tag}) {
        $r = \%att;
       if(ref($root->{$tag}) =~ /ARRAY/) {
           push @{$root->{$tag}}, $r;
       }
       else
       {
           $root->{$tag} = [$root->{$tag}, $r];
       }

    }
    else {
        $r = $root->{$tag} = \%att;
    }

    $r->{_order} = [] unless(exists $r->{_order});
    foreach my $k ( sort keys %att ) {
        push @{$r->{_order}}, $k; 
    }


    for(my $i = 1; $i < @{$kids} ; $i += 2)
    {
            if(ref($kids->[$i+1]))
            {
                    process($r, $kids->[$i],$kids->[$i+1], $depth+1);
            }
            else {
                my $text = $kids->[$i+1];
                # $root->{_ctext} = $text;
                #print "->$text\n";
            }
    }

    return $root;


}
#print Dumper($x);


