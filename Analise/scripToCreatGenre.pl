#!/usr/bin/perl
#!/usr/local/bin/perl on Unix like systems is called the "shebang" line. This is normally:
my $file = 'movie_metadataVirgula.csv';
my @data;
open(my $fh, '<', $file) or die "Can't read file '$file' [$!]\n";
while (my $line = <$fh>) {
    chomp $line;
    my @fields = split(/,/, $line);
    push @data, \@fields;
}