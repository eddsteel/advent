#!/usr/bin/env perl
use strict;
use warnings;

$/="\n\n";
my @records;
open(FILE, "<./input/6");
while (<FILE>) {
    push(@records, $_);
}
close(FILE);

sub star_one() {
    my $total=0;
    foreach my $record (@records) {
        $total += count_uniq($record);
    }
    print($total);
}

sub star_two() {
    my $total=0;
    foreach my $record (@records) {
        $total += count_common($record);
    }
    print($total);
}

sub count_uniq {
    my $str = $_[0];
    $str =~ tr/\n//d;
    my %count_of;
    foreach my $character(split('', $str)) {
        $count_of{$character}++;
    }

    return scalar keys %count_of;
}

sub count_common {
    my $str = $_[0];
    $str =~ s/^\n|\n$//g;
    my $people = $str =~ tr/\n//d;
    $people++;
    my %count_of;
    foreach my $character(split('', $str)) {
        $count_of{$character}++;
    }
    my $result = 0;
    foreach my $char(keys %count_of) {
        if ($count_of{$char} == $people) {
            $result++;
        }
    }
    return $result;
}

sub examples() {
    print("3=");
    print(count_uniq("abc\n"));
    print("\n3=");
    print(count_uniq("a\nb\nc\n"));
    print("\n3=");
    print(count_uniq("ab\nac\n"));
    print("\n1=");
    print(count_uniq("a\na\na\na\n"));
    print("\n1=");
    print(count_uniq("b\n"));
    print("\n3=");
    print(count_common("abc\n"));
    print("\n0=");
    print(count_common("a\nb\nc\n"));
    print("\n1=");
    print(count_common("ab\nac\n"));
    print("\n1=");
    print(count_common("a\na\na\na\n"));
    print("\n1=");
    print(count_common("b\n"));
    print("\n");
}

# examples();
# exit;

print("Answer 1: ");
star_one();
print("\nAnswer 2: ");
star_two();
print("\n");
