#!/bin/env bash

tile=$(cat input/3)
tile_width=$(w=$(head -n 1 input/3 | wc -c); ((w--)); echo $w)
example=$(cat input/3.example)
example_width=$(w=$(head -n 1 input/3.example | wc -c); ((w--)); echo $w)

function char_at {
    local line="$1"
    local column="$2"
    local length="$(echo $line | wc -c)"
    if [ $column -eq 0 ]; then ((column=length-1)); fi
    echo -n " ($column)" >&2
    echo "$line" | cut -b $column
}

function line_at {
    local tile="$1"
    local row="$2"
    echo "$tile" | head -n $row | tail -n 1
}

function tree_at {
    [ "$(char_at "$1" "$2")" = "#" ]
}

function count_trees {
    width="$1"
    input="$2"
    inc="$3"
    skip="$4"

    char=1
    skipc=0
    trees=0

    echo "$input" | while read line; do
        if [ "$skipc" -gt 0 ]; then
            ((skipc--));
        else
            skipc=$skip
            echo -n "$char" >&2
            if tree_at "$line" "$char"; then echo ' X' >&2;((trees++)); else echo ' O' >&2; fi
            ((char=($char + $inc) % $width))
            echo $trees
        fi
    done | tail -n 1
}

example() {
    t1=$(count_trees "$example_width" "$example" 1 0 2>/dev/null)
    t2=$(count_trees "$example_width" "$example" 3 0 2>/dev/null)
    t3=$(count_trees "$example_width" "$example" 5 0 2>/dev/null)
    t4=$(count_trees "$example_width" "$example" 7 0 2>/dev/null)
    t5=$(count_trees "$example_width" "$example" 1 1 2>/dev/null)
    ((ans=t1*t2*t3*t4*t5))
    echo $ans
}

star_one() {
  echo -n "answer 1: "
  count_trees "$tile_width" "$tile" 3 0 2>/dev/null  
}

star_two() {
    echo -n "answer 2: "
    t1=$(count_trees "$tile_width" "$tile" 1 0 2>/dev/null)
    t2=$(count_trees "$tile_width" "$tile" 3 0 2>/dev/null)
    t3=$(count_trees "$tile_width" "$tile" 5 0 2>/dev/null)
    t4=$(count_trees "$tile_width" "$tile" 7 0 2>/dev/null)
    t5=$(count_trees "$tile_width" "$tile" 1 1 2>/dev/null)
    ((ans=t1*t2*t3*t4*t5))
    echo $ans
}

star_one
star_two
#example

