"PART 1"

priorityList := #(
    $a $b $c $d $e $f $g $h $i $j $k $l $m $n $o $p $q $r $s $t $u $v $w $x $y $z
    $A $B $C $D $E $F $G $H $I $J $K $L $M $N $O $P $Q $R $S $T $U $V $W $X $Y $Z
).

dir := FileSystem disk workingDirectory.
stream := (dir / '/path/to/day3-input.txt' ) readStream.
rawInput := stream upToAll: 'EOF'.
stringList := rawInput trim splitOn: (String lf).

firstSumList := stringList collect: [ :s |
	midIdx := s size / 2.
	leftSet := Set newFrom: (s first: midIdx).
	rightSet := Set newFrom: (s last: midIdx).
	charInCommonSet := leftSet intersection: rightSet.
	charValue := charInCommonSet collect: [ :c | priorityList indexOf: c ].
	charValue max.
].

solution1 := firstSumList sumNumbers. "7737"
Transcript show: solution1.

"PART 2"

indexedList := stringList collectWithIndex: [ :s :i | { s . i } ].
groupedList := indexedList groupByRuns: [ :e |
	idx := (e at: 2) + 1.
	(idx / 3) rounded.
	].

secondSumList := groupedList collect: [ :t |
	firstSet := Set newFrom: ((t at: 1) at: 1).
	secondSet := Set newFrom: ((t at: 2) at: 1).
	thirdSet := Set newFrom: ((t at: 3) at: 1).
	charInCommonSet := firstSet intersection: secondSet.
	charInCommonSet := charInCommonSet intersection: thirdSet.
	charValue := charInCommonSet collect: [ :c | priorityList indexOf: c ].
	charValue max.
].

solution2 := secondSumList sumNumbers. "2697"
Transcript show: solution2.
