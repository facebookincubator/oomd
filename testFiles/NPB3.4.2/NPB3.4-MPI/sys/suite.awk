BEGIN { SMAKE = "make" } {
  if ($1 !~ /^#/ &&  NF > 1) {
    printf "cd `echo %s|tr '[a-z]' '[A-Z]'`; %s clean;", $1, SMAKE;
    printf "%s CLASS=%s", SMAKE, $2;
    if ( NF > 2 ) {
      if ( $3 ~ /^blk/ ||  $3 ~ /^BLK/ ) {
        printf " VERSION=%s", $3;
        if ( NF > 3 ) {
          printf " SUBTYPE=%s", $4;
        }
      } else {
        printf " SUBTYPE=%s", $3;
        if ( NF > 3 ) {
          printf " VERSION=%s", $4;
        }
      }
    }
    printf "; cd ..\n";
  }
}
