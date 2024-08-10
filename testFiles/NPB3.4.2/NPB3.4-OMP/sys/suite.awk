BEGIN { SMAKE = "make" } {
  if ($1 !~ /^#/ &&  NF > 1) {
    printf "cd `echo %s|tr '[a-z]' '[A-Z]'`; %s clean;", $1, SMAKE;
    printf "%s CLASS=%s", SMAKE, $2;
    if (NF > 2) {
      printf " VERSION=%s", $3;
    }
    printf "; cd ..\n";
  }
}
