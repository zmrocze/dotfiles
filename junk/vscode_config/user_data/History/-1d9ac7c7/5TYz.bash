fp=$(git rev-parse --git-path hooks)/pre-commit
if test ! -e "$fp"
then
  set -x
  echo -e '#!/bin/sh\n\n,format check' > "$fp" \
    && chmod +x "$fp"
  set +x
fi