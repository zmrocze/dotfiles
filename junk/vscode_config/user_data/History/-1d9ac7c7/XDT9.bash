
set -x -v
echo -e '#!/bin/sh\n\nformat check' > "fp" \
  && chmod +x "fp"
set +x +v