#!/bin/bash

## Copyright (C) (>>>YEAR<<<) (>>>USER_NAME<<<)
##
## Author: (>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>
## Date: (>>>VC_DATE<<<)
## License: (>>>LICENSE<<<)


main () {

    (>>>POINT<<<)

}


signals="HUP TERM QUIT KILL STOP EXIT"
for signal in $signals;do
    trap 'echo "Caught ${signal} signal."' "$signal"
done
trap 'echo "Caught INT signal."; trap - INT; kill -INT $$' INT


main "$@"
