#!/bin/bash

## Copyright (C) (>>>YEAR<<<) (>>>USER_NAME<<<)
##
## Author: (>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>
## Date: (>>>VC_DATE<<<)
## License: (>>>LICENSE<<<)

cleanup () {
    echo "Caught $1 signal."
}

signals="HUP TERM QUIT KILL STOP EXIT"
for signal in $signals;do
    trap "cleanup ${signal}" "$signal"
done
trap 'cleanup INT; trap - INT; kill -INT $$' INT


main () {

    (>>>POINT<<<)

}

main "$@"
