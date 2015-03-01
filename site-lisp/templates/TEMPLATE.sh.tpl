#!/bin/bash

## Copyright (C) (>>>YEAR<<<) (>>>USER_NAME<<<)
##
## Author: (>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>
## Date: (>>>VC_DATE<<<)
## License: (>>>LICENSE<<<)


function main () {

    (>>>POINT<<<)

}


signals="SIGHUP SIGTERM SIGQUIT SIGINT SIGKILL SIGSTOP EXIT"
for signal in $signals;do
    trap "{ echo \"Caught ${signal}.\"; }" "$signal"
done


main "${@:1}"
