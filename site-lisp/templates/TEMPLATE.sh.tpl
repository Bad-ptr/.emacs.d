#!/bin/bash

## Copyright (C) (>>>YEAR<<<) (>>>USER_NAME<<<)
##
## Author: (>>>USER_NAME<<<) ((>>>USER_NICKNAME<<<)) <(>>>USER_MAIL<<<)>
## Date: (>>>VC_DATE<<<)
## License: (>>>LICENSE<<<)


# return status of failed command in pipe chain
#set -o pipefail
# substitute unset variable = error
#set -u
# exit if any command fails
#set -e
# print shell lines as they are read
#set -v
# print commands with arguments as they are executed
#set -x


NEED_ROOT=$'(>>>NEED_ROOT<<<)'

if [ -n "$NEED_ROOT" ]; then
	if [[ $EUID -ne 0 ]]; then
		exec sudo /bin/bash "$0" "$@"
	fi
fi


cleanup () {
	echo "Caught $1 signal."
}

signals='HUP TERM QUIT KILL STOP EXIT'
for signal in $signals;do
	trap "cleanup ${signal}" "$signal"
done
trap 'cleanup INT; trap - INT; kill -INT $$' INT

NEED_USER_INTERACTION='(>>>NEED_USER_INTERACTION<<<)'
X_TERMINAL_TO_USE='x-terminal-emulator'

findXtermToUse () {
	for tTu in "$X_TERMINAL_TO_USE" rxvt-unicode rxvt urxvt termit xterm gnome-terminal
	do
		X_TERMINAL_TO_USE="$tTu"
		if which "$X_TERMINAL_TO_USE"; then
			return 0
		fi
	done

	X_TERMINAL_TO_USE=x-terminal-emulator
	return 1
}

(>>>MAIN_ACTION_NAME<<<) () {
	(>>>POINT<<<)
}

help () {
	echo "Usage: $0 [help|h|quit|q|(>>>MAIN_ACTION_NAME<<<)]"
}

main () {

	FARGUMENT="$1"
	DEFAULT_ARGUMENT=(>>>DEFAULT_ARGUMENT_VALUE<<<)

	if [ -n "$FARGUMENT" ]; then
		echo 'This script takes no arguments!'
		exit 1
	fi

	if [ -z "$NEED_USER_INTERACTION" ] || [ -t 0 -a -t 1 -a -t 2 ]; then

		shift
		case "$FARGUMENT" in
			help|h )
				help
				;;
			quit|q )
				exit
				;;
			(>>>MAIN_ACTION_NAME<<<) )
				(>>>MAIN_ACTION_NAME<<<)
				;;
			* )
				TMOUT=20
				select FARGUMENT in help (>>>MAIN_ACTION_NAME<<<) quit
				do
					FARGUMENT="${FARGUMENT:-${REPLY:-$DEFAULT_ARGUMENT}}"
					exec "$0" "$FARGUMENT" "$@"
					break
				done
				exec "$0" "$DEFAULT_ARGUMENT" "$@"
				;;
			esac

	else
		if [ -n "$NEED_USER_INTERACTION" ]
		then
			if [ -n "$DISPLAY" ] && which "$X_TERMINAL_TO_USE"; then
				exec "$X_TERMINAL_TO_USE" -e "/bin/bash -c '${0} ""$@""'"
			fi
		fi
	fi

}

main "$@"
